(require 'async)
(require 'cl-lib)
(require 'seq)

(defgroup ivy-hoogle-appearance nil
  "Ivy Hoogle Appearance.")

(defcustom ivy-hoogle-delay-ms 200
  "TODO"
  :type 'integer)

(defcustom ivy-hoogle-num-candidates 20
  "TODO"
  :type 'integer)

(defcustom ivy-hoogle-use-haskell-fontify t
  "When non-nil, use haskell-mode fontification to display candidates"
  :type 'boolean)

(defface ivy-hoogle-candidate-source-face
  '((t :inherit shadow))
  "Face used to display the candidate source (package and module if
available)"
  :group 'ivy-hoogle-appearance)

(defface ivy-hoogle-candidate-face
  '((t :inherit default))
  "Face used to display the candidate when
`ivy-hoogle-use-haskell-fontify' is not `t'"
  :group 'ivy-hoogle-appearance)

(cl-defstruct ivy-hoogle-source
  url
  package
  package-url
  module
  module-url)

(cl-defstruct ivy-hoogle-result
  "TODO"
  item
  sources
  doc-html

  formatted
  width)

(defmacro ivy-hoogle-define-candidate-properties (&rest names)
  (let ((result (mapcan (lambda (name)
                          (let ((fn-name (intern (concat "ivy-hoogle-candidate-" (symbol-name name)))))
                            `((defun ,fn-name (candidate)
                                (get-text-property 0 ',name candidate))

                              (gv-define-setter ,fn-name (val x)
                                `(let ((tmp ,val))
                                   (progn (put-text-property 0 1 ',',name tmp ,x)
                                          tmp))))))
                        names)))
    `(progn ,@result)))

(ivy-hoogle-define-candidate-properties formatted result)

(defun make-ivy-hoogle-candidate (result)
  (let ((item (ivy-hoogle-result-item result)))
    (setf (ivy-hoogle-candidate-result item) result)
    item))

(defvar ivy-hoogle--timer nil)
(defvar ivy-hoogle--history nil)
(defvar ivy-hoogle--cache (make-hash-table :test 'equal))
(defvar ivy-hoogle--process-query nil)
(defvar ivy-hoogle--process nil)

(defun ivy-hoogle--group-by (elems key-fn)
  (let ((groups (make-hash-table :test #'equal))
        (keys (mapcar key-fn elems)))
    (cl-loop for (key . elem) in (cl-mapcar #'cons keys elems)
             do
             (let ((key-elems (gethash key groups)))
               (puthash key (cons elem key-elems) groups)))
    (cl-loop for key in keys with result = nil
             do
             (let ((group (nreverse (gethash key groups))))
               (when group
                 (remhash key groups)
                 (push (cons key group) result)))
             finally return (nreverse result))))

(defun ivy-hoogle--format-sources (sources)
  (let ((sources-by-package (ivy-hoogle--group-by sources #'ivy-hoogle-source-package)))
    (cl-loop for (package . package-sources) in sources-by-package
             when (not (null package))
             collect (let ((modules (mapcar #'ivy-hoogle-source-module package-sources)))
                       (setq modules (seq-remove #'null modules))
                       (string-join (cons package modules) " "))
             into result
             finally return (string-join result ", "))))

(defun ivy-hoogle--source-lessp (source-a source-b)
  (let ((package-a (ivy-hoogle-source-package source-a))
        (package-b (ivy-hoogle-source-package source-b))
        (module-a (ivy-hoogle-source-module source-a))
        (module-b (ivy-hoogle-source-module source-b)))
    (if (string-equal package-a package-b)
        (string-lessp module-a module-b)
      (string-lessp package-a package-b))))

(defun ivy-hoogle--merge-results (results)
  (let ((result (car results))
        (sources (apply #'append (mapcar #'ivy-hoogle-result-sources results))))
    (setq sources (sort sources #'ivy-hoogle--source-lessp))
    (setf (ivy-hoogle-result-sources result) sources)
    result))

(defun ivy-hoogle--group-results (results)
  (let* ((key-fn (lambda (result)
                   (cons (ivy-hoogle-result-item result)
                         (ivy-hoogle-result-doc-html result))))
         (groups (ivy-hoogle--group-by results key-fn)))
    (cl-loop for (_ . group) in groups
             collect (ivy-hoogle--merge-results group))))

(defun ivy-hoogle--set-candidates (candidates)
  ;; TODO: how do I avoid using an internal variable
  (setq ivy--old-cands candidates))

(defun ivy-hoogle--get-candidates nil
  ivy--old-cands)

(defun ivy-hoogle--shorten (str width)
  (let ((len (length str)))
    (cond ((>= width len) str)
          ((< width 10) "")
          (t (concat (substring str 0 (- width 1)) "…")))))

(defun ivy-hoogle--display-candidate (candidate)
  (let ((result (ivy-hoogle-candidate-result candidate)))
    (if (null result)
        candidate
      (unless (ivy-hoogle-candidate-formatted candidate)
        (let* ((item (ivy-hoogle-result-item result))
               (sources (ivy-hoogle--format-sources (ivy-hoogle-result-sources result)))
               (formatted (if (not ivy-hoogle-use-haskell-fontify)
                              (ivy--add-face item 'ivy-hoogle-candidate-face)
                            (require 'haskell-font-lock)
                            (haskell-fontify-as-mode item 'haskell-mode))))
          (put-text-property 0 1 'sources sources formatted)
          (setf (ivy-hoogle-candidate-formatted candidate) formatted)))
      (copy-sequence (ivy-hoogle-candidate-formatted candidate)))))

(defun ivy-hoogle--format-candidate (width candidate)
  (let ((sources (get-text-property 0 'sources candidate)))
    (if (null sources)
        candidate
      (let* ((min-spaces 4)
             (width-remaining (- width (length candidate) min-spaces)))
        (setq sources (ivy-hoogle--shorten sources width-remaining))
        (if (string-empty-p sources)
            candidate
          (let ((num-spaces (+ (- width-remaining (length sources)) min-spaces)))
            (concat candidate
                    (make-string num-spaces ?\ )
                    (ivy--add-face sources 'ivy-hoogle-candidate-source-face))))))))

(defun ivy-hoogle--format-candidates (candidates)
  (let ((width (window-width)))
    (mapcar (lambda (candidate)
              (ivy-hoogle--format-candidate width candidate))
            candidates)))

(defun ivy-hoogle--format-function (candidates)
  (let ((formatted (ivy-hoogle--format-candidates candidates)))
    (ivy--format-function-generic
     (lambda (str) (ivy--add-face str 'ivy-current-match))
     (lambda (str) str)
     formatted
     "\n")))

(defun ivy-hoogle--parse-result (line)
  (let* ((parsed (json-parse-string line))
         (item (gethash "item" parsed))
         (url (gethash "url" parsed))
         (module-obj (gethash "module" parsed (make-hash-table)))
         (module (gethash "name" module-obj))
         (module-url (gethash "url" module-obj))
         (package-obj (gethash "package" parsed (make-hash-table)))
         (package (gethash "name" package-obj))
         (package-url (gethash "url" package-obj))
         (doc-html (gethash "docs" parsed)))
    (when (and item url)
      (let ((source (make-ivy-hoogle-source
                     :url url
                     :module module
                     :module-url module-url
                     :package package
                     :package-url package-url)))
        (make-ivy-hoogle-result
         :item item
         :doc-html doc-html
         :sources (list source))))))

(defun ivy-hoogle--on-finish (process)
  (let* ((output (with-current-buffer (process-buffer process) (buffer-string)))
         (lines (string-lines output t))
         results
         candidates)
    (cl-loop for line in lines
             unless (or (string-prefix-p "--" line)
                        (string-prefix-p "No results found" line))
             collect (ivy-hoogle--parse-result line) into raw-results
             finally (setq results (ivy-hoogle--group-results raw-results)))
    (setq candidates
          (if (null results)
              '("No results found")
            (mapcar #'make-ivy-hoogle-candidate results)))
    (puthash ivy-hoogle--process-query candidates ivy-hoogle--cache)
    (ivy-hoogle--set-candidates candidates)
    (ivy-update-candidates candidates)
    (ivy-hoogle--cleanup-process)))

(defun ivy-hoogle--start-hoogle (query)
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process)
  (let ((args `("search"
                "--count" ,(number-to-string ivy-hoogle-num-candidates)
                "--jsonl"
                ,query)))
    (setq ivy-hoogle--process-query query)
    (setq ivy-hoogle--process
          (apply 'async-start-process "hoogle" "hoogle" 'ivy-hoogle--on-finish args))))

(defun ivy-hoogle--queue-update (query)
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process)
  (setq ivy-hoogle--timer
        (run-with-timer
         (/ ivy-hoogle-delay-ms 1000.0)
         nil
         'ivy-hoogle--start-hoogle query)))

(defun ivy-hoogle--cleanup nil
  (clrhash ivy-hoogle--cache)
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process))

(defun ivy-hoogle--cleanup-timer nil
  (when ivy-hoogle--timer
    (cancel-timer ivy-hoogle--timer)
    (setq ivy-hoogle--timer nil)))

(defun ivy-hoogle--cleanup-process nil
  (setq ivy-hoogle--process-query nil)
  (when ivy-hoogle--process
    (delete-process ivy-hoogle--process)
    (kill-buffer (process-buffer ivy-hoogle--process))
    (setq ivy-hoogle--process nil)))

(defun ivy-hoogle--cached-candidates (query)
  (gethash query ivy-hoogle--cache))

(defun ivy-hoogle--candidates (query)
  (let ((query (string-trim query)))
    (if (equal query "")
        nil
      (let ((candidates (or (ivy-hoogle--cached-candidates query)
                            (progn (ivy-hoogle--queue-update query)
                                   (ivy-hoogle--get-candidates)))))
        (ivy-hoogle--set-candidates candidates)
        candidates))))

(defun ivy-hoogle--action (x)
  (message "%s" x))

(defun ivy-hoogle nil
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 0))
    (ivy-read
     "Hoogle: "
     #'ivy-hoogle--candidates
     :action #'ivy-hoogle--action
     :dynamic-collection t
     :require-match t
     :unwind #'ivy-hoogle--cleanup
     :history 'ivy-hoogle--history
     :caller 'ivy-hoogle)))

;; TODO: ivy-occur does not fontify matches
;; TODO: bogus highlighting in the candidate list
;; TODO: ivy-partial (TAB) simply inserts current line as completion
(ivy-configure 'ivy-hoogle
  :display-transformer-fn #'ivy-hoogle--display-candidate
  :format-fn #'ivy-hoogle--format-function
  )
