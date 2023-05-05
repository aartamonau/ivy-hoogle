(require 'async)
(require 'haskell-font-lock)
(require 'seq)

(defgroup ivy-hoogle-appearance nil
  "Ivy Hoogle Appearance.")

(defcustom ivy-hoogle-delay-ms 100
  "TODO"
  :type 'integer)

(defcustom ivy-hoogle-num-candidates 20
  "TODO"
  :type 'integer)

(defface ivy-hoogle-candidate-type-face
  '((t :inherit haskell-keyword-face))
  "Face used to highlight the candidate type (package, module, etc.)"
  :group 'ivy-hoogle-appearance)

(defface ivy-hoogle-candidate-name-face
  '((t :inherit haskell-type-face))
  "Face used to highlight the candidate name"
  :group 'ivy-hoogle-appearance)

(ivy-configure 'ivy-hoogle
  :format-fn #'ivy-hoogle--format-function)

(cl-defstruct ivy-hoogle-candidate
  "TODO"
  item
  url
  module
  module-url
  package
  package-url
  doc-html
  doc-text

  formatted
  width)

(defvar ivy-hoogle--timer nil)
(defvar ivy-hoogle--history nil)
(defvar ivy-hoogle--cache (make-hash-table :test 'equal))
(defvar ivy-hoogle--process-query nil)
(defvar ivy-hoogle--process nil)

(defun ivy-hoogle--set-candidates (candidates)
  ;; TODO: how do I avoid using an internal variable
  (setq ivy--old-cands candidates))

(defun ivy-hoogle--get-candidates nil
  ivy--old-cands)

(defun ivy-hoogle--shorten (str width)
  (let ((len (length str)))
    (cond ((>= width len) str)
          ((< width 10) "")
          (t (concat (substring str 0 (- width 3)) "...")))))

(defun ivy-hoogle--format-candidate (width candidate)
  (let* ((item (ivy-hoogle-candidate-item candidate))
         (package (ivy-hoogle-candidate-package candidate))
         (module (ivy-hoogle-candidate-module candidate))
         (package-and-module
          (let ((joined (string-join
                         (seq-remove 'null (list package module))
                         " ")))
            (if joined
                (concat "(" joined ")")
              "")))
         (min-spaces 4)
         (width-remaining (- width (length item))))
    (apply 'concat
           (haskell-fontify-as-mode item 'haskell-mode)
           (let* ((short (ivy-hoogle--shorten package-and-module
                                              (- width-remaining min-spaces)))
                  (num-spaces (- width-remaining (length short))))
             (unless (string-empty-p short)
               (list (make-string num-spaces ?\ )
                     (ivy--add-face short 'shadow)))))))

(defun ivy-hoogle--format-candidates (candidates)
  (let ((width (window-width)))
    (mapcar (lambda (candidate)
              (if (equal (ivy-hoogle-candidate-width candidate) width)
                  (copy-sequence (ivy-hoogle-candidate-formatted candidate))
                (let ((formatted (ivy-hoogle--format-candidate width candidate)))
                  (setf (ivy-hoogle-candidate-formatted candidate) formatted
                        (ivy-hoogle-candidate-width candidate) width)
                  (copy-sequence formatted))))
            candidates)))

(defun ivy-hoogle--format-function (candidates)
  (let ((formatted (ivy-hoogle--format-candidates candidates)))
    (ivy--format-function-generic
     (lambda (str) (ivy--add-face str 'ivy-current-match))
     (lambda (str) str)
     formatted
     "\n")))

(defun ivy-hoogle--parse-item (line)
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
      (make-ivy-hoogle-candidate
       :item item
       :url url
       :doc-html doc-html
       :module module
       :module-url module-url
       :package package
       :package-url package-url))))

(defun ivy-hoogle--on-finish (process)
  (let* ((output (with-current-buffer (process-buffer process) (buffer-string)))
         (lines (string-lines output t))
         (candidates (pcase lines
                       ('("No results found") nil)
                       (_ (cl-loop for line in lines
                                   unless (string-prefix-p "--" line)
                                   for item = (ivy-hoogle--parse-item line)
                                   collect item)))))
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

;; TODO: handle errors
(defun ivy-hoogle--run-hoogle (query)
  (let* ((args `("search"
                 "--count" ,(number-to-string ivy-hoogle-num-candidates)
                 "--jsonl"
                 ,query))
         (lines (apply 'process-lines "hoogle" args)))
    (pcase lines
      ('("No results found") nil)
      (_ (cl-loop for line in lines
                  unless (string-prefix-p "--" line)
                  for item = (ivy-hoogle--parse-item line)
                  collect item)))))

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
     'ivy-hoogle--candidates
     :action 'ivy-hoogle--action
     :dynamic-collection t
     :unwind 'ivy-hoogle--cleanup
     :history 'ivy-hoogle--history
     :caller 'ivy-hoogle)))
