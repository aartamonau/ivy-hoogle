(require 'async)
(require 'cl-lib)
(require 'haskell-font-lock)
(require 'seq)

(defgroup ivy-hoogle-appearance nil
  "Ivy Hoogle Appearance.")

(defcustom ivy-hoogle-delay-ms 200
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

(cl-defstruct ivy-hoogle-source
  url
  package
  package-url
  module
  module-url)

(cl-defstruct ivy-hoogle-candidate
  "TODO"
  item
  sources
  doc-html

  formatted
  width)

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

(defun ivy-hoogle--merge-candidates (candidates)
  (let ((candidate (car candidates))
        (sources (apply #'append (mapcar #'ivy-hoogle-candidate-sources candidates))))
    (setq sources (sort sources #'ivy-hoogle--source-lessp))
    (setf (ivy-hoogle-candidate-sources candidate) sources)
    candidate))

(ivy-hoogle--format-candidates
 (list
  (ivy-hoogle--merge-candidates
   (list (make-ivy-hoogle-candidate
          :item "foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b"
          :doc-html "foldl"
          :sources (list (make-ivy-hoogle-source :url "url1" :package "base" :module "Prelude")))
         (make-ivy-hoogle-candidate
          :item "foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b"
          :doc-html "foldl"
          :sources (list (make-ivy-hoogle-source :url "url2" :package "base" :module "Data.List")))))))

(defun ivy-hoogle--group-candidates (candidates)
  (let* ((key-fn (lambda (candidate)
                   (cons (ivy-hoogle-candidate-item candidate)
                         (ivy-hoogle-candidate-doc-html candidate))))
         (groups (ivy-hoogle--group-by candidates key-fn)))
    (cl-loop for (_ . group) in groups
             collect (ivy-hoogle--merge-candidates group))))

(ivy-hoogle--group-candidates
 (list (make-ivy-hoogle-candidate
        :item "foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b"
        :doc-html "foldl"
        :sources (list (make-ivy-hoogle-source :url "url1" :package "base" :module "Prelude")))
       (make-ivy-hoogle-candidate
        :item "foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b"
        :doc-html "foldl"
        :sources (list (make-ivy-hoogle-source :url "url2" :package "base" :module "Data.List")))
       (make-ivy-hoogle-candidate
        :item "foldl :: (b -> a -> b) -> b -> [a] -> b"
        :doc-html "foldl for lists"
        :sources (list (make-ivy-hoogle-source :url "url3" :package "xmonad" :module "XMonad.Core")))))

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
         (sources (ivy-hoogle--format-sources (ivy-hoogle-candidate-sources candidate)))
         (min-spaces 4)
         (width-remaining (- width (length item))))
    (apply #'concat
           (haskell-fontify-as-mode item 'haskell-mode)
           (let* ((short (ivy-hoogle--shorten sources (- width-remaining min-spaces)))
                  (num-spaces (- width-remaining (length short))))
             (unless (string-empty-p short)
               (list (make-string num-spaces ?\ )
                     (ivy--add-face short 'shadow)))))))

(defun ivy-hoogle--format-candidates (candidates)
  (let ((width (window-width)))
    (mapcar (lambda (candidate)
              (cond ((stringp candidate) candidate)
                    ((equal (ivy-hoogle-candidate-width candidate) width)
                     (copy-sequence (ivy-hoogle-candidate-formatted candidate)))
                    (t (let ((formatted (ivy-hoogle--format-candidate width candidate)))
                         (setf (ivy-hoogle-candidate-formatted candidate) formatted
                               (ivy-hoogle-candidate-width candidate) width)
                         (copy-sequence formatted)))))
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
      (let ((source (make-ivy-hoogle-source
                     :url url
                     :module module
                     :module-url module-url
                     :package package
                     :package-url package-url)))
      (make-ivy-hoogle-candidate
       :item item
       :doc-html doc-html
       :sources (list source))))))

(defun ivy-hoogle--on-finish (process)
  (let* ((output (with-current-buffer (process-buffer process) (buffer-string)))
         (lines (string-lines output t))
         candidates)
    (cl-loop for line in lines
             unless (or (string-prefix-p "--" line)
                        (string-prefix-p "No results found" line))
             collect (ivy-hoogle--parse-item line) into results
             finally (setq candidates (ivy-hoogle--group-candidates results)))
    (unless candidates
      (setq candidates (list "No results found")))
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
     'ivy-hoogle--candidates
     :action 'ivy-hoogle--action
     :dynamic-collection t
     :require-match t
     :unwind 'ivy-hoogle--cleanup
     :history 'ivy-hoogle--history
     :caller 'ivy-hoogle)))
