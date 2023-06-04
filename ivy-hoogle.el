(require 'async)
(require 'cl-lib)
(require 'colir)
(require 'font-lock)
(require 'ivy)
(require 'seq)
(require 'shr)

(defgroup ivy-hoogle-appearance nil
  "Ivy Hoogle Appearance.")

(defcustom ivy-hoogle-delay-ms 200
  "TODO"
  :type 'integer)

(defcustom ivy-hoogle-num-candidates 20
  "TODO"
  :type 'integer)

(defcustom ivy-hoogle-use-haskell-fontify t
  "When non-nil, use `haskell-mode' fontification to display
candidates. If `haskell-font-lock' is unavailable, the value will
be ignored."
  :type 'boolean)

(defcustom ivy-hoogle-help-reserved-characters 10
  "Reserve these many characters in the help window when displaying
documentation for the selected candidate. The rendered
documentation will span the width of the window minus the number
of reserved characters."
  :type 'integer)

(defcustom ivy-hoogle-help-max-width nil
  "If not `nil', defines the maximum width of the documentation
rendered in the help window. When `nil', the value of `shr-width'
or, if it's not set, `shr-max-width' will be used. If the latter
is not set either, the full width of the window minus
`ivy-hoogle-help-reserved-characters' characters will be used."
  :type '(choice (integer :tag "Fixed width in characters")
                 (const :tag "Use the full width of the window" nil)))

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

(defface ivy-hoogle-doc-code-face
  '((t :inherit (fixed-pitch font-lock-function-name-face)))
  "Face used to display code blocks in the documentation buffer"
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
  doc-html)

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

(defun ivy-hoogle-candidate-p (candidate)
  (not (null (ivy-hoogle-candidate-result candidate))))

(defvar ivy-hoogle--timer nil)
(defvar ivy-hoogle--history nil)
(defvar ivy-hoogle--cache (make-hash-table :test 'equal))
(defvar ivy-hoogle--process-query nil)
(defvar ivy-hoogle--process nil)
(defvar ivy-hoogle--sync-candidates nil)
(defvar ivy-hoogle--fetch-mode 'async
  "Either 'async or 'sync. The latter will make ivy-hoogle--action
fetch candidates synchronously.")
(defvar-local ivy-hoogle--occur-initalized nil
  "Buffer local variable set in occur buffers to indicate whether
the buffer has already been initialized.")

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
             when package
             collect (let ((modules (mapcar #'ivy-hoogle-source-module package-sources)))
                       (setq modules (seq-remove #'null modules))
                       (string-join (cons package modules) " "))
             into result
             finally return (string-join result ", "))))

(defun ivy-hoogle--merge-results (results)
  (let ((result (car results))
        (sources (apply #'append (mapcar #'ivy-hoogle-result-sources results))))
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
  ;; ivy does not update this variable for dynamic collections for some
  ;; reason, so I need to cheat and do it myself
  ;;
  ;; otherwise things like ivy-resume and ivy-restrict-to-matches don't work
  (setq ivy--old-cands candidates))

(defun ivy-hoogle--get-candidates nil
  ivy--old-cands)

(defun ivy-hoogle--shorten (str width)
  (let ((len (length str)))
    (cond ((>= width len) str)
          ((< width 10) "")
          (t (concat (substring str 0 (- width 1)) "…")))))

(defun ivy-hoogle--display-candidate-set-sources (candidate sources)
  (put-text-property 0 1 'sources sources candidate))

(defun ivy-hoogle--display-candidate-get-sources (candidate)
  (get-text-property 0 'sources candidate))

(defun ivy-hoogle--display-candidate (candidate)
  (let ((result (ivy-hoogle-candidate-result candidate)))
    (if (null result)
        candidate
      (unless (ivy-hoogle-candidate-formatted candidate)
        (let* ((item (ivy-hoogle-result-item result))
               (sources (ivy-hoogle--format-sources (ivy-hoogle-result-sources result)))
               (formatted (if (or (not ivy-hoogle-use-haskell-fontify)
                                  (null (require 'haskell-font-lock nil 'noerror)))
                              (ivy--add-face item 'ivy-hoogle-candidate-face)
                            (haskell-fontify-as-mode item 'haskell-mode))))
          (ivy-hoogle--display-candidate-set-sources formatted sources)
          (setf (ivy-hoogle-candidate-formatted candidate) formatted)))
      (copy-sequence (ivy-hoogle-candidate-formatted candidate)))))

(defun ivy-hoogle--format-candidate (width candidate)
  (let ((sources (ivy-hoogle--display-candidate-get-sources candidate)))
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
  (let ((width (- (window-width)
                  ;; leave one extra column on the right when running in the
                  ;; terminal, otherwise the candidates will have a
                  ;; continuation character displayed (not quite sure why)
                  (if (display-graphic-p) 0 1))))
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

(defun ivy-hoogle--process-args (query)
  `("search"
    "--count" ,(number-to-string ivy-hoogle-num-candidates)
    "--jsonl"
    ,query))

(defun ivy-hoogle--process-read-candidates (process)
  (let* ((output (with-current-buffer (process-buffer process) (buffer-string)))
         (lines (string-lines output t)))
    (cl-loop for line in lines
             unless (or (string-prefix-p "--" line)
                        (string-prefix-p "No results found" line))
             collect (ivy-hoogle--parse-result line) into results
             finally return (mapcar #'make-ivy-hoogle-candidate
                                    (ivy-hoogle--group-results results)))))

(defun ivy-hoogle--on-finish (process)
  (let ((candidates (or (ivy-hoogle--process-read-candidates process)
                        (ivy-hoogle--no-results))))
    (puthash ivy-hoogle--process-query candidates ivy-hoogle--cache)
    (ivy-hoogle--set-candidates candidates)
    (ivy-update-candidates candidates)
    (ivy-hoogle--cleanup-process)))

(defun ivy-hoogle--start-hoogle (query)
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process)
  (setq ivy-hoogle--process-query query)
  (setq ivy-hoogle--process
        (apply 'async-start-process
               "hoogle-process"
               "hoogle"
               'ivy-hoogle--on-finish
               (ivy-hoogle--process-args query))))

(defun ivy-hoogle--call-hoogle-sync-set-candidates (process)
  (let ((candidates (ivy-hoogle--process-read-candidates process)))
    (setq ivy-hoogle--sync-candidates candidates)))

(defun ivy-hoogle--call-hoogle-sync (query)
  (let ((process (apply 'async-start-process
                        "hoogle"
                        "hoogle"
                        #'ivy-hoogle--call-hoogle-sync-set-candidates
                        (ivy-hoogle--process-args query))))
    (async-wait process)
    (delete-process process)
    (kill-buffer (process-buffer process))
    ivy-hoogle--sync-candidates))

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
        (ivy-hoogle--no-results)
      (if (eq ivy-hoogle--fetch-mode 'sync)
          (ivy-hoogle--call-hoogle-sync query)
        (let ((cached (ivy-hoogle--cached-candidates query)))
          (if cached
              cached
            (ivy-hoogle--queue-update query)
            (ivy-hoogle--updating)))))))

(defun ivy-hoogle--updating nil
  '("Updating…"))

(defun ivy-hoogle--no-results nil
  '("No results"))

(defun ivy-hoogle--re-builder (str)
  (ivy--regex-plus str))

(defun ivy-hoogle--highlight-function (str)
  ;; When ivy-restrict-to-matches is called, it resets dynamic-collection to
  ;; false. This implementation detail is used to determine whether to
  ;; highlight the matching bits in the output:
  ;;
  ;;   - when we are still in dynamic mode, don't highlight anything, since
  ;;     there's not necessarily an obvious correspondence between the input
  ;;     and the outputs
  ;;
  ;;   - once ivy-restrict-to-matches was called, we're back to pure
  ;;     text-based matching, so highlighting is actually useful
  ;;
  ;; I could not find any other way to achieve this behavior but to rely on
  ;; implementation details of ivy.
  (if (ivy-state-dynamic-collection ivy-last)
      str
    (ivy--highlight-default str)))

(defun ivy-hoogle--occur-press-and-switch ()
  (interactive)
  (let ((candidate (buffer-substring (line-beginning-position) (line-end-position))))
    (ivy-hoogle--show-doc candidate t)))

(defun ivy-hoogle--occur-press ()
  (interactive)
  (let ((candidate (buffer-substring (line-beginning-position) (line-end-position))))
    (ivy-hoogle--show-doc candidate)))

(defun ivy-hoogle--occur-click (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (ivy-hoogle--occur-press))))

(defun ivy-hoogle--occur-function (candidates)
  (setq candidates (cl-remove-if-not #'ivy-hoogle-candidate-p candidates))

  (unless ivy-hoogle--occur-initalized
    (font-lock-mode -1)
    (ivy-occur-mode)
    (use-local-map (copy-keymap ivy-occur-mode-map))
    (local-set-key [remap ivy-occur-press-and-switch] #'ivy-hoogle--occur-press-and-switch)
    (local-set-key [remap ivy-occur-press] #'ivy-hoogle--occur-press)
    (local-set-key [remap ivy-occur-click] #'ivy-hoogle--occur-click)
    (read-only-mode)

    ;; a hack to reuse the same buffer for occur results
    (let* ((buffer-name "*hoogle occur*")
           (other-buffer (get-buffer buffer-name)))
      (when other-buffer
        ;; if the old occur buffer was selected and we get it killed, this will
        ;; cause an error: ivy will try to select the buffer and that will fail;
        ;; so update the buffer in the state to prevent this
        (when (eq (ivy-state-buffer ivy-last) other-buffer)
          (setf (ivy-state-buffer ivy-last) (current-buffer)))
        (let ((window (get-buffer-window other-buffer)))
          (kill-buffer other-buffer)
          ;; if the buffer was visible, show the replacement buffer in the same
          ;; window
          (set-window-buffer window (current-buffer))))
      (rename-buffer buffer-name))

    ;; asynchronous fetching works only in the minibuffer
    (set (make-variable-buffer-local 'ivy-hoogle--fetch-mode) 'sync)

    (setq ivy-hoogle--occur-initalized t))

  (let ((inhibit-read-only t))
    (insert (format "%d candidates:\n" (length candidates)))
    (cl-loop for candidate in candidates
             do (let ((displayed (ivy-hoogle--display-candidate candidate)))
                  (add-text-properties
                   0
                   (length displayed)
                   '(mouse-face
                     highlight
                     help-echo "mouse-1: call ivy-action")
                   displayed)
                  (insert displayed ?\n)))

    ;; go to the first match
    (goto-char (point-min))
    (forward-line 1)))

(defun ivy-hoogle--render-tag-pre (dom)
  (let ((start (point))
        (inline (not (looking-at "^\s*$"))))
    (if inline
        ;; <pre> appears both as an inline tag and a block tag; in the former
        ;; case, we don't want any new lines and separators inserted
        ;;
        ;; for now, assume that <pre> is inline if there's anything on the
        ;; same line that precedes it;
        ;;
        ;; TODO: what if there's nothing before the tag, but there's something
        ;; after it?
        (shr-generic dom)
      (shr-tag-hr nil)
      (shr-tag-pre dom)
      (flush-lines "^\s*$" start (point-max))
      (goto-char (point-max))
      (shr-tag-hr nil))
    (colir-blend-face-background start
                                 (point-max)
                                 'ivy-hoogle-doc-code-face)))

(defun ivy-hoogle--render-tag-tt (dom)
  (let ((start (point)))
    (shr-tag-code dom)
    (colir-blend-face-background start (point-max) 'ivy-hoogle-doc-code-face)))

(defun ivy-hoogle--render-doc (doc)
  (let (;; render using a fixed-pitch font by default; unlike (shr-use-fonts
        ;; nil), it doesn't prevent shr form using italic font when necessary
        (shr-current-font 'fixed-pitch)
        (start (point))
        (shr-external-rendering-functions
         `((pre . ivy-hoogle--render-tag-pre)
           (tt . ivy-hoogle--render-tag-tt))))
    (insert doc)
    (goto-char start)
    (cl-loop while (< (point) (point-max))
             do (progn (forward-paragraph)
                       (insert "<p/>" ?\n)))
    (shr-render-region start (point))))

(defun ivy-hoogle--render-width nil
  "Calculate the width to use when rendering the candidate."
  (let* ((window-width (window-body-width))
         ;; the width will be the minimum of the max width or the window width
         ;; with some characters reserved for overlays
         (width (min (if (< window-width ivy-hoogle-help-reserved-characters)
                         ;; not much we can do
                         window-width
                       (- window-width
                          ;; Reserve some width for overlays rendered by minor
                          ;; modes like linum. Because of these overlays, the
                          ;; rendered text is likely to wrap around if the full
                          ;; width of the window is used.
                          ivy-hoogle-help-reserved-characters))
                     (or ivy-hoogle-help-max-width
                         shr-width
                         shr-max-width))))
    width))

(defun ivy-hoogle--render-candidate (candidate)
  (let* ((shr-width (ivy-hoogle--render-width))
         (displayed (ivy-hoogle--display-candidate candidate))
         (sources (ivy-hoogle--display-candidate-get-sources displayed))
         (result (ivy-hoogle-candidate-result candidate))
         (start (point)))
    (insert displayed ?\n ?\n)
    (when (not (string-empty-p sources))
      (insert (ivy--add-face sources 'ivy-hoogle-candidate-source-face) ?\n ?\n))
    ;; use the same font shr will use
    (add-face-text-property start (point) 'fixed-pitch)
    (ivy-hoogle--render-doc (ivy-hoogle-result-doc-html result))))

(defun ivy-hoogle--action (candidate)
  (if (not (ivy-hoogle-candidate-p candidate))
      ;; if a non-candidate got selected, like the informational "Updating" or
      ;; "No results", restart selection
      ;;
      ;; I wish I could just disallow selecting these fake candidates, but
      ;; there doesn't seem to be a way to do that
      (ivy-resume)
    (ivy-hoogle--show-doc candidate t)))

(defun ivy-hoogle--show-doc (candidate &optional select-window)
  (let ((buffer-name "*Help*")
        (help-window-select select-window))
    (with-current-buffer (get-buffer-create buffer-name)
      ;; show in a new window, unless the buffer is already visible
      (unless (get-buffer-window)
        (display-buffer (current-buffer) '(nil . ((inhibit-same-window . t)))))
      (with-help-window buffer-name
        (with-selected-window (get-buffer-window)
          (ivy-hoogle--render-candidate candidate))))))

(defun ivy-hoogle-occur ()
  "Show current candidates in an occur buffer. See `ivy-occur' for
more details."
  (interactive)
  (let ((ivy-hoogle--fetch-mode 'sync))
    (ivy-occur)))

(defun ivy-hoogle-avy ()
  "Indicate that ivy-avy does not work with ivy-hoogle"
  (interactive)
  (user-error "Ivy-hoogle does not support avy integration"))

(defun ivy-hoogle nil
  (interactive)
  (let ((map (make-sparse-keymap))
        (ivy-dynamic-exhibit-delay-ms 0))
    (cl-flet ((rebind (command replacement)
                      (cl-loop for key in (where-is-internal command ivy-minibuffer-map)
                               do (define-key map key replacement))))
      ;; ivy-avy is not supported
      (rebind #'ivy-avy #'ivy-hoogle-avy)
      ;; ivy-occur requires some special handling
      (rebind #'ivy-occur #'ivy-hoogle-occur))
    (ivy-read
     "Hoogle: "
     #'ivy-hoogle--candidates
     :action #'ivy-hoogle--action
     :dynamic-collection t
     :require-match t
     :unwind #'ivy-hoogle--cleanup
     :history 'ivy-hoogle--history
     :caller 'ivy-hoogle
     :keymap map)))

;; TODO: add links to packages and modules
;; TODO: handle links in the documentation
;; TODO: "Symbol's function definition is void" when refreshing help buffer
;; TODO: going through history is janky
;; TODO: next/previous-error in the occur window
(ivy-configure 'ivy-hoogle
  :display-transformer-fn #'ivy-hoogle--display-candidate
  :format-fn #'ivy-hoogle--format-function
  :occur #'ivy-hoogle--occur-function
  )

;; the highlight function can only be overridden by associating it with the
;; regex building function directly
(setf (alist-get 'ivy-hoogle ivy-re-builders-alist) #'ivy-hoogle--re-builder)
(setf (alist-get #'ivy-hoogle--re-builder ivy-highlight-functions-alist) #'ivy-hoogle--highlight-function)
