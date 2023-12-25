;;; ivy-hoogle.el --- Search Hoogle using ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Aliaksei Artamonau

;; Author: Aliaksei Artamonau <aliaksiej.artamonau@gmail.com>
;; URL: https://github.com/aartamonau/ivy-hoogle
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (async "1.9") (ivy "0.13.2"))
;; Keywords: matching haskell hoogle

;; This file is part of ivy-hoogle.

;; ivy-hoogle is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ivy-hoogle is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ivy-hoogle.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `ivy-hoogle' and `ivy-hoogle-thing-at-point' that allow
;; querying Hoogle, the Haskell API search engine, using `ivy-read'-based
;; interface.
;;
;; See README.md for further details.

;;; Code:

(require 'async)
(require 'button)
(require 'cl-lib)
(require 'font-lock)
(require 'ivy)
(require 'seq)
(require 'shr)

(defgroup ivy-hoogle nil
  "Ivy interface to Hoogle, the Haskell API search engine."
  :group 'convenience)

(defgroup ivy-hoogle-faces nil
  "Faces used by `ivy-hoogle'."
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-program "hoogle"
  "Hoogle executable to use."
  :type 'file
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-delay-ms 200
  "Wait for more input this long before calling hoogle."
  :type 'integer
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-num-candidates 20
  "The maximum number of candidates to fetch for each query."
  :type 'integer
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-use-haskell-fontify t
  "When non-nil, use `haskell-mode' fontification to display candidates.

If `haskell-font-lock' is unavailable, the value is ignored."
  :type 'boolean
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-fontify-code-as-haskell t
  "When non-nil, fontify code blocks as haskell using `haskell-mode'.

Will only have effect if `ivy-hoogle-use-haskell-fontify' is non-nil."
  :type 'boolean
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-help-reserved-characters 10
  "The number of characters to reserve in the help window displaying a candidate.

The rendered documentation will span the width of the window
minus the number of reserved characters."
  :type 'integer
  :group 'ivy-hoogle)

(defcustom ivy-hoogle-help-max-width nil
  "When non-nil, maximum width a rendered candidate will use in the help window.

When nil, the value of `shr-width' or, if it's not set,
`shr-max-width' will be used.  If the latter is not set either,
the full width of the window minus
`ivy-hoogle-help-reserved-characters' characters will be used."
  :type '(choice (integer :tag "Fixed width in characters")
                 (const :tag "Use the full width of the window" nil))
  :group 'ivy-hoogle)

(defface ivy-hoogle-candidate-source-face
  '((t :inherit shadow))
  "The face used to display the candidate's source package and module."
  :group 'ivy-hoogle-faces)

(defface ivy-hoogle-candidate-face
  '((t :inherit (fixed-pitch minibuffer-prompt)))
  "The face used to display the candidate when Haskell fontification is disabled."
  :group 'ivy-hoogle-faces)

(defface ivy-hoogle-doc-code-face
  '((t :inherit (fixed-pitch font-lock-function-name-face)))
  "The face used to display code blocks in the help window.

Applied only if `ivy-hoogle-fontify-code-as-haskell' is nil or
`haskell-mode' could not be loaded."
  :group 'ivy-hoogle-faces)

(defface ivy-hoogle-doc-code-background-face
  '((((class color) (background light))
     :background "#d3d3d3" :extend t)
    (((class color) (background dark))
     :background "#333333" :extend t))
  "Background applied to code blocks in the documentation buffer."
  :group 'ivy-hoogle-faces)

(defface ivy-hoogle-doc-xref-link-face
  '((t :inherit (fixed-pitch font-lock-constant-face underline) :weight normal))
  "The face used to display links to other functions the documentation buffer."
  :group 'ivy-hoogle-faces)

(cl-defstruct ivy-hoogle--source
  "One source location where a candidate is defined."
  (url
   nil
   :documentation "A link to the candidate in this source.")
  (package
   nil
   :documentation "The name of the package where the candidate is defined.")
  (package-url
   nil
   :documentation "The URL linking to the package.")
  (module
   nil
   :documentation "The name of the module where the candidate is defined.")
  (module-url
   nil
   :documentation "The URL linking to the module."))

(cl-defstruct ivy-hoogle--result
  "A Hoogle result."
  (item
   nil
   :documentation "A string with the found candidate.")
  (unique-count
   nil
   :documentation "An integer to disambiguate candidates that are named identically,
but have different documentation. This is needed because `ivy'
does not deal with duplicates in the candidate list very well.")
  (sources
   nil
   :documentation "A list of `ivy-hoogle--source' structures for
  each module where the candidate is defined.")
  (doc-html
   nil
   :documentation "The documentation string for the candidate with occasional html
   tags."))

(defun ivy-hoogle--result-url (result)
  "Get the URL to Haddock describing RESULT."
  (pcase (ivy-hoogle--result-sources result)
    (`(,head . ,_) (ivy-hoogle--source-url head))
    (_ nil)))

(defmacro ivy-hoogle--define-candidate-properties (&rest properties)
  "Define PROPERTIES that can be attached to a candidate.

A candidate is the result of a query represented as a string
where the properties are attached to it as text properties.

For each property, this macro defines a getter function named
ivy-hoogle-candidate-<property> as well as a setter of the same
name that can be assigned to using `setf`."
  (declare (debug (&rest symbolp)))
  (let ((result (mapcan (lambda (name)
                          (let ((fn-name (intern (concat "ivy-hoogle-candidate-" (symbol-name name)))))
                            `((defun ,fn-name (candidate)
                                (get-text-property 0 ',name candidate))

                              (gv-define-setter ,fn-name (val x)
                                `(let ((tmp ,val))
                                   (progn (put-text-property 0 1 ',',name tmp ,x)
                                          tmp))))))
                        properties)))
    `(progn ,@result)))

(ivy-hoogle--define-candidate-properties formatted result)

(defun ivy-hoogle--make-candidate (result)
  "Make a candidate from RESULT.

The original result is attached to the candidate as a text
property."
  (let ((candidate (ivy-hoogle--result-item result))
        (unique-count (ivy-hoogle--result-unique-count result)))
    (setf (ivy-hoogle-candidate-result candidate) result)

    ;; Workaround: add a suffix consisting of spaces to make candidates
    ;; unique, without candidates being unique, certain ivy features like
    ;; marking/unmarking, resuming a session won't work correctly
    (when (and unique-count (> unique-count 0))
      (setq candidate
            (concat candidate
                    (make-string unique-count ?\ ))))
    candidate))

(defun ivy-hoogle--candidate-p (candidate)
  "Return non-nil if CANDIDATE is a valid `ivy-hoogle' candidate.

Valid candidates are those created by `ivy-hoogle--make-candidate'."
  (not (null (ivy-hoogle-candidate-result candidate))))

(defvar ivy-hoogle--timer nil
  "When this timer fires, the hoogle process is started.

The timer is set to `ivy-hoogle-delay-ms' milliseconds in the
future every time the user updates the query.  So this prevents
us of spawning a new hoogle process just to immediately terminate
it once the query is updated.")

(defvar ivy-hoogle--history nil
  "The history of queries is stored here.")

(defvar ivy-hoogle--cache (make-hash-table :test 'equal)
  "A cache of fetched results active for the duration of an `ivy-hoogle' call.

Once `ivy-hoogle' returns, the cache is cleaned up.")

(defvar ivy-hoogle--process-query nil
  "Stores the query to pass to the hoogle process.")

(defvar ivy-hoogle--process nil
  "Stores the async process interacting with the hoogle CLI.")

(defvar ivy-hoogle--sync-candidates nil
  "When `ivy-hoogle--fetch-mode' is \\='sync, query results will be stored here.")

(defvar ivy-hoogle--fetch-mode 'async
  "Fetch mode (either \\='async or \\='sync).

The latter will make `ivy-hoogle--action' fetch candidates
synchronously.")

(defvar-local ivy-hoogle--occur-initalized nil
  "Set in occur buffers after the buffer is initialized.")

(defun ivy-hoogle--group-by (elems key-fn)
  "Group ELEMS on the keys returned by the KEY-FN.

Return the list of pairs (key . group) preserving the order of
elements in ELEMS."
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
  "Format SOURCES grouped on the package name into a string.

The result will be of the form \"package1 Module1, Module2, package2 ...\""
  (let ((sources-by-package (ivy-hoogle--group-by sources #'ivy-hoogle--source-package)))
    (cl-loop for (package . package-sources) in sources-by-package
             when package
             collect (let ((modules (mapcar #'ivy-hoogle--source-module package-sources)))
                       (setq modules (seq-remove #'null modules))
                       (string-join (cons package modules) " "))
             into result
             finally return (string-join result ", "))))

(defun ivy-hoogle--group-results (results)
  "Group identical results in RESULTS.

Two results are considered identical if they have identical items
and documentation.  Return only unique results, but attach the
sources that contributed into each one of them."
  (let* ((key-fn (lambda (result)
                   (cons (ivy-hoogle--result-item result)
                         (ivy-hoogle--result-doc-html result))))
         (groups (ivy-hoogle--group-by results key-fn)))
    (cl-loop for (_ . group) in groups
             collect
             (let ((result (car group))
                   (sources (apply #'append (mapcar #'ivy-hoogle--result-sources group))))
               (setf (ivy-hoogle--result-sources result) sources)
               result))))

(defun ivy-hoogle--uniquify (results)
  "Add unique counters to RESULTS where necessary.

Results that have the same item will get assigned counters making
each occurrence unique.

RESULTS must have already grouped using `ivy-hoogle--group-results'."
  (let ((counts (make-hash-table :test #'equal)))
    (cl-loop for result in results
             do
             (let* ((item (ivy-hoogle--result-item result))
                    (count (or (gethash item counts) 0)))
               (setf (ivy-hoogle--result-unique-count result) count)
               (puthash item (+ 1 count) counts))
             finally return results)))

(defun ivy-hoogle--shorten (str width)
  "Truncate STR to maximum WIDTH.

Add ellipses at the end if the string was truncated."
  (let ((len (length str)))
    (cond ((>= width len) str)
          ((< width 10) "")
          (t (concat (substring str 0 (- width 1)) "…")))))

(defun ivy-hoogle--display-candidate-set-sources (candidate sources)
  "Attach SOURCES to CANDIDATE."
  (put-text-property 0 1 'sources sources candidate))

(defun ivy-hoogle--display-candidate-get-sources (candidate)
  "Get sources from CANDIDATE.

Return the sources that were previously attached by
`ivy-hoogle--display-candidate-get-sources'."
  (get-text-property
   ;; When `ivy' marks a candidate, it destructive modifies it prepending
   ;; `ivy-mark-prefix' to it. This affects where we should look for the
   ;; sources.
   (if (member candidate ivy-marked-candidates) (length ivy-mark-prefix) 0)
   'sources
   candidate))

;; Since haskell-font-lock is loaded dynamically, declare
;; haskell-fontify-as-mode to avoid byte/native-compilation warnings.
(declare-function haskell-fontify-as-mode "haskell-font-lock" (text mode))

(defun ivy-hoogle--haskell-mode-fontify (do-fontify str default-face)
  "Fontify STR using `haskell-mode' (if available) and DO-FONTIFY is t.

Otherwise apply DEFAULT-FACE to STR."
  (if (or (not do-fontify)
          (null (require 'haskell-font-lock nil 'noerror)))
      (progn (font-lock-append-text-property 0 (length str) 'face default-face str)
             str)
    (haskell-fontify-as-mode str 'haskell-mode)))

(defun ivy-hoogle--display-candidate (candidate)
  "Prepare CANDIDATE to be displayed in the minibuffer.

The item is fontified, the sources are formatted and attached to
the result."
  (let ((marked (member candidate ivy-marked-candidates))
        (result (ivy-hoogle-candidate-result candidate)))
    (if (null result)
        candidate
      (unless (ivy-hoogle-candidate-formatted candidate)
        (let* ((item (ivy-hoogle--result-item result))
               (sources (ivy-hoogle--format-sources (ivy-hoogle--result-sources result)))
               (formatted
                (ivy-hoogle--haskell-mode-fontify ivy-hoogle-use-haskell-fontify
                                                  item
                                                  'ivy-hoogle-candidate-face)))
          (ivy-hoogle--display-candidate-set-sources formatted sources)
          (setf (ivy-hoogle-candidate-formatted candidate) formatted)))
      (concat
       (when marked ivy-mark-prefix)
       (copy-sequence (ivy-hoogle-candidate-formatted candidate))))))

(defun ivy-hoogle--format-candidate (width candidate)
  "Format CANDIDATE to a string where WIDTH is the maximum width to use.

CANDIDATE must be a result of a call to `ivy-hoogle--display-candidate'.

The result is used to display CANDIDATE in the minibuffer."
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
  "Format CANDIDATES before displaying them in the minibuffer."
  (let ((width (- (window-width)
                  ;; leave one extra column on the right when running in the
                  ;; terminal, otherwise the candidates will have a
                  ;; continuation character displayed (not quite sure why)
                  (if (display-graphic-p) 0 1))))
    (mapcar (lambda (candidate)
              (ivy-hoogle--format-candidate width candidate))
            candidates)))

(defun ivy-hoogle--format-function (candidates)
  "Format CANDIDATES into a single multi-line string."
  (let ((formatted (ivy-hoogle--format-candidates candidates)))
    ;; this follows the behavior of ivy-format-function-line: the entire line
    ;; corresponding to the selected candidate is highlighted
    (ivy--format-function-generic
     (lambda (str)
       (setq str (concat str "\n"))
       ;; Don't highlight "No results"
       (if (ivy-hoogle--candidate-p str)
           (ivy--add-face str 'ivy-current-match)
         str))
     (lambda (str) (concat str "\n"))
     formatted
     "")))

(defun ivy-hoogle--parse-result (line)
  "Parse one LINE of output into a result."
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
      (let ((source (make-ivy-hoogle--source
                     :url url
                     :module module
                     :module-url module-url
                     :package package
                     :package-url package-url)))
        (make-ivy-hoogle--result
         :item item
         :unique-count 0
         :doc-html doc-html
         :sources (list source))))))

(defun ivy-hoogle--process-args (query)
  "Command-line flags and arguments to pass to the hoogle CLI for QUERY."
  `("search"
    "--count" ,(number-to-string ivy-hoogle-num-candidates)
    "--jsonl"
    ,query))

(defun ivy-hoogle--process-read-candidates (process)
  "Read query results from PROCESS."
  (let* ((output (with-current-buffer (process-buffer process) (buffer-string)))
         (lines (string-lines output t)))
    (cl-loop for line in lines
             unless (or (string-prefix-p "--" line)
                        (string-prefix-p "No results found" line))
             collect (ivy-hoogle--parse-result line) into results
             finally return (mapcar #'ivy-hoogle--make-candidate
                                    (ivy-hoogle--uniquify (ivy-hoogle--group-results results))))))

(defun ivy-hoogle--on-finish (process)
  "Read results from PROCESS, update minibuffer, clean up."
  (let ((candidates (or (ivy-hoogle--process-read-candidates process)
                        (ivy-hoogle--no-results))))
    (ivy-hoogle--cache-candidates ivy-hoogle--process-query candidates)
    ;; ivy does not update this variable for dynamic collections for some
    ;; reason, so I need to cheat and do it myself
    ;;
    ;; otherwise things like `ivy-resume' and `ivy-restrict-to-matches' don't work
    (setq ivy--old-cands candidates)
    ;; Don't preserve marks when input changes.
    (setq ivy-marked-candidates nil)
    (ivy-update-candidates candidates)
    (ivy-hoogle--cleanup-process)))

(defun ivy-hoogle--start-hoogle (query)
  "Start a new hoogle process for QUERY."
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process)
  (setq ivy-hoogle--process-query query)
  (setq ivy-hoogle--process
        ;; prevent async from prompting for a password based on matching the
        ;; command output against somewhat a arbitrary regex
        (let ((async-prompt-for-password nil))
          (apply 'async-start-process
                 "hoogle"
                 ivy-hoogle-program
                 'ivy-hoogle--on-finish
                 (ivy-hoogle--process-args query)))))

(defun ivy-hoogle--call-hoogle-sync-set-candidates (process)
  "Read results from PROCESS and store them in `ivy-hoogle--sync-candidates'."
  (let ((candidates (ivy-hoogle--process-read-candidates process)))
    (setq ivy-hoogle--sync-candidates candidates)))

(defun ivy-hoogle--call-hoogle-sync (query)
  "Fetch results for QUERY synchronously."
  (let ((process (apply 'async-start-process
                        "hoogle-sync"
                        ivy-hoogle-program
                        #'ivy-hoogle--call-hoogle-sync-set-candidates
                        (ivy-hoogle--process-args query))))
    (async-wait process)
    (delete-process process)
    (kill-buffer (process-buffer process))
    ivy-hoogle--sync-candidates))

(defun ivy-hoogle--queue-update (query)
  "Request an asynchronous update for QUERY."
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process)
  (setq ivy-hoogle--timer
        (run-with-timer
         (/ ivy-hoogle-delay-ms 1000.0)
         nil
         'ivy-hoogle--start-hoogle query)))

(defun ivy-hoogle--cancel-update nil
  "Cancel any outstanding update."
  (ivy-hoogle--cleanup-timer)
  (ivy-hoogle--cleanup-process))

(defun ivy-hoogle--cleanup nil
  "Clean up after an invocation of `ivy-hoogle'.

Clears the cache, disarms the timer, kills the update process."
  (clrhash ivy-hoogle--cache)
  (ivy-hoogle--cancel-update))

(defun ivy-hoogle--cleanup-timer nil
  "Disarm the update timer."
  (when ivy-hoogle--timer
    (cancel-timer ivy-hoogle--timer)
    (setq ivy-hoogle--timer nil)))

(defun ivy-hoogle--cleanup-process nil
  "Kill the update process."
  (setq ivy-hoogle--process-query nil)
  (when ivy-hoogle--process
    (delete-process ivy-hoogle--process)
    (kill-buffer (process-buffer ivy-hoogle--process))
    (setq ivy-hoogle--process nil)))

(defun ivy-hoogle--cached-candidates (query)
  "Fetch the cached candidates for QUERY."
  (gethash query ivy-hoogle--cache))

(defun ivy-hoogle--cache-candidates (query candidates)
  "Cached CANDIDATES for QUERY."
  (puthash query candidates ivy-hoogle--cache))

(defun ivy-hoogle--candidates (query)
  "Fetch candidates for QUERY.

Called by `ivy-read' whenever the user updates the query.  The
function will check for a cached result first.  If no such result
is found, the function will queue an update.  After there's no
more input for more than `ivy-hoogle-delay-ms' milliseconds, a
hoogle process is started and, when it's done, the results are
displayed in the minibuffer."
  (let* ((query (string-trim query))
         (cached (ivy-hoogle--cached-candidates query))
         candidates)
    (setq candidates
          (cond ((equal query "")
                 (ivy-hoogle--cancel-update)
                 ;; show "No results" message on empty input
                 (ivy-hoogle--no-results))
                (cached
                 (ivy-hoogle--cancel-update)
                 ;; return the cached result if it's present
                 cached)
                ((eq ivy-hoogle--fetch-mode 'sync)
                 ;; don't cache results here because the cache is only cleaned
                 ;; up if `ivy-hoogle--candidates' is called from the
                 ;; minibuffer, so caching would introduce a space leak
                 (ivy-hoogle--call-hoogle-sync query))
                ((equal (ivy-state-initial-input ivy-last) query)
                 ;; when `ivy-hoogle' is called with initial input, do the first
                 ;; fetch synchronously
                 (let ((candidates (ivy-hoogle--call-hoogle-sync query)))
                   ;; also cache the result
                   (ivy-hoogle--cache-candidates query candidates)))
                (t
                 ;; otherwise, fetch asynchronously
                 (ivy-hoogle--queue-update query)
                 (ivy-hoogle--updating))))

    ;; just like in `ivy-hoogle--on-finish', we need to set `ivy--old-cands'
    ;; explicitly, because ivy does not do this for dynamic collections
    (setq ivy--old-cands candidates)
    ;; Don't preserve marks when input changes.
    (setq ivy-marked-candidates nil)

    candidates))

(defun ivy-hoogle--updating nil
  "A candidate list indicating that no results are available yet."
  '("Updating…"))

(defun ivy-hoogle--no-results nil
  "A candidate list indicating that nothing was found."
  '("No results"))

(defun ivy-hoogle--re-builder (str)
  "Build a regex sequence from STR.

We need a custom regex builder function so we can associate
`ivy-hoogle--highlight-function' with it."
  (ivy--regex-plus str))

(defun ivy-hoogle--highlight-function (query)
  "Highlight the parts of a rendered candidate that match QUERY.

But do so only once `ivy-restrict-to-matches' was called."
  ;; When `ivy-restrict-to-matches' is called, it resets dynamic-collection to
  ;; false. This implementation detail is used to determine whether to
  ;; highlight the matching bits in the output:
  ;;
  ;;   - when we are still in dynamic mode, don't highlight anything, since
  ;;     there's not necessarily an obvious correspondence between the input
  ;;     and the outputs
  ;;
  ;;   - once `ivy-restrict-to-matches' was called, we're back to pure
  ;;     text-based matching, so highlighting is actually useful
  ;;
  ;; I could not find any other way to achieve this behavior but to rely on
  ;; implementation details of ivy.
  (if (ivy-state-dynamic-collection ivy-last)
      query
    (ivy--highlight-default query)))

(defun ivy-hoogle--occur-function (candidates)
  "Render CANDIDATES in the occur buffer."
  (setq candidates (cl-remove-if-not #'ivy-hoogle--candidate-p candidates))

  (unless ivy-hoogle--occur-initalized
    (font-lock-mode -1)
    (ivy-occur-mode)
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
    (setq-local ivy-hoogle--fetch-mode 'sync)
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
                  ;; need place 4 spaces in front of the candidate, because
                  ;; otherwise `ivy-occur' won't recognize it (see
                  ;; `ivy-occur-press')
                  (insert "    " displayed ?\n)))

    ;; go to the first match
    (goto-char (point-min))
    (forward-line 1)))

(defun ivy-hoogle--render-code (dom)
  "Render DOM as code.

This is called for <pre> and <tt> tags.  If both
`ivy-hoogle-use-haskell-fontify' and
`ivy-hoogle-fontify-code-as-haskell' are non-nil, uses
`haskell-mode' to fontify the code block.  Otherwise, simply adds
`ivy-hoogle-doc-code-face' to the contents of the code block.  If
it looks like the tag is standalone (not surrounded by text on
the same line).  In either case,
`ivy-hoogle-doc-code-background-face' is applied to the rendered
code block."
  (let ((start (point))
        (inline (not (looking-at "^\s*$"))))
    (cl-flet ((insert-fontified (fn)
                (insert (ivy-hoogle--haskell-mode-fontify
                         (and ivy-hoogle-use-haskell-fontify
                              ivy-hoogle-fontify-code-as-haskell)
                         (with-temp-buffer
                           (funcall fn)
                           (buffer-substring (point-min) (point-max)))
                         'ivy-hoogle-doc-code-face))))

      (if inline
          ;; <pre> appears both as an inline tag and a block tag; in the former
          ;; case, we don't want any new lines and separators inserted
          ;;
          ;; for now, assume that <pre> is inline if there's anything on the
          ;; same line that precedes it;
          ;;
          ;; TODO: what if there's nothing before the tag, but there's something
          ;; after it?
          (insert-fontified (lambda () (shr-generic dom)))
        (insert-fontified
         (lambda ()
           (shr-tag-pre dom)
           (flush-lines "^\s*$" (point-min) (point-max)))))
      (font-lock-append-text-property start (point)
                                      'face 'ivy-hoogle-doc-code-background-face))))

(defun ivy-hoogle--render-tag-a (dom)
  "Render DOM as a link.

Called for <a> tags.  It will either create an external link or a
link to the definition that will pop in a new `ivy-hoogle'
session depending on the contents of the tag."
  (let* ((start (point))
         (target (pcase (dom-children dom)
                   ((and `(,head)
                         (guard (stringp head)))
                    head)
                   (_ nil)))
         (parsed (url-generic-parse-url target))
         (url
          ;; does the content look like a url?
          (when (url-type parsed)
            target)))
    ;; Some links have a fragment like Data.Foldable#laws. But
    ;; we can't do anything useful with it, so we'll just strip it.
    (setq target (url-filename parsed))
    (cond (url
           (shr-generic dom)
           (ivy-hoogle--urlify start url))
          (target
           (ivy-hoogle--make-xref-link target))
          (t (shr-generic dom)))))

(defun ivy-hoogle--make-url-link (text url)
  "Insert TEXT in the current buffer and turn it into a link to URL."
  (let ((start (point)))
    (insert text)
    (ivy-hoogle--urlify start url)))

(defun ivy-hoogle--urlify (start url)
  "Attach URL to the text between START and the current point in the active buffer."
  (shr-urlify start url)
  (add-text-properties start (point)
                       (list 'action #'ivy-hoogle--follow-url
                             'keymap button-map)))

(defun ivy-hoogle--follow-url (button)
  "Follow the URL associated with BUTTON.

It is called when a link to an external URL is activated."
  (goto-char (button-start button))
  (save-excursion
    (shr-browse-url t)
    (let ((url (shr-url-at-point nil)))
      (when url
        (message "Opened %s" url)))))

(define-button-type 'ivy-hoogle-xref-link
  'face 'ivy-hoogle-doc-xref-link-face
  'action #'ivy-hoogle--follow-xref-link)

(defun ivy-hoogle--make-xref-link (target)
  "Insert TARGET in the current buffer and turn it into an xref link."
  (insert-text-button target
                      'type 'ivy-hoogle-xref-link
                      'ivy-hoogle-query target
                      'help-echo (format "Search hoogle for \"%s\"" target)))

(defun ivy-hoogle--follow-xref-link (button)
  "Pop a new `ivy-hoogle` session for the definition from BUTTON."
  (let ((query (get-text-property button 'ivy-hoogle-query)))
    (if (not query)
        (message "No ivy-hoogle xref link at point")
      (ivy-hoogle query))))

(defun ivy-hoogle--render-doc (doc)
  "Render DOC in the current buffer.

DOC string may contain html tags and is rendered using `shr'."
  (let (;; render using a fixed-pitch font by default; unlike setting
        ;; `shr-use-fonts' to `nil', it doesn't prevent `shr' form using italic
        ;; font when necessary
        (shr-current-font 'fixed-pitch)
        (start (point))
        (shr-external-rendering-functions
         `((pre . ivy-hoogle--render-code)
           (tt . ivy-hoogle--render-code)
           (a . ivy-hoogle--render-tag-a))))
    (insert doc)
    (goto-char start)
    (cl-loop while (< (point) (point-max))
             do (progn (forward-paragraph)
                       (insert "<p/>" ?\n)))
    (shr-render-region start (point))))

(defun ivy-hoogle--render-width nil
  "Calculate the width to use when rendering the candidate."
  (let* (;; just in case there are multiple windows displaying the buffer,
         ;; pick the smallest width out of all of them
         (window-width (apply #'min (mapcar #'window-body-width (get-buffer-window-list))))
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
                         ;; `shr-max-width' isn't available in the version of
                         ;; shr bundled with emacs 27
                         (and (boundp 'shr-max-width) shr-max-width)
                         window-width))))
    width))

(defun ivy-hoogle--render-candidate (candidate)
  "Render CANDIDATE in the current buffer."
  (let* ((shr-width (ivy-hoogle--render-width))
         (displayed (ivy-hoogle--display-candidate candidate))
         (result (ivy-hoogle-candidate-result candidate))
         (sources (ivy-hoogle--result-sources result))
         (start (point)))
    (insert displayed)
    (let ((button-url (ivy-hoogle--result-url result)))
      (when button-url
        (insert ?\n ?\n)
        (let ((button-start (point)))
          (insert "[Open in browser]")
          (ivy-hoogle--urlify button-start button-url))))
    (insert ?\n ?\n)
    (ivy-hoogle--render-sources shr-width sources)
    ;; use the same font shr will use
    (add-face-text-property start (point) 'fixed-pitch)
    (ivy-hoogle--render-doc (ivy-hoogle--result-doc-html result))))

(defun ivy-hoogle--render-sources-nobreak-p ()
  "Break candidate sources on package bounaries.

See `ivy-hoogle--render-sources'."
  (save-excursion
    (skip-chars-backward " \t")
    (unless (bolp)
      (backward-char 1)
      (not (looking-at ", ")))))

(defun ivy-hoogle--render-sources (width sources)
  "Render candidate SOURCES in the help buffer.

Turns them into external links to the corresponding packages and
modules on Hackage.

WIDTH is the maximum width to use in characters."
  (let ((sources-by-package (ivy-hoogle--group-by sources #'ivy-hoogle--source-package))
        (start (point))
        (first t))
    (cl-loop for (package . package-sources) in sources-by-package
             when package
             do
             (progn
               (unless first
                 (insert ", "))
               (setq first nil)
               (let ((package-url
                      (cl-first (seq-remove #'null
                                            (mapcar #'ivy-hoogle--source-package-url
                                                    package-sources)))))
                 (if package-url
                     (ivy-hoogle--make-url-link package package-url)
                   (insert package))

                 (cl-loop for source in package-sources
                          when (and source (ivy-hoogle--source-module source))
                          do
                          (insert " ")
                          (let ((module (ivy-hoogle--source-module source))
                                (module-url (ivy-hoogle--source-module-url source)))
                                (if module-url
                                    (ivy-hoogle--make-url-link module module-url)
                                  (insert module)))))))

    ;; add a new line only if we inserted something in the buffer above
    (unless (equal start (point))
      (insert ?\n ?\n))

    ;; override the face used the face used by `ivy-hoogle--make-xref-link'
    (font-lock-prepend-text-property start (point) 'face 'ivy-hoogle-candidate-source-face)

    ;; format the sources to fit into the buffer width
    (let ((fill-column width)
          (fill-prefix nil)
          ;; break sources only on package boundary
          (fill-nobreak-predicate '(ivy-hoogle--render-sources-nobreak-p)))
      (fill-region start (point)))))

(defun ivy-hoogle--action (candidate)
  "Open a help window with the documentation for CANDIDATE.

If CANDIDATE is a fake candidate like that returned by
`ivy-hoogle--no-results', restart `ivy-hoogle'."
  ;; Workaround: when the candidate is selected using ivy-avy, all text
  ;; properties are stripped from it. since all of our metadata lives in text
  ;; properties, we would just throw such candidate away and restart ivy.
  ;;
  ;; To work around we fetch the candidate using the internal candidate index
  ;; and list of candidates.
  (unless (ivy-hoogle--candidate-p candidate)
    (condition-case err
        (setq candidate
              (or (nth ivy--index ivy--all-candidates) candidate))
      (error
       (display-warning 'ivy-hoogle
                        (format "Couldn't fetch candidate: %s"
                                (error-message-string err))))))

  (if (not (ivy-hoogle--candidate-p candidate))
      ;; if a non-candidate got selected, like the informational "Updating" or
      ;; "No results", restart selection
      ;;
      ;; I wish I could just disallow selecting these fake candidates, but
      ;; there doesn't seem to be a way to do that
      (ivy-resume)
    (ivy-hoogle--show-doc candidate)))

(defun ivy-hoogle--show-doc (candidate)
  "Display a help window with the documentation for CANDIDATE."
  (let ((buffer-name "*Help*"))
    (help-setup-xref `(ivy-hoogle--show-doc ,candidate) nil)
    (with-current-buffer (get-buffer-create buffer-name)
      ;; display the buffer, unless it's already visible this is required for
      ;; us to be able to determine rendering width for shr (see
      ;; `ivy-hoogle--render-width')
      (unless (get-buffer-window)
        (display-buffer (current-buffer)))
      (with-help-window buffer-name
        (ivy-hoogle--render-candidate candidate))
      (set-window-point (get-buffer-window) (point-min)))))

(defun ivy-hoogle--occur ()
  "Show current candidates in the occur buffer.

See `ivy-occur' for more details."
  (interactive)
  (let ((ivy-hoogle--fetch-mode 'sync))
    (ivy-occur)))

(defun ivy-hoogle--browse-candidate (candidate)
  "Open documentation for CANDIDATE in the external browser."
  (let* ((result (ivy-hoogle-candidate-result candidate))
         (url (ivy-hoogle--result-url result)))
    (if url
        (progn
          (funcall browse-url-secondary-browser-function url)
          (message "Opened %s" url))
      (message "No URL found"))))

(defun ivy-hoogle--alt-done ()
  "Open documentation for the selected candidate in the external browser."
  (ivy-exit-with-action
   (lambda (candidate)
     (if (not (ivy-hoogle--candidate-p candidate))
         ;; if a non-candidate got selected, like the informational "Updating" or
         ;; "No results", restart selection
         (ivy-resume)
       (ivy-hoogle--browse-candidate candidate)))))

(defun ivy-hoogle--init ()
  "Initialize `ivy-hoogle' session."
  (when (eq this-command 'ivy-resume)
    ;; `ivy-read' has special logic around `ivy-resume' that reuses cached
    ;; results from the previous session. It also unconditionally sets
    ;; `ivy--old-cands' to be equal to only those candidates that match the
    ;; initial input (the query from the previous session). This in turn makes
    ;; marking any candidates outside that set impossible.
    ;;
    ;; To work around, set the current command to `ivy-hoogle'. This forces
    ;; `ivy-read' to refetch the results, and so marking works as expected.
    (setq this-command 'ivy-hoogle)))

;;;###autoload
(defun ivy-hoogle (&optional initial)
  "Query Hoogle interactively using `ivy'.

Optional INITIAL is the initial query to use."
  (interactive)
  (let ((map (make-sparse-keymap))
        (ivy-dynamic-exhibit-delay-ms 0)
        ;; ivy calls `read-from-minibuffer' that adds the selected candidate
        ;; in the history; but then ivy itself also adds the entered text;
        ;;
        ;; it makes for a confusing experience; essentially, there'll be
        ;; duplicates in the history, but then ivy's attempt at deduplicating
        ;; the history also won't work, because the candidate and the input
        ;; are likely not to be equal
        ;;
        ;; setting `history-add-new-input' to `nil' tells
        ;; `read-from-minibuffer' not to update the history
        (history-add-new-input nil))
    (cl-flet ((rebind (command replacement)
                      (cl-loop for key in (where-is-internal command ivy-minibuffer-map)
                               do (define-key map key replacement))))
      ;; `ivy-occur' requires some special handling
      (rebind 'ivy-occur #'ivy-hoogle--occur))
    (ivy-read
     "Hoogle: "
     #'ivy-hoogle--candidates
     :action #'ivy-hoogle--action
     :dynamic-collection t
     :require-match t
     :unwind #'ivy-hoogle--cleanup
     :history 'ivy-hoogle--history
     :caller 'ivy-hoogle
     :keymap map
     :initial-input initial)))

;;;###autoload
(defun ivy-hoogle-thing-at-point nil
  "Query Hoogle for the symbol at point."
  (interactive)
  (ivy-hoogle
   (or (thing-at-point 'symbol t)
       ;; Operators like >>= are not symbols, so fallback to a more general
       ;; category.
       (thing-at-point 'sexp t)
       ;; If everything else fails, start a session with an empty initial
       ;; input.
       "")))

(ivy-configure 'ivy-hoogle
  :display-transformer-fn #'ivy-hoogle--display-candidate
  :format-fn #'ivy-hoogle--format-function
  :occur #'ivy-hoogle--occur-function
  :alt-done-fn #'ivy-hoogle--alt-done
  :init-fn #'ivy-hoogle--init)

;; the highlight function can only be overridden by associating it with the
;; regex building function directly
(setf (alist-get 'ivy-hoogle ivy-re-builders-alist) #'ivy-hoogle--re-builder)
(setf (alist-get #'ivy-hoogle--re-builder ivy-highlight-functions-alist) #'ivy-hoogle--highlight-function)

(ivy-add-actions
 'ivy-hoogle
 '(("b" ivy-hoogle--browse-candidate "browse")))

(provide 'ivy-hoogle)

;;; ivy-hoogle.el ends here
