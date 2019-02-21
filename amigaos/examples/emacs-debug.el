
(defun edb-filt-all (stat)
  t)

(defun edb-filt-singles (stat)
  (= (nth 2 stat) 1))

(defun edb-filt-arrays (stat)
  (not (= (nth 2 stat) 1)))


(defun edb-mstat-filter (list filter)
  (let ((new-list))
    (mapcar '(lambda (stat)
	       (if (funcall filter stat)
		   (setq new-list (cons stat new-list)))
	       nil) list)
    (nreverse new-list)))
  

(defun edb-memstat-2 ()
  (interactive)
  (amiga-memory-stat-print (amiga-memory-stat))
)

(defun edb-memstat ()
  (interactive)
  (amiga-memory-stat-print (amiga-memory-stat))
)

(defun amiga-memory-stat-print (list)
  "Format LIST, which may produced by amiga-memory-stat, and print it out."
  (interactive)
  (let ((tot-nmb 0)
	(tot-siz 0)
	(tot-free 0)
	(tot-touched 0)
	(tbl-head    "  Address         Size     Number   Free    Touched     Wasted")
	(tbl-line    " ==============================================================")
	(tbl-rowfmt  " 0x%08x    %8d     %4d     %4d   %8d   %8d\n")
	)
    (princ (format "%s\n%s\n%s\n" tbl-line tbl-head tbl-line))
    (mapcar '(lambda (stat)
	       (let ((addr (nth 0 stat))
		     (siz (nth 1 stat))
		     (nmb (nth 2 stat))
		     (free (nth 3 stat))
		     (touched (nth 4 stat))
		     )
		 (setq tot-nmb (+ tot-nmb nmb))
		 (setq tot-siz (+ tot-siz (* nmb siz)))
		 (setq tot-free (+ tot-free (* free siz)))
		 (setq tot-touched (+ tot-touched touched))
		 (princ (format tbl-rowfmt addr siz nmb free touched (* free siz))))
	       ) list)

    (princ (format 
	    "%s\n TOTAL:   bytes used: %d  bytes free: %d
          tot-nmb: %d  tot-touched: %d\n" 
	    tbl-line tot-siz tot-free tot-nmb tot-touched))
    ) nil )

;; helper functions for elp (Emacs Lisp Profiler)
(defun edb-elp-c-mode ()
  (interactive)
  (elp-instrument-list
   '(c-add-style
     c-adjust-state c-append-backslash c-auto-newline c-backslash-region
     c-backward-conditional c-backward-into-nomenclature
     c-backward-syntactic-ws c-backward-to-start-of-do
     c-backward-to-start-of-if c-beginning-of-inheritance-list
     c-beginning-of-macro c-beginning-of-statement
     c-beginning-of-statement-1 c-c++-menu c-c-menu c-calculate-state
     c-comment-indent c-comment-line-break-function c-common-init
     c-crosses-statement-barrier-p c-delete-backslash c-echo-parsing-error
     c-electric-backspace c-electric-brace c-electric-colon
     c-electric-delete c-electric-lt-gt c-electric-pound
     c-electric-semi&comma c-electric-slash c-electric-star
     c-end-of-statement c-end-of-statement-1 c-fill-paragraph
     c-forward-conditional c-forward-into-nomenclature
     c-forward-syntactic-ws c-get-offset c-gnu-impose-minimum
     c-guess-basic-syntax c-hack-state c-idl-menu c-in-literal
     c-in-method-def-p c-indent-command c-indent-defun c-indent-exp
     c-indent-line c-indent-region c-initialize-builtin-style
     c-initialize-cc-mode c-inside-bracelist-p c-intersect-lists
     c-java-menu c-just-after-func-arglist-p c-keep-region-active
     c-langelem-col c-least-enclosing-brace c-lineup-C-comments
     c-lineup-ObjC-method-args c-lineup-ObjC-method-args-2
     c-lineup-ObjC-method-call c-lineup-arglist
     c-lineup-arglist-close-under-paren c-lineup-arglist-intro-after-paren
     c-lineup-comment c-lineup-java-inher c-lineup-java-throws
     c-lineup-math c-lineup-multi-inher c-lineup-runin-statements
     c-lineup-streamop c-lookup-lists c-make-inherited-keymap
     c-make-styles-buffer-local c-mark-function c-mode c-mode-menu
     c-most-enclosing-brace c-narrow-out-enclosing-class c-objc-menu
     c-outline-level c-parse-state c-point c-populate-syntax-table
     c-postprocess-file-styles c-progress-fini c-progress-init
     c-progress-update c-read-offset c-safe-position c-scope-operator
     c-search-uplist-for-classkey c-semi&comma-inside-parenlist
     c-set-offset c-set-style c-set-style-1 c-set-style-2
     c-show-syntactic-information c-skip-case-statement-forward
     c-skip-conditional c-snug-do-while c-style-gnu-2-amiga
     c-submit-bug-report c-toggle-auto-hungry-state c-toggle-auto-state
     c-toggle-hungry-state c-up-conditional c-update-modeline
     c-use-java-style c-version c-whack-state))
  )

(defun edb-elp-tags-search ()
  (interactive)
  (elp-instrument-list
   '(
     erase-buffer
     expand-file-name
     find-file-name-handler
     find-operation-coding-system
     get-buffer
     get-buffer-create
     get-file-buffer
     insert-file-contents
     kill-all-local-variables
     kill-buffer
     looking-at
     next-file
     re-search-forward
     run-hooks
     set-auto-coding
     set-buffer
     string-match
     tags-search
     visit-tags-table-buffer
     )))

(defun edb-elp-outline ()
  (interactive)
  (elp-instrument-list
   '(
     outline-back-to-heading
     outline-backward-same-level
     outline-copy-overlay
     outline-discard-overlays
     outline-end-of-heading
     outline-end-of-subtree
     outline-flag-region
     outline-flag-subtree
     outline-font-lock-level
     outline-forward-same-level
     outline-get-last-sibling
     outline-get-next-sibling
     outline-isearch-open-invisible
     outline-level
     outline-mark-subtree
     outline-minor-mode
     outline-mode
     outline-next-heading
     outline-next-preface
     outline-next-visible-heading
     outline-on-heading-p
     outline-previous-visible-heading
     outline-up-heading
     outline-visible
     )))

(defun edb-elp-font-lock ()
  (interactive)
  (elp-instrument-list
   '(
     font-lock-add-keywords
     font-lock-after-change-function
     font-lock-after-fontify-buffer
     font-lock-after-unfontify-buffer
     font-lock-append-text-property
     font-lock-apply-highlight
     font-lock-apply-syntactic-highlight
     font-lock-change-major-mode
     font-lock-choose-keywords
     font-lock-compile-keyword
     font-lock-compile-keywords
     font-lock-default-fontify-buffer
     font-lock-default-fontify-region
     font-lock-default-unfontify-buffer
     font-lock-default-unfontify-region
     font-lock-eval-keywords
     font-lock-fillin-text-property
     font-lock-fontify-anchored-keywords
     font-lock-fontify-block
     font-lock-fontify-buffer
     font-lock-fontify-keywords-region
     font-lock-fontify-region
     font-lock-fontify-syntactic-anchored-keywords
     font-lock-fontify-syntactic-keywords-region
     font-lock-fontify-syntactically-region
     font-lock-match-c++-style-declaration-item-and-skip-to-next
     font-lock-match-c-style-declaration-item-and-skip-to-next
     font-lock-mode
     font-lock-prepend-text-property
     font-lock-set-defaults
     font-lock-turn-off-thing-lock
     font-lock-turn-on-thing-lock
     font-lock-unfontify-buffer
     font-lock-unfontify-region
     font-lock-unset-defaults
     font-lock-value-in-major-mode
     )))

(defun edb-elp-info ()
  (interactive)
  (require 'info)
  (elp-instrument-list
   '(
     Info-backward-node
     Info-build-node-completions
     Info-cease-edit
     Info-check-pointer
     Info-complete-menu-item
     Info-directory
     Info-edit
     Info-edit-mode
     Info-exit
     Info-extract-menu-counting
     Info-extract-menu-item
     Info-extract-menu-node-name
     Info-extract-pointer
     Info-final-node
     Info-find-emacs-command-nodes
     Info-find-index-name
     Info-find-node
     Info-follow-nearest-node
     Info-follow-reference
     Info-following-node-name
     Info-fontify-node
     Info-forward-node
     Info-get-token
     Info-goto-emacs-command-node
     Info-goto-emacs-key-command-node
     Info-goto-node
     Info-help
     Info-index
     Info-index-next
     Info-insert-dir
     Info-kill-buffer
     Info-last
     Info-last-menu-item
     Info-last-preorder
     Info-menu
     Info-menu-update
     Info-mode
     Info-mode-menu
     Info-mouse-follow-nearest-node
     Info-next
     Info-next-menu-item
     Info-next-preorder
     Info-next-reference
					;     Info-no-error
     Info-nth-menu-item
     Info-prev
     Info-prev-reference
     Info-read-node-name
     Info-read-node-name-1
     Info-read-subfile
     Info-restore-point
     Info-scroll-down
     Info-scroll-up
     Info-search
     Info-select-node
     Info-set-mode-line
     Info-speedbar-button
     Info-speedbar-buttons
     Info-speedbar-menu
					;     Info-split
     Info-summary
					;     Info-tagify
     Info-top-node
     Info-try-follow-nearest-node
     Info-undefined
     Info-up
					;     Info-validate
					;     info-complete-symbol
					;     info-lookup-file
					;     info-lookup-reset
					;     info-lookup-symbol
					;     info-standalonenfo-tagify
     ;;     Info-validate
     ;;     Info-tagify
     )))

(define-key emacs-lisp-mode-map [menu-bar emacs-lisp elp-instrument]
  '("Instrument Function for Profiling" . elp-instrument-function))


