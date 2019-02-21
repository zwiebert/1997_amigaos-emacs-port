;; /gg/lib/emacs/site-lisp/site-start.el - Site startup file for Emacs

;; modeline clock format
(setq display-time-24hr-format t)

;; UMS-sendmail interface
(setq sendmail-program "~/bin/sendmail")

;;config switches
(setq amiga-working-killpg nil) ;; SIGTSTP is not implemented
;;(setq process-connection-type nil) ;; pty(4) doesn't play with me

(setq load-path (append load-path (list "~/lib/emacs/lisp")))

;; This doesn't work.  The hook is not called before invoking ftp(1).
(add-hook 'ange-ftp-process-startup-hook 
(lambda () 
  (if process-connection-type
      (error "Ftp needs a pty(4) (hint: set process-connection-type \
to 't' and start l:fifo.handler)"))))

;; global keybindings =======================================================
(global-set-key "\eo" 'ted)

;; enable 8 bit output (and disable MULE since Emacs-20.2)
(standard-display-european 1)

;; version dependent (maybe I should really split this file and load
;; it from version dependent site-start.el)

;;;;(require 'iso-syntax)
;;;;(require "international/iso-transl")

;;FIXME-bw: This should only done on  console, but not on terminals
(if window-system ()
  ;; German umlauts in ISO-8859-1
  (global-set-key  "\M-d"  "\C-q\M-d");; "a
  (global-set-key  "\M-D" "\C-q\M-D");;  "A
  (global-set-key  "\M-v" "\C-q\M-v");; "o
  (global-set-key  "\M-V" "\C-q\M-V");; "O
  (global-set-key  "\M-|" "\C-q\M-|");; "u
  (global-set-key  "\M-\\" "\C-q\M-\\");; "U
  (global-set-key  "\M-_" "\C-q\M-_");; sz
  )

  ;; ==== global keybinding ===
  (global-set-key "\M-\C-y" 'vip-mode)
  ;; HELP key:
  (global-set-key [help] 'help-command)
  (global-set-key [help help] 'help-for-help)
  ;; F keys:
  (global-set-key [f1] 'next-error)
  (global-set-key [f2] 'what-line)
  (global-set-key [f3] 'register-to-point)
  (global-set-key [f4] 'gud-print)
  (global-set-key [f5] 'dabbrev-expand)
  (global-set-key [f6] '(lambda () (interactive)  "narrow to next page"
			  (widen) (forward-char) (forward-page) (narrow-to-page)))
  (global-set-key [f7] '(lambda () (interactive) 
			  (find-tag nil t)))
  (global-set-key [f8] 'gud-next)
  (global-set-key [f9] 'switch-to-buffer)
;;  (global-set-key [f10] 'other-window) used for tmm-menu since 19.34

  (global-set-key [S-f1] 'compile)
  (global-set-key [S-f2] 'goto-line)
  (global-set-key [S-f3] 'point-to-register)
  (global-set-key [S-f4] nil)
  (global-set-key [S-f5] nil)
  (global-set-key [S-f6] '(lambda () (interactive) "narrow to previous page"
			    (widen) (backward-char) (backward-page) (narrow-to-page)))
  (global-set-key [S-f7] nil)
  (global-set-key [S-f8] 
		  '(lambda ()  (interactive) "run cpp on buffer"
		     (shell-command-on-region (point-min) 
					      (point-max) 
					      "cpp")))
  (global-set-key [S-f9] 'list-buffers)
  (global-set-key [S-f0] 'delete-window)

(if window-system () 
  ;;====== some default keybindings does not work so good on german Amiga keyboard ==
  ;; Map bindings of unused ALT- keys to \M-
  (global-set-key "\M-*" 'scroll-down);; = ALT-v
  (global-set-key "\M-:" 'backward-word);; = ALT-b
  (global-set-key "\M-p" 'kill-word);; = ALT-d
  )
;;  Shortcut  for "\C-x ^" using the <#/^> key on german keyboards.
(global-set-key "\C-^" 'enlarge-window)
(global-set-key "\M-\C-^" 'shrink-window)

;; Use "/bin/cpp" instead of "/lib/cpp" in "cmacro.el"
(setq c-macro-preprocessor "/bin/cpp -C")


;; gnus
(setq gnus-default-subscribed-newsgroups
      '("de.alt.netdigest")
      )

