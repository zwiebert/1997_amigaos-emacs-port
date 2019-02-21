;; This file is included by "x-win.el" when configured for
;; Amiga-OS/Intuition (note: I've defined no own window system)
;; -bw/26-May-98


;; Provide a default for "-d".  This seems to be required, because the
;; startup assumes the same meaning for $DISPLAY and "-d" commandline
;; parameter.  We cannot have -d without DISPLAY.  ???-bw/07-Mar-99
(setenv "DISPLAY" "Workbench")

;; turn on smart refresh if user want it. The default is simple
;; refresh because big no-care-refresh windows seems to make Intuition
;; on AGA very slow -bw/14-Jun-98
(setq amiga-simple-refresh (not (getenv "EMACS_INTUI_SMART")))


;; experimental Amiga menu -bw/21-May-98

(define-key global-map [menu-bar Amiga] 
  (cons "Amiga" (make-sparse-keymap "Amiga")))
(global-set-key [menu-bar Amiga font-request]
		'("Set Default Font" . amiga-asl-set-default-font))

(global-set-key [menu-bar Amiga rule3] '("--" . nil))

(global-set-key [menu-bar Amiga change-directory]
		'("Change current directory" . cd))

(global-set-key [menu-bar Amiga rule2]
		'("--" . nil))

(global-set-key [menu-bar Amiga save-some-buffers]
		'("Save all changes" . save-some-buffers))

(global-set-key [?\s-W] 'amiga-asl-write-file)
(global-set-key [menu-bar Amiga asl-write-file]
		'("Save as..." . amiga-asl-write-file))

(defalias 'amiga-menu-save-buffer 'save-buffer)
(global-set-key [?\s-w] 'amiga-menu-save-buffer)
(global-set-key [menu-bar Amiga save-buffer]
		'("Save" . amiga-menu-save-buffer))

(global-set-key [?\s-n] 'amiga-asl-write-region)
(global-set-key [menu-bar Amiga asl-write-region]
		'("Save block to file..." . amiga-asl-write-region))

(global-set-key [menu-bar Amiga rule1] '("--" . nil))

(global-set-key [?\s-O] 'amiga-asl-insert-file)
(global-set-key [menu-bar Amiga asl-insert-file]
		'("Include file..." . amiga-asl-insert-file))

(global-set-key [?\s-o] 'amiga-asl-find-file)
(global-set-key [menu-bar Amiga asl-open-file]
		'("Open..." . amiga-asl-find-file))

(defalias 'amiga-open-new-frame 'make-frame-command)
(global-set-key [?\s-?] 'amiga-open-new-frame)
(global-set-key [menu-bar Amiga amiga-open-new-frame]
		'("Open new frame" . amiga-open-new-frame))

;; returns t if cursor is not in minibuffer.  Works for both Emacs-19
;; and Emacs-20 -bw/31-Jul-98
(defun amiga--not-minibuffer ()
  (not (window-minibuffer-p 
	(frame-selected-window
	 (if (boundp 'menu-updating-frame)
	     menu-updating-frame ;since Emacs-20
	   (selected-frame))))))
  
;; enabling/disabling menu items
(put 'amiga-menu-save-buffer 'menu-enable '(and (buffer-modified-p)
						(amiga--not-minibuffer)))
(put 'amiga-asl-write-file 'menu-enable  '(amiga--not-minibuffer))
(put 'amiga-asl-find-file 'menu-enable  '(amiga--not-minibuffer))
(put 'amiga-asl-insert-file 'menu-enable '(amiga--not-minibuffer))



;; new for Emacs-20.3
(setq focus-follows-mouse nil)

;; This is the normal behavior for Intuition applications (otherwise
;; it would be possible to end up with an iconified Emacs on a
;; backdrop Workbench which is total obscured by a busy-blocked MUI
;; newsreader window)
(add-hook 'server-switch-hook 'raise-frame)

;; IDCMP provides 8 bit input.
(set-input-mode (car (current-input-mode))
		(nth 1 (current-input-mode))
		0)

;; toggle state of numeric keypad -bw/21-May-98
(if (boundp 'amiga--nk-numlock)
    (progn
      (global-set-key [16777307] ;; => "\-[" 
		      (lambda () (interactive) (setq amiga--nk-numlock nil)))
      (global-set-key [H-kp-numlock] 
		      (lambda () (interactive) (setq amiga--nk-numlock t)))))

;; Amiga common keymappings
(global-set-key [S-up] [prior])
(global-set-key [S-down] [next])
(global-set-key [S-left] [?\C-a])
(global-set-key [S-right] [?\C-e])
(global-set-key [A-up] [?\M-a])		; ???
(global-set-key [A-down] [?\M-e])	; ???
(global-set-key [A-left] [C-left])
(global-set-key [A-right] [C-right])
(global-set-key [delete] [?\C-d])
(global-set-key [?\s-s] 'save-buffer)
;; copy/paste (super-key == RCommand/RAmiga)
(global-set-key  [?\s-c] [?\M-w])
(global-set-key [?\s-v] [?\C-y])
;; XXX
(global-set-key [kp-numlock] [?\[])
(global-set-key [S-kp-numlock] [?{])

;; Amiga fonts for font menu
(setq x-fixed-font-alist
      '("Font menu"
	("Courier"
	 ("11" "-amiga-Courier-medium-r-normal--11")
	 ("13" "-amiga-Courier-medium-r-normal--13")
	 ("15" "-amiga-Courier-medium-r-normal--15")
	 ("19" "-amiga-Courier-medium-r-normal--19")
	 ("24" "-amiga-Courier-medium-r-normal--24")
	 ("11 bold" "-amiga-Courier-bold-r-normal--11")
	 ("13 bold" "-amiga-Courier-bold-r-normal--13")
	 ("15 bold" "-amiga-Courier-bold-r-normal--15")
	 ("19 bold" "-amiga-Courier-bold-r-normal--19")
	 ("24 bold" "-amiga-Courier-bold-r-normal--24")
	 ("11 slant" "-amiga-Courier-medium-o-normal--11")
	 ("13 slant" "-amiga-Courier-medium-o-normal--13")
	 ("15 slant" "-amiga-Courier-medium-o-normal--15")
	 ("19 slant" "-amiga-Courier-medium-o-normal--19")
	 ("24 slant" "-amiga-Courier-medium-o-normal--24")
	 ("11 bold slant" "-amiga-Courier-bold-o-normal--11")
	 ("13 bold slant" "-amiga-Courier-bold-o-normal--13")
	 ("15 bold slant" "-amiga-Courier-bold-o-normal--15")
	 ("19 bold slant" "-amiga-Courier-bold-o-normal--19")
	 ("24 bold slant" "-amiga-Courier-bold-o-normal--24"))
	("DBFont"
	 ("8" "-amiga-DBFont-medium-r-normal--8")
	 ("12" "-amiga-DBFont-medium-r-normal--12")
	 ("8 bold" "-amiga-DBFont-bold-r-normal--8")
	 ("12 bold" "-amiga-DBFont-bold-r-normal--12")
	 ("8 slant" "-amiga-DBFont-medium-o-normal--8")
	 ("12 slant" "-amiga-DBFont-medium-o-normal--12")
	 ("8 bold slant" "-amiga-DBFont-bold-o-normal--8")
	 ("12 bold slant" "-amiga-DBFont-bold-o-normal--12"))
	("LetterGothic"
	 ("11" "-amiga-LetterGothic-medium-r-normal--11")
	 ("13" "-amiga-LetterGothic-medium-r-normal--13")
	 ("15" "-amiga-LetterGothic-medium-r-normal--15")
	 ("19" "-amiga-LetterGothic-medium-r-normal--19")
	 ("24" "-amiga-LetterGothic-medium-r-normal--24")
	 ("11 bold" "-amiga-LetterGothic-bold-r-normal--11")
	 ("13 bold" "-amiga-LetterGothic-bold-r-normal--13")
	 ("15 bold" "-amiga-LetterGothic-bold-r-normal--15")
	 ("19 bold" "-amiga-LetterGothic-bold-r-normal--19")
	 ("24 bold" "-amiga-LetterGothic-bold-r-normal--24")
	 ("11 slant" "-amiga-LetterGothic-medium-o-normal--11")
	 ("13 slant" "-amiga-LetterGothic-medium-o-normal--13")
	 ("15 slant" "-amiga-LetterGothic-medium-o-normal--15")
	 ("19 slant" "-amiga-LetterGothic-medium-o-normal--19")
	 ("24 slant" "-amiga-LetterGothic-medium-o-normal--24")
	 ("11 bold slant" "-amiga-LetterGothic-bold-o-normal--11")
	 ("13 bold slant" "-amiga-LetterGothic-bold-o-normal--13")
	 ("15 bold slant" "-amiga-LetterGothic-bold-o-normal--15")
	 ("19 bold slant" "-amiga-LetterGothic-bold-o-normal--19")
	 ("24 bold slant" "-amiga-LetterGothic-bold-o-normal--24"))
	("Topaz"
	 ("7" "-amiga-Topaz-medium-r-normal--7")
	 ("8" "-amiga-Topaz-medium-r-normal--8")
	 ("9" "-amiga-Topaz-medium-r-normal--9")
	 ("11" "-amiga-Topaz-medium-r-normal--11")
	 ("7 bold" "-amiga-Topaz-bold-r-normal--7")
	 ("8 bold" "-amiga-Topaz-bold-r-normal--8")
	 ("9 bold" "-amiga-Topaz-bold-r-normal--9")
	 ("11 bold" "-amiga-Topaz-bold-r-normal--11")
	 ("7 slant" "-amiga-Topaz-medium-o-normal--7")
	 ("8 slant" "-amiga-Topaz-medium-o-normal--8")
	 ("9 slant" "-amiga-Topaz-medium-o-normal--9")
	 ("11 slant" "-amiga-Topaz-medium-o-normal--11")
	 ("7 bold slant" "-amiga-Topaz-bold-o-normal--7")
	 ("8 bold slant" "-amiga-Topaz-bold-o-normal--8")
	 ("9 bold slant" "-amiga-Topaz-bold-o-normal--9")
	 ("11 bold slant" "-amiga-Topaz-bold-o-normal--11"))
	))


;; ???-bw/26-May-98: We could use ASL on Console too.  Should we?

(defun amiga-asl-find-file ()
  "find file list choosed by ASL file requester"
  (interactive)
  (mapcar  'find-file ;; '(lambda (s) (find-file (amiga--dos-to-unix-path s)))
	   (amiga-popup-file-request
	    (amiga--unix-to-dos-path (file-truename "")))))

(defun amiga-asl-insert-file ()
  "insert file list choosed by ASL file requester"
  (interactive)
  (mapcar  'insert-file (amiga-popup-file-request
			 (amiga--unix-to-dos-path (file-truename "")))))

(defun amiga-asl-find-alternate-file ()
  "find file list choosed by ASL file requester"
  (interactive)
  (mapcar  'find-file
	   (amiga-popup-file-request
	    (amiga--unix-to-dos-path (file-truename ""))
	    (if buffer-file-name
		(substring buffer-file-name
			   (length (file-name-directory buffer-file-name)))))))

(defun amiga-asl-write-file ()
  "write to new file choosed by ASL file requester"
  (interactive)
  (write-file (car (amiga-popup-file-request nil nil t))))

(defun amiga-asl-write-region ()
  "write to new file choosed by ASL file requester"
  (interactive)
  (write-region  (point) (mark) (car (amiga-popup-file-request nil nil t))))

(defun amiga-asl-set-default-font ()
  "Set a new default font choosed by ASL font requester"
  (interactive)
  (let ((font (amiga-popup-font-request)))
    (if font 
	(set-default-font (format "-amiga-%s-medium-r---%d" 
				  (car font) (cdr font))))))

(or (fboundp 'amiga--set-font) 
    (fset 'amiga--set-font (symbol-function 'amiga-set-font)))
(defun amiga-set-font (font size)
  (interactive        "sFont: 
nSize: ")
  (if (not (and window-system (boundp 'amiga-i-windows)))
      (amiga--set-font font size)
    (set-default-font (format "-amiga-%s-medium-r---%d" font size))
    (redraw-frame (selected-frame))))
