;;; amiga.el --- Function key definitions for Amiga console and emulators.

;; Author: Bert Winkelmann <bertw@in-brb.de>
;; Keywords: terminals

;;; Commentary:

;; Uses the Emacs 19 terminal initialization features --- won't work
;; with 18.
;;
;; We have to define the shifted function keys here, because it seems
;; not possible to do this in termcap.
;;
;; Unfortunallay there are 2 different prefixes for function keys:
;; "\e[" and "\233" (aka "M-\e").  Since "\e\e" isn't bound by Emacs
;; anymore it's no problem to define every function key twice.

;;; Code:

;; Some function key may be already defined by termcap.  But don't
;; bother with that.

;; disable MULE for Amiga Console (FIXME-bw/16-Dec-97)
(setq-default enable-multibyte-characters nil)


;; use [help] instead C-h
(global-set-key [?\C-h] nil)

;; swap ^H<=>DEL. (C-h does not work for help. Use the [help] key or
;; define [f1] if your keyboard don't have a help key. -bw/13-Dec-97
;; [this is not compatible with 'keyboard-translate: (load "term/keyswap")]
(keyboard-translate ?\177 ?\^h)
(keyboard-translate ?\^h ?\177)

;; CON: traps ^C even in raw mode!  Let's use C-z instead.
(keyboard-translate ?\C-z ?\C-c)


(mapcar 
 '(lambda (pair)
    ;; termcap uses prefix "\233" (CSI) with CON: on my machine
    (define-key function-key-map (concat "\233" (car pair)) (cdr pair))
    ;; prefix "\e[" is used by niftyterm on my machine
    (define-key function-key-map (concat "\e[" (car pair)) (cdr pair))
    (if system-uses-terminfo
	;; ncurses uses prefix "\e" on my machine
	(define-key function-key-map (concat "\e" (car pair)) (cdr pair)))
    )
 '(
   ("0~" . [f1])
   ("1~" . [f2])
   ("2~" . [f3])
   ("3~" . [f4])
   ("4~" . [f5])
   ("5~" . [f6])
   ("6~" . [f7])
   ("7~" . [f8])
   ("8~" . [f9])
   ("9~" . [f10])
   ("10~" . [S-f1])
   ("11~" . [S-f2])
   ("12~" . [S-f3])
   ("13~" . [S-f4])
   ("14~" . [S-f5])
   ("15~" . [S-f6])
   ("16~" . [S-f7])
   ("17~" . [S-f8])
   ("18~" . [S-f9])
   ("19~" . [S-f10])
   ("?~" . [help])
   ("A" . [up])
   ("B" . [down])
   ("C" . [right])
   ("D" . [left])
   ("T" . [S-up])
   ("S" . [S-down])
   (" A" . [S-left])
   (" @" . [S-right])
   ("Z" . [S-tab])
   ))

(define-key global-map [S-up] [prior])
(define-key global-map [S-down] [next])
(define-key global-map [S-right] [?\C-e])
(define-key global-map [S-left] [?\C-a])

;;; amiga.el ends here
