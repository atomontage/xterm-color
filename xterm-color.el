;;; xterm-color.el --- ANSI & XTERM 256 color support -*- lexical-binding: t -*-
;;
;; Copyright (C) 2010-2017 xristos@sdf.lonestar.org
;; All rights reserved
;;
;; Version: 1.6.1 - 2017-1-2
;; Author: xristos@sdf.lonestar.org
;; URL: https://github.com/atomontage/xterm-color
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: faces
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Translates ANSI control sequences into text properties.
;;
;; * Regular ANSI colors
;;
;; * XTERM 256 colors
;;
;; * Works with compilation-mode (experimental)
;;
;; * Works with eshell
;;
;; * More accurate than ansi-color.el
;;
;; * Should perform much better than ansi-color.el
;;
;;; Usage:
;;
;; Call xterm-color-filter to propertize strings that you can then insert into
;; a buffer. All state is kept in buffer-local variables which means that
;; control sequences can span xterm-color-filter call boundaries.
;;
;; Example:
;;
;; (xterm-color-filter "[0;1;3;4")
;; (xterm-color-filter ";35")
;; (xterm-color-filter ";mThis is only a test")
;; (xterm-color-filter "[0m")
;;
;;
;; You may also use xterm-color-colorize-buffer, interactively or from elisp,
;; to colorize an entire buffer.
;;
;;
;; * You can replace ansi-color.el with xterm-color for all comint buffers:
;;
;;
;; + comint install
;;
;; (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;        (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))
;;
;; + comint uninstall
;;
;; (progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;        (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))
;;
;; If running a shell (M-x shell) also set TERM accordingly (xterm-256color)
;;
;;
;; * You can also use it with eshell (and thus get color output from system ls):
;;
;;
;; (require 'eshell)
;;
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (setq xterm-color-preserve-properties t)))
;;
;;  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;;
;;  Don't forget to setenv TERM xterm-256color too
;;
;;
;; * Compilation buffers
;;
;;
;; You may use `compilation-shell-minor-mode' with comint-mode buffers (e.g. M-x shell)
;; and the comint configuration previously described.
;;
;; For standalone compilation-mode buffers, you may use the following
;; configuration:
;;
;; (setq compilation-environment '("TERM=xterm-256color"))
;;
;; (add-hook 'compilation-start-hook
;;           (lambda (proc)
;;             ;; We need to differentiate between compilation-mode buffers
;;             ;; and running as part of comint (which at this point we assume
;;             ;; has been configured separately for xterm-color)
;;             (when (eq (process-filter proc) 'compilation-filter)
;;               ;; This is a process associated with a compilation-mode buffer.
;;               ;; We may call `xterm-color-filter' before its own filter function.
;;               (set-process-filter
;;                proc
;;                (lambda (proc string)
;;                  (funcall 'compilation-filter proc (xterm-color-filter string)))))))
;;
;; Standalone compilation-mode buffer support is experimental, report any
;; issues that may arise.
;;
;;
;;; Test:
;;
;; M-x xterm-color-test
;;
;; For comint or eshell:
;;
;; M-x shell || M-x eshell
;;
;; perl tests/xterm-colortest && perl tests/256colors2.pl
;;
;;
;;; Code:

(require 'cl-lib)

(defgroup xterm-color nil
  "Translates ANSI control sequences to text properties."
  :prefix "xterm-color-"
  :group 'processes)

;;
;; CUSTOM
;;

(defcustom xterm-color-debug nil
  "Print ANSI state machine debug information in *Messages* if T."
  :type 'boolean
  :group 'xterm-color)

(defcustom xterm-color-names
  ["#192033"    ; black
   "#A93F43"    ; red
   "#59963A"    ; green
   "#BE8A2D"    ; yellow
   "#4068A3"    ; blue
   "#7F60A7"    ; magenta
   "#4E9B9B"    ; cyan
   "#7E8A90"]   ; white
  "The default colors to use as regular ANSI colors."
  :type '(vector string string string string string string string string)
  :group 'xterm-color)

(defcustom xterm-color-names-bright
  ["#666666"    ; black
   "#EC6261"    ; red
   "#ADCF44"    ; green
   "#F0C649"    ; yellow
   "#63B4F6"    ; blue
   "#CB77F9"    ; magenta
   "#86D7DB"    ; cyan
   "#D3D2D1"]   ; white
  "The default colors to use as bright ANSI colors."
  :type '(vector string string string string string string string string)
  :group 'xterm-color)

;;
;; Buffer locals, used by state machine
;;

(defvar xterm-color--current nil
  "Hash table with current ANSI color.")

(make-variable-buffer-local 'xterm-color--current)

(defvar xterm-color-preserve-properties nil
  "If T, preserve existing text properties on input about to be filtered.
This should be NIL most of the time as it can mess up the internal state
machine if it encounters ANSI data with text properties applied.  It is
really meant for and works fine with eshell.")

(make-variable-buffer-local 'xterm-color-preserve-properties)

(defvar xterm-color--char-buffer ""
  "Buffer with characters that the current ANSI color applies to.
In order to avoid having per-character text properties, we grow this
buffer dynamically until we encounter an ANSI reset sequence.

Once that happens, we generate a single text property for the entire string.")

(make-variable-buffer-local 'xterm-color--char-buffer)

(defvar xterm-color--csi-buffer ""
  "Buffer with current ANSI CSI sequence bytes.")

(make-variable-buffer-local 'xterm-color--csi-buffer)

(defvar xterm-color--osc-buffer ""
  "Buffer with current ANSI OSC sequence bytes.")

(make-variable-buffer-local 'xterm-color--osc-buffer)

(defvar xterm-color--state :char
  "The current state of the ANSI sequence state machine.")

(make-variable-buffer-local 'xterm-color--state)

(defvar xterm-color--attributes 0)

(make-variable-buffer-local 'xterm-color--attributes)


(defconst +xterm-color--bright+    1)
(defconst +xterm-color--italic+    2)
(defconst +xterm-color--underline+ 4)
(defconst +xterm-color--strike+    8)
(defconst +xterm-color--negative+  16)
(defconst +xterm-color--frame+     32)
(defconst +xterm-color--overline+  64)

;;
;; Functions
;;

(cl-defun xterm-color--string-properties (string)
  (cl-loop
   with res = '()
   with pos = 0
   do
   (let ((next-pos (next-property-change pos string)))
     (if next-pos
         (progn
           (push (list pos (text-properties-at pos string) (substring string pos next-pos)) res)
           (setq pos next-pos))
       (push (list pos (text-properties-at pos string) (substring string pos)) res)
       (cl-return-from xterm-color--string-properties (nreverse res))))))

(defun xterm-color--message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS if `xterm-color-debug' is T."
  (when xterm-color-debug
    (let ((message-truncate-lines t))
      (apply 'message format-string args)
      (message nil))))

(defun xterm-color--dispatch-SGR (elems)
  (cl-loop
   for elem = (cl-first elems)
   while elem do
   (cond
    ;; Reset
    ((= 0 elem)
     (clrhash xterm-color--current)
     (setq xterm-color--attributes 0)
     (setq elems (cdr elems)))
    ;; ANSI FG color
    ((and (>= elem 30)
          (<= elem 37))
     (setf (gethash 'foreground-color xterm-color--current)
           (- elem 30))
     (setq elems (cdr elems)))
    ;; ANSI BG color
    ((and (>= elem 40)
          (<= elem 47))
     (setf (gethash 'background-color xterm-color--current)
           (- elem 40))
     (setq elems (cdr elems)))
    ;; XTERM 256 FG color
    ((= 38 elem)
     (setf (gethash 'foreground-color xterm-color--current)
           (xterm-color--256 (cl-third elems)))
     (setq elems (cl-cdddr elems)))
    ;; XTERM 256 BG color
    ((= 48 elem)
     (setf (gethash 'background-color xterm-color--current)
           (xterm-color--256 (cl-third elems)))
     (setq elems (cl-cdddr elems)))
    ;; Reset to default FG color
    ((= 39 elem)
     (remhash 'foreground-color xterm-color--current)
     (setq elems (cdr elems)))
    ;; Reset to default BG color
    ((= 49 elem)
     (remhash 'background-color xterm-color--current)
     (setq elems (cdr elems)))
    ;; Bright color
    ((= 1 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--bright+))
     (setq elems (cdr elems)))
    ;; Faint color, emulated as normal intensity
    ((= 2 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--bright+)))
     (setq elems (cdr elems)))
    ;; Italic
    ((= 3 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--italic+))
     (setq elems (cdr elems)))
    ;; Underline
    ((= 4 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--underline+))
     (setq elems (cdr elems)))
    ;; Negative
    ((= 7 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--negative+))
     (setq elems (cdr elems)))
    ;; Strike through
    ((= 9 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--strike+))
     (setq elems (cdr elems)))
    ;; Normal intensity
    ((= 22 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--bright+)))
     (setq elems (cdr elems)))
    ;; No italic
    ((= 23 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--italic+)))
     (setq elems (cdr elems)))
    ;; No underline
    ((= 24 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--underline+)))
     (setq elems (cdr elems)))
    ;; No negative
    ((= 27 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--negative+)))
     (setq elems (cdr elems)))
    ;; No strike through
    ((= 29 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--strike+)))
     (setq elems (cdr elems)))
    ;; Frame
    ((= 51 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--frame+))
     (setq elems (cdr elems)))
    ;; Overline
    ((= 53 elem)
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--overline+))
     (setq elems (cdr elems)))
    ;; No frame
    ((= 54 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--frame+)))
     (setq elems (cdr elems)))
    ;; No overline
    ((= 55 elem)
     (setq xterm-color--attributes
           (logand xterm-color--attributes
                   (lognot +xterm-color--overline+)))
     (setq elems (cdr elems)))
    ;; AIXTERM hi-intensity FG color
    ((and (>= elem 90)
          (<= elem 97))
     (setf (gethash 'foreground-color xterm-color--current)
           (- elem 90))
     (setq xterm-color--attributes
           (logior xterm-color--attributes
                   +xterm-color--bright+))
     (setq elems (cdr elems)))
    ;; Fallback
    (t (xterm-color--message "xterm-color: not implemented SGR attribute %s" elem)
       (setq elems (cdr elems))))))


(defun xterm-color--dispatch-csi (csi)
  (let* ((len (1- (length csi)))
         (term (aref csi len)))
    (cond ((= ?m term)
           ;; SGR
           (if (= len 0)
               (xterm-color--dispatch-SGR '(0))
             (let ((len (1- len)))              ; Don't need the ?m character
               (subst-char-in-string 59 ? csi t)
               (xterm-color--dispatch-SGR
                ;; The following is somewhat faster than
                ;; (mapcar 'string-to-number (split-string csi))
                (cl-loop with idx = 0
                         while (<= idx len)
                         for (num . end) = (read-from-string csi idx (1+ len))
                         do (progn (cl-assert (integerp num))
                                   (setf idx end))
                         collect num)))))
          ((= ?J term)
           ;; Clear screen
           (xterm-color--message "xterm-color: %s CSI not implemented (clear screen)" csi))
          ((= ?C term)
           (let ((num (string-to-number (substring csi 0 len))))
             (setq xterm-color--char-buffer
                   (concat xterm-color--char-buffer
                           (make-string num 32)))))
          (t
           (xterm-color--message "xterm-color: %s CSI not implemented" csi)))))


(defun xterm-color--256 (color)
  (cond ((and (>= color 232)
              (<= color 255))
         ;; Grayscale
         (let ((val (+ 8 (* (- color 232) 10))))
           (format "#%02x%02x%02x"
                   val val val)))
        ((<= color 7)
         ;; Normal ANSI color
         (aref xterm-color-names color))
        ((and (>= color 8)
              (<= color 15))
         ;; Bright ANSI color
         (aref xterm-color-names-bright (- color 8)))
        (t (let* ((color-table [0 #x5f #x87 #xaf #xd7 #xff])
                  (color (- color 16))
                  (red (/ color 36))
                  (color (mod color 36))
                  (green (/ color 6))
                  (color (mod color 6))
                  (blue color))
             ;; XTERM 256 color
             (format "#%02x%02x%02x"
                     (aref color-table red)
                     (aref color-table green)
                     (aref color-table blue))))))

(defun xterm-color--make-property ()
  (let ((ret nil)
        (fg (gethash 'foreground-color xterm-color--current))
        (bg (gethash 'background-color xterm-color--current)))
    (cl-macrolet ((is-set? (attrib) `(> (logand ,attrib xterm-color--attributes) 0)))
      (when (is-set? +xterm-color--italic+)
        (push `(:slant italic) ret))
      (when (is-set? +xterm-color--underline+)
        (push `(:underline t) ret))
      (when (is-set? +xterm-color--strike+)
        (push `(:strike-through t) ret))
      (when (is-set? +xterm-color--negative+)
        (push `(:inverse-video t) ret))
      (when (is-set? +xterm-color--overline+)
        (push `(:overline t) ret))
      (when (is-set? +xterm-color--frame+)
        (push `(:box t) ret))
      (when fg
        (push `(:foreground ,(if (stringp fg)
                                 fg
                               (if (is-set? +xterm-color--bright+)
                                   (aref xterm-color-names-bright fg)
                                 (aref xterm-color-names fg))))
              ret))
      (when bg
        (push `(:background ,(if (stringp bg) bg (aref xterm-color-names bg)))
              ret)))
    ret))

(defun xterm-color-filter-real (string)
  "Translate ANSI color sequences in STRING into text properties.
Returns new STRING with text properties applied.

This function strips text properties that may be present in STRING."
  ;; It is *a lot* faster to keep track of propertized strings in a list
  ;; and mapconcat at the end, than using a temporary buffer to insert them.
  (let ((result nil))
    (cl-macrolet ((output (x) `(push ,x result))
                  (update (x place) `(setq ,place (concat ,place (string ,x))))
                  (new-state (state) `(setq xterm-color--state ,state))
                  (has-color? () `(or (> (hash-table-count xterm-color--current) 0)
                                      (not (= xterm-color--attributes 0))))
                  (maybe-fontify ()
                                 `(when (> (length xterm-color--char-buffer) 0)
                                    (if (has-color?)
                                        (output (propertize xterm-color--char-buffer 'xterm-color t
                                                            (if font-lock-mode 'font-lock-face 'face) (xterm-color--make-property)))
                                      (output xterm-color--char-buffer))
                                    (setq xterm-color--char-buffer ""))))
      (cl-loop for char across string do
	       (cl-case xterm-color--state
		 (:char
		  (cond
		   ((= char 27)		; ESC
		    (maybe-fontify)
		    (new-state :ansi-esc))
		   (t
		    (if (has-color?)
			(update char xterm-color--char-buffer)
		      (output (string char))))))
		 (:ansi-esc
		  (cond ((= char ?\[)
			 (new-state :ansi-csi))
			((= char ?\])
			 (new-state :ansi-osc))
			(t
			 (update char xterm-color--char-buffer)
			 (new-state :char))))
		 (:ansi-csi
		  (update char xterm-color--csi-buffer)
		  (when (and (>= char #x40)
			     (<= char #x7e))
		    ;; Dispatch
		    (xterm-color--dispatch-csi xterm-color--csi-buffer)
		    (setq xterm-color--csi-buffer "")
		    (new-state :char)))
		 (:ansi-osc
		  ;; Read entire sequence
		  (update char xterm-color--osc-buffer)
		  (cond ((= char 7)
			 ;; BEL
					;(xterm-color--dispatch-osc xterm-color--osc-buffer)
			 (setq xterm-color--osc-buffer "")
			 (new-state :char))
			((= char 27)
			 ;; ESC
			 (new-state :ansi-osc-esc))))
		 (:ansi-osc-esc
		  (update char xterm-color--osc-buffer)
		  (cond ((= char ?\\)
					;(xterm-color--dispatch-osc xterm-color--osc-buffer)
			 (setq xterm-color--osc-buffer "")
			 (new-state :char))
			(t (new-state :ansi-osc))))))
      (when (eq xterm-color--state :char) (maybe-fontify)))
    (mapconcat 'identity (nreverse result) "")))


(defun xterm-color-filter (string)
  "Translate ANSI color sequences in STRING into text properties.
Returns new STRING with text properties applied.

This function will check if `xterm-color-preserve-properties' is
set to T and only call `xterm-color-filter-real' on substrings
that do not have text properties applied (passing through the rest
unmodified).  Preserving properties in this fashion is really a hack
and not very robust as there may be situations where text properties
are applied on ANSI data, which will mess up the state machine.
It works fine with and is really meant for eshell though.

This can be inserted into `comint-preoutput-filter-functions'."
  (when (null xterm-color--current)
    (setq xterm-color--current (make-hash-table)))
  (if (not xterm-color-preserve-properties)
      (xterm-color-filter-real string)
    (cl-loop with res = nil
	     for (_ props substring) in (xterm-color--string-properties string) do
	     (push (if props substring (xterm-color-filter-real substring))
		   res)
	     finally return (mapconcat 'identity (nreverse res) ""))))

;; This will be removed in 2.0, it's here so as not to break existing configs
;; for 1.0 -> 1.6 transition.
(defalias 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)

;;
;; Interactive
;;


(cl-defun xterm-color-colorize-buffer ()
  "Apply `xterm-color-filter' to current buffer, and replace its contents."
  (interactive)
  (let ((read-only-p buffer-read-only))
    (when read-only-p
      (unless (y-or-n-p "Buffer is read only, continue colorizing? ")
        (cl-return-from xterm-color-colorize-buffer))
      (read-only-mode -1))
    (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
    (goto-char (point-min))
    (when read-only-p (read-only-mode 1))))


;;
;; Tests
;;

(let ((test-attributes
       '((1  . "bright")
         (51 . "frame")
         (3  . "italic")
         (4  . "underline")
         (7  . "negative")
         (9  . "strike through")
         (53 . "overline"))))

  (defun xterm-color--test-ansi ()
    ;; System colors
    (insert "* ANSI system colors\n\n")
    (cl-loop for color from 40 to 47 do
	     (insert (xterm-color-filter (format "[0;%sm  " color)))
	     finally (insert (xterm-color-filter "[0m\n\n")))

    ;; Attributes (no color)
    (insert "* ANSI attributes (default colors)\n\n")
    (cl-loop for (attrib . name) in test-attributes do
	     (insert (xterm-color-filter (format "[0;%smThis is only a test![0m\t --[ %s ]\n" attrib name)))
	     finally (insert "\n"))

    ;; Attributes (blue fg)
    (insert "* ANSI attributes (blue foreground)\n\n")
    (cl-loop for (attrib . name) in test-attributes do
	     (insert (xterm-color-filter (format "[0;34;%smThis is only a test![0m\t --[ %s ]\n" attrib name)))
	     finally (insert "\n"))

    ;; Attributes (blue bg)
    (insert "* ANSI attributes (blue background)\n\n")
    (cl-loop for (attrib . name) in test-attributes do
	     (insert (xterm-color-filter (format "[0;44;%smThis is only a test![0m\t --[ %s ]\n" attrib name)))
	     finally (insert "\n"))))

(defun xterm-color--test-xterm ()
  ;; Normal ANSI colors mapped to XTERM
  (insert "* ANSI colors mapped to XTERM\n\n")
  (cl-loop for color from 0 to 7 do
	   (insert (xterm-color-filter (format "[48;5;%sm  " color)))
	   finally (insert (xterm-color-filter "[0m\n\n")))

  ;; Bright ANSI colors mapped to XTERM
  (insert "* ANSI bright colors mapped to XTERM\n\n")
  (cl-loop for color from 8 to 15 do
	   (insert (xterm-color-filter (format "[48;5;%sm  " color)))
	   finally (insert (xterm-color-filter "[0m\n\n")))

  ;; XTERM 256 color cubes
  (insert "*  XTERM 256 color cubes\n\n")
  (cl-loop for green from 0 to 5 do
	   (cl-loop for red from 0 to 5 do
		    (cl-loop for blue from 0 to 5
			     for color = (+ 16 (* 36 red) (* green 6) blue) do
			     (insert (xterm-color-filter (format "[48;5;%sm  [0m" color))))
		    (insert (xterm-color-filter "[0m ")))
	   (insert "\n"))
  (insert "\n")

  (insert "*  XTERM color grayscale ramp\n\n")
  (cl-loop for color from 232 to 255 do
	   (insert (xterm-color-filter (format "[48;5;%sm  " color)))
	   finally (insert (xterm-color-filter "[0m\n\n"))))

(defun xterm-color-test ()
  "Create and display a new buffer that contains ANSI control sequences."
  (interactive)
  (let* ((name (generate-new-buffer-name "xterm-color-test"))
         (buf (get-buffer-create name)))
    (switch-to-buffer buf))
  (xterm-color--test-ansi)
  (xterm-color--test-xterm)
  (setq buffer-read-only t))


(provide 'xterm-color)
;;; xterm-color.el ends here
