;;; xterm-color.el --- ANSI & XTERM 256 color support -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018 xristos@sdf.lonestar.org
;; All rights reserved

;; Version: 1.7 - 2018-2-2
;; Author: xristos <xristos@sdf.lonestar.org>
;; URL: https://github.com/atomontage/xterm-color
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: faces

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
;; Translate ANSI control sequences into text properties.
;;
;; * Regular ANSI colors
;;
;; * XTERM 256 colors
;;
;; * Works with compilation-mode
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
;; * You can replace ansi-color.el with xterm-color for all comint buffers,
;;   but this may create problems with modes that propertize strings
;;   and feed them through comint-preoutput-filter-functions since xterm-color-filter
;;   will strip all text properties.
;;
;;   The recommended configuration is to remove ansi-color-process-output from
;;   comint-output-filter-functions and add xterm-color-filter as the *first*
;;   hook in the *buffer-local* comint-preoutput-filter-functions for any comint-based
;;   mode that you would like it to affect (e.g. shell-mode).
;;
;;   An example configuration for shell-mode (M-x shell) is shown below:
;;
;; (setq comint-output-filter-functions
;;       (remove 'ansi-color-process-output comint-output-filter-functions))
;;
;; (add-hook 'shell-mode-hook
;;           (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
;;
;; Also set TERM accordingly (xterm-256color)
;;
;; * You can also use it with eshell (and thus get color output from system ls):
;;
;; (require 'eshell)
;;
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda ()
;;             (setq xterm-color-preserve-properties t)))
;;
;;  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;;
;;  Don't forget to setenv TERM xterm-256color
;;
;;
;; * Compilation buffers
;;
;;
;; You may use `compilation-shell-minor-mode' with shell-mode buffers
;; and the configuration previously described.
;;
;; For standalone compilation-mode buffers, you may use the following
;; configuration:
;;
;; (setq compilation-environment '("TERM=xterm-256color"))
;;
;; (add-hook 'compilation-start-hook
;;           (lambda (proc)
;;             ;; Need to differentiate between compilation-mode buffers
;;             ;; and running as part of comint (which at this point we assume
;;             ;; has been configured separately for xterm-color)
;;             (when (eq (process-filter proc) 'compilation-filter)
;;               ;; This is a process associated with a compilation-mode buffer.
;;               ;; `xterm-color-filter' should be called before its own filter function.
;;               (set-process-filter
;;                proc
;;                (lambda (proc string)
;;                  (funcall 'compilation-filter proc (xterm-color-filter string)))))))

;;; Test:
;;
;; M-x xterm-color-test
;;
;; For shell or eshell:
;;
;; M-x shell || M-x eshell
;;
;; perl tests/xterm-colortest && perl tests/256colors2.pl

;;; Code:

(require 'cl-lib)

(defgroup xterm-color nil
  "Translate ANSI control sequences to text properties."
  :prefix "xterm-color-"
  :group 'processes)

;;;
;;; CUSTOM
;;;

(defcustom xterm-color-debug nil
  "Print ANSI state machine debug information in *Messages* if not NIL."
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

;;;
;;; Buffer locals, used by state machine
;;;

(defvar xterm-color-preserve-properties nil
  "If T, preserve existing text properties on input about to be filtered.
This should be NIL most of the time as it can mess up the internal state
machine if it encounters ANSI data with text properties applied.  It is
really meant for and works fine with eshell.")

(make-variable-buffer-local 'xterm-color-preserve-properties)

(defvar xterm-color--current-fg nil)

(make-variable-buffer-local 'xterm-color--current-fg)

(defvar xterm-color--current-bg nil)

(make-variable-buffer-local 'xterm-color--current-bg)

(defvar xterm-color--char-list nil
  "List with characters that the current ANSI color applies to.
All characters are stored in reverse, LIFO, order.")

(make-variable-buffer-local 'xterm-color--char-list)

(defvar xterm-color--CSI-list nil
  "List with current ANSI CSI sequence bytes (characters).
All characters are stored in reverse, LIFO, order.")

(make-variable-buffer-local 'xterm-color--CSI-list)

(defvar xterm-color--state :char
  "The current state of the ANSI sequence state machine.")

(make-variable-buffer-local 'xterm-color--state)

(defvar xterm-color--attributes 0
  "Bitvector that keeps track of bright, italic, underline, strike-through,
inverse-color, frame, overline SGR state machine bits.")

(make-variable-buffer-local 'xterm-color--attributes)

(defvar xterm-color--face-cache (make-hash-table :weakness 'value)
  "Cache for auto-generated faces.")

;;;
;;; Constants
;;;

(defconst +xterm-color--table-256+ [0 #x5f #x87 #xaf #xd7 #xff])

;;;
;;; Internal API
;;;

(cl-defun xterm-color--string-properties (string)
  (cl-loop
   with pos = 0 and result do
   (let ((next-pos (next-property-change pos string)))
     (if next-pos
         (progn
           (push (list pos (text-properties-at pos string) (substring string pos next-pos)) result)
           (setq pos next-pos))
       (push (list pos (text-properties-at pos string) (substring string pos)) result)
       (cl-return-from xterm-color--string-properties (nreverse result))))))

(defun xterm-color--message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS if `xterm-color-debug' is not NIL."
  (when xterm-color-debug
    (let ((message-truncate-lines t))
      (apply 'message format-string args)
      (message nil))))

(defmacro xterm-color--with-constant-attributes (&rest body)
  (declare (indent defun))
  `(cl-symbol-macrolet
       ((+bright+              1)
        (+italic+              2)
        (+underline+           4)
        (+strike-through+      8)
        (+negative+           16)
        (+frame+              32)
        (+overline+           64))
     ,@body))

(defmacro xterm-color--make-SGR-dispatch-table (elem SGR &rest body)
  (declare (indent defun))
  `(xterm-color--with-constant-attributes
     (cl-macrolet ((set-fg (c)     `(setq xterm-color--current-fg ,c))
                   (set-bg (c)     `(setq xterm-color--current-bg ,c))
                   (set-attr (a)   `(setq xterm-color--attributes
                                          (logior xterm-color--attributes ,a)))
                   (clear-attr (a) `(setq xterm-color--attributes
                                          (logand xterm-color--attributes
                                                  (logand #xff (lognot ,a)))))
                   (clear-attrs () `(setq xterm-color--attributes 0)))
       (cl-loop
        for ,elem = (cl-first ,SGR)
        while ,SGR do
        (cl-case ,elem
          ,@(cl-loop for plist in body
                     for match = (plist-get plist :match)
                     for jump  = (plist-get plist :jump)
                     collect (append match `((setq ,SGR ,(or jump `(cdr ,SGR))))))
          (t (xterm-color--message "xterm-color: not implemented SGR attribute %s" ,elem)
             (setq ,SGR (cdr ,SGR))))))))

(defsubst xterm-color--dispatch-SGR (SGR)
  "Update state machine based on SGR which should be a list of SGR attributes (integers)."
  (xterm-color--make-SGR-dispatch-table elem SGR
    (:match (0 (set-fg nil) (set-bg nil) (clear-attrs)))         ; RESET everything
    (:match ((30 31 32 33 34 35 36 37) (set-fg (- elem 30))))    ; ANSI FG color
    (:match ((40 41 42 43 44 45 46 47) (set-bg (- elem 40))))    ; ANSI BG color
    (:match (38 (set-fg (cl-third SGR))) :jump (cl-cdddr SGR))   ; XTERM 256 FG color
    (:match (48 (set-bg (cl-third SGR))) :jump (cl-cdddr SGR))   ; XTERM 256 BG color
    (:match (39 (set-fg nil)))
    (:match (49 (set-bg nil)))
    (:match (1 (set-attr +bright+)))
    (:match (2 (clear-attr +bright+)))
    (:match (3 (set-attr +italic+)))
    (:match (4 (set-attr +underline+)))
    (:match (7 (set-attr +negative+)))
    (:match (9 (set-attr +strike-through+)))
    (:match (22 (clear-attr +bright+)))
    (:match (23 (clear-attr +italic+)))
    (:match (24 (clear-attr +underline+)))
    (:match (27 (clear-attr +negative+)))
    (:match (29 (clear-attr +strike-through+)))
    (:match (51 (set-attr +frame+)))
    (:match (53 (set-attr +overline+)))
    (:match (54 (clear-attr +frame+)))
    (:match (55 (clear-attr +overline+)))
    (:match ((90 91 92 93 94 95 96 97)                           ; AIXTERM hi-intensity FG
             (set-fg (- elem 90))
             (set-attr +bright+)))))

(defsubst xterm-color--SGR-params (list)
  (cl-loop
   with mul = 1 and num = 0 and ret
   for c    = (car list) while c do
   (if (/= 59 c)
       (let ((e (- c 48)))
         (unless (<= 0 e 9) (error "Invalid SGR attribute: %s" e))
         (cl-incf num (* mul e))
         (setq mul (* mul 10)))
     (push num ret)
     (setq num 0 mul 1))
   (setq list (cdr list))
   finally return (push num ret)))

(defsubst xterm-color--dispatch-CSI ()
  "Update state machine based on CSI parameters collected so far.
The parameters are taken from `xterm-color--CSI-list'."
  (let* ((csi xterm-color--CSI-list)
         (term (car csi))
         (params (cdr csi)))
    (setq xterm-color--CSI-list nil)
    (cond ((= ?m term)
           ;; SGR
           (xterm-color--dispatch-SGR
            (if (null params)
                '(0)
              (xterm-color--SGR-params params))))
          (t
           (xterm-color--message
            "xterm-color: %s CSI %s not implemented"
            csi (if (= ?J term) "(clear screen)" ""))))))


(defmacro xterm-color--with-macro-helpers (&rest body)
  `(xterm-color--with-constant-attributes
    (cl-macrolet
        ((output (x)          `(push ,x result))
         (update-char-list () `(push char xterm-color--char-list))
         (update-CSI-list ()  `(push char xterm-color--CSI-list))

         (new-state (s)       `(setq state ,s))
         (has-color? ()       `(or xterm-color--current-fg
                                   xterm-color--current-bg
                                   (/= xterm-color--attributes 0)))
         (is-set? (attrib)    `(/= (logand ,attrib xterm-color--attributes) 0))
         (face-cache-get ()   `(gethash (logior (ash xterm-color--attributes 16)
                                                (ash (or xterm-color--current-bg 0) 8)
                                                (or xterm-color--current-fg 0))
                                        xterm-color--face-cache))
         (face-add (k v)      `(setq plistf (plist-put plistf ,k ,v)))
         (make-face ()        `(or (face-cache-get)
                                   (let (plistf)
                                     (when (is-set? +italic+)         (face-add :slant 'italic))
                                     (when (is-set? +underline+)      (face-add :underline t))
                                     (when (is-set? +strike-through+) (face-add :strike-through t))
                                     (when (is-set? +negative+)       (face-add :inverse-video t))
                                     (when (is-set? +overline+)       (face-add :overline t))
                                     (when (is-set? +frame+)          (face-add :box t))
                                     (when xterm-color--current-fg
                                       (face-add :foreground
                                                 (xterm-color-256
                                                  (if (and (<= xterm-color--current-fg 7)
                                                           (is-set? +bright+))
                                                      (+ xterm-color--current-fg 8)
                                                    xterm-color--current-fg))))
                                     (when xterm-color--current-bg
                                       (face-add :background (xterm-color-256 xterm-color--current-bg)))
                                     (setf (face-cache-get) plistf))))
         (maybe-fontify ()    '(when xterm-color--char-list
                                 (let ((s (concat (nreverse xterm-color--char-list))))
                                   (when (has-color?)
                                     (add-text-properties
                                      0 (length s)
                                      (list 'xterm-color t (if font-lock-mode 'font-lock-face 'face) (make-face))
                                      s))
                                   (output s))
                                 (setq xterm-color--char-list nil))))
      ,@body)))

;;;
;;; Exports
;;;

;;;###autoload
(defun xterm-color-filter-strip (string)
  "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function strips text properties that may be present in STRING."
  (xterm-color--with-macro-helpers
   (cl-loop
    with state = xterm-color--state and result
    for char across string do
    (cl-case state
      (:char
       (cond
        ((= char 27)                    ; ESC
         (maybe-fontify)
         (new-state :ansi-esc))
        (t
         (if (has-color?)
             (update-char-list)
           (output (string char))))))
      (:ansi-esc
       (cond ((= char ?\[)
              (new-state :ansi-csi))
             ((= char ?\])
              (new-state :ansi-osc))
             (t
              (update-char-list)
              (new-state :char))))
      (:ansi-csi
       (update-CSI-list)
       (when (and (>= char #x40)
                  (<= char #x7e))
         (xterm-color--dispatch-CSI)
         (new-state :char)))
      (:ansi-osc
       ;; OSC sequences are skipped
       (cond ((= char 7)
              (new-state :char))
             ((= char 27)
              ;; ESC
              (new-state :ansi-osc-esc))))
      (:ansi-osc-esc
       (cond ((= char ?\\)
              (new-state :char))
             (t (new-state :ansi-osc)))))
    finally return
    (progn (when (eq state :char) (maybe-fontify))
           (setq xterm-color--state state)
           (mapconcat 'identity (nreverse result) "")))))

;;;###autoload
(defun xterm-color-filter (string)
  "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function will check if `xterm-color-preserve-properties' is
set to T and only call `xterm-color-filter-strip' on substrings
that do not have text properties applied (passing through the rest
unmodified).  Preserving properties in this fashion is really a hack
and not very robust as there may be situations where text properties
are applied on ANSI data, which will mess up the state machine.
It works fine with and is really meant for eshell though.

This can be inserted into `comint-preoutput-filter-functions'."
  (if (not xterm-color-preserve-properties)
      (xterm-color-filter-strip string)
    (cl-loop
     with result
     for (_ props substring) in (xterm-color--string-properties string) do
     (push (if props substring (xterm-color-filter-strip substring))
           result)
     finally return (mapconcat 'identity (nreverse result) ""))))

;;;###autoload
(defun xterm-color-256 (color)
  (cond ((and (>= color 232)
              (<= color 255))
         ;; Grayscale
         (let ((val (+ 8 (* (- color 232) 10))))
           (format "#%02x%02x%02x" val val val)))
        ((<= color 7)
         ;; Normal ANSI color
         (aref xterm-color-names color))
        ((and (>= color 8)
              (<= color 15))
         ;; Bright ANSI color
         (aref xterm-color-names-bright (- color 8)))
        (t (let* ((color  (- color 16))
                  (red    (/ color 36))
                  (color  (mod color 36))
                  (green  (/ color 6))
                  (color  (mod color 6))
                  (blue   color))
             ;; XTERM 256 color
             (format "#%02x%02x%02x"
                     (aref +xterm-color--table-256+ red)
                     (aref +xterm-color--table-256+ green)
                     (aref +xterm-color--table-256+ blue))))))

;;;
;;; Interactive
;;;

;;;###autoload
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


;;;
;;; Tests
;;;

(defmacro xterm-color--bench (path &optional repetitions)
  `(benchmark-run-compiled ,repetitions
     (with-temp-buffer
       (insert-file-contents-literally ,path)
       (xterm-color-colorize-buffer))))

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

;;;###autoload
(defun xterm-color-test ()
  "Create and display a new buffer that contains ANSI control sequences."
  (interactive)
  (let* ((name (generate-new-buffer-name "*xterm-color-test*"))
         (buf (get-buffer-create name)))
    (switch-to-buffer buf))
  (xterm-color--test-ansi)
  (xterm-color--test-xterm)
  (setq buffer-read-only t)
  (goto-char (point-min)))


(provide 'xterm-color)
;;; xterm-color.el ends here
