;;; xterm-color.el --- ANSI & XTERM 256 color support -*- lexical-binding: t -*-

;; Copyright (C) 2010-2019 xristos@sdf.lonestar.org
;; All rights reserved

;; Version: 1.8 - 2019-03-01
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
;; Interactively or from Emacs Lisp call xterm-color-colorize-buffer
;; to colorize an entire buffer.
;;
;; In Emacs Lisp, call xterm-color-filter to propertize strings that you can
;; then insert into a buffer. All state is kept in buffer-local variables
;; which means that control sequences can span xterm-color-filter call boundaries.
;;
;; Example:
;;
;; (let ((buffer (generate-new-buffer "*xterm-color-test*")))
;;   (with-current-buffer buffer
;;     (insert (xterm-color-filter "[0;1;3;4"))
;;     (insert (xterm-color-filter ";35"))
;;     (insert (xterm-color-filter ";51mThis is only a test"))
;;     (insert (xterm-color-filter "[0m")))
;;   (switch-to-buffer buffer))
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

(defvar xterm-color--face-cache nil
  "Cache for auto-generated face attributes.")

(make-variable-buffer-local 'xterm-color--face-cache)

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


;;;
;;; SGR state machine
;;;


(cl-defmacro xterm-color--with-SGR-constants (&body body)
  (declare (indent defun))
  `(cl-symbol-macrolet
       ((+bright+           1)
        (+italic+           2)
        (+underline+        4)
        (+strike-through+   8)
        (+negative+        16)
        (+frame+           32)
        (+overline+        64))
     ,@body))

(cl-defmacro xterm-color--create-SGR-table ((attrib SGR-list) &body body)
  "Create an iteration/dispatch table based on provided rules that match SGR attributes.
For each attribute in SGR-LIST, check to see if it matches a rule in BODY. If so,
evaluate the rule body.

ATTRIB should be a symbol that will be bound to SGR-LIST attributes in BODY.
SGR-LIST should be a list of SGR attributes (integers) in LIFO order.
BODY should contain rules with each rule being a list of form:

 (:match (condition &optional skip) rule-body-form..)

CONDITION should be a Lisp form which will be evaluated as part of a COND
condition clause. If it is an atom, it will be rewritten to (= CONDITION ATTRIB).
Otherwise it will be used as is. As per COND statement, if CONDITION evaluates
to T, rule body forms will be evaluated as part of the body of the COND clause.
If SKIP is given, it should be a function that will be used to iterate over SGR-LIST,
by returning a list that the next iteration will use as SGR-LIST. If not given, CDR will
be used, meaning the iteration will go down the SGR-LIST one element at a time. By
using other functions, it is possible to skip elements."
  (declare (indent defun))
  `(xterm-color--with-SGR-constants
     (cl-macrolet
         ;; The following macros can be used in the match rule bodies
         ((set-a! (attr &key clear)
                  (if clear
                      `(setq xterm-color--attributes
                             (logand xterm-color--attributes
                                     (logand #xff (lognot ,attr))))
                    `(setq xterm-color--attributes
                           (logior xterm-color--attributes ,attr))))
          (set-f! (fg-color) `(setq xterm-color--current-fg ,fg-color))
          (set-b! (bg-color) `(setq xterm-color--current-bg ,bg-color))
          (reset! ()         `(setq xterm-color--current-fg nil
                                    xterm-color--current-bg nil
                                    xterm-color--attributes 0)))
       (cl-loop
        for ,attrib = (cl-first ,SGR-list)
        while ,SGR-list do
        (cond
         ,@(cl-loop
            for skip = nil
            for (tag (c . rest) . rule-body) in body
            when (not (eq tag :match)) do
            (error "Rule (%s (%s..)..) does not start with :match" tag c)
            when rest do
            (setq skip (car rest))
            (and (cdr rest) (error "Rule (%s (%s..)..) has malformed arguments: %s" tag c rest))
            ;; Condition part of the COND clause
            collect `(,(if (atom c) `(= ,c ,attrib) c)
                      ;; Body of the COND clause
                      ,@rule-body
                      ;; Last expression of the body of the COND clause
                      ;; that determines how many SGR attributes will be
                      ;; skipped. If a skip argument is provided to the
                      ;; match rule, it will be funcalled with SGR-list
                      ;; as the argument. Otherwise CDR will be used which
                      ;; will skip to the next SGR attribute.
                      (setq ,SGR-list ,(if skip
                                           `(funcall ,skip ,SGR-list)
                                         `(cdr ,SGR-list)))))
         (t (xterm-color--message "xterm-color: not implemented SGR attribute %s" ,attrib)
            (setq ,SGR-list (cdr ,SGR-list))))))))


(defsubst xterm-color--dispatch-SGR (SGR-list)
  "Update state machine based on SGR-LIST which should be a list of SGR attributes (integers)."
  (xterm-color--create-SGR-table (elem SGR-list)
    (:match (0)  (reset!))                             ; RESET everything
    (:match ((<= 30 elem 37)) (set-f! (- elem 30)))     ; ANSI FG color
    (:match ((<= 40 elem 47)) (set-b! (- elem 40)))     ; ANSI BG color
    (:match (39) (set-f! nil))                         ; RESET FG color
    (:match (49) (set-b! nil))                         ; RESET BG color
    (:match (1)  (set-a! +bright+))
    (:match (2)  (set-a! +bright+ :clear t))
    (:match (3)  (set-a! +italic+))
    (:match (4)  (set-a! +underline+))
    (:match (7)  (set-a! +negative+))
    (:match (9)  (set-a! +strike-through+))
    (:match (22) (set-a! +bright+ :clear t))
    (:match (23) (set-a! +italic+ :clear t))
    (:match (24) (set-a! +underline+ :clear t))
    (:match (27) (set-a! +negative+ :clear t))
    (:match (29) (set-a! +strike-through+ :clear t))
    (:match (38 'cl-cdddr)                             ; XTERM 256 FG color
            (set-f! (cl-third SGR-list)))
    (:match (48 'cl-cdddr)                             ; XTERM 256 BG color
            (set-b! (cl-third SGR-list)))
    (:match (51) (set-a! +frame+))
    (:match (53) (set-a! +overline+))
    (:match (54) (set-a! +frame+))
    (:match (55) (set-a! +overline+))
    (:match ((<= 90 elem 97))                           ; AIXTERM hi-intensity FG
            ;; Rather than setting the bright attribute, which would be a bug,
            ;; rescale the color to fall within the range 8-15 which
            ;; xterm-color-256 will map to xterm-color-names-bright.
            (set-f! (- elem 82)))))

(defsubst xterm-color--SGR-attributes (list)
  "Convert LIFO list of SGR characters to FIFO list of SGR attributes (integers).
Characters should be in the ASCII set 0-9 (decimal 48 to 57) and are converted
to integer digits by subtracting 48 from each character. E.g. Character 48 will
be converted to integer digit 0, character 49 to integer digit 1 and so on.
Character 59 (;) is not converted but signifies that all accumulated integer
digits should be reversed and combined into a single integer (SGR attribute).

Examples:

Given (48) return (0)
Given (59) return (0)
Given (48 49 50) return (210)
Given (48 49 50 59 50 50 59 48 49) return (10 22 210)

Returns FIFO list of SGR attributes."
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


;;;
;;; CSI state machine
;;;


(defsubst xterm-color--dispatch-CSI ()
  "Update state machine based on CSI parameters collected so far.
The parameters are taken from `xterm-color--CSI-list' which stores them
in LIFO order."
  (let* ((csi xterm-color--CSI-list)
         (term (car csi))                              ; final parameter, terminator
         (params (cdr csi)))                           ; rest of parameters, LIFO order
    (setq xterm-color--CSI-list nil)
    (cond ((= ?m term)
           ;; SGR
           (xterm-color--dispatch-SGR
            (if (null params)
                '(0)
              (xterm-color--SGR-attributes params))))
          (t
           (xterm-color--message
            "xterm-color: %s CSI %s not implemented"
            csi (if (= ?J term) "(clear screen)" ""))))))

(defmacro xterm-color--with-ANSI-macro-helpers (&rest body)
  (declare (indent defun))
  `(xterm-color--with-SGR-constants
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
  (or xterm-color--face-cache
      (setq xterm-color--face-cache (make-hash-table :weakness 'value)))
  (xterm-color--with-ANSI-macro-helpers
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

;;;###autoload
(defun xterm-color-clear-cache ()
  "Clear xterm color face attribute cache.
You may want to call this if you change `xterm-color-names' or
`xterm-color-names-bright' at runtime and you want to see the changes
take place in a pre-existing buffer that has had xterm-color initialized.

Since the cache is buffer-local and created on-demand when needed, this has no
effect when called from a buffer that does not have a cache."
  (interactive)
  (and xterm-color--face-cache
       (clrhash xterm-color--face-cache)
       (message "Cleared xterm-color face attribute cache")))


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
	     finally (insert "\n"))

    ;; Various test cases
    (insert "* Various\n\n")
    (insert (xterm-color-filter "Default [34;1mBright blue[39m Reset-fg-color [34mBlue (should be bright)[0m\t --[ Reseting FG color should not affect other SGR bits ]\n"))
    (insert (xterm-color-filter "Default [94mBright blue[34m Switch-to-blue (should be normal intensity)[0m\t --[ AIXTERM bright color should not set bright SGR bit ]\n"))
    (insert "\n")))

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
