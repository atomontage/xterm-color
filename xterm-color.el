;;; xterm-color.el --- ANSI, XTERM 256 and Truecolor support -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020 xristos@sdf.org
;; All rights reserved

;; Modified: 2020-05-10
;; Version: 2.0
;; Author: xristos <xristos@sdf.org>
;; URL: https://github.com/atomontage/xterm-color
;; Package-Requires: ((emacs "24.4"))
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
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Translate ANSI control sequences into text properties through state machine
;; emulation. This provides a far more accurate, comprehensive result than
;; `ansi-color.el' that is built-into Emacs, without compromising on performance.
;;
;; Please see README.org for documentation including example configurations.
;;
;;; Notes:
;;
;; Unsupported SGR attributes: 5 (slow blink), 6 (rapid blink), 8 (conceal),
;; 10 (primary font), 11-19 (alternative font), 20 (fraktur), 21 (double underline),
;; 25 (blink off), 29 (reveal), 52 (encircled), 60-65 (ideogram)
;;
;; Most of these can not be mapped to Emacs face properties. The rest may be
;; supported in a future release.
;;
;; Supported SGR attributes: Look at `xterm-color--dispatch-SGR'.
;; SGR attribute 1 is rendered as bright unless `xterm-color-use-bold-for-bright'
;; is non-nil which will, if current font has a bold variant, switch to bold.
;; SGR attributes 38 and 48 are supported in both their 256 color and truecolor
;; (24-bit) variants.

;;; Test:
;;
;; M-x xterm-color-test
;;
;; In shell or eshell:
;;
;; perl tests/xterm-colortest && perl tests/256colors2.pl
;;
;; printf "\x1b[0;1;3;4;35;51mThis is only a test\x1b[0m\n"
;;
;; Comparison with ansi-color.el:
;;
;; M-x xterm-color-test-raw then M-x xterm-color-colorize-buffer
;;
;; and contrast with
;;
;; M-x xterm-color-test-raw then M-: (ansi-color-apply-on-region (point-min) (point-max))
;;
;; Use `xterm-color--bench' for benchmarks during development.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup xterm-color nil
  "Translate ANSI control sequences to text properties."
  :prefix "xterm-color-"
  :group 'processes)


;;;
;;; CUSTOM
;;;


(defcustom xterm-color-debug nil
  "If non-nil, print ANSI state machine debug information in *Messages*."
  :type 'boolean
  :group 'xterm-color)

(defcustom xterm-color-use-bold-for-bright nil
  "If non-nil, render bright foreground attribute as bold."
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
  "Default colors to use as regular ANSI colors."
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
  "Default colors to use as bright ANSI colors."
  :type '(vector string string string string string string string string)
  :group 'xterm-color)


;;;
;;; Buffer locals, used by state machine
;;;


(defvar-local xterm-color-preserve-properties nil
  "If non-nil, preserve existing text properties on input about to be filtered.
Effectively this skips ANSI control sequence processing for input parts
that have text properties applied. This should be nil most of the time.
It is really meant for and works ok with eshell.")

(defvar-local xterm-color-render t
  "If non-nil, render SGR attributes. Otherwise, discard them.
The latter enables processing and filtering out ANSI control sequences,
without applying them to the text.")

(defvar-local xterm-color--current-fg nil)

(defvar-local xterm-color--current-bg nil)

(defvar-local xterm-color--char-list nil
  "List of characters that the current ANSI color applies to.
All characters are stored in reverse, LIFO, order.")

(defvar-local xterm-color--CSI-list nil
  "List of current ANSI CSI sequence bytes (characters).
All characters are stored in reverse, LIFO, order.")

(defvar-local xterm-color--state :char
  "Current state of ANSI state machine.

Can be one of :char, :set-char, :ansi-esc, :ansi-csi, :ansi-osc,
:ansi-osc-esc.")

(defvar-local xterm-color--attributes 0
  "Bitvector that keeps track of state machine attributes.

These are: bright, italic, underline, strike-through, inverse-color,
frame, overline.")

(defvar-local xterm-color--face-cache nil
  "Cache for auto-generated face attributes.")

(defvar-local xterm-color--truecolor-face-cache nil
  "Cache for auto-generated face attributes.")


;;;
;;; Constants
;;;


(defconst +xterm-color--table-256+ [0 #x5f #x87 #xaf #xd7 #xff])


;;;
;;; Internal API
;;;

;; The face caching scheme requires an integer width of at least 56 bits
;; to cache faces derived from truecolor (24-bit) ANSI sequences. Truecolor
;; support is therefore disabled on e.g. machines with 32-bit integers.
(defvar xterm-color--support-truecolor (>= (1+ (floor (log most-positive-fixnum 2)))
                                           59))

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

(defun xterm-color--convert-text-properties-to-overlays (beg end)
  "Transform face text properties between BEG and END, to equivalent overlays."
  (save-excursion
    (goto-char beg)
    (let ((face-prop (if (or (get-text-property (point) 'font-lock-face)
                             (next-single-property-change (point) 'font-lock-face))
                         'font-lock-face 'face)))
      (while (< (point) end)
        (let* ((pos (point))
               (current-value (get-text-property pos face-prop))
               (next-change (next-single-property-change pos face-prop nil end)))
          (when current-value
            (let ((ov (make-overlay pos next-change)))
              (overlay-put ov face-prop current-value)
              (overlay-put ov 'xterm-color t)))
          (goto-char next-change)))
      (remove-text-properties beg end (list 'xterm-color nil face-prop nil)))))

(defun xterm-color--message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS.
Also see `xterm-color-debug'."
  (when xterm-color-debug
    (let ((message-truncate-lines t))
      (message "xterm-color: %s" (apply #'format format-string args))
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
  "Create an iteration/dispatch table based on provided rules that
match SGR attributes.
For each attribute in SGR-LIST, check to see if it matches a rule in BODY and
evaluate the rule body if that is the case.

ATTRIB must be a symbol that is bound to SGR-LIST attributes in BODY.
SGR-LIST must be a list of SGR attributes (integers) in LIFO order.
BODY must contain rules with each rule being a list of form:

 (:match (condition &key (skip 1)) rule-body-form..)

CONDITION must be a Lisp form which is evaluated as part of a COND
condition clause. If it is an atom, it is rewritten to (eq CONDITION ATTRIB).
Otherwise it is used as is. As per COND statement, if CONDITION evaluates
to non-nil, rule body forms are evaluated as part of the body of the COND
clause.

SKIP, if given, must be an integer specifying the number of elements that
should be skipped before the next iteration. The default is 1,
going down SGR-LIST one element at a time."
  (declare (indent defun))
  `(xterm-color--with-SGR-constants
     (cl-macrolet
         ;; The following macros should be used in rule bodies
         ((set-a! (attr)   `(setq xterm-color--attributes
                                  (logior xterm-color--attributes ,attr)))
          (unset-a! (attr) `(setq xterm-color--attributes
                                  (logand xterm-color--attributes
                                          (logand #xff (lognot ,attr)))))

          (set-f! (fg-color) `(setq xterm-color--current-fg ,fg-color))
          (set-b! (bg-color) `(setq xterm-color--current-bg ,bg-color))

          (set-truecolor! (r g b current-color)
                          ;; A single integer must be able to
                          ;; hold and distinguish between:
                          ;;
                          ;; - 24bit truecolor values
                          ;; - ANSI colors
                          ;; - XTerm 256 colors
                          ;;
                          ;; The following packing scheme achieves that.
                          `(setq ,current-color
                                 (logior (ash r 25) (ash g 17) (ash b 9)
                                         #x100)))

          (reset! ()      `(setq xterm-color--current-fg nil
                                 xterm-color--current-bg nil
                                 xterm-color--attributes 0)))
       (cl-loop
        for ,attrib = (cl-first ,SGR-list)
        while ,SGR-list do
        (cond
         ,@(cl-loop
            for skip = 1
            for (tag (c . rest) . rule-body) in body
            when (not (eq tag :match)) do
            (error "Rule (%s (%s..)..) does not start with :match" tag c)
            when rest do
            (setq skip (plist-get rest :skip))
            (when (or (null skip) (cddr rest))
              (error "Rule (%s (%s..)..) has malformed arguments: %s" tag c rest))
            ;; Condition part of COND
            collect `(,(if (atom c) `(eq ,c ,attrib) c)
                      ;; Body of COND
                      ,@rule-body
                      (setq ,SGR-list
                            ,(if (> skip 1)
                                 `(nthcdr ,skip ,SGR-list)
                               `(cdr ,SGR-list)))))
         (t (xterm-color--message "Not implemented SGR attribute %s" ,attrib)
            (setq ,SGR-list (cdr ,SGR-list))))))))

(defsubst xterm-color--dispatch-SGR (SGR-list)
  "Update state machine based on SGR-LIST (list of SGR attributes /integers)."
  (xterm-color--create-SGR-table (elem SGR-list)
    (:match (0)  (reset!))                              ; RESET everything
    (:match ((<= 30 elem 37)) (set-f! (- elem 30)))     ; ANSI FG color
    (:match ((<= 40 elem 47)) (set-b! (- elem 40)))     ; ANSI BG color
    (:match (39) (set-f!   nil))                        ; RESET FG color (switch to default)
    (:match (49) (set-b!   nil))                        ; RESET BG color (switch to default)
    (:match (1)  (set-a!   +bright+))
    (:match (2)  (unset-a! +bright+))
    (:match (22) (unset-a! +bright+))

    (:match ((and (eq 38 (cl-first SGR-list))
                  (eq 2 (cl-second SGR-list)))          ; Truecolor (24-bit) FG color
             :skip 5)
            (when xterm-color--support-truecolor
              (if-let ((r (cl-third SGR-list))
                       (g (cl-fourth SGR-list))
                       (b (cl-fifth SGR-list)))
                  (if (or (> r 255) (> g 255) (> b 255))
                      (xterm-color--message "SGR 38;2;%s;%s;%s exceeds range"
                                            r g b)
                    (set-truecolor! r g b xterm-color--current-fg))
                (xterm-color--message "SGR 38;2;%s;%s;%s error, expected 38;2;R;G;B"
                                      r g b))))
    (:match ((and (eq 38 (cl-first SGR-list))
                  (eq 5 (cl-second SGR-list)))
             :skip 3)                                   ; XTERM 256 FG color
            (if-let ((color (cl-third SGR-list)))
                (if (> color 255)
                    (xterm-color--message "SGR 38;5;%s exceeds range" color)
                  (set-f! color))
              (xterm-color--message "SGR 38;5;%s error, expected 38;5;COLOR"
                                    color)))

    (:match ((and (eq 48 (cl-first SGR-list))
                  (eq 2 (cl-second SGR-list)))          ; Truecolor (24-bit) BG color
             :skip 5)
            (when xterm-color--support-truecolor
              (if-let ((r (cl-third SGR-list))
                       (g (cl-fourth SGR-list))
                       (b (cl-fifth SGR-list)))
                  (if (or (> r 255) (> g 255) (> b 255))
                      (xterm-color--message "SGR 48;2;%s;%s;%s exceeds range"
                                            r g b)
                    (set-truecolor! r g b xterm-color--current-bg))
                (xterm-color--message "SGR 48;2;%s;%s;%s error, expected 48;2;R;G;B"
                                      r g b))))

    (:match ((and (eq 48 (cl-first SGR-list))
                  (eq 5 (cl-second SGR-list)))
             :skip 3)                                   ; XTERM 256 BG color
            (if-let ((color (cl-third SGR-list)))
                (if (> color 255)
                    (xterm-color--message "SGR 48;5;%s exceeds range" color)
                  (set-b! color))
              (xterm-color--message "SGR 48;5;%s error, expected 48;5;COLOR"
                                    color)))
    (:match ((<= 90 elem 97))                           ; AIXTERM hi-intensity FG
            ;; Rather than setting bright, which would be wrong,
            ;; rescale color to fall within 8-15 so that it gets
            ;; mapped to xterm-color-names-bright by xterm-color-256
            (set-f! (- elem 82)))
    ;; Same for BG, rescale to 8-15
    (:match ((<= 100 elem 107)) (set-b! (- elem 92)))   ; AIXTERM hi-intensity BG

    (:match (51) (set-a!   +frame+))
    (:match (53) (set-a!   +overline+))
    (:match (54) (unset-a! +frame+))
    (:match (55) (unset-a! +overline+))
    (:match (4)  (set-a!   +underline+))
    (:match (24) (unset-a! +underline+))
    (:match (3)  (set-a!   +italic+))
    (:match (23) (unset-a! +italic+))
    (:match (9)  (set-a!   +strike-through+))
    (:match (29) (unset-a! +strike-through+))
    (:match (7)  (set-a!   +negative+))
    (:match (27) (unset-a! +negative+))))

(defsubst xterm-color--SGR-attributes (list)
  "Convert LIFO list of SGR characters to FIFO list of SGR attributes (integers).

Returns FIFO list of SGR attributes or nil on errors.

Characters must be in the ASCII set 0-9 (decimal 48 to 57) and are converted
to integer digits by subtracting 48 from each character. E.g. Character 48
is converted to integer digit 0, character 49 to integer digit1..
Character 59 (;) is not converted but signifies that all accumulated integer
digits should be reversed and combined into a single integer (SGR attribute).

Examples:

Given (48) return (0)
Given (59) return (0 0)
Given (48 49 50) return (210)
Given (48 49 50 59 50 50 59 48 49) return (10 22 210)"
  (cl-loop
   with mul = 1 and n = 0 and ret
   for c in list do
   (if (/= 59 c)
       (let ((e (- c 48)))
         (unless (<= 0 e 9)
           (xterm-color--message "Invalid SGR attribute %s" c)
           (cl-return))
         (cl-incf n (* mul e))
         (setq mul (* mul 10)))
     (push n ret)
     (setq n 0 mul 1))
   finally return (push n ret)))


;;;
;;; CSI state machine
;;;


(defsubst xterm-color--dispatch-CSI ()
  "Update state machine based on CSI parameters collected so far.
Parameters are taken from `xterm-color--CSI-list' which stores them
in LIFO order."
  (let* ((csi    xterm-color--CSI-list)
         (term   (car csi))               ; final parameter, terminator
         (params (cdr csi)))              ; rest of parameters, LIFO order
    (setq xterm-color--CSI-list nil)
    (cond ((eq ?m term)
           ;; SGR
           (let ((SGR-list (if (null params) '(0)
                             (xterm-color--SGR-attributes params))))
             (when SGR-list
               (xterm-color--dispatch-SGR SGR-list))))
          (t
           (xterm-color--message "%s CSI not implemented" csi)))))

(defmacro xterm-color--with-ANSI-macro-helpers (&rest body)
  (declare (indent defun))
  `(xterm-color--with-SGR-constants
     (cl-symbol-macrolet ((fg           xterm-color--current-fg)
                          (bg           xterm-color--current-bg)
                          (attrs        xterm-color--attributes)
                          (bold-bright  xterm-color-use-bold-for-bright))
       (cl-macrolet
           ((out! (x)            `(push ,x result))
            (push-char! (c)      `(push ,c xterm-color--char-list))
            (push-csi! (c)       `(push ,c xterm-color--CSI-list))
            (state! (s)          `(setq state ,s))
            (graphics? ()        `(or fg bg (/= attrs 0)))
            (has? (attr)         `(/= (logand ,attr attrs) 0))
            (fmt-24bit (color)   `(format "#%06x" ,color))
            (fmt-256 (color)     `(xterm-color-256 ,color))

            ;; Unpacks a packed truecolor value (as stored in
            ;; `xterm-color--current-fg' and `xterm-color--current-fg'.
            (unpack (color)      `(ash ,color -9))

            ;; To avoid hash collisions, a different packing scheme is used
            ;; for hash table keys. It can encode two colors (foreground
            ;; and background) that can either be truecolor 24bit or XTerm 256
            ;; color 8bit. XTerm 256 color values subsume ANSI colors, a
            ;; separate encoding scheme is not needed.
            ;;
            ;; The scheme used also accounts for the combination of a truecolor
            ;; with an XTerm 256 color as part of the same hashed entry. Since
            ;; two different hash tables are used to work around 32bit Emacs
            ;; limited integer range, two packing schemes are needed:
            ;;
            ;; High<         25 bits       >Low
            ;; ATTR[7 bits]BG[9 bits]FG[9 bits] where BG and FG are each
            ;; encoded as the 8bit color value shifted left by 1 and combined
            ;; with a flag bit which is set when the color is present.
            ;;
            ;; High<         59 bits       >Low
            ;; ATTR[7 bits]BG[26 bits]FG[26 bits] where BG and FG are each
            ;; encoded as the 24bit (RGB) or 8bit color value shifted left by
            ;; 2 and combined with 2 flag bits that are set when the value
            ;; is 24bit (high bit) and when the color is present (low bit).
            (pack-256 (color)    `(if ,color (logior (ash ,color 1) 1) 0))
            (pack-24bit (color)  `(if ,color
                                      (if (> ,color 255)
                                          (logior (ash (unpack ,color) 2) 3)
                                        (logior (ash ,color 2) 1))
                                    0))
            ;; If at least one of foreground / background color is a 24bit
            ;; truecolor value: Second packing scheme with
            ;; `xterm-color--truecolor-face-cache' is used.
            ;;
            ;; Every other case, including when no colors are present:
            ;; First packing scheme with `xterm-color--face-cache' is used.
            (pack-key-into (k)   `(cond ((or (and fg (> fg 255))
                                             (and bg (> bg 255)))
                                         ;; At least one truecolor 24bit value
                                         (setq ,k (logior (ash attrs 52)
                                                          (ash (pack-24bit bg) 26)
                                                          (pack-24bit fg)))
                                         xterm-color--truecolor-face-cache)
                                        (t ;; No truecolor 24bit value
                                         (setq ,k (logior (ash attrs 18)
                                                          (ash (pack-256 bg) 9)
                                                          (pack-256 fg)))
                                         xterm-color--face-cache)))

            (face! (k v)         `(setq plistf (plist-put plistf ,k ,v)))
            (make-color-fg ()    `(if (and bold-bright
                                           (< fg 256)
                                           (or (has? +bright+) (<= 8 fg 15)))
                                      (progn (face! :weight 'bold)
                                             (face! :foreground
                                                    (fmt-256 (if (<= 8 fg) (- fg 8) fg))))
                                    (face! :foreground
                                           (if (> fg 255)
                                               (fmt-24bit (unpack fg))
                                             (fmt-256 (if (and (<= fg 7) (has? +bright+))
                                                          (+ fg 8)
                                                        fg))))))
            (make-color-bg ()    `(face! :background (cond ((> bg 255) (fmt-24bit (unpack bg)))
                                                           (t (fmt-256 bg)))))
            (make-face ()        `(let* (k
                                         (table (pack-key-into k)))
                                    (or (gethash k table)
                                        (let (plistf)
                                          (when (has? +italic+)         (face! :slant 'italic))
                                          (when (has? +underline+)      (face! :underline t))
                                          (when (has? +strike-through+) (face! :strike-through t))
                                          (when (has? +negative+)       (face! :inverse-video t))
                                          (when (has? +overline+)       (face! :overline t))
                                          (when (has? +frame+)          (face! :box t))

                                          (cond (fg (make-color-fg))
                                                (t (when (and bold-bright (has? +bright+))
                                                     (face! :weight 'bold))))

                                          (when bg (make-color-bg))
                                          (puthash k plistf table)))))
            (maybe-fontify ()    '(when xterm-color--char-list
                                    (let ((s (concat (nreverse xterm-color--char-list))))
                                      (when (and xterm-color-render (graphics?))
                                        (add-text-properties
                                         0 (length s)
                                         (list 'xterm-color t
                                               (if font-lock-mode 'font-lock-face 'face)
                                               (make-face))
                                         s))
                                      (out! s))
                                    (setq xterm-color--char-list nil))))
         ,@body))))


;;;
;;; Exports
;;;


;;;###autoload
(defun xterm-color-filter-strip (string)
  "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

In order to get maximum performance, this function strips text properties
if they are present in STRING."
  (unless xterm-color--face-cache
    (setq xterm-color--face-cache
          (make-hash-table :weakness 'value)))
  (unless xterm-color--truecolor-face-cache
    (setq xterm-color--truecolor-face-cache
          (make-hash-table :weakness 'value)))
  (xterm-color--with-ANSI-macro-helpers
    (cl-loop
     with state = xterm-color--state and result
     for char across string do
     (cond
      ((eq state :char)
       (cond
        ((eq char 27)                    ; ESC
         (maybe-fontify)
         (state! :ansi-esc))
        (t
         (if (graphics?)
             (push-char! char)
           (out! (list char))))))
      ((eq state :ansi-esc)
       (cond ((eq char ?\[)
              (state! :ansi-csi))
             ((eq char ?\])
              (state! :ansi-osc))
             ((or (eq char ?\()
                  (eq char ?\)))
              (state! :set-char))
             (t
              (push-char! char)
              (state! :char))))
      ((eq state :ansi-csi)
       (push-csi! char)
       (when (and (>= char #x40)
                  (<= char #x7e))
         (xterm-color--dispatch-CSI)
         (state! :char)))
      ((eq state :ansi-osc)
       ;; OSC sequences are skipped
       (cond ((eq char 7)
              (state! :char))
             ((eq char 27)
              ;; ESC
              (state! :ansi-osc-esc))))
      ((eq state :ansi-osc-esc)
       (cond ((eq char ?\\)
              (state! :char))
             (t (state! :ansi-osc))))
      ((eq state :set-char)
       (xterm-color--message "%s SET-CHAR not implemented" char)
       (state! :char)))
     finally return
     (progn (when (eq state :char) (maybe-fontify))
            (setq xterm-color--state state)
            (apply 'concat (nreverse result))))))

;;;###autoload
(defun xterm-color-filter (string)
  "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function checks if `xterm-color-preserve-properties' is non-nil
and only calls `xterm-color-filter-strip' on substrings that do not
have text properties applied (passing through the rest unmodified).
Preserving properties in this fashion is not very robust as there may
be situations where text properties are applied on ANSI data, which
will desync the state machine.

Preserving properties works ok with and is really meant for eshell.

This can be inserted into `comint-preoutput-filter-functions'."
  (if (not xterm-color-preserve-properties)
      (xterm-color-filter-strip string)
    (cl-loop
     with result
     for (_ props substring) in (xterm-color--string-properties string) do
     (push (if props substring (xterm-color-filter-strip substring))
           result)
     finally return (apply 'concat (nreverse result)))))

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
(cl-defun xterm-color-colorize-buffer (&optional use-overlays)
  "Apply `xterm-color-filter' to current buffer, and replace its contents.
Colors are applied using \\='face, unless font-lock-mode is active, in
which case \\='font-lock-face is used. Operation with font-lock mode active
is not recommended.

If USE-OVERLAYS is non-nil, colors are applied to the buffer using overlays
instead of text properties. A C-u prefix arg causes overlays to be used."
  (interactive "P")
  (let ((read-only-p buffer-read-only))
    (when read-only-p
      (unless (y-or-n-p "Buffer is read only, continue colorizing? ")
        (cl-return-from xterm-color-colorize-buffer))
      (read-only-mode -1))
    (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
    (when (and xterm-color-render use-overlays)
      (xterm-color--convert-text-properties-to-overlays (point-min) (point-max)))
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
       (xterm-color--message "Cleared face attribute cache"))
  (and xterm-color--truecolor-face-cache
       (clrhash xterm-color--truecolor-face-cache)
       (xterm-color--message "Cleared truecolor face attribute cache")))


;;;
;;; Tests
;;;


(defmacro xterm-color--bench (path &optional repetitions)
  `(benchmark-run-compiled ,repetitions
     (with-temp-buffer
       (insert-file-contents-literally ,path)
       (xterm-color-colorize-buffer))))

(defvar xterm-color--test-do-filter t)

(cl-defmacro xterm-color--with-tests (&body body)
  `(cl-labels ((ansi-filter (msg &rest args)
                            (insert
                             (if xterm-color--test-do-filter
                                 (xterm-color-filter
                                  (apply #'format msg args))
                               (apply #'format msg args))))
               (test (name &rest attribs)
                     (ansi-filter "\x1b[0;%smThis is only a test!\x1b[0m\t --[ %s\n"
                                  (mapconcat #'identity attribs ";")
                                  name)))
     ,@body))

(defun xterm-color--test-ansi ()
  (xterm-color--with-tests
   (let ((test-attributes
          '(("1"    . "bright")
            ("51"   . "frame")
            ("3"    . "italic")
            ("4"    . "underline")
            ("7"    . "negative")
            ("9"    . "strike through")
            ("53"   . "overline")
            ("1;51" . "bright + frame")
            ("1;3"  . "bright + italic")
            ("1;4"  . "bright + underline")
            ("1;7"  . "bright + negative")
            ("1;9"  . "bright + strike through")
            ("1;53" . "bright + overline"))))

     ;; Attributes (no color)
     (insert "* ANSI attributes (default colors)\n")

     (if xterm-color-use-bold-for-bright
         (insert "  Expect: Bold instead of bright")
       (insert "  Expect: Bright not to be rendered since no foreground color is set"))
     (insert "\n\n")

     (cl-loop for (attrib . name) in test-attributes
              do (test name attrib)
              finally (insert "\n"))

     (insert "* ANSI attributes (blue foreground)\n")

     (if xterm-color-use-bold-for-bright
         (insert "  Expect: Bold instead of bright")
       (insert "  Expect: Bright rendered as bright color"))
     (insert "\n\n")

     (cl-loop for (attrib . name) in test-attributes
              do (test name "34" attrib)
              finally (insert "\n"))

     (insert "* ANSI attributes (blue background)\n")

     (if xterm-color-use-bold-for-bright
         (insert "  Expect: Bold instead of bright")
       (insert "  Expect: Bright not to be rendered since no foreground color is set"))
     (insert "\n\n")

     (cl-loop for (attrib . name) in test-attributes
              do (test name "44" attrib)
              finally (insert "\n"))

     (insert "* ANSI attributes (AIXTERM blue foreground)\n")

     (if xterm-color-use-bold-for-bright
         (insert "  Expect: Bold instead of bright")
       (insert "  Expect: Bright color everywhere due to AIXTERM"))
     (insert "\n\n")

     (cl-loop for (attrib . name) in test-attributes
              do (test name "94" attrib)
              finally (insert "\n"))

     (insert "* ANSI attributes (AIXTERM red background)\n")
     (insert "  Expect: Bright background color due to AIXTERM\n")
     (if xterm-color-use-bold-for-bright
         (insert "  Expect: Bold instead of bright for foreground\n\n")
       (insert "\n"))

     (cl-loop for (attrib . name) in test-attributes
              do (test name "101" attrib)
              finally (insert "\n"))

     (insert "* Misc\n")
     (if xterm-color-use-bold-for-bright
         (progn
           (insert "  Expect: Bold instead of bright\n")
           (insert "          Otherwise bright rendered as normal intensity\n\n"))
       (insert "\n"))

     (insert "; Resetting FG color should not affect other SGR bits\n")
     (ansi-filter "Default \x1b[34;1mBright blue\x1b[39m Reset-fg-color \x1b[34mBlue (bright)\x1b[0m\n\n")
     (insert "; AIXTERM bright color should not set bright SGR bit\n")
     (ansi-filter "Default \x1b[94mBright blue\x1b[34m Switch-to-blue (normal)\x1b[0m\n")
     (insert "\n"))))

(defun xterm-color--test-xterm ()
  (xterm-color--with-tests
   ;; System colors
   (cl-loop for color from 40 to 47
            do (ansi-filter "\x1b[0;%sm  " color)
            finally (ansi-filter "\x1b[0m * ANSI system colors\n"))

   ;; Normal ANSI colors mapped to XTERM
   (cl-loop for color from 0 to 7
            do (ansi-filter "\x1b[48;5;%sm  " color)
            finally (ansi-filter "\x1b[0m * ANSI colors mapped to XTERM\n"))

   ;; Bright ANSI colors mapped to XTERM
   (cl-loop for color from 8 to 15
            do (ansi-filter "\x1b[48;5;%sm  " color)
            finally (ansi-filter "\x1b[0m * ANSI bright colors mapped to XTERM\n\n"))

   ;; XTERM 256 color cubes
   (insert "*  XTERM 256 color cubes\n\n")

   (cl-loop for green from 0 to 5 do
            (cl-loop for red from 0 to 5 do
                     (cl-loop for blue from 0 to 5
                              for color = (+ 16 (* 36 red) (* green 6) blue)
                              do (ansi-filter "\x1b[48;5;%sm  \x1b[0m" color))
                     (ansi-filter "\x1b[0m "))
            (insert "\n"))

   ;; Truecolor color ramps
   (insert "\n")
   (insert "*  Truecolor\n\n")
   (cond (xterm-color--support-truecolor
          ;; Adapted from: https://gist.github.com/XVilka/8346728
          (cl-loop
           with steps = 77
           for c from 0 below steps
           for r = (- 255 (* c (/ 255 steps)))
           for g = (* c (/ 510 steps))
           for b = (* c (/ 255 steps)) do
           (when (> g 255) (setq g (- 510 g)))
           (ansi-filter "\x1b[48;2;%s;%s;%sm \x1b[m" r g b)))
         (t
          (insert "Truecolor is not supported on Emacs 32bit")))

   (insert "\n\n")
   (insert "*  XTERM color grayscale ramp\n\n")

   (cl-loop for color from 232 to 255
            do (ansi-filter "\x1b[48;5;%sm  " color)
            finally (ansi-filter "\x1b[0m\n\n"))))

;;;###autoload
(defun xterm-color-test ()
  "Create, display and render a new buffer containing ANSI control sequences."
  (interactive)
  (let* ((name (generate-new-buffer-name "*xterm-color-test*"))
         (buf (get-buffer-create name)))
    (switch-to-buffer buf))

  (xterm-color--test-xterm)

  (let ((xterm-color-use-bold-for-bright nil))
    (xterm-color--test-ansi))
  (xterm-color-clear-cache)

  (insert "; Temporarily setting `xterm-color-use-bold-for-bright' to T\n")
  (insert "; Current font needs to have a bold variant for following tests\n\n")

  (let ((xterm-color-use-bold-for-bright t))
    (xterm-color--test-ansi))

  (setq buffer-read-only t)
  (goto-char (point-min)))

;;;###autoload
(defun xterm-color-test-raw ()
  "Create and display a new buffer containing ANSI SGR control sequences.
ANSI sequences are not processed. One can use a different Emacs package,
such as ansi-color.el to do so. This is really meant to be used for easy
comparisons/benchmarks with libraries that offer similar functionality."
  (interactive)
  (let* ((name (generate-new-buffer-name "*xterm-color-test-raw*"))
         (buf (get-buffer-create name)))
    (switch-to-buffer buf))

  (let (xterm-color--test-do-filter)
    (xterm-color--test-xterm)
    (xterm-color--test-ansi))
  (goto-char (point-min)))

(provide 'xterm-color)
;;; xterm-color.el ends here
