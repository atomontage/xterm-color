;;; xterm-color.el --- ANSI & XTERM 256 color support

;; Copyright (C) 2010 xristos@sdf.lonestar.org
;; All rights reserved

;; Version: 0.5 - 2010-05-27
;; Author: xristos@sdf.lonestar.org
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
;; * Regular ANSI
;; * XTERM 256 color
;;
;; xterm-color.el should perform much better than ansi-color.el
;;
;;; Install/Uninstall (comint):
;;
;; 
;; (progn (setq comint-preoutput-filter-functions '(xterm-color-filter))
;;        (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
;;        (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))
;; 
;; (progn (setq comint-preoutput-filter-functions nil)
;;        (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;        (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))
;;
;; 
;;; Test:
;;
;; M-x shell
;; wget http://www.frexx.de/xterm-256-notes/data/256colors2.pl
;; wget http://www.frexx.de/xterm-256-notes/data/xterm-colortest
;; perl xterm-colortest && perl 256colors2.pl
;;
;;; Code:

(defvar xterm-color-current (make-hash-table)
  "Current ANSI color.")

(defvar xterm-color-ring ""
  "Ring buffer with characters that the current ANSI color applies to.
In order to avoid having per-character text properties, we grow this
buffer dynamically until we encounter an ANSI reset sequence.
Once that happens, we generate a single text property for the entire string.")

(defvar xterm-color-var ""
  "Temporary string that holds ANSI sequence variables (integers).")

(defvar xterm-color-state :char
  "The current state of the ANSI sequence state machine.")

(defcustom xterm-color-names
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "The default colors to use for regular ANSI colors (both fg and bg)."
  :type '(vector string string string string string string string string)
  :group 'xterm-color)

(defcustom xterm-color-names-bright
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "The default colors to use for bright ANSI colors (both fg and bg)."
  :type '(vector string string string string string string string string)
  :group 'xterm-color)

(mapc 'make-variable-buffer-local
      '(xterm-color-current
        xterm-color-ring
        xterm-color-var
        xterm-color-state))

(defun xterm-color-unfontify-region (beg end)
  "Replacement function for `font-lock-default-unfontify-region'.
When font-lock is active in a buffer, you cannot simply add
face text properties to the buffer.  Font-lock will remove the face
text property using `font-lock-unfontify-region-function'.  If you want
to insert the string returned by `xterm-color-filter' into such buffers,
you must set `font-lock-unfontify-region-function' to
`xterm-color-unfontify-region'.  This function will not remove all face
text properties unconditionally.  It will keep the face text properties
if the property `xterm-color' is set. A possible way to install this would be:

\(add-hook 'font-lock-mode-hook
	  \(function (lambda ()
		      \(setq font-lock-unfontify-region-function
			    'xterm-color-unfontify-region))))"
  (when (boundp 'font-lock-syntactic-keywords)
    (remove-text-properties beg end '(syntax-table nil)))
  (while (setq beg (text-property-not-all beg end 'face nil))
    (setq beg (or (text-property-not-all beg end 'xterm-color t) end))
    (when (get-text-property beg 'face)
      (let ((end-face (or (text-property-any beg end 'face nil)
			  end)))
	(remove-text-properties beg end-face '(face nil))
	(setq beg end-face)))))

(defun xterm-color-dispatch-ansi-var (var)
  (cond ((= var 38)
         (xterm-color-new-state :xterm-fg))
        ((= var 0)
         (clrhash xterm-color-current))
        ((= var 48)
         (xterm-color-new-state :xterm-bg))
        ((= var 30)
         (remhash 'foreground-color
                  xterm-color-current))
        ((and (>= var 31)
              (<= var 37))
         (puthash 'foreground-color
                  (aref xterm-color-names (- var 30))
                  xterm-color-current))
        ((and (>= var 40)
              (<= var 47))
         (puthash 'background-color
                  (aref xterm-color-names (- var 40))
                  xterm-color-current))))

(defun xterm-color-256 (color)
  (cond ((and (>= color 232)
              (<= color 255))
         ;; Greyscale
         (let ((val (+ 8 (* (- 255 color) 10))))
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

(defun xterm-color-make-property (hash-table)
  (let ((ret nil))
    (maphash (lambda (k v)
               (push (cons k v) ret))
             hash-table)
    ret))

(defun xterm-color-new-state (state)
  ;(message "xterm-color: %s -> %s" xterm-color-state state)
  (setq xterm-color-state state))


(defun xterm-color-filter (string)
  "Translate ANSI color sequences in STRING into text properties.
Returns new region.
This can be inserted into `comint-preoutput-filter-functions'.
Also see `xterm-color-unfontify-region'."
  (let ((result nil))
    (macrolet ((insert (x) `(push ,x result))
               (maybe-fontify
                ()
                `(when (> (length xterm-color-ring) 0)
                   (if (> (hash-table-count xterm-color-current) 0)
                       (insert (propertize xterm-color-ring 'xterm-color t
                                           'face (xterm-color-make-property xterm-color-current)))
                    (insert xterm-color-ring))
                  (setq xterm-color-ring ""))))
      (loop for char across string do
            (case xterm-color-state
              (:char
               (cond
                ((= char 27)            ; ESC
                 (maybe-fontify)
                 (xterm-color-new-state :ansi-esc))
                (t
                 (if (> (hash-table-count xterm-color-current) 0)
                     (setq xterm-color-ring
                           (concat xterm-color-ring (string char)))
                   (insert (string char))))))
              (:ansi-esc
               (cond ((= char ?\[)
                      (xterm-color-new-state :ansi-var))
                     ((= char ?\])
                      (insert (concat (string 27)
                                      (string ?\])))
                      (xterm-color-new-state :ansi-invalid-0))
                     (t
                      (insert (string char)))))
              (:ansi-invalid-0
               (insert (string char))
               (when (= char 59)
                 (xterm-color-new-state :char)))
              (:ansi-invalid
               ;; Read and discard everything until we get m
               (when (= char ?m)
                 (xterm-color-new-state :char)))
              (:ansi-var
               (cond ((= char 59)       ; end of variable
                      (let ((var (string-to-number xterm-color-var)))
                        (setq xterm-color-var "")
                        (xterm-color-dispatch-ansi-var var)))
                     ((= char ?m)
                      (cond ((string= "" xterm-color-var)
                             ;; ANSI reset ESC[m
                             (clrhash xterm-color-current))
                            (t (let ((var (string-to-number xterm-color-var)))
                                 (xterm-color-dispatch-ansi-var var)
                                 (setq xterm-color-var ""))))
                      (xterm-color-new-state :char))
                     ((= char ?0)
                      (setq xterm-color-var
                            (concat xterm-color-var (string char)))
                      (xterm-color-new-state :maybe-term-query))
                     (t
                      (setq xterm-color-var
                            (concat xterm-color-var (string char))))))
              (:maybe-term-query
               (cond ((= char ?c)       ; ESC[0c query terminal, ignore
                      (setq xterm-color-var "")
                      (xterm-color-new-state :char))
                     ((= char ?m)
                      (clrhash xterm-color-current)
                      (setq xterm-color-var "")
                      (xterm-color-new-state :char))
                     (t (setq xterm-color-var
                              (concat xterm-color-var (string char)))
                        (xterm-color-new-state :ansi-var))))
              (:xterm-fg
               (cond ((= char ?5)
                      (xterm-color-new-state :xterm-fg-1))
                     (t
                      (message "invalid ansi escape sequence: %s:%c" xterm-color-state char)
                      (xterm-color-new-state :ansi-invalid))))
              (:xterm-fg-1
               (cond ((= char 59)
                      (xterm-color-new-state :xterm-fg-2))
                     (t
                      (message "invalid ansi escape sequence: %s:%c" xterm-color-state char)
                      (xterm-color-new-state :ansi-invalid))))
              (:xterm-fg-2
               (cond ((= char ?m)
                      (puthash 'foreground-color
                               (xterm-color-256 (string-to-number xterm-color-var))
                               xterm-color-current)
                      (xterm-color-new-state :char)
                      (setq xterm-color-var ""))
                     (t
                      (setq xterm-color-var
                            (concat xterm-color-var (string char))))))
              (:xterm-bg
               (cond ((= char ?5)
                      (xterm-color-new-state :xterm-bg-1))
                     (t
                      (message "invalid ansi escape sequence: %s:%c" xterm-color-state char)
                      (xterm-color-new-state :ansi-invalid))))
              (:xterm-bg-1
               (cond ((= char 59)
                      (xterm-color-new-state :xterm-bg-2))
                     (t
                      (message "invalid ansi escape sequence: %s:%c" xterm-color-state char)
                      (xterm-color-new-state :ansi-invalid))))
              (:xterm-bg-2
               (cond ((= char ?m)
                      (puthash 'background-color
                               (xterm-color-256 (string-to-number xterm-color-var))
                               xterm-color-current)
                      (xterm-color-new-state :char)
                      (setq xterm-color-var ""))
                     (t
                      (setq xterm-color-var
                            (concat xterm-color-var (string char))))))))
      (when (eq xterm-color-state :char)
        (maybe-fontify)))
    (mapconcat 'identity (nreverse result) "")))

(provide 'xterm-color)
;;; xterm-color.el ends here