;;; xterm-color-tests-utils.el -*- lexical-binding: t -*-


(defmacro xterm-color-tests--verify-state-transitions (sequence)
  "SEQUENCE is a list of expected states and ANSI escape sequences causing transitions.

Elements 0, 2, 4, ... of SEQUENCE are ANSI escape sequences causing state transitions.
Elements 1, 3, 5, ... of SEQUENCE are expected states, i.e. plists of face properties."
  `(let ((escape-sequences (xterm-color-tests--even-elements ,sequence))
         (expected-states (xterm-color-tests--odd-elements ,sequence))
         (visible-text "a"))
     (with-temp-buffer
       (dolist (escape-sequence escape-sequences)
         (insert escape-sequence)
         (insert visible-text))
       (goto-char (point-min))
       (xterm-color-colorize-buffer)
       (goto-char (point-min))
       (dolist (expected-state expected-states)
         (xterm-color-tests--assert-plists-equal expected-state
                                                 (get-char-property (point) 'face))
         (forward-char (length visible-text))))))

(defun xterm-color-tests--even-elements (list)
  "Return even elements of LIST."
  (cl-assert (cl-evenp (length list)))
  (mapcar (lambda (i) (nth (* 2 i) list))
          (number-sequence 0 (/ (1- (length list)) 2))))

(defun xterm-color-tests--odd-elements (list)
  "Return even elements of LIST."
  (cl-assert (cl-evenp (length list)))
  (mapcar (lambda (i) (nth (1+ (* 2 i)) list))
          (number-sequence 0 (/ (1- (length list)) 2))))

(defmacro xterm-color-tests--assert-plists-equal (plist-1 plist-2)
  "Assert that the PLIST-1 contains the same set of key-value pairs as PLIST-2."
  `(progn
     (should (equal (length ,plist-1) (length ,plist-2)))
     (dolist (key (xterm-color-tests--even-elements ,plist-1))
       (should (equal (plist-get ,plist-1 key)
                      (plist-get ,plist-2 key))))))

(defun xterm-color-tests--truecolor (r g b)
  (format "#%02x%02x%02x" r g b))

;;; xterm-color-tests-utils.el ends here
