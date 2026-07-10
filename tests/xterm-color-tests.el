;;; xterm-color-tests.el -*- lexical-binding: t -*-


(ert-deftest xterm-color-tests--sequence-of-fg-colors ()
  (xterm-color-tests--verify-state-transitions
   `("\x1b[30m"
     (:foreground ,(xterm-color-256 0))
     "\x1b[31m"
     (:foreground ,(xterm-color-256 1)))))

(ert-deftest xterm-color-tests--set-fg-and-bg-in-one-sequence ()
  (xterm-color-tests--verify-state-transitions
   `("\x1b[30;41m"
     (:foreground ,(xterm-color-256 0)
      :background ,(xterm-color-256 1)))))

(ert-deftest xterm-color-tests--set-ansi-fg-and-xterm-256-bg-in-one-sequence ()
  (xterm-color-tests--verify-state-transitions
   `("\x1b[30;48;5;255m"
     (:foreground ,(xterm-color-256 0)
      :background ,(xterm-color-256 255)))))

(ert-deftest xterm-color-tests--set-ansi-fg-and-truecolor-bg-in-one-sequence ()
  (xterm-color-tests--verify-state-transitions
   `("\x1b[30;48;2;253;254;255m"
     (:foreground ,(xterm-color-256 0)
      :background ,(xterm-color-tests--truecolor 253 254 255)))))

;;; xterm-color-tests.el ends here
