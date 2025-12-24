EMACS = emacs

test:
	$(EMACS) -batch \
		-l xterm-color.el \
		-l tests/xterm-color-tests-utils.el \
		-l tests/xterm-color-tests.el \
		-f ert-run-tests-batch-and-exit

.PHONY: test
