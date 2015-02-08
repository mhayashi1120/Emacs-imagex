EMACS = emacs

check: compile
	$(EMACS) -q -batch -eval "(check-declare-file \"image+.el\")" 2>&1 | grep -e "Checking"
	$(EMACS) -q -batch -l image+.el -l image+-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l image+.elc -l image+-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile image+.el

clean:
	rm -f *.elc
