EMACS   ?= emacs
PACKAGE := bott
RM      ?= rm -f

%.elc: %.el
	$(EMACS) --quick --batch --funcall=batch-byte-compile $<

all: bott

bott: $(PACKAGE).elc

.PHONY: emacs-Q
emacs-Q:
	$(EMACS) --quick --load=$(PACKAGE).el

test: bott
	$(EMACS) --quick --batch --directory=. --load=$(PACKAGE)-tests.el \
		--funcall=ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	$(RM) $(PACKAGE).elc
