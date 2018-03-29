EMACS ?= emacs

.PHONY: all
all: ghub.el
	@printf "* Checking Emacs Version...\n"
	@$(EMACS) --version | head -1
	@printf "* Compiling...\n"
	$(EMACS) -Q --batch -l ghub.el -f batch-byte-compile github-stars.el

ghub.el:
	@printf "* Downloading $@...\n"
	curl -O https://raw.githubusercontent.com/magit/ghub/master/ghub.el

xuchunyang:
	@for cmd in emacs emacs-25.1.1 emacs-25.3.1; do \
	    make EMACS=$$cmd ;\
	done
