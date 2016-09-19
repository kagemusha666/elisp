
SHELL=/bin/sh
SCRIPTS=$(wildcard *.el)

.PHONY: help install

all: help

install: 
	mkdir -p $(HOME)/.emacs.d/elisp
	cp .emacs $(HOME)/.emacs
	cp $(SCRIPTS) $(HOME)/.emacs.d/elisp/

help:
	@echo "To install scripts do:"
	@echo " make install"
