stow: emacs nixpkgs taffybar


emacs:
	if test ! -d $(HOME)/.config/emacs; then mkdir $(HOME)/.config/emacs; fi
	stow -Sv -d config/ -t $(HOME)/.config/emacs emacs

nixpkgs:
	if test ! -d $(HOME)/.config/nixpkgs; then mkdir $(HOME)/.config/nixpkgs; fi
	stow -Sv -d config/ -t $(HOME)/.config/nixpkgs nixpkgs

.PHONY: stow emacs nixpkgs taffybar
