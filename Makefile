stow: emacs

emacs:
	stow -Sv -d config/ -t $HOME/.config/emacs emacs

.PHONY: stow emacs
