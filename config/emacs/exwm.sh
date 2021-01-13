#!/bin/bash

exec dbus-launch --exit-with-session emacs --debug-init --eval '(load-file "~/.config/emacs/exwm.el")'
