#!/usr/bin/env sh

source $(dirname $0)/lib.sh

hyprctl dispatch -- movetoworkspacesilent "special:ws$(curws)"


