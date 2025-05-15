#!/usr/bin/env sh

source $(dirname $0)/lib.sh

hyprctl dispatch -- togglespecialworkspace "ws$(curws)"
