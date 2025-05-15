curws () {
    hyprctl -j activeworkspace | jq '.id'
}
