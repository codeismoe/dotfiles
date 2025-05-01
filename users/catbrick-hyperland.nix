{
  enable = true;
  package = null;
  portalPackage = null;
  systemd = {
    enable = true;
    enableXdgAutostart = true;
  };

  plugins = [ ];

  settings = {
    "$mod" = "SUPER";
    "$terminal" = "kitty";
    "$fileManager" = "dolphin";

    exec-once = [
      "nm-applet &"
      "blueman &"
      "waybar &"
    ];

    monitor = ",preferred,auto,1";
    general = {
      gaps_in = 1;
      gaps_out = 1;
      border_size = 3;
      "col.active_border" = "rgba(dc8a78ff) rgba(8839efff) 45deg";
      "col.inactive_border" = "rgba(7287fdcc) rgba(179299cc) 45deg";
      layout = "master";
      resize_on_border = true;
      extend_border_grab_area = true;
    };

    group =  {
     "col.border_active" = "rgba(dc8a78ff) rgba(8839efff) 45deg";
     "col.border_inactive" = "rgba(7287fdcc) rgba(179299cc) 45deg";
     "col.border_locked_active" = "rgba(dc8a78ff) rgba(8839efff) 45deg";
     "col.border_locked_inactive" = "rgba(7287fdcc) rgba(179299cc) 45deg";
    };

    decoration = {
      rounding = 0;
      shadow.enabled = false;
      blur.enabled = true;
    };

    layerrule = "blur,waybar";

    misc = {
      disable_hyprland_logo = true;
      disable_splash_rendering = true;
    };

    animations.enabled = false;

    dwindle = {
      pseudotile = false;
      preserve_split = true;
      
      smart_split = false;
      smart_resizing = false;
      
      default_split_ratio = 1.1;
    };

    # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
    master = {
      new_status = "inherit";
      mfact = "0.55";
      orientation = "left";
      slave_count_for_center_master = "2";
      center_master_slaves_on_right = true;
    };

    bind =
      [
        "$mod, E, exec, emacsclient -c"
        "$mod SHIFT, E, exec, emacsclient -ce '(eshell \"new\")'"
        "$mod, return, exec, kitty"
        "$mod, W, exec, rofi -show drun"
        "$mod SHIFT, W, exec, rofi-pass"

        "$mod SHIFT, Q, killactive"
        "$mod CTRL, Q, exit"
        "$mod, F, fullscreen"
        "$mod, space, togglefloating"
        # "$mod, S, togglesplit"
        
        "$mod, G, togglegroup"
        "$mod, A, changegroupactive, b"
        "$mod, D, changegroupactive, f"

        "$mod, S, layoutmsg, focusmaster"
        "$mod SHIFT, S, layoutmsg, swapwithmaster"
        "$mod CTRL, H, layoutmsg, orientationleft"
        "$mod CTRL, L, layoutmsg, orientationright"
        "$mod CTRL, J, layoutmsg, orientationbottom"
        "$mod CTRL, K, layoutmsg, orientationtop"
        "$mod CTRL, I, layoutmsg, orientationcenter"

        "$mod, comma, layoutmsg, addmaster"
        "$mod, period, layoutmsg, removemaster"
        
        "$mod, H, movefocus, l"
        "$mod, L, movefocus, r"
        "$mod, K, movefocus, u"
        "$mod, J, movefocus, d"
        "$mod SHIFT, H, movewindoworgroup, l"
        "$mod SHIFT, L, movewindoworgroup, r"
        "$mod SHIFT, K, movewindoworgroup, u"
        "$mod SHIFT, J, movewindoworgroup, d"
      ]
      ++ (
        # workspaces
        # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
        builtins.concatLists (builtins.genList (i:
          let ws = i + 1;
          in [
            "$mod, code:1${toString i}, workspace, ${toString ws}"
            "$mod SHIFT, code:1${toString i}, movetoworkspacesilent, ${toString ws}"
            "$mod CTRL, code:1${toString i}, changegroupactive, ${toString ws}"
          ]) 9)
      );

    bindel = [
      ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
      ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
      ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
      ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
      ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
    ];

    bindm = [
      "$mod, mouse:272, movewindow"
      "$mod, mouse:272, togglefloating"
      "$mod, mouse:273, resizewindow"
    ];

    workspace = [
      # "w[tv1], gapsout:0, gapsin:0"
      # "f[1], gapsout:0, gapsin:0"
      
    ];
    windowrule = [
    #   "bordersize 0, floating:0, onworkspace:w[tv1]"
    #   "rounding 0, floating:0, onworkspace:w[tv1]"
    #   "bordersize 0, floating:0, onworkspace:f[1]"
    #   "rounding 0, floating:0, onworkspace:f[1]"
    ];
  };

  extraConfig = ''
    bind = $mod, R, submap, resize
    submap = resize
    binde = , h, resizeactive, 20 0
    binde = , l, resizeactive, -20 0
    binde = , k, resizeactive, 0 -20
    binde = , j, resizeactive, 0 20
    bind = , catchall, submap, reset
    submap = reset
  '';
}
