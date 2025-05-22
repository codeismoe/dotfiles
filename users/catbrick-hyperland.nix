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

    input = {
      kb_options = "ctrl:nocaps";
    };

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
      groupbar = {
        enabled = true;
        gradients = 1;
        render_titles = 1;
        "col.active" = "rgba(8839efff)";
        "col.inactive" = "rgba(179299cc)";
        "col.locked_active" = "rgba(dc8a78ff)";
        "col.locked_inactive" = "rgba(7287fdcc)";
      };
    };

    decoration = {
      rounding = 0;
      shadow.enabled = false;
      blur = {
        enabled = false;
      };
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
      center_master_fallback = "right";
    };

    bind =
      [
        "$mod, E, exec, emacsclient -c"
        "$mod SHIFT, E, exec, emacsclient -ce '(eshell \"new\")'"
        "$mod, return, exec, kitty"
        "$mod, W, exec, rofi -show combi -combi-modes 'drun,window' -modes combi"
        "$mod SHIFT, W, exec, rofi-pass"

        "$mod SHIFT, Q, killactive"
        "$mod CTRL, Q, exit"
        "$mod, F, fullscreen"
        "$mod SHIFT, F, togglefloating"
        "$mod, C, exec, togglespec.sh"
        "$mod SHIFT, C, exec, movespec.sh"
        "$mod, space, togglespecialworkspace"
        "$mod SHIFT, space, movetoworkspace, special"
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

    bindit = [
      "$mod, SUPER_L, exec, pkill -SIGUSR1 waybar"
    ];

    binditr = [
      ", SUPER_L, exec, pkill -SIGUSR1 waybar"
    ];

    bindm = [
      "$mod, mouse:272, movewindow"
      "$mod, mouse:272, togglefloating"
      "$mod, mouse:273, resizewindow"
    ];

    workspace = [
      "s[true], gapsout:25"
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
