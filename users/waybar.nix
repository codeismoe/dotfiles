{lib, ...}:

{
  programs.waybar = let
    waybar-settings = {
      bar_id = "bar-0";
      "ipc" = true;
      "spacing" = 4;
      "modules-left" = [
        "pulseaudio"
        "tray"
        "battery"
      ];
      "modules-center" = [
        "niri/workspaces"
      ];
      "modules-right" = [
        "niri/window"
        "clock"
      ];

      "niri/workspaces" = {
        "disable-scroll" = true;
        "all-outputs" = false;
        "warp-on-scroll" = false;
        "format" = "{icon}";
        "format-icons" = {
          "active" = " ";
          "default" = " ";
          "empty" = "";
        };
      };
      "niri/window" = {
        "format" = "{}";
      };
      "tray" = {
        "spacing" = 10;
      };
      "clock" = {
        "tooltip-format" = "<big>{ =%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        "format-alt" = "{ =%Y-%m-%d}";
      };
      "cpu" = {
        "format" = "{usage}% ";
        "tooltip" = false;
      };
      "memory" = {
        "format" = "{}% ";
      };
      "backlight" = {
        "format" = "{percent}% {icon}";
        "format-icons" = ["" "" "" "" "" "" "" "" ""];
      };
      "battery" = {
        "states" = {
          "warning" = 30;
          "critical" = 15;
        };
        "format" = "{capacity}% {icon}";
        "format-full" = "{capacity}% {icon}";
        "format-charging" = "{capacity}% ";
        "format-plugged" = "{capacity}% ";
        "format-alt" = "{time} {icon}";

        "format-icons" = ["" "" "" "" ""];
      };

      "power-profiles-daemon" = {
        "format" = "{icon}";
        "tooltip-format" = "Power profile = {profile}\nDriver = {driver}";
        "tooltip" = true;
        "format-icons" = {
          "default" = "";
          "performance" = "";
          "balanced" = "";
          "power-saver" = "";
        };
      };

      "network" = {
        "format-wifi" = "{essid} ({signalStrength}%) ";
        "format-ethernet" = "{ipaddr}/{cidr}";
        "tooltip-format" = "{ifname} via {gwaddr}";
        "format-linked" = "{ifname} (No IP)";
        "format-disconnected" = "Disconnected ⚠";
        "format-alt" = "{ifname} = {ipaddr}/{cidr}";
      };

      "pulseaudio" = {
        "format" = "{volume}% {icon}  {format_source}";
        "format-bluetooth" = "{volume}% {icon} {format_source}";
        "format-bluetooth-muted" = "{icon} {format_source}";
        "format-muted" = "{format_source}";
        "format-source" = "{volume}% ";
        "format-source-muted" = "";
        "format-icons" = {
          "headphone" = "";
          "phone" = "";
          "portable" = "";
          "car" = "";
          "default" = ["" "" ""];
        };
        "on-click" = "pavucontrol";
      };
    };
  in {
    enable = true;
    systemd.enable = true;
    settings = [waybar-settings];
    style = lib.mkAfter ''
      #workspaces button.empty {
        padding: 0px;
        border: 0px;
        margin: 0px;
      }

      #workspaces button.active {
        margin-left: 3px;
        padding-left: 12px;
        padding-right: 12px;
        margin-right: 3px;
        border-bottom: 3px solid @base05;
        color: @base05;
     }
     #workspaces button.focused {
        margin-left: 3px;
        padding-left: 12px;
        padding-right: 12px;
        margin-right: 3px;
        border-bottom: 3px solid @base08;
        color: @base08;
     }
    '';
  };

  stylix.targets.waybar.addCss = false;
}
