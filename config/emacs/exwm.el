(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "DP-0" 2 "HDMI-0" 3 "DVI-D-0"))
(add-hook 'exwm-randr-screen-change-hook
      (lambda ()
        (start-process-shell-command
         "xrandr" nil "xrandr --output DVI-D-0 --mode 1920x1080 --pos 2560x1440 --output HDMI-0 --mode 1920x1080 --pos 320x0 --output DP-0 --primary --mode 2560x1440 --pos 0x1080 --rotate normal")))
(exwm-randr-enable)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(exwm-randr-refresh)
