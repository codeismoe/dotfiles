{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  gitUpdater,
  gnome-themes-extra,
  gtk-engine-murrine,
  jdupes,
  sassc,
  themeVariants ? [ ], # default: blue
  colorVariants ? [ ], # default: all
  sizeVariants ? [ ], # default: standard
  tweaks ? [ ],
  round ? "6px",
  wallpapers ? false,
  withGrub ? false,
  grubScreens ? [ ], # default: 1080p
}:

let
  pname = "graphite-gtk-theme";
in
lib.checkListOfEnum "${pname}: theme variants"
  [
    "default"
    "purple"
    "pink"
    "red"
    "orange"
    "yellow"
    "green"
    "teal"
    "blue"
    "all"
  ]
  themeVariants
  lib.checkListOfEnum
  "${pname}: color variants"
  [ "standard" "light" "dark" ]
  colorVariants
  lib.checkListOfEnum
  "${pname}: size variants"
  [ "standard" "compact" ]
  sizeVariants
  lib.checkListOfEnum
  "${pname}: tweaks"
  [
    "nord"
    "black"
    "darker"
    "rimless"
    "normal"
    "float"
    "colorful"
  ]
  tweaks
  lib.checkListOfEnum
  "${pname}: grub screens"
  [ "1080p" "2k" "4k" ]
  grubScreens

  stdenvNoCC.mkDerivation
  rec {
    inherit pname;
    version = "daa33f4f7b3991acb1c93dea13f58b4705ee244e";

    src = fetchFromGitHub {
      owner = "vinceliuice";
      repo = "graphite-gtk-theme";
      rev = version;
      hash = "sha256-+POgrP1A6pbYjyITRBco0K42c6nTiIspo8vIcAS/+vE=";
    };

    nativeBuildInputs = [
      jdupes
      sassc
    ];

    buildInputs = [
      gnome-themes-extra
    ];

    propagatedUserEnvPkgs = [
      gtk-engine-murrine
    ];

    postPatch = ''
      patchShebangs install.sh wallpaper/install-wallpapers.sh

      substituteInPlace wallpaper/install-wallpapers.sh \
       --replace-fail /usr/share $out/share \
       --replace-fail '[[ "$UID" -eq "$ROOT_UID" ]]' true
    '';

    installPhase = ''
      runHook preInstall

      name= ./install.sh \
        ${lib.optionalString (themeVariants != [ ]) "--theme " + builtins.toString themeVariants} \
        ${lib.optionalString (colorVariants != [ ]) "--color " + builtins.toString colorVariants} \
        ${lib.optionalString (sizeVariants != [ ]) "--size " + builtins.toString sizeVariants} \
        ${lib.optionalString (tweaks != [ ]) "--tweaks " + builtins.toString tweaks} \
        ${lib.optionalString (round != "") "--round ${round} "} \
        --dest $out/share/themes

      ${lib.optionalString wallpapers "sh -x wallpaper/install-wallpapers.sh"}

      ${lib.optionalString withGrub ''
        (
        cd other/grub2

        patchShebangs install.sh

        ./install.sh --justcopy --dest $out/share/grub/themes \
          ${lib.optionalString (builtins.elem "nord" tweaks) "--theme nord"} \
          ${lib.optionalString (grubScreens != [ ]) "--screen " + builtins.toString grubScreens}
        )
      ''}

      jdupes --quiet --link-soft --recurse $out/share

      runHook postInstall
    '';

    passthru.updateScript = gitUpdater { };

    meta = with lib; {
      description = "Flat Gtk+ theme based on Elegant Design";
      homepage = "https://github.com/vinceliuice/Graphite-gtk-theme";
      license = licenses.gpl3Only;
      platforms = platforms.unix;
      maintainers = [ maintainers.romildo ];
    };
  }
