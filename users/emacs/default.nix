{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-pgtk;
      config = ./emacs.org;
      defaultInitFile = true;
      alwaysEnsure = true;
      alwaysTangle = true;
    };
  };
}
