{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
      # {
      # package = pkgs.emacs-pgtk;
      # config = ./emacs.org;
      # defaultInitFile = true;
      # alwaysEnsure = true;
      # alwaysTangle = true;
      # };
  };
}
