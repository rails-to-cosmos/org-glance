{ pkgs, ... }:

{
  packages = with pkgs; [
    cask
    emacs
  ];
}
