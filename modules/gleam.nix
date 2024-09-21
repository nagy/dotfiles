{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    gleam
    erlang_27
  ];

}
