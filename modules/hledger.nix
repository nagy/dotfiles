{ pkgs, nur, ... }:

{
  environment.systemPackages = [
    pkgs.hledger
    pkgs.hledger-ui
    pkgs.hledger-web
    nur.repos.nagy.hledger-fmt
  ];
}
