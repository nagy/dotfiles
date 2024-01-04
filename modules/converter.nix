{ pkgs, ... }:

let
  pcap2txt = pkgs.writeShellScriptBin ",pcap,txt" ''
    exec ${pkgs.wireshark-cli}/bin/tshark -r - "$@"|awk '{$1="";print substr($0,2)}'
  '';
  pcap2json = pkgs.writeShellScriptBin ",pcap,json" ''
    exec ${pkgs.wireshark-cli}/bin/tshark -r - -T json "$@"
  '';
  pcap2xml = pkgs.writeShellScriptBin ",pcap,xml" ''
    exec ${pkgs.wireshark-cli}/bin/tshark -r - -T pdml "$@"
  '';
  json2yaml = pkgs.writeShellScriptBin ",json,yaml" ''
    ${pkgs.jq}/bin/jq --sort-keys | exec ${pkgs.yq-go}/bin/yq --prettyPrint --no-colors "$@"
  '';
  json2txt = pkgs.writeShellScriptBin ",json,txt" ''
    ${pkgs.jq}/bin/jq --sort-keys | exec ${pkgs.gron}/bin/gron "$@"
  '';
in
{
  environment.systemPackages =
    [ pcap2txt pcap2json pcap2xml json2yaml json2txt ];
}
