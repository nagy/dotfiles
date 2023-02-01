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
     ${pkgs.jq}/bin/jq | exec ${pkgs.yq-go}/bin/yq --prettyPrint --no-colors "$@"
  '';
  json2lines = pkgs.writeShellScriptBin ",json,lines" ''
     ${pkgs.jq}/bin/jq | exec ${pkgs.gron}/bin/gron "$@"
  '';
  # encoding
  any2hex = pkgs.writeShellScriptBin ",any,hex" ''
     exec hexdump -vC "$@"
  '';
  any2b64 = pkgs.writeShellScriptBin ",any,b64" ''
     exec base64 "$@"
  '';
  any2b32 = pkgs.writeShellScriptBin ",any,b32" ''
     exec base32 "$@"
  '';
  # hashes
  any2sha256 = pkgs.writeShellScriptBin ",any,sha256" ''
     exec sha256sum "$@"
  '';
  any2sha1 = pkgs.writeShellScriptBin ",any,sha1" ''
     exec sha1sum "$@"
  '';
  any2md5 = pkgs.writeShellScriptBin ",any,md5" ''
     exec md5sum "$@"
  '';
in
{
  environment.systemPackages =  [
    pcap2txt
    pcap2json
    pcap2xml
    json2yaml
    json2lines
    any2hex
    any2b64
    any2b32
    any2sha256
    any2sha1
    any2md5
  ];
}
