{ config, lib, pkgs, ... }:

let
  inherit (lib) escapeShellArg;
  cfg = config.nagy.xcompose;
  mkXComposeLine = key: lst: ''
    (
    set -e
    ucode=$(echo -n ${escapeShellArg key} \
               | ${pkgs.glibc.bin}/bin/iconv -f utf8 -t utf32be \
               | ${pkgs.xxd}/bin/xxd -p \
               | sed -r 's/^0+/U/' \
               | ${pkgs.perl}/bin/perl -ne 'print uc')
    unumber=$(sed -r 's/^U/0x/' <<< $ucode)
    list=${escapeShellArg lst}
    name=$(${pkgs.nur.repos.nagy.unum}/bin/unum $unumber|sed 1d|awk '{$1="";$2="";$3="";$4="";$5=""}1'|xargs)
    printf "%-50s : \"%s\" %9s # %s\n" "$list" ${
      escapeShellArg key
    } $ucode "$name"
    )
  '';
  theScript = pkgs.writeShellScript "script" (lib.concatStrings
    (lib.mapAttrsToList mkXComposeLine (cfg // defaultKeys)));
  generatedFile =
    pkgs.runCommandLocal "XCompose-gen" { } "${theScript}|sort> $out";
  defaultKeys = {

    # Emoji
    # http://unicode.org/emoji/charts/full-emoji-list.html
    "🙂" = [ "<Multi_key>" "<colon>" "<parenright>" ];
    "🙁" = [ "<Multi_key>" "<colon>" "<parenleft>" ];
    "😢" = [ "<Multi_key>" "<semicolon>" "<parenleft>" ];
    "😀" = [ "<Multi_key>" "<colon>" "<D>" ];
    "😛" = [ "<Multi_key>" "<colon>" "<P>" ];
    "🤓" = [ "<Multi_key>" "<n>" "<e>" "<r>" "<d>" ];
    # "❤️" = [ "<Multi_key>" "<h>" "<e>" "<a>" "<r>" "<t>" ];
    "🔑" = [ "<Multi_key>" "<k>" "<e>" "<y>" ];
    "👀" = [ "<Multi_key>" "<e>" "<y>" "<e>" "<s>" ];
    "🥚" = [ "<Multi_key>" "<e>" "<g>" "<g>" ];
    "⎈" = [ "<Multi_key>" "<h>" "<e>" "<l>" "<m>" ];
    "🍀" = [ "<Multi_key>" "<c>" "<l>" "<o>" "<v>" "<e>" "<r>" ];
    "💡" = [ "<Multi_key>" "<b>" "<u>" "<l> <b>" ];
    "⍼" = [ "<Multi_key>" "<r>" "<a>" "<z>" "<z>" ];
    "☆" = [ "<Multi_key>" "<s>" "<t>" "<a>" "<r>" ];
    "🔅" = [ "Ctrl" "<Multi_key>" "<o>" "<x>" ];
    "🔆" = [ "Ctrl" "<Multi_key>" "<o>" "<X>" ];
    "💸" = [ "<Multi_key>" "<dollar>" "<slash>" ];
    "🐕" = [ "<Multi_key>" "<d>" "<o>" "<g>" ];
    # "🐕" = [ "<Multi_key>" "<k>" "<9>" ]; # canine
    "🐖" = [ "<Multi_key>" "<p>" "<i>" "<g>" ];
    "🐛" = [ "<Multi_key>" "<b>" "<u>" "<g>" ];
    "📀" = [ "<Multi_key>" "<d>" "<v>" "<d>" ];
    "📦" = [ "<Multi_key>" "<p>" "<k>" "<g>" ];
    "🐍" = [ "<Multi_key>" "<s>" "<n>" "<a>" "<k>" "<e>" ];
    # "🐍" = [ "<Multi_key>" "<s>" "<n>" "<e>" "<k>" ];
    "🐜" = [ "<Multi_key>" "<a>" "<n>" "<t>" ];
    "☉" = [ "<Multi_key>" "<s>" "<u>" "<n>" ];
    "🔥" = [ "<Multi_key>" "<f>" "<r>" ];
    "✄" = [ "<Multi_key>" "<XF86Cut>" ];

    # fraktur
    "𝒻" = [ "<Multi_key>" "<dstroke>" "<f>" ];
    "∃" = [ "<Multi_key>" "<E>" "<E>" ];
    "∄" = [ "<Multi_key>" "<slash>" "<E>" "<E>" ];
    "∀" = [ "<Multi_key>" "<A>" "<A>" ];
    "⌀" = [ "<Multi_key>" "<0>" "<slash>" ];
    # "⌀" = [ "<Multi_key>" "<slash>" "<0>" ];
    "🞉" = [ "<Multi_key>" "<O>" "<O>" "<O>" ];

    "ƻ" = [ "<Multi_key>" "<2>" "<minus>" ];
    # "ƻ" = [ "<Multi_key>" "<minus>" "<2>" ];
    "ǂ" = [ "<Multi_key>" "<minus>" "<minus>" "<bar>" ];
    "🆥" = [ "<Multi_key>" "<U25A1>" "<d>" ];
    "ⓐ" = [ "<Multi_key>" "<circle>" "<a>" ];
    "●" = [ "<Multi_key>" "<circle>" "<circle>" ];
    "🅐" = [ "<Multi_key>" "Alt" "<circle>" "<A>" ];
    "🅑" = [ "<Multi_key>" "Alt" "<circle>" "<B>" ];
    "∡" = [ "<Multi_key>" "<less>" "<parenright>" ];
    "◌" = [ "<Multi_key>" "<circle>" "<period>" ];
    "⬚" = [ "<Multi_key>" "<U25A1>" "<period>" ];
    "⓵" = [ "<Multi_key>" "<Multi_key>" "<circle>" "<1>" ];
    "⓶" = [ "<Multi_key>" "<Multi_key>" "<circle>" "<2>" ];
    "⓷" = [ "<Multi_key>" "<Multi_key>" "<circle>" "<3>" ];

    # dice
    "⚀" = [ "<Multi_key>" "<onesuperior>" ];
    "⚁" = [ "<Multi_key>" "<twosuperior>" ];
    "⚂" = [ "<Multi_key>" "<threesuperior>" ];
    "⚃" = [ "<Multi_key>" "<onequarter>" ];
    "⚄" = [ "<Multi_key>" "<onehalf>" ];
    "⚅" = [ "<Multi_key>" "<notsign>" ];

    # shades
    "░" = [ "Alt" "<Multi_key>" "<1>" ];
    "▒" = [ "Alt" "<Multi_key>" "<2>" ];
    "▓" = [ "Alt" "<Multi_key>" "<3>" ];
    "🞕" = [ "<Multi_key>" "<Return>" "<e>" ];
    "▧" = [ "<Multi_key>" "<Return>" "<w>" ];
    "▨" = [ "<Multi_key>" "<Return>" "<r>" ];
    "⎅" = [ "<Multi_key>" "<Return>" "<d>" ];
    "▤" = [ "<Multi_key>" "<Return>" "<f>" ];
    "▥" = [ "<Multi_key>" "<Return>" "<g>" ];

    "₿" = [ "<Multi_key>" "<EuroSign>" ];
    "⎵" = [ "<Multi_key>" "<U230B>" "<U230A>" ];
    # "⎵" = [ "<Multi_key>" "<U230A>" "<U230B>" ];
    "⏚" = [ "<Multi_key>" "<U2641>" ];

    # misc
    # Pentagon
    "⛤" = [ "<Multi_key>" "<U2B20>" ];
    # Hexagon
    "♄" = [ "<Multi_key>" "<U2B21>" ];
    "⟀" = [ "<Multi_key>" "<3>" "<d>" ];

    "⌚" = [ "<Multi_key>" "<tslash>" ];
    "⦇" = [ "Ctrl" "<Multi_key>" "<parenleft>" ];
    "⦈" = [ "Ctrl" "<Multi_key>" "<parenright>" ];

    "⫽" = [ "<Multi_key>" "<slash>" "<slash>" ];
    "≡" = [ "<Multi_key>" "<equal>" "<equal>" ];
    "⏻" = [ "<Multi_key>" "<o>" "<exclam>" ];
    "⎎" = [ "<Multi_key>" "<h>" "<y>" "<s>" ];
    # "⎎" = [ "<Multi_key>" "<asciitilde>" "<slash>" ];
    "⑁" = [ "<Multi_key>" "<bar>" "<h>" ];
    "🞋" = [ "<dead_breve>" "<t>" ];
    "🞖" = [ "<dead_breve>" "<T>" ];
    "🞜" = [ "<dead_breve>" "Alt" "<t>" ];
    "⋔" = [ "<dead_breve>" "<h>" ];
    "⏢" = [ "<dead_breve>" "<f>" ];
    "◇" = [ "<dead_breve>" "<d>" ];
    "⧖" = [ "<dead_breve>" "<D>" ];
    "‗" = [ "<Multi_key>" "<underscore>" "<minus>" ];
    "⫪" = [ "<Multi_key>" "<underscore>" "<underscore>" ];
    "⫫" = [ "<Multi_key>" "<macron>" "<macron>" ];
    "〜" = [ "<Multi_key>" "<Multi_key>" "<asciitilde>" ];

  };
in {

  options = {
    nagy.xcompose = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = { };
      description = "xcompose keys";
    };
  };

  config = lib.mkIf config.services.xserver.enable {

    environment.etc."XCompose".source = generatedFile;

  };

}
