{ ... }:

{
  programs.readline = {
    enable = true;
    includeSystemConfig = false; # THIS IS IMPORTANT
    variables = {
      # Be 8 bit clean.
      input-meta = true;
      output-meta = true;
      # To allow the use of 8bit-characters like the german umlauts, uncomment
      # the line below. However this makes the meta key not work as a meta key,
      # which is annoying to those which don't need to type in 8-bit characters.
      convert-meta = false;
      keyseq-timeout = 1000;
      # Display a list of the matching files
      show-all-if-ambiguous = true;
      # Perform partial completion on the first Tab press,
      # only start cycling full results on the second Tab press
      menu-complete-display-prefix = true;
      # Color files by types
      # Note that this may cause completion text blink in some terminals (e.g. xterm).
      colored-stats = true;
      # Append char to indicate type
      visible-stats = true;
      # Mark symlinked directories
      mark-symlinked-directories = true;
      # Color the common prefix
      colored-completion-prefix = true;
      # set show-mode-in-prompt on
      show-all-if-unmodified = true;

      echo-control-characters = false;

      # disable completion queries
      # https://superuser.com/questions/601992/how-can-i-disable-that-display-all-possibilities-and-more-stuff-in-bash
      completion-query-items = 0;
      page-completions = false;

    };

    extraConfig = ''
      TAB: menu-complete
      "\e[Z": menu-complete-backward

      # Cycle through history based on characters already typed on the line
      "\e[A":history-search-backward
      "\e[B":history-search-forward

      set bell-style visible

      "\e\C-u": universal-argument
      "\e\C-Df": dump-functions
      "\e\C-Dm": dump-macros
      "\e\C-Dv": dump-variables

      $if Bash
       "\eu": "\C-ucd ..\C-j"
       "\el": "\C-uls\C-j"
       "\eL": "\C-uls -lah\C-j"
       "\ew": "\C-awatch \C-m"
       "\ee": "\C-aexec \C-e\C-m"
       "\ep\eR": "\C-a\C-kreset\C-m"
       "\epN": "\C-a\C-kncdu -rr\C-m"
       "\eph": "\C-a\C-khtop\C-m"
       "\epd": "\C-a\C-kdstat\C-m"
       "\ep\eh": "\C-e --help\C-m"
       "\ep\ev": "\C-e --version\C-m"
       "\ep\ej": "\C-e | jq .\C-m"
       "\et": "\C-a\C-kcd $(mktemp -d)\C-m"

       # nix stuff
       "\epnhb": "\C-a\C-khome-manager build\C-m"
       "\epnhi": "\C-a\C-khome-manager instantiate\C-m"
       "\epnhs": "\C-a\C-khome-manager switch\C-m"
       "\epnr": "\C-a\C-knix repl '<nixos>'\C-m"
       "\epnR": "\C-a\C-knix run\C-m"
       "\epnB": "\C-a\C-knix build\C-m"
       "\epns": "\C-a\C-kexec nix-shell\C-m"
       "\epncl": "\C-a\C-knix-channel --list\C-m"
       "\epncu": "\C-a\C-knix-channel --update\C-m"
       "\epnfs": "\C-a\C-knix flake show\C-m"
       "\epnfm": "\C-a\C-knix flake metadata\C-m"
       "\epnb": "\C-a\C-knix-build\C-m"
       "\eons": "\C-a\C-knix-shell -p "
       "\eon\es": "\C-a\C-knix-store "
       "\eoni": "\C-a\C-knix-env -f '<nixpkgs>' -iA "
       "\eone": "\C-a\C-knix-env -e "
       "\eonb": "\C-a\C-knix-build -A "
       "\eonc": "\C-a\C-knix-channel "
       "\eonh": "\C-a\C-khome-manager "
       "\eonr": "\C-a\C-knix run "
       "\eonR": "\C-a\C-knix run "
       "\eonE": "\C-a\C-knix edit nixos."
       "\eonS": "\C-a\C-knix search "
       "\en\er": "\C-a\C-knix run\C-m"
       "\en\eb": "\C-a\C-knix build\C-m"
       "\epnei": "\C-a\C-knix-env -f . -i\C-m"

       # Xorg
       "\ep\eX": "\C-a\C-k\C-lstartx\C-m"

       # misc
       "\C-xs": "\C-asudo \C-e\C-m"
       "\epp": "\C-a\C-kpython\C-m"
       "\epSc": "\C-a\C-kscreen\C-m"
       "\epgs": "\C-a\C-kgit status\C-m"
       "\epgf": "\C-a\C-kgit fetch\C-m"
       "\epgd": "\C-a\C-kgit diff\C-m"
       "\epgw": "\C-a\C-kgit worktree\C-m"
       "\eppm": "\C-a\C-kpulsemixer\C-m"
       "\eop": "\C-a\C-kpython "
       "\eog": "\C-a\C-kgit "
       "\eogcl": "\C-a\C-kgit clone "
       "\eogco": "\C-a\C-kgit checkout "
       "\eoq": "\C-a\C-ksqlite3 "
       "\eo\es": "\C-a\C-kssh "
       "\eod": "\C-a\C-kmkdir -p "
       "\ept": "\C-a\C-ktree -l\C-m"
       "\em\em": "\C-a\C-kmake\C-m"
       "\em\eM": "\C-a\C-kmake -B\C-m"
       "\em\ec": "\C-a\C-kmake check VERBOSE=1\C-m"
       "\em\ei": "\C-a\C-kmake install\C-m"
       "\emt": "\C-a\C-kmake test\C-m"
       "\emh": "\C-a\C-kmake help\C-m"
       "\emr": "\C-a\C-kmake clean\C-m"
       "\em\er": "\C-a\C-kmake clean\C-m"
       "\emI": "\C-a\C-kmake install -j4\C-m"
       "\emA": "\C-a\C-kmake all -j4\C-m"
       "\eM": "\C-a\C-kmake -j4\C-m"
       "\e\C-l": "\C-a\C-klf\C-m"
      $endif
    '';
  };
}
