{
  # TODO use doas
  # security.doas.enable = true;
  # security.sudo.enable = false;

  # https://askubuntu.com/questions/493002/global-sudo-session-in-ubuntu
  security.sudo.extraConfig = ''
    Defaults:user !tty_tickets, timestamp_timeout=60
  '';

  security.sudo = {
    execWheelOnly = true;
  };

}
