{
  services.yggdrasil = {
    group = "wheel";
    openMulticastPort = true;
    settings = {
      IfName = "ygg0";
      # fix port
      MulticastInterfaces = [
        {
          Regex = ".*";
          Beacon = true;
          Listen = true;
          Port = 9001;
          Priority = 0;
          Password = "";
        }
      ];
    };
  };
}
