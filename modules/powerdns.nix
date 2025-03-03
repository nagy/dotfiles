{
  services.pdns-recursor = {
    forwardZonesRecurse =
      {
        "" = "1.1.1.1";
        # "" = "8.8.8.8:853";
      };
  };
}
