# configuration for Espressif devices

{
  # We know, that Espressif devices have a serial, which is unique for all products.
  services.udev.extraRules = ''
    KERNEL=="ttyACM*" \
    , ENV{ID_VENDOR}=="Espressif" \
    , SYMLINK+="tty$env{ID_SERIAL_SHORT}" \
    , SYMLINK+="serial/by-serial/$env{ID_SERIAL_SHORT}"

    KERNEL=="ttyACM*" \
    , ENV{ID_VENDOR}=="Espressif_Systems" \
    , SYMLINK+="tty$env{ID_SERIAL_SHORT}" \
    , SYMLINK+="serial/by-serial/$env{ID_SERIAL_SHORT}"

    KERNEL=="ttyACM*" \
    , ENV{ID_VENDOR}=="Espressif_Systems" \
    , SYMLINK+="tty$env{ID_SERIAL_SHORT}" \
    , SYMLINK+="serial/by-serial/$env{ID_SERIAL_SHORT}"
  '';
}
