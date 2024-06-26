# IOb Versat Linux Kernel Drivers
- Structure:
    - `drivers/`: directory with linux kernel module drivers for iob_versat
        - `iob_versat_main.c`: driver source
        - `[iob_versat.h]` and `[iob_versat_sysfs.h]`: header files generated by:
        ```bash
        python3 .path/to/iob-linux/scripts/drivers.py iob_versat -o [output_dir]
        ```
        - `driver.mk`: makefile segment with `iob_versat-obj:` target for driver
          compilation
    - `user/`: directory with user application example that uses iob_versat
      drivers
        - `iob_versat_user.c`: example user application that uses iob_versat
          drivers
        - `Makefile`: user application compilation targets
    - `iob_versat.dts`: device tree template with iob_versat node
        - manually add the `versat` node to the system device tree so the
          iob_versat is recognized by the linux kernel
