A few external libraries are required to be installed for use with the HYSPLIT executables provided within this package.

All programs require the following external packages:
1) gnu compiler (gfortran and gcc)
2) libm
3) libc

MPI dependent executables (hycm_std, hycm_ens, hycm_cb4, hytm_std) will need the following additional libraries:
1) mpich and additional libraries required by mpich

Additional libraries needed for arw2arl, cdf2arl, mer2arl, con2cdf3, and con2cdf4:
1) netcdff and additional libraries required for netcdff (see netcdf4 documentation)

Additional libraries needed for api2arl_v1, api2arl_v2, api2arl_v3, api2arl_v4, and era52arl:
1) eccodes and additional libraries required for eccodes (see eccodes documentation)

To see a full list of required libraries linked to all of the executables, type the following in the exec subdirectory:
ldd *

To see a full list of linked libraries for a specific executable, type the following in the exec subdirectory:
ldd {executable_name}

If a linked library is not found and the library has been installed, add the path of the library to your LD_LIBRARY_PATH. The below example checks to see what libraries are linked to the api2arl_v1 program, adds the path to the eccodes library to LD_LIBRARY_PATH (in csh), and then checks to see if all of the libraries are properly linked to api2arl_v1 again:

[exec]$ ldd api2arl_v1 
        linux-vdso.so.1 =>  (0x00007ffc27740000)
        libeccodes_f90.so => not found
        libeccodes.so => not found
        libgfortran.so.3 => /usr/lib64/libgfortran.so.3 (0x000000327cc00000)
        libm.so.6 => /lib64/libm.so.6 (0x00000039b5000000)
        libgcc_s.so.1 => /lib64/libgcc_s.so.1 (0x00000039b8400000)
        libc.so.6 => /lib64/libc.so.6 (0x00000039b4c00000)
        /lib64/ld-linux-x86-64.so.2 (0x0000563fc7a05000)

[exec]$ setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/opt/eccodes/lib

[exec]$ ldd api2arl_v1 
        linux-vdso.so.1 =>  (0x00007ffda6bef000)
        libeccodes_f90.so => /opt/eccodes/lib/libeccodes_f90.so (0x00007f4207f4a000)
        libeccodes.so => /opt/eccodes/lib/libeccodes.so (0x00007f4207b02000)
        libgfortran.so.3 => /usr/lib64/libgfortran.so.3 (0x000000327cc00000)
        libm.so.6 => /lib64/libm.so.6 (0x00000039b5000000)
        libgcc_s.so.1 => /lib64/libgcc_s.so.1 (0x00000039b8400000)
        libc.so.6 => /lib64/libc.so.6 (0x00000039b4c00000)
        libpng12.so.0 => /usr/lib64/libpng12.so.0 (0x00000039b9000000)
        libz.so.1 => /lib64/libz.so.1 (0x00000039b6000000)
        libjasper.so.1 => /usr/lib64/libjasper.so.1 (0x00000039ba000000)
        libjpeg.so.62 => /usr/lib64/libjpeg.so.62 (0x00000039c6800000)
        libopenjpeg.so.2 => /usr/lib64/libopenjpeg.so.2 (0x00000039b5800000)
        libpthread.so.0 => /lib64/libpthread.so.0 (0x00000039b5400000)
        /lib64/ld-linux-x86-64.so.2 (0x000055fdfaf1a000)

