# Northwest Energy Efficiency Alliance

### Instructions to obtain the EXT files

* ``neea_extract.R``: generate a raw file per circuit.
* ``mv *-Mains.csv folder`` in command line to move all ``*-Mains.csv`` files to a separate ``folder``. Then proceed as below in ``folder``.
* ``raw2ext.R``: conversion from raw to ext.
* ``tz_correction.R``: correct time zones.