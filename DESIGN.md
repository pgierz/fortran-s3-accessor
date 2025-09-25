The overall idea is to design a FORTRAN-capable S3 interface

* Use the following tools: fpm, ford, cmake
* The actual I/O after the library is finished is no longer important (e.g. NetCDF, HDF5, YAML, etc, etc....)
* The system should look like standard fortran open I/O
* A namelist should control whether S3 or "standard" local filesystem access is used
* It should be an includable library to be used from outside
* The library should be small and concise, only handling this particular use case
* There should be extensive unit tests
* Documentation should be in-code, processible via FORD
* FORTRAN 90 is desired, but more modern FORTRAN is also supported
