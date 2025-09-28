# Quick Commands

## Clean Build
- `rm -rf build/`
- `fpm clean` (then press 'y' or just `rm -rf build/`)

## FPM
- `fpm build` - Build library and executables
- `fpm run test_simple` - Run main S3 test program
- `fpm test` - Run unit tests

## CMake
- `mkdir build && cd build`
- `cmake .. && make`
- `./s3_test` - Run main S3 test program