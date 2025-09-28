#!/usr/bin/env python3
"""
S3 NetCDF reading example using xarray and s3fs.

This script demonstrates reading NetCDF data from AWS S3 using the same
approach that the Fortran S3 accessor library aims to provide.
"""

import s3fs
import xarray as xr

def main():
    print("S3 NetCDF Reading Example")
    print("=" * 40)

    # Create S3 filesystem with ECMWF endpoint (European source)
    fs = s3fs.S3FileSystem( anon=True,)

    # ECMWF ERA5 climate data from European source
    # Note: This is a placeholder URL - actual bucket structure needs verification
    s3_url = "s3://esgf-world/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/fx/areacella/gn/v20200212/areacella_fx_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn.nc"

    print(f"Reading NetCDF file from S3: {s3_url}")
    print()

    try:
        with fs.open(s3_url) as fileObj:
            ds = xr.open_dataset(fileObj, engine='h5netcdf')

            print("Dataset successfully opened!")
            print()
            print("Dataset representation:")
            print(ds)

    except Exception as e:
        print(f"Error reading dataset: {e}")
        return 1

    return 0

if __name__ == "__main__":
    exit(main())
