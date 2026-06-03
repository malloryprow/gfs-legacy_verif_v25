#!/bin/ksh
set -x

# =========================================================
# Interpolate GFS GRIB2 data 0.25° to  1.0° lat-lon grids
# =========================================================

# 1. Parse arguments
grib_in=${1?"Error: Input grib2 file is required as first argument"}
grib_out=${2?"Error: Output grib2 file is required as second argument"}

tmp1="${grib_out}_tmp1"
tmp2="${grib_out}_tmp2"
final="${grib_out}"

# 2. Determine resolutions
new_grid_args="$new_grid_args -new_grid latlon 0:360:1.0 90:181:-1.0 ${tmp1}"
res_list="$res_list 1p0"

# =========================================================
wgrib2 "${grib_in}" \
        -set_grib_type same \
        -new_grid_winds earth \
        -new_grid_interpolation bilinear \
        -if ':(CSNOW|CRAIN|CFRZR|CICEP|ICSEV):' -new_grid_interpolation neighbor -fi \
        -set_bitmap 1 \
        -set_grib_max_bits 16 \
        -if ':(APCP|ACPCP|PRATE|CPRAT):' -set_grib_max_bits 25 -fi \
        -if ':(APCP|ACPCP|PRATE|CPRAT|DZDT):' -new_grid_interpolation budget -fi \
        ${new_grid_args}

# =====================================================
# 4. Post-processing loop
# =====================================================
# --- RH cap at 100 with controlled precision ---
wgrib2 "${tmp1}" \
    -not_if ':RH:' -grib "${final}" \
    -if ':RH:' \
        -rpn "10:*:0.5:+:floor:1000:min:10:/" \
        -set_grib_type same \
        -set_scaling -1 0 \
        -grib_out "${final}"

rm -f "${tmp1}"

# --- LAND / ICEC consistency fix ---
land_count=$(wgrib2 "${final}" -match ":LAND:" | wc -l)
icec_count=$(wgrib2 "${final}" -match ":ICEC:" | wc -l)

if [[ "$land_count" -ge 1 && "$icec_count" -ge 1 ]]; then
    mv "${final}" "${tmp2}"

    wgrib2 "${tmp2}" \
        -if ':LAND:' -rpn 'sto_1' -fi \
        -if ':ICEC:' -rpn 'rcl_1:0:==:*' -fi \
        -set_grib_type same \
        -set_scaling same same \
        -grib_out "${final}"

    rm -f "${tmp2}"
fi
