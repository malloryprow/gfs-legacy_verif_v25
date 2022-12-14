#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
VDATE_YYYY=`echo $VDATE | cut -c1-4`
VDATE_mm=`echo $VDATE | cut -c5-6`
VDATE_dd=`echo $VDATE | cut -c7-8`

cyc_list_GFS="00 12"
hr_list_GFS="000 012 024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216 228 240"
cyc_bufr_GFS="00 06 12 18"

hpssprod_dir=/NCEPPROD/hpssprod/runhistory/rh${VDATE_YYYY}/${VDATE_YYYY}${VDATE_mm}/${VDATE}

for nc in $cyc_list_GFS
do
    # Analysis
    anl_file=gfs.${VDATE}/${nc}/atmos/gfs.t${nc}z.pgrb2.1p00.anl
    if [ -s $COMINgfs/$anl_file ]; then
        echo "Found $COMINgfs/$anl_file"
        cp $COMINgfs/$anl_file ${DATAcurfv3gfs_VDATE}/.
    else
        echo "Didn't find $COMINgfs/$anl_file, try HPSS"
        htar -xf $hpssprod_dir/com_gfs_${gfs_ver}_prod_gfs.${VDATE}_${nc}.gfs_pgrb2.tar ./$anl_file
        mv $anl_file ${DATAcurfv3gfs_VDATE}/.
    fi
    # Analysis index
    anl_idx_file=gfs.${VDATE}/${nc}/atmos/gfs.t${nc}z.pgrb2.1p00.anl.idx
    if [ -s $COMINgfs/$anl_idx_file ]; then
        echo "Found $COMINgfs/$anl_idx_file"
        cp $COMINgfs/$anl_idx_file ${DATAcurfv3gfs_VDATE}/.
    else
        echo "Didn't find $COMINgfs/$anl_idx_file, try HPSS"
        htar -xf $hpssprod_dir/com_gfs_${gfs_ver}_prod_gfs.${VDATE}_${nc}.gfs_pgrb2.tar ./$anl_idx_file
        mv $anl_idx_file ${DATAcurfv3gfs_VDATE}/.
    fi
    # Forecast files
    for nfile in $hr_list_GFS
    do
        fcst_file=gfs.${VDATE}/${nc}/atmos/gfs.t${nc}z.pgrb2.1p00.f${nfile}
        if [ -s $COMINgfs/$fcst_file ]; then
            echo "Found $COMINgfs/$fcst_file"
            cp $COMINgfs/$fcst_file ${DATAcurfv3gfs_VDATE}/.
        else
            echo "Didn't find $COMINgfs/$fcst_file, try HPSS"
            htar -xf $hpssprod_dir/com_gfs_${gfs_ver}_prod_gfs.${VDATE}_${nc}.gfs_pgrb2.tar $fcst_file
            mv $fcst_file ${DATAcurfv3gfs_VDATE}/.
        fi
    done
done

for pbuf in $cyc_bufr_GFS
do
    prepbufr_file=gfs.${VDATE}/${pbuf}/atmos/gfs.t${pbuf}z.prepbufr
    if [ -s $COMINobsproc/$prepbufr_file ]; then
        echo "Found $COMINobsproc/$prepbufr_file"
        cp $COMINobsproc/$prepbufr_file ${DATAcurfv3gfs_VDATE}/.
    else
        echo "Didn't find $COMINobsproc/$prepbufr_file, try HPSS"
        htar -xf $hpssprod_dir/com_obsproc_${obsproc_ver}_prod_gfs.${VDATE}_${nc}.gfs.tar ./$prepbufr_file
        mv $prepbufr_file ${DATAcurfv3gfs_VDATE}/.
    fi
    if [ -s ${DATAcurfv3gfs_VDATE}/gfs.t${pbuf}z.prepbufr ];then
        chmod 750 ${DATAcurfv3gfs_VDATE}/gfs.t${pbuf}z.prepbufr
        chgrp rstprod ${DATAcurfv3gfs_VDATE}/gfs.t${pbuf}z.prepbufr
    fi
    cqc_events_file=gfs.${VDATE}/${pbuf}/atmos/gfs.t${pbuf}z.cqc_events
    if [ -s $COMINobsproc/$cqc_events_file ]; then
        echo "Found $COMINobsproc/$cqc_events_file"
        cp $COMINobsproc/$cqc_events_file ${DATAcurfv3gfs_VDATE}/.
    else
        echo "Didn't find $COMINobsproc/$cqc_events_file, try HPSS"
        htar -xf $hpssprod_dir/com_obsproc_${obsproc_ver}_prod_gfs.${VDATE}_${nc}.gfs.tar ./$cqc_events_file
        mv $cqc_events_file ${DATAcurfv3gfs_VDATE}/.
    fi
    cqc_stncnt_file=gfs.${VDATE}/${pbuf}/atmos/gfs.t${pbuf}z.cqc_stncnt
    if [ -s $COMINobsproc/$cqc_stncnt_file ]; then
        echo "Found $COMINobsproc/$cqc_stncnt_file"
        cp $COMINobsproc/$cqc_stncnt_file ${DATAcurfv3gfs_VDATE}/.
    else
        echo "Didn't find $COMINobsproc/$cqc_stncnt_file, try HPSS"
        htar -xf $hpssprod_dir/com_obsproc_${obsproc_ver}_prod_gfs.${VDATE}_${nc}.gfs.tar ./$cqc_stncnt_file
        mv $cqc_stncnt_file ${DATAcurfv3gfs_VDATE}/.
    fi
    cqc_stnlst_file=gfs.${VDATE}/${pbuf}/atmos/gfs.t${pbuf}z.cqc_stnlst
    if [ -s $COMINobsproc/$cqc_stnlst_file ]; then
        echo "Found $COMINobsproc/$cqc_stnlst_file"
        cp $COMINobsproc/$cqc_stnlst_file ${DATAcurfv3gfs_VDATE}/.
    else
        echo "Didn't find $COMINobsproc/$cqc_stnlst_file, try HPSS"
        htar -xf $hpssprod_dir/com_obsproc_${obsproc_ver}_prod_gfs.${VDATE}_${nc}.gfs.tar $cqc_stnlst_file
        mv $cqc_stnlst_file ${DATAcurfv3gfs_VDATE}/.
    fi
done
