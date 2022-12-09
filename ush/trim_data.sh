#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t00z.pgrb2.1p00.anl .

cnvgrib -g21 gfs.t00z.pgrb2.1p00.anl gfs.t00z.pgrbanl
export err=$?

wgrib gfs.t00z.pgrbanl | egrep "(:MSL:|:850 mb:|:500 mb:|:250 mb:|:100 mb:)" \
   | wgrib -i gfs.t00z.pgrbanl -grib -o tmpfile

wgrib tmpfile | egrep "(:HGT:|:TMP:|:RH:|:UGRD:|:VGRD:|:PRMSL:|:VVEL:|:VWSH:)"  \
   | wgrib -i tmpfile -grib -o ver.gfs.t00z.pgrbanl

rm -f tmpfile

grbindex ver.gfs.t00z.pgrbanl ver.gfs.t00z.pgrbianl

cp ver.gfs.t00z.pgrbanl $DATA_GRIB
cp ver.gfs.t00z.pgrbianl $DATA_GRIB

cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t12z.pgrb2.1p00.anl .

cnvgrib -g21 gfs.t12z.pgrb2.1p00.anl gfs.t12z.pgrbanl
export err=$?

wgrib gfs.t12z.pgrbanl | egrep "(:MSL:|:850 mb:|:500 mb:|:250 mb:|:100 mb:)" \
  | wgrib -i gfs.t12z.pgrbanl -grib -o tmpfile

wgrib tmpfile | egrep "(:HGT:|:TMP:|:RH:|:UGRD:|:VGRD:|:PRMSL:|:VVEL:|:VWSH:)" \
  | wgrib -i tmpfile -grib -o ver.gfs.t12z.pgrbanl

rm -f tmpfile

grbindex ver.gfs.t12z.pgrbanl ver.gfs.t12z.pgrbianl

cp ver.gfs.t12z.pgrbanl $DATA_GRIB
cp ver.gfs.t12z.pgrbianl $DATA_GRIB

hr_list_GFS="000 012 024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216 228 240"

for nfile in $hr_list_GFS
do

  cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t00z.pgrb2.1p00.f${nfile} .

  cnvgrib -g21 gfs.t00z.pgrb2.1p00.f${nfile} gfs.t00z.pgrbf${nfile}
  export err=$?

  wgrib gfs.t00z.pgrbf${nfile} | egrep "(:MSL:|:850 mb:|:500 mb:|:250 mb:|:100 mb:)" \
  | wgrib -i gfs.t00z.pgrbf${nfile} -grib -o tmpfile

  wgrib tmpfile | egrep "(:HGT:|:TMP:|:RH:|:UGRD:|:VGRD:|:PRMSL:|:VVEL:|:VWSH:)" \
  | wgrib -i tmpfile -grib -o ver.gfs.t00z.pgrbf${nfile}

  rm -f tmpfile

  grbindex ver.gfs.t00z.pgrbf${nfile} ver.gfs.t00z.pgrbif${nfile}

  cp ver.gfs.t00z.pgrbf${nfile} $DATA_GRIB
  cp ver.gfs.t00z.pgrbif${nfile} $DATA_GRIB


 cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t12z.pgrb2.1p00.f${nfile} .

  cnvgrib -g21 gfs.t12z.pgrb2.1p00.f${nfile} gfs.t12z.pgrbf${nfile}
  export err=$?

  wgrib gfs.t12z.pgrbf${nfile} | egrep "(:MSL:|:850 mb:|:500 mb:|:250 mb:|:100 mb:)" \
  | wgrib -i gfs.t12z.pgrbf${nfile} -grib -o tmpfile

  wgrib tmpfile | egrep "(:HGT:|:TMP:|:RH:|:UGRD:|:VGRD:|:PRMSL:|:VVEL:|:VWSH:)" \
   | wgrib -i tmpfile -grib -o ver.gfs.t12z.pgrbf${nfile}

  rm -f tmpfile

  grbindex ver.gfs.t12z.pgrbf${nfile} ver.gfs.t12z.pgrbif${nfile}

  cp ver.gfs.t12z.pgrbf${nfile} $DATA_GRIB
  cp ver.gfs.t12z.pgrbif${nfile} $DATA_GRIB

done
