#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

######################################################################
# SECTION 1.  Computing QCADP                                        #
#             QCADP consists of qualities checks and restrictions    #
#             which are applied to Radiosonde data                   #
######################################################################

for ct in 00 12
do
   #############################
   #   Produce namelist file   #
   #############################
cat <<\qc2EOF >qc_namlst
  &INPARM ANLD='PGRBANLX', ANLI='PGRBIANL', IGRID=3, RPT=0,
  XS=3.9,3.75,3.0,3.5, LPRDAT=.FALSE.,INTPM=1, INDXA='G' /
qc2EOF
  #################################
  #   Set input data file paths   #
  #################################
  if ! [ -s $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.prepbufr ]
  then
    echo "ADP NOT AVAILABLE"
    echo "QCADP AND VERIFICATIONS CANNOT BE DONE"
    #msg="GDAS prepbufr MISSING: QCADP AND VERIFICATIONS CANNOT BE DONE. EXITING"
    exit
  fi
  if ! [ -s $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.pgrb2.1p00.anl.idx ]
  then
    echo "ANL NOT AVAILABLE"
    echo "QCADP AND VERIFICATIONS CANNOT BE DONE"
    #msg="GFS pgrbianl MISSING: QCADP AND VERIFICATIONS CANNOT BE DONE. EXITING"
    exit
  fi
  #pgm=hrly_qcadp
  #export pgm;. prep_step
  cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.pgrb2.1p00.anl pgrb2
  cnvgrib -g21 pgrb2 PGRBANLX
  export err=$?
  grbindex PGRBANLX PGRBIANL
  export err=$?
  echo $VDATE$ct > arkvdate
  export FORT8="arkvdate"
  export FORT9="qc_namlst"
  export FORT10=$COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.prepbufr
  export FORT11="PGRBANLX"
  export FORT12="PGRBIANL"
  export FORT60="qcadp"
  export FORT70="tosda"
  $EXECgfs_legacy_verif_v25/hrly_qcadp 1>>qcadp_prt 2>>qcadp_err
  #export err=$?; err_chk
  #####################################################
  # Save the printout file and append the output file #
  #####################################################
  cp qcadp $DATA/qcadp.$ct
  cat tosda >> $DATA_SUMAC41/tosda_dat$VDATE_yy$VDATE_mm"."$VDATE_dd
  cat qcadp >> $DATA_SUMAC41/qcadp_dat$VDATE_yy$VDATE_mm"."$VDATE_dd
  export err=$?
  if [ $err -ne 0 ]
  then
    err_exit
  fi
done

#############################
# End of generating QCADP   #
# End of SECTION 1.         #
#############################
