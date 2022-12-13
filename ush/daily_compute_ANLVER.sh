#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

##########################################################
# SECTION 3. Verification of Forecast against Analyses   #
#            This is known as the ANLVER portion         #
##########################################################

for ct in 00 12
do
  ##############################################
  # Setting the current time/cycle and month   #
  ##############################################
  export CURTIME=$VDATE$ct
  echo $CURTIME > hrly_currdate.in
  echo $CURTIME > hrly_date.in
  export d0=`echo $VDATE | cut -c1-8`
  export MONTH=`echo $CURTIME | cut -c5-6`
  cp $FIXgfs_legacy_verif_v25/hrly_s1labels.in .
  cp $FIXgfs_legacy_verif_v25/hrly_limits.in .
  cp $FIXgfs_legacy_verif_v25/hrly_verif.in .
  icn_stats=" gfs12 gfs24 gfswnd gmslzt "
  for nstats in $icn_stats
  do
    #start of if for gfs12 stats
    if test "$nstats" = 'gfs12'
    then
      #############################################
      # Copying  GFS files for 12 hour intervals  #
      #############################################
      if [ $ct -eq 00 ]
      then
        cp $COMINverd_grib/ver.gfs.t00z.pgrbanl GRIBAD0
        cp $COMINverd_grib/ver.gfs.t00z.pgrbianl GRIBAI0
        n=0
      else
        cp $COMINverd_grib/ver.gfs.t12z.pgrbanl GRIBAD0
        cp $COMINverd_grib/ver.gfs.t12z.pgrbianl GRIBAI0
        cp $COMINverd_grib/ver.gfs.t00z.pgrbf012 GRIBFD1
        cp $COMINverd_grib/ver.gfs.t00z.pgrbif012 GRIBFI1
        n=1
      fi
      ###############################################
      # Loop which takes the time out to 120hr      #
      # n is define as the number of day(s) back    #
      ###############################################
      for thr in 1 2 3 4 5
      do
        ndate=`finddate.sh $VDATE d-$thr`
        d1=`echo $ndate | cut -c1-8`
        n=`expr $n + 1`
        fcst1=`expr $n '*' 12`
        if [ $fcst1 -lt 100 ]
        then
           fcst1="0"$fcst1
        fi
        if [ $fcst1 -lt 120 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t12z.pgrbf${fcst1} GRIBFD$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t12z.pgrbif${fcst1} GRIBFI$n
        fi
        if [ $fcst1 -eq 120 ] && [ $ct -eq 12 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t12z.pgrbf${fcst1} GRIBFDL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t12z.pgrbif${fcst1} GRIBFIL
        fi
        n=`expr $n + 1`
        fcst1=`expr $n '*' 12`
        if [ $fcst1 -lt 100 ]
        then
          fcst1="0"$fcst1
        fi
        if [ $fcst1 -lt 120 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t00z.pgrbf${fcst1} GRIBFD$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t00z.pgrbif${fcst1} GRIBFI$n
        fi
        if [ $fcst1 -eq 120 ] && [ $ct -eq 00 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t00z.pgrbf${fcst1} GRIBFDL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t00z.pgrbif${fcst1} GRIBFIL
        fi
      done
      #pgm=hrly_s1daily
      #export pgm;. prep_step
      export FORT10="hrly_date.in"
      export FORT11="hrly_currdate.in"
      export FORT12="hrly_s1labels.in"
      export FORT13="$FIXgfs_legacy_verif_v25/hrly_gfs.in"
      export FORT14="hrly_limits.in"
      export FORT18="hrly_verif.in"
      export FORT20="GRIBAD0"
      export FORT21="GRIBAI0"
      export FORT22="GRIBFD1"
      export FORT23="GRIBFI1"
      export FORT24="GRIBFD2"
      export FORT25="GRIBFI2"
      export FORT26="GRIBFD3"
      export FORT27="GRIBFI3"
      export FORT28="GRIBFD4"
      export FORT29="GRIBFI4"
      export FORT30="GRIBFD5"
      export FORT31="GRIBFI5"
      export FORT32="GRIBFD6"
      export FORT33="GRIBFI6"
      export FORT34="GRIBFD7"
      export FORT35="GRIBFI7"
      export FORT36="GRIBFD8"
      export FORT37="GRIBFI8"
      export FORT38="GRIBFD9"
      export FORT39="GRIBFI9"
      export FORT40="GRIBFDL"
      export FORT41="GRIBFIL"
      export FORT60="$DATA_ANLVER_FCST1/$CURTIME.gfs.out"
      OUT2=$DATA_ANLVER_FCST1/$CURTIME.gfs.out
      echo EXECUTING
      $EXECgfs_legacy_verif_v25/hrly_s1daily  > $DATA_ANLVER_FCST1/s1gfs.$CURTIME
      rm GRIB* fort*
      cat $OUT2 >> $DATA_ANLVER_DATA1/GFS.$MONTH.in
      echo "End of the GFS model (every 12hr)"
    fi
    #end of if for gfs12 stats

    #start of if for gfs24 stats
    if test "$nstats" = 'gfs24'
    then
      ##############################################################
      # Copying GFS Forecast fields for 24 hour intervals (GFSM)   #
      ##############################################################
      cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbanl GRIBAD0
      cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbianl GRIBAI0
      n=0
      for thr in 1 2 3 4 5 6 7 8 9 10
      do
        ndate=`finddate.sh $VDATE d-${thr}`
        d1=`echo $ndate | cut -c1-8`
        n=`expr $n + 1`
        fcst1=`expr $n '*' 24`
        if [ $fcst1 -lt 100 ]
        then
          fcst1="0"$fcst1
        fi
        if [ $fcst1 -lt 240 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf${fcst1} GRIBFD$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif${fcst1} GRIBFI$n
        fi

        if [ $fcst1 -eq 240 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf${fcst1} GRIBFDL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif${fcst1} GRIBFIL
        fi
      done
      #pgm=hrly_s1daily
      #export pgm;. prep_step
      export FORT10="hrly_date.in"
      export FORT11="hrly_currdate.in"
      export FORT12="hrly_s1labels.in"
      export FORT13="$FIXgfs_legacy_verif_v25/hrly_gfsm.in"
      export FORT14="hrly_limits.in"
      export FORT18="hrly_verif.in"
      export FORT20="GRIBAD0"
      export FORT21="GRIBAI0"
      export FORT22="GRIBFD1"
      export FORT23="GRIBFI1"
      export FORT24="GRIBFD2"
      export FORT25="GRIBFI2"
      export FORT26="GRIBFD3"
      export FORT27="GRIBFI3"
      export FORT28="GRIBFD4"
      export FORT29="GRIBFI4"
      export FORT30="GRIBFD5"
      export FORT31="GRIBFI5"
      export FORT32="GRIBFD6"
      export FORT33="GRIBFI6"
      export FORT34="GRIBFD7"
      export FORT35="GRIBFI7"
      export FORT36="GRIBFD8"
      export FORT37="GRIBFI8"
      export FORT38="GRIBFD9"
      export FORT39="GRIBFI9"
      export FORT40="GRIBFDL"
      export FORT41="GRIBFIL"
      export FORT60="$DATA_ANLVER_FCST1/$CURTIME.gfsm.out"
      OUT2=$DATA_ANLVER_FCST1/$CURTIME.gfsm.out
      echo "EXECUTING"
      $EXECgfs_legacy_verif_v25/hrly_s1daily  > $DATA_ANLVER_FCST1/s1gfsm.$CURTIME
      cat $OUT2  >> $DATA_ANLVER_DATA1/GFL.$MONTH.in
      rm GRIB* fort*
      echo "End of the GFSM (every 24hr)"
    fi
    #end of if for gfs24 stats

    #start of if for gfswnd stats
    ##################################################################
    # Verifying WMO STANDARD VERIFICATION SCORES WMOW FOR GFS WINDS  #
    ##################################################################
    if test "$nstats" = 'gfswnd'
    then
      #################################################################
      #  Forecast grib and index files have already been generated    # 
      #  from the above step GFS 24 hour (GFSM)                       # 
      #################################################################
      cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbanl GRIBAD0
      cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbianl GRIBAI0
      n=0
      ###############################################
      # Loop which takes the time out to 240hr      #
      # n is define as the number of day(s) back    #
      ###############################################
      for thr in 1 2 3 4 5 6 7 8 9 10
      do
        ndate=`finddate.sh $VDATE d-${thr}`
        d1=`echo $ndate | cut -c1-8`
        n=`expr $n + 1`
        fcst1=`expr $n '*' 24`
        if [ $fcst1 -lt 100 ]
        then
          fcst1="0"$fcst1
        fi
        if [ $fcst1 -lt 240 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf${fcst1} GRIBFD$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif${fcst1} GRIBFI$n
        fi
        if [ $fcst1 -eq 240 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf${fcst1} GRIBFDL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif${fcst1} GRIBFIL
        fi
      done
      #pgm=hrly_wmow
      #export pgm;. prep_step
      export FORT11="hrly_currdate.in"
      export FORT12="$FIXgfs_legacy_verif_v25/hrly_wndlabels.in"
      export FORT13="$FIXgfs_legacy_verif_v25/hrly_wgfs.in"
      export FORT14="$FIXgfs_legacy_verif_v25/hrly_gfsareas.in"
      export FORT20="GRIBAD0"
      export FORT25="GRIBAI0"
      export FORT31="GRIBFD1"
      export FORT51="GRIBFI1"
      export FORT32="GRIBFD2"
      export FORT52="GRIBFI2"
      export FORT33="GRIBFD3"
      export FORT53="GRIBFI3"
      export FORT34="GRIBFD4"
      export FORT54="GRIBFI4"
      export FORT35="GRIBFD5"
      export FORT55="GRIBFI5"
      export FORT36="GRIBFD6"
      export FORT56="GRIBFI6"
      export FORT37="GRIBFD7"
      export FORT57="GRIBFI7"
      export FORT38="GRIBFD8"
      export FORT58="GRIBFI8"
      export FORT39="GRIBFD9"
      export FORT59="GRIBFI9"
      export FORT40="GRIBFDL"
      export FORT60="GRIBFIL"
      ###################
      #  output files   #
      ###################
      export FORT70="$DATA_ANLVER_DATA1/w850"
      export FORT71="$DATA_ANLVER_DATA1/w500"
      export FORT72="$DATA_ANLVER_DATA1/w250"
      OUT2=$DATA_ANLVER_DATA1/w850
      OUT3=$DATA_ANLVER_DATA1/w500
      OUT4=$DATA_ANLVER_DATA1/w250
      export OUT2 OUT3 OUT4
      echo "EXECUTING"
      $EXECgfs_legacy_verif_v25/hrly_wmow  > $DATA_ANLVER_FCST1/wgfs.$CURTIME
      echo "MONTH = $MONTH"
      rm fort* GRIB*
      cat $OUT2 >> $DATA_ANLVER_DATA1/$MONTH.w850.in${ct}
      cat $OUT3 >> $DATA_ANLVER_DATA1/$MONTH.w500.in${ct}
      cat $OUT4 >> $DATA_ANLVER_DATA1/$MONTH.w250.in${ct}
      rm $OUT2 $OUT3 $OUT4
      echo "End of the WMOW  wind calculations"
    fi
    #end of if for gfswnd stats

    #start of if for gmslzt stats
    #######################################################################
    # Verifying WMO STANDARD VERIFICATION SCORES WMOX FOR GFS MSLP, Z, T  #
    #######################################################################
    if test "$nstats" = 'gmslzt'
    then
      ###############################################
      # Loop which takes the time out to 240hr      #
      # n is define as the number of day(s) back    #
      ###############################################
      n=0
      cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbanl GRIBAD0
      cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbianl GRIBAI0
      for thr in 1 2 3 4 5 6 7 8 9 10
      do
        ndate=`finddate.sh $VDATE d-${thr}`
        d1=`echo $ndate | cut -c1-8`
        n=`expr $n + 1`
        fcst1=`expr $n '*' 24`
        if [ $fcst1 -lt 100 ]
        then
          fcst1="0"$fcst1
        fi
        if [ $fcst1 -lt 240 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf${fcst1} GRIBFD$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif${fcst1} GRIBFI$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbanl GRIBAD$n
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbianl GRIBAI$n
        fi
        if [ $fcst1 -eq 240 ]
        then
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf${fcst1} GRIBFDL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif${fcst1} GRIBFIL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbanl GRIBADL
          cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbianl GRIBAIL
        fi
      done
      #pgm=hrly_wmox
      #export pgm;. prep_step
      export FORT11="hrly_currdate.in"
      export FORT12="$FIXgfs_legacy_verif_v25/hrly_wmolabels.in"
      export FORT13="$FIXgfs_legacy_verif_v25/hrly_xgfs.in"
      export FORT20="GRIBAD0"
      export FORT25="GRIBAI0"
      export FORT31="GRIBFD1"
      export FORT51="GRIBFI1"
      export FORT41="GRIBAD1"
      export FORT61="GRIBAI1"
      export FORT32="GRIBFD2"
      export FORT52="GRIBFI2"
      export FORT42="GRIBAD2"
      export FORT62="GRIBAI2"
      export FORT33="GRIBFD3"
      export FORT53="GRIBFI3"
      export FORT43="GRIBAD3"
      export FORT63="GRIBAI3"
      export FORT34="GRIBFD4"
      export FORT54="GRIBFI4"
      export FORT44="GRIBAD4"
      export FORT64="GRIBAI4"
      export FORT35="GRIBFD5"
      export FORT55="GRIBFI5"
      export FORT45="GRIBAD5"
      export FORT65="GRIBAI5"
      export FORT36="GRIBFD6"
      export FORT56="GRIBFI6"
      export FORT46="GRIBAD6"
      export FORT66="GRIBAI6"
      export FORT37="GRIBFD7"
      export FORT57="GRIBFI7"
      export FORT47="GRIBAD7"
      export FORT67="GRIBAI7"
      export FORT38="GRIBFD8"
      export FORT58="GRIBFI8"
      export FORT48="GRIBAD8"
      export FORT68="GRIBAI8"
      export FORT39="GRIBFD9"
      export FORT59="GRIBFI9"
      export FORT49="GRIBAD9"
      export FORT69="GRIBAI9"
      export FORT40="GRIBFDL"
      export FORT60="GRIBFIL"
      export FORT50="GRIBADL"
      export FORT70="GRIBAIL"
      ##################
      #  output files  #
      #################
      export FORT71="$DATA_ANLVER_DATA1/xmslp"
      export FORT72="$DATA_ANLVER_DATA1/x850z"
      export FORT73="$DATA_ANLVER_DATA1/x850t"
      export FORT74="$DATA_ANLVER_DATA1/x500z"
      export FORT75="$DATA_ANLVER_DATA1/x500t"
      export FORT76="$DATA_ANLVER_DATA1/x250z"
      export FORT77="$DATA_ANLVER_DATA1/x250t"
      OUT2=$DATA_ANLVER_DATA1/xmslp
      OUT3=$DATA_ANLVER_DATA1/x850z
      OUT4=$DATA_ANLVER_DATA1/x850t
      OUT5=$DATA_ANLVER_DATA1/x500z
      OUT6=$DATA_ANLVER_DATA1/x500t
      OUT7=$DATA_ANLVER_DATA1/x250z
      OUT8=$DATA_ANLVER_DATA1/x250t
      export OUT2 OUT3 OUT4 OUT5 OUT6 OUT7 OUT8
      echo "EXECUTING"
      $EXECgfs_legacy_verif_v25/hrly_wmox  > $DATA_ANLVER_FCST1/xgfs.$CURTIME
      echo "MONTH = $MONTH"
      cat $OUT2 >> $DATA_ANLVER_DATA1/$MONTH.xmslp.in${ct}
      cat $OUT3 >> $DATA_ANLVER_DATA1/$MONTH.x850z.in${ct}
      cat $OUT4 >> $DATA_ANLVER_DATA1/$MONTH.x850t.in${ct}
      cat $OUT5 >> $DATA_ANLVER_DATA1/$MONTH.x500z.in${ct}
      cat $OUT6 >> $DATA_ANLVER_DATA1/$MONTH.x500t.in${ct}
      cat $OUT7 >> $DATA_ANLVER_DATA1/$MONTH.x250z.in${ct}
      cat $OUT8 >> $DATA_ANLVER_DATA1/$MONTH.x250t.in${ct}
      rm $OUT2 $OUT3 $OUT4 $OUT5 $OUT6 $OUT7 $OUT8
      rm fort* GRIB*
      echo "End of the WMOX calculations"
    fi
    #end of if for gmslzt stats
  done
done

###########################################################
# End of generating ANLVER - Forecast against Analyses    #
# End of SECTION 3.                                       #
###########################################################
