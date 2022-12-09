#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

#############################################################
# SECTION 2. VERIFICATION OF FORECAST AGAINST RADIOSONDE    #
#            This is known as the SUMAC4 portion which      #
#            uses the QCADP which was generated in          #
#            SECTION 1                                      #
#############################################################

###############################
# Looping on the Cycle time   #
###############################

for ct in 00 12
do

   ##############################################
   # Setting the current time/cycle and month   #
   ##############################################

   echo "ANALYSIS DATE-TIME IS $VDATE$ct"

   ########################################################
   # Verification begins.                                 #
   # Copy an executable objective file for SUMAC4,        #
   # station lists for verifying areas and verification   #
   # table into working directory.                        #
   ########################################################

   cp $FIXgfs_legacy_verif_v25/hrly_subarea.dic     . 
   export err1=$?
   cp $FIXgfs_legacy_verif_v25/hrly_verf_tab_$ct   . 
   export err2=$?
   tot=`expr $err1 + $err2`
   if [ $tot -ne 0 ]
   then
     err_chk
   fi

   ############################################################################
   # Produce a common namelist file for verifying forecasts from most models  #
   ############################################################################
cat <<\sumac1EOF >sm_namlst1
     &OPTION  RPT=2, VDATE1=1998071212, VDATE2=2999123112, /
     &OPTION  PRSTAT=.FALSE., WRSTAT=.TRUE., MINWND=0, /
     &OPTION  MAXWND=150, DIAG=.TRUE., INTPM=1, /
sumac1EOF

   icn_models=" gfs12 gfs24 ukmet jets "
   for nmodels in $icn_models
   do

     # start of if for gfs12
     #####################################################
     # Verifying GFS forecasts against radiosondes:      #
     #     Computing S1 scores for 12 hours intervals:   #
     #         000HR 012HR 024HR 036HR 048HR 060HR       #
     #         072HR 084HR 096HR 108HR and 120HR         #
     #####################################################
     if test "$nmodels" = 'gfs12'
     then
     ###########################
     # Produce namelist file   #
     ###########################
cat <<\sumac2EOF >sm_namlst2
       &FILES   RPT=0, NFILE=11, FHR='F000','F012','F024',
             'F036','F048','F060','F072','F084','F096',
             'F108','F120',KRUN='LRG2',
       /
sumac2EOF
     #################################
     # Copying the forecast fields   #
     #################################
            if [ $ct -eq 00 ]
            then
              cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbf000 F000DAT
              cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbif000 F000NDX
              n=0
            else
              cp $COMINverd_grib/ver.gfs.t12z.pgrbf000 F000DAT
              cp $COMINverd_grib/ver.gfs.t12z.pgrbif000 F000NDX
              cp $COMINverd_grib/ver.gfs.t00z.pgrbf012 F012DAT
              cp $COMINverd_grib/ver.gfs.t00z.pgrbif012 F012NDX
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
              fcst=`expr $n '*' 12`
              hr=`expr $n '*' 12`
              if [ $hr -lt 100 ]
              then
                hr="0"$hr
                fcst="0"$fcst
              fi
              if [ $fcst -lt 132 ]
              then
                cp $COMINverf/verd.$d1/grib/ver.gfs.t12z.pgrbf$hr F${fcst}DAT
                cp $COMINverf/verd.$d1/grib/ver.gfs.t12z.pgrbif$hr F${fcst}NDX
              fi
              n=`expr $n + 1`
              fcst=`expr $n '*' 12`
              hr=`expr $n '*' 12`
              if [ $hr -lt 100 ]
              then
                hr="0"$hr
                fcst="0"$fcst
              fi
              if [ $fcst -lt 132 ]
              then
                cp $COMINverf/verd.$d1/grib/ver.gfs.t00z.pgrbf$hr F${fcst}DAT
                cp $COMINverf/verd.$d1/grib/ver.gfs.t00z.pgrbif$hr F${fcst}NDX
              fi
            done
     ################################################
     # Verifying GFS forecast against radiosondes   #
     ################################################
            #pgm=hrly_sumac4
            #export pgm;. prep_step
            export FORT7="hrly_subarea.dic"
            export FORT8="qcadp.$ct"
            export FORT9="sm_namlst1"
            export FORT10="hrly_verf_tab_$ct"
            export FORT11="F000DAT"
            export FORT12="F000NDX"
            export FORT13="F012DAT"
            export FORT14="F012NDX"
            export FORT15="F024DAT"
            export FORT16="F024NDX"
            export FORT17="F036DAT"
            export FORT18="F036NDX"
            export FORT19="F048DAT"
            export FORT20="F048NDX"
            export FORT21="F060DAT"
            export FORT22="F060NDX"
            export FORT23="F072DAT"
            export FORT24="F072NDX"
            export FORT70="jetstats"
            export FORT71="dlystats"
            $EXECgfs_legacy_verif_v25/hrly_sumac4 <sm_namlst2 1>sumac4_prt 2>sumac4_err
            export err=$?; err_chk
     ###########################################
     # Append GFS printout and output files    #
     ###########################################
            if [ $ct -eq 00 ]
            then
              ID="02"
            elif [ $ct -eq 12 ]
            then
              ID="06"
            fi
            cp dlystats $DATA/dlystat_dat$VDATE_mm$VDATE_dd$ID
            #export err=$?; err_chk
     fi
     #end of if for gfs12

     #start of if for gfs24
     #####################################################
     # Verifying GFS forecasts against radiosondes:      #
     #     Computing S1 scores for 24 hours intervals:   #
     #         000HR 024HR 048HR 072HR 096HR 120HR       #
     #         144HR 168HR 192HR 216HR and 240HR         #
     #####################################################
     if test "$nmodels" = 'gfs24'
     then
     ###########################
     # Produce namelist file   #
     ###########################
cat <<\sumac2EOF >sm_namlst2
       &FILES   RPT=0, NFILE=11, FHR='F000','F024','F048',
             'F072','F096','F120','F144','F168','F192',
             'F216','F240',KRUN='LRG ',
       /
sumac2EOF
     ##################################
     # Copying the forecasts fields   #
     ##################################
       cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbf000 F000DAT
       cp $COMINverd_grib/ver.gfs.t${ct}z.pgrbif000 F000NDX
       n=0
     ###############################################
     # Loop which takes the time out to 240hr      #
     # n is define as the number of day(s) back    #
     ###############################################
       for thr in 1 2 3 4 5 6 7 8 9 10
       do
         ndate=`finddate.sh $VDATE d-$thr`
         d1=`echo $ndate | cut -c1-8`
         n=`expr $n + 1`
         fcst=`expr $n '*' 24`
         hr=`expr $n '*' 24`
         if [ $hr -lt 100 ]
         then
           hr="0"$hr
           fcst="0"$fcst
         fi
         cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbf$hr F${fcst}DAT
         cp $COMINverf/verd.$d1/grib/ver.gfs.t${ct}z.pgrbif$hr F${fcst}NDX
       done
     ################################################
     # Verifying GFS forecast against radiosondes   #
     ################################################
       #pgm=hrly_sumac4
       #export pgm;. prep_step
       export FORT7="hrly_subarea.dic"
       export FORT8="qcadp.$ct"
       export FORT9="sm_namlst1"
       export FORT10="hrly_verf_tab_$ct"
       export FORT11="F000DAT"
       export FORT12="F000NDX"
       export FORT13="F024DAT"
       export FORT14="F024NDX"
       export FORT15="F048DAT"
       export FORT16="F048NDX"
       export FORT17="F072DAT"
       export FORT18="F072NDX"
       export FORT19="F096DAT"
       export FORT20="F096NDX"
       export FORT21="F120DAT"
       export FORT22="F120NDX"
       export FORT23="F144DAT"
       export FORT24="F144NDX"
       export FORT25="F168DAT"
       export FORT26="F168NDX"
       export FORT27="F192DAT"
       export FORT28="F192NDX"
       export FORT29="F216DAT"
       export FORT30="F216NDX"
       export FORT31="F240DAT"
       export FORT32="F240NDX"
       export FORT70="jetstats"
       export FORT71="dlystats"
       $EXECgfs_legacy_verif_v25/hrly_sumac4 <sm_namlst2 1>sumac4_prt 2>sumac4_err
       #export err=$?; err_chk
       if [ $ct -eq 00 ]
       then
         ID="10"
       elif [ $ct -eq 12 ]
       then
         ID="14"
       fi
       cp dlystats $DATA/dlystat_dat$VDATE_mm$VDATE_dd$ID
       #export err=$?; err_chk
     fi
     #end of if for gfs24

     #start of if for ukmet
     ############################################################
     # Verifying UKM forecasts against radiosondes:             #
     # Computing S1 Scores for UKMET for 12 hours intervals:    #
     #    000HR 012HR 024HR 036HR 048HR 060HR and 072HR         #
     ############################################################
     if test "$nmodels" = 'ukmet'
     then
     ###########################
     # Produce namelist file   #
     ###########################
cat <<\sumac2EOF >sm_namlst2
       &FILES   RPT=0, NFILE=7, FHR='F000','F012','F024',
             'F036','F048','F060','F072',KRUN='UKM ',
       /
sumac2EOF
     ##################################
     # Copying the forecasts fields   #
     ##################################
       if [ $ct -eq 00 ]
       then
         cp $COMINukm/ukmet.$VDATE/ukmet.t00z.ukmet00.grib2 ukm1
         cnvgrib -g21 ukm1 F000DAT
         export err=$?
         grbindex F000DAT F000NDX
         export err=$?
         n=0
       else
         cp $COMINukm/ukmet.$VDATE/ukmet.t12z.ukmet00.grib2 ukm2
         cnvgrib -g21 ukm2 F000DAT
         export err=$?
         grbindex F000DAT F000NDX
         export err=$?
         cp $COMINukm/ukmet.$VDATE/ukmet.t00z.ukmet12.grib2 ukm3
         cnvgrib -g21 ukm3 F012DAT
         export err=$?
         grbindex F012DAT F012NDX
         export err=$?
         n=1
       fi
       ###############################################
       # Loop which takes the time out to 72hr       #
       # n is define as the number of day(s) back    #
       ###############################################
       for thr in 1 2 3 4
       do
         ndate=`finddate.sh $VDATE d-$thr`
         d1=`echo $ndate`
         n=`expr $n + 1`
         fcst=`expr $n '*' 12`
         hr=`expr $n '*' 12`
         if [ $hr -lt 100 ]
         then
           fcst="0"$fcst
         fi
         if [ $fcst -lt 84 ]
         then
           cp $COMINukm/ukmet.${d1}/ukmet.t12z.ukmet$hr.grib2 ukm${fcst}
           cnvgrib -g21 ukm${fcst} F${fcst}DAT
           export err=$?
           grbindex F${fcst}DAT F${fcst}NDX
           export err=$?
         fi
         n=`expr $n + 1`
         fcst=`expr $n '*' 12`
         hr=`expr $n '*' 12`
         if [ $hr -lt 100 ]
         then
           fcst="0"$fcst
         fi
         if [ $fcst -lt 84 ]
         then
           cp $COMINukm/ukmet.${d1}/ukmet.t00z.ukmet$hr.grib2 ukma${fcst}
           cnvgrib -g21 ukma${fcst} F${fcst}DAT
           export err=$?
           grbindex F${fcst}DAT F${fcst}NDX
           export err=$?
         fi
       done
     ################################################
     # Verifying UKMET forecast against radiosondes #
     ################################################
       #pgm=hrly_sumac4
       #export pgm;. prep_step
       export FORT7="hrly_subarea.dic"
       export FORT8="qcadp.$ct"
       export FORT9="sm_namlst1"
       export FORT10="hrly_verf_tab_$ct"
       export FORT11="F000DAT"
       export FORT12="F000NDX"
       export FORT13="F012DAT"
       export FORT14="F012NDX"
       export FORT15="F024DAT"
       export FORT16="F024NDX"
       export FORT17="F036DAT"
       export FORT18="F036NDX"
       export FORT19="F048DAT"
       export FORT20="F048NDX"
       export FORT21="F060DAT"
       export FORT22="F060NDX"
       export FORT23="F072DAT"
       export FORT24="F072NDX"
       export FORT70=jetstats
       export FORT71=dlystats
       $EXECgfs_legacy_verif_v25/hrly_sumac4 <sm_namlst2 1>sumac4_prt
       #export err=$?; err_chk
     ###########################################
     # Append UKM printout and output files    #
     ###########################################
       if [ $ct -eq 00 ]
       then
         ID="26"
       elif [ $ct -eq 12 ]
       then
         ID="30"
       fi
       cp dlystats $DATA/dlystat_dat$VDATE_mm$VDATE_dd$ID
       export err=$?; err_chk
     fi
     #end of if for ukmet

     #start of if for jets
     ########################################################
     #  VERIFYING 24 AND 36 HOURS JET FORECASTS FROM GFS    #
     #  AND UKM WITH MINWND=41 m/s AND MINWND=61 m/s        #
     ########################################################
     if test "$nmodels" = 'jets'
     then
       cdate=$VDATE_mm$VDATE_dd
       ndatem1=`finddate.sh $VDATE d-1`
       VDATEm1=`echo $ndatem1`
       ndatem2=`finddate.sh $VDATE d-2`
       VDATEm2=`echo $ndatem2`
       #############################
       #   Produce namelist file   #
       #############################
       i=1
       while [ $i -le 2 ]
       do
         if [ $i -eq 1 ]
         then
cat <<\sumac1EOF >sm_namlst1
           &OPTION  RPT=0, VDATE1=1991010100, VDATE2=9999123112,
             PRSTAT=.FALSE., WRSTAT=.FALSE., MINWND=41,
             MAXWND=150,  DIAG=.FALSE., INTPM=1,
           /
sumac1EOF
         elif [ $i -eq 2 ]
         then
cat <<\sumac1EOF >sm_namlst1
           &OPTION  RPT=0, VDATE1=1991010100, VDATE2=9999123112,
             PRSTAT=.FALSE., WRSTAT=.FALSE., MINWND=61,
             MAXWND=150,  DIAG=.FALSE., INTPM=1,
           /
sumac1EOF
         fi
         #pgm=hrly_sumac4
         #export pgm;. prep_step
         icn_jet=" gfs ukm "
         for nfile in $icn_jet
         do
           if test "$nfile" = 'gfs'
           then
             if [ $ct -eq 00 ]
             then
               cp $COMINverf/verd.$VDATEm1/grib/ver.gfs.t00z.pgrbf024 F024DAT
               cp $COMINverf/verd.$VDATEm1/grib/ver.gfs.t00z.pgrbif024 F024NDX
               cp $COMINverf/verd.$VDATEm2/grib/ver.gfs.t12z.pgrbf036 F036DAT
               cp $COMINverf/verd.$VDATEm2/grib/ver.gfs.t12z.pgrbif036 F036NDX
               IDS=$nfile$cdate"01"
cat <<\sumac2EOF >sm_namlst2
               &FILES   RPT=0, NFILE=2, FHR='F024', 'F036',KRUN='GFS1',
               /
sumac2EOF
             elif [ $ct -eq 12 ]
             then
               cp $COMINverf/verd.$VDATEm1/grib/ver.gfs.t12z.pgrbf024 F024DAT
               cp $COMINverf/verd.$VDATEm1/grib/ver.gfs.t12z.pgrbif024 F024NDX
               cp $COMINverf/verd.$VDATEm1/grib/ver.gfs.t00z.pgrbf036 F036DAT
               cp $COMINverf/verd.$VDATEm1/grib/ver.gfs.t00z.pgrbif036 F036NDX
               IDS=$nfile$cdate"05"
cat <<\sumac2EOF >sm_namlst2
               &FILES   RPT=0, NFILE=2, FHR='F024', 'F036',KRUN='GFS2',
               /
sumac2EOF
             fi
           elif test "$nfile" = 'ukm'
           then
             if [ $ct -eq 00 ]
             then
               cp $COMINukm/ukmet.$VDATEm1/ukmet.t00z.ukmet24.grib2 ukmc
               cnvgrib -g21 ukmc F024DAT
               export err=$?
               grbindex F024DAT F024NDX
               export err=$?
               cp $COMINukm/ukmet.$VDATEm2/ukmet.t12z.ukmet36.grib2 ukmb
               cnvgrib -g21 ukmb F036DAT
               export err=$?
               grbindex F036DAT F036NDX
               export err=$?
               IDS=$nfile$cdate"09"
cat <<\sumac2EOF >sm_namlst2
               &FILES   RPT=0, NFILE=2, FHR='F024', 'F036',KRUN='UKM1',
               /
sumac2EOF
             elif [ $ct -eq 12 ]
             then
               cp $COMINukm/ukmet.$VDATEm1/ukmet.t12z.ukmet24.grib2  ukmc
               cnvgrib -g21 ukmc F024DAT
               export err=$?
               grbindex F024DAT F024NDX
               export err=$?
               cp $COMINukm/ukmet.$VDATEm1/ukmet.t00z.ukmet36.grib2  ukmd
               cnvgrib -g21 ukmd F036DAT
               export err=$?
               grbindex F036DAT F036NDX
               export err=$?
               IDS=$nfile$cdate"13"
cat <<\sumac2EOF >sm_namlst2
               &FILES   RPT=0, NFILE=2, FHR='F024', 'F036',KRUN='UKM2',
               /
sumac2EOF
             fi
           fi
           export FORT7="hrly_subarea.dic"
           export FORT8="qcadp.$ct"
           export FORT9="sm_namlst1"
           export FORT10="hrly_verf_tab_$ct"
           export FORT11="F024DAT"
           export FORT12="F024NDX"
           export FORT13="F036DAT"
           export FORT14="F036NDX"
           export FORT70="jetstats"
           export FORT71="dlystats"
           $EXECgfs_legacy_verif_v25/hrly_sumac4 <sm_namlst2 1>sumac4_prt 2>sumac4_err
           #export err=$?; err_chk
     ###########################################
     # Append GFS printout and output files    #
     ###########################################
           ID=$IDS
           cat jetstats >> $DATA/$IDS
           export err=$?; err_chk
         done
         i=`expr $i + 1 `
       done
     fi
     #end of if for jets
   done
done

##############################################
# Joining the daily statistics to be used    #
# for the monthly averages                   #
##############################################
########################################
#  for winds 850, 500 and 250          #
#   GFS jets - 01 03 05 07             #
# UKMET jets - 09 11 13 15             #
#                                      #
# Now for the models being used        #
#   cyc   00 06 12 18                  #
# GFS12 - 02 04 06 08                  #
# GFS24 - 10 12 14 16                  #
#   NAM - 18 20 22 24                  #
# UKMET - 26 28 30 32                  #
########################################

##############
# 00Z cycle  #
##############
cat dlystat*02 >> $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd}
cat dlystat*10 >> $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd}
cat dlystat*26 >> $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd}

##############
# 12Z cycle  #
##############
cat dlystat*06 >> $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd}
cat dlystat*14 >> $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd}
cat dlystat*30 >> $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd}

###############
#  minwnd=41  #
###############
cat gfs*01 >> $DATA_SUMAC41/accumjetstat${VDATE_yy}${VDATE_mm}.${VDATE_dd}
cat ukm*09 >> $DATA_SUMAC41/accumjetstat${VDATE_yy}${VDATE_mm}.${VDATE_dd}

##############
#  minwnd=61 #
##############
cat gfs*05 >> $DATA_SUMAC41/accumjetstat${VDATE_yy}${VDATE_mm}.${VDATE_dd}
cat ukm*13 >> $DATA_SUMAC41/accumjetstat${VDATE_yy}${VDATE_mm}.${VDATE_dd}

###########################################################
# End of generating SUMAC4 - Forecast against Radiosonde  #
# End of SECTION 2.                                       #
###########################################################
