#!/bin/bash
###############################################################################
# Name of Script: ex_gfs_legacy_verif_v25_yearly.sh 
# Purpose of Script: This script generates yearly verification
# Log history:
###############################################################################

set -x

# Update Yearly Archive
echo "------------------------------------------------"
echo "  Yearly Verification - Update Historic Archive"
echo "------------------------------------------------"
cd $DATA
python ${USHgfs_legacy_verif_v25}/yearly_update_historic_archive.py ${VDATE_YYYY}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA/s1_historic_archive_yearly_data.txt $COMOUThistoricarch
fi

# Create yearly S1 Graphic
echo "------------------------------------------------"
echo "  Yearly Verification - S1 Graphic"
echo "------------------------------------------------"
cd $DATA
python ${USHgfs_legacy_verif_v25}/yearly_plot_s1_scores.py ${VDATE_YYYY}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA/historic_s1_yearly_scores.png $COMOUThistoricarch
    cp -pv $DATA/historic_s1_yearly_scores.png $COMOUThistoricarch/historic_s1_yearly_scores_${VDATE_YYYY}.png
fi
