# gfs-legacy_verif_v25

This is legacy verification code for the GFS that spans back to the 1970s. It is set up to run on WCOSS2.

Set up:
1. Change directories to "gfs-legacy_verif_v25/sorc" (cd gfs-legacy_verif_v25/sorc)
2. Execute the script "build.src" (./build.src); this compiles the Fortran codes

How To Run Daily Verificaton:
1. Change directories to "gfs-legacy_verif_v25/ecf" (cd gfs-legacy_verif_v25/ecf)
2. Open "jgfs_legacy_verif_v25_daily.ecf", and change any configuration settings (lines 41-50), but it is set up to run out of the box (vi jgfs_legacy_verif_v25_daily.ecf)
3. Submit jgfs_legacy_verif_v25_daily.ecf (qsub jgfs_legacy_verif_v25_daily.ecf)

How To Run Monthly Verificaton:
1. Change directories to "gfs-legacy_verif_v25/ecf" (cd gfs-legacy_verif_v25/ecf)
2. Open "jgfs_legacy_verif_v25_monthly.ecf", and change any configuration settings (lines 41-54), but it is set up to run out of the box (vi jgfs_legacy_verif_v25_monthly.ecf)
3. Submit jgfs_legacy_verif_v25_monthly.ecf (qsub jgfs_legacy_verif_v25_monthly.ecf)

How To Run Yearly Verificaton:
1. Change directories to "gfs-legacy_verif_v25/ecf" (cd gfs-legacy_verif_v25/ecf)
2. Open "jgfs_legacy_verif_v25_yearly.ecf", and change any configuration settings (lines 41-53), but it is set up to run out of the box (vi jgfs_legacy_verif_v25_yearly.ecf)
3. Submit jgfs_legacy_verif_v25_yearly.ecf (qsub jgfs_legacy_verif_v25_yearly.ecf)
