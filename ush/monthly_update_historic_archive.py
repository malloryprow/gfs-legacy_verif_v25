import os
import sys
import shutil
import datetime
import pandas as pd
import numpy as np
import warnings

# Process run time agruments
if len(sys.argv) != 3:
    print("ERROR: Not given correct number of run time agruments..."
          +os.path.basename(__file__)+" YYYY mm")
    sys.exit(1)
else:
    VDATE_YYYY = sys.argv[1]
    VDATE_mm = sys.argv[2]

# Read in environment variables
DATA = os.environ['DATA']
COMINverm_anlverYYYY_wmoreports = os.environ['COMINverm_anlverYYYY_wmoreports']
COMINhistoricarch = os.environ['COMINhistoricarch']

# Set and get files
s1GFS500_filename = os.path.join(
    COMINverm_anlverYYYY_wmoreports, 's1GFS500'
)
shutil.copy(
    os.path.join(COMINhistoricarch, 's1_historic_archive_monthly_rawdata.txt'),
    os.path.join(DATA, 's1_historic_archive_monthly_rawdata.txt')
)
monthly_rawdata_filename = os.path.join(
    DATA, 's1_historic_archive_monthly_rawdata.txt'
)

# Read s1GFS500 contents to find line header with our matching month and year
with open(s1GFS500_filename) as f:
    s1GFS500_data = f.readlines()
for line in range(len(s1GFS500_data)):
    if VDATE_mm+' '+VDATE_YYYY in s1GFS500_data[line]:
        break
# Now get the data for that month
# Note: After matching header line, the next 5 lines
#       are the matching data for this month for
#       various forecast hours and regions
#
#         line 1, index 0-8   = fhr12
#         line 1, index 9-17  = fhr24
#         line 2, index 0-8   = fhr36
#         line 2, index 9-17  = fhr48
#         line 3, index 0-8   = fhr60
#         line 3, index 9-17  = fhr72
#         line 4, index 0-8   = fhr84
#         line 4, index 9-17  = fhr96
#         line 5, index 0-8   = fhr108
#         line 5, index 9-17  = fhr120
#
#       The index in each forecast hour grouping
#       is for a particular regions (9 regions)
#
#         fhr group index 0 = GBL
#         fhr group index 1 = 59
#         fhr group index 2 = W33
#         fhr group index 3 = E33
#         fhr group index 4 = 49
#         fhr group index 5 = RGNAR1
#         fhr group index 6 = LL59
#         fhr group index 7 = LLW33
#         fhr group index 8 = LLE33
#
#       No idea what these even mean or what the regions
#       look like, but apparently the only region that matters
#       about is "49" or index 4
month_header_line = s1GFS500_data[line]
month_data_fhr12_reg49 = (
    list(filter(None, s1GFS500_data[line+1].split(' ')))[:9][4]
)
month_data_fhr24_reg49 = (
    list(filter(None, s1GFS500_data[line+1].split(' ')))[-9:][4]
)
month_data_fhr36_reg49 = (
    list(filter(None, s1GFS500_data[line+2].split(' ')))[:9][4]
)
month_data_fhr48_reg49 = (
    list(filter(None, s1GFS500_data[line+2].split(' ')))[-9:][4]
)
month_data_fhr60_reg49 = (
    list(filter(None, s1GFS500_data[line+3].split(' ')))[:9][4]
)
month_data_fhr72_reg49 = (
    list(filter(None, s1GFS500_data[line+3].split(' ')))[-9:][4]
)
month_data_fhr84_reg49 = (
    list(filter(None, s1GFS500_data[line+4].split(' ')))[:9][4]
)
month_data_fhr96_reg49 = (
    list(filter(None, s1GFS500_data[line+4].split(' ')))[-9:][4]
)
month_data_fhr108_reg49 = (
    list(filter(None, s1GFS500_data[line+5].split(' ')))[:9][4]
)
month_data_fhr120_reg49 = (
    list(filter(None, s1GFS500_data[line+5].split(' ')))[-9:][4]
)

# Only keep record of the values for forecast hours
# 36 and 72 and the files in the file are saved as
# raw_score*10^1
raw_score_fhr36_reg49 = str(round(int(month_data_fhr36_reg49) * 10**-1, 1))
raw_score_fhr72_reg49 = str(round(int(month_data_fhr72_reg49) * 10**-1, 1))

# Now write this to the monthly data archive file
print(VDATE_YYYY+VDATE_mm.zfill(2)+', '
      +raw_score_fhr36_reg49+', '
      +raw_score_fhr72_reg49+'\n')
with open(monthly_rawdata_filename, 'a') as monthly_rawdata_file:
    monthly_rawdata_file.write(VDATE_YYYY+VDATE_mm.zfill(2)+', '
                               +raw_score_fhr36_reg49+', '
                               +raw_score_fhr72_reg49+'\n')
monthly_rawdata_file.close()
