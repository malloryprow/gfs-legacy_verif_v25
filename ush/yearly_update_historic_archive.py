import os
import sys
import datetime
import shutil
import pandas as pd
import numpy as np
import warnings

# Settings
warnings.filterwarnings('ignore')

# Process run time agruments
if len(sys.argv) != 2:
    print("ERROR: Not given correct number of run time agruments..."
          +os.path.basename(__file__)+" YYYY")
    sys.exit(1)
else:
    VDATE_YYYY = sys.argv[1]

# Read in environment variables
DATA = os.environ['DATA']
COMINhistoricarch = os.environ['COMINhistoricarch']

# Set dates
PDY = datetime.datetime.strptime(os.environ['PDY'], '%Y%m%d')
PDY_year = str(PDY.year)
if int(PDY.year) == int(VDATE_YYYY):
    print("Trying to run yearly verification though year isn't done, exit")
    sys.exit()
year_str = str(VDATE_YYYY)

# Set and get files
monthly_rawdata_filename = os.path.join(
    COMINhistoricarch, 's1_historic_archive_monthly_rawdata.txt'
)
shutil.copy(
    os.path.join(COMINhistoricarch, 's1_historic_archive_yearly_data.txt'),
    os.path.join(DATA, 's1_historic_archive_yearly_data.txt')
)
yearly_data_filename = os.path.join(
    DATA, 's1_historic_archive_yearly_data.txt'
)

# Read the monthly raw data
monthly_raw_data = pd.read_csv(monthly_rawdata_filename,
                               sep=', ',
                               index_col=0)
yearmonth = monthly_raw_data.index
str01 = year_str+'01'
str12 = year_str+'12'
yearmon01 = float(str01)
yearmon12 = float(str12)
idx_yearmon01 = monthly_raw_data.index.get_loc(yearmon01)
idx_yearmon12 = monthly_raw_data.index.get_loc(yearmon12)
fhr36_data = monthly_raw_data['fhr36']
fhr36_year_subset = np.array(
    fhr36_data.iloc[idx_yearmon01:idx_yearmon12+1],
    dtype='float'
)
fhr36_year_mean = round(fhr36_year_subset.mean(), 2)
fhr36_score = str(round(100 * (1-fhr36_year_mean/70), 2))
fhr72_data = monthly_raw_data['fhr72']
fhr72_year_subset = np.array(
    fhr72_data.iloc[idx_yearmon01:idx_yearmon12+1],
    dtype='float'
)
fhr72_year_mean = round(fhr72_year_subset.mean(), 2)
fhr72_score = str(round(100 * (1-fhr72_year_mean/70), 2))

# Now write this to the yearly data archive file
print(year_str+', '
      +round(fhr36_score, 2)+', '
      +round(fhr72_score, 2)+'\n')
with open(yearly_data_filename, 'a') as yearly_data_file:
    yearly_data_file.write(year_str+', '
                           +round(fhr36_score, 2)+', '
                           +round(fhr72_score, 2)+'\n')
yearly_data_file.close()
