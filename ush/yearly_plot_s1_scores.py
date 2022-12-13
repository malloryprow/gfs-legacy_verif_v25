import os
import sys
import pandas as pd
import numpy as np
import datetime
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
import matplotlib.dates as md
import warnings

# Settings
warnings.filterwarnings('ignore')
plt.rcParams['font.weight'] = 'bold'
plt.rcParams['axes.labelsize'] = 15
plt.rcParams['axes.labelweight'] = 'bold'
plt.rcParams['xtick.labelsize'] = 15
plt.rcParams['ytick.labelsize'] = 15
plt.rcParams['axes.titlesize'] = 15
plt.rcParams['axes.titleweight'] = 'bold'
plt.rcParams['axes.formatter.useoffset'] = False
plt.rcParams['figure.subplot.left'] = 0.055
plt.rcParams['figure.subplot.right'] = 0.975
plt.rcParams['figure.subplot.top'] = 0.775
plt.rcParams['figure.subplot.bottom'] = 0.075

# Process run time agruments
if len(sys.argv) != 2:
    print("ERROR: Not given correct number of run time agruments..."
          +os.path.basename(__file__)+" YYYY")
    sys.exit(1)
else:
    VDATE_YYYY = sys.argv[1]

# Read in environment variables
DATA = os.environ['DATA']
FIXgfs_legacy_verif_v25 = os.environ['FIXgfs_legacy_verif_v25']
COMINhistoricarch = os.environ['COMINhistoricarch']
SENDWEB = os.environ['SENDWEB']
if SENDWEB == 'YES':
    WEBHOST = os.environ['WEBHOST']
    WEBUSER = os.environ['WEBUSER']
    WEBDIR = os.environ['WEBDIR']

# Set dates
PDY = datetime.datetime.strptime(os.environ['PDY'], '%Y%m%d')
PDY_year = str(PDY.year)
if int(PDY.year) == int(VDATE_YYYY):
    print("Trying to run yearly verification though year isn't done, exit")
    sys.exit()
year_str = str(VDATE_YYYY)

# Logos
noaa_logo_img_array = matplotlib.image.imread(
    os.path.join(FIXgfs_legacy_verif_v25, 'noaa.png')
)
doc_logo_img_array = matplotlib.image.imread(
    os.path.join(FIXgfs_legacy_verif_v25, 'doc.png')
)

# HPC History
emc_hpc_history_file = os.path.join(FIXgfs_legacy_verif_v25,
                                   'emc_hpc_history.txt')
emc_hpc_history = pd.read_csv(emc_hpc_history_file,
                              sep=', ',
                              header=None,
                              names=['Computer', 'Year'])

# S1 Data
s1_yearly_file = os.path.join(COMINhistoricarch,
                              's1_historic_archive_yearly_data.txt')
s1_data = pd.read_csv(s1_yearly_file,
                      sep=', ',
                      index_col=0)
years = s1_data.index.values
fhr36 = s1_data['fhr36']
fhr72 = s1_data['fhr72']
fhr72.iloc[np.where(fhr72 == '--')[0]] = np.nan
fhr72 = np.asarray(fhr72, dtype='float')
# S1 Data from Excel
#s1_xlsx_file = os.path.join(s1_dir,
#                            'historic_archive',
#                            'full_archive',
#                            'S1_Scores_1955_2018.xlsx')
#s1_xlsx = pd.ExcelFile(s1_xlsx_file)
#s1_data = pd.read_excel(s1_xlsx, 'chart data',
#                        usecols="A:B,D:E",
#                        skiprows=1,
#                        index_col=0)
#years = s1_data.index
#emp_skill = s1_data['Empirical Skill']
#avn36 = s1_data['AVN36']
#avn72 = s1_data['AVN72']

# Plot
fig, ax = plt.subplots(1,1,figsize=(16,8), dpi=250)
ax.grid(True)
ax.tick_params(axis='x')
ax.set_xticks(np.arange(1970,2031,1), minor=True)
ax.set_xlabel('Year')
ax.set_xlim([1970, 2030])
ax.tick_params(axis='y')
ax.set_yticks(np.arange(0,101,10))
ax.set_ylabel('Ratio of Gradients (Forecast - Observed)')
ax.set_ylim([0,100])
ax.plot(years, fhr36, color='blue', linewidth=2,
        marker='o', markersize=7, label='36 Hour Forecast')
ax.plot(years, fhr72, color='red', linewidth=2,
        marker='o', markersize=7, label='72 Hour Forecast')
ax.legend(bbox_to_anchor=(0.2, 1.075, 0.6, 0.0), loc='center',
          ncol=2, fontsize='16', mode='expand', borderaxespad=0.)
plt.suptitle('NCEP Operational Forecast Skill', fontsize=20, y=0.98,
             fontweight='bold')
ax.annotate('36 and 72 Hour Height Forecasts @ 500 MB over North America\n'
            +'[100 * (1-S1/70) Method]',
            fontsize=16, va='center', ha='center',
            xy=(0.5, 1.1825), xycoords='axes fraction')
fig.figimage(noaa_logo_img_array,
             ax.bbox.xmax-300, fig.bbox.ymax-325,
             zorder=1)
fig.figimage(doc_logo_img_array,
             ax.bbox.xmin, fig.bbox.ymax-325,
             zorder=1)
idx = 0
while idx < len(emc_hpc_history):
    computer = emc_hpc_history.iloc[idx]['Computer'].replace(' ', '\n')
    year = emc_hpc_history.iloc[idx]['Year']
    ax.annotate(computer, xy=(year, 1), xytext=(year, 10), xycoords='data',
                fontsize=10, ha='center', va='center',
                arrowprops=dict(arrowstyle='->')) 
    idx+=1
print("Saving image as "+os.path.join(DATA, 'historic_s1_yearly_scores.png'))
plt.savefig(os.path.join(DATA, 'historic_s1_yearly_scores.png'), dpi=250)
plt.close()

# Send files to web
if SENDWEB == 'YES':
    yearly_s1_file = os.path.join(DATA, 'historic_s1_yearly_scores.png')
    yearly_s1_web_dir = os.path.join(WEBDIR, 's1', 'gfs_data', 'annual_plots')
    os.system('    ssh -q -l '+WEBUSER+' '+WEBHOST
              +' "mkdir -p '+yearly_s1_web_dir+' "')
    print("Sending "+yearly_s1_file+" to "+yearly_s1_web_dir)
    os.system('scp '
              +yearly_s1_file+' '
              +WEBUSER+'@'+WEBHOST+':'+yearly_s1_web_dir+'/.')
