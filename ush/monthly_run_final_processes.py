import re
import sys
import os
import datetime
import pandas as pd
import numpy as np
import warnings
import glob

# Settings
warnings.filterwarnings('ignore')

# Process run time agruments
if len(sys.argv) != 3:
    print("ERROR: Not given correct number of run time agruments..."
          +os.path.basename(__file__)+" YYYY mm")
    sys.exit(1)
else:
    VDATE_YYYY = sys.argv[1]
    VDATE_mm = sys.argv[2]

# Read in environment variables
hostname = os.environ['HOSTNAME']
user = os.environ['USER']
DATA = os.environ['DATA']
COMINhistoricarch = os.environ['COMINhistoricarch']
COMINcurfv3gfs = os.environ['COMINcurfv3gfs']
COMINverf = os.environ['COMINverf']
COMINverm = os.environ['COMINverm']
SENDWEB = os.environ['SENDWEB']
if SENDWEB == 'YES':
    WEBHOST = os.environ['WEBHOST']
    WEBUSER = os.environ['WEBUSER']
    WEBDIR = os.environ['WEBDIR']

# Get month name
month_name = (datetime.datetime.strptime(VDATE_YYYY+VDATE_mm, '%Y%m')\
              .strftime('%B'))
if int(VDATE_mm) == 7 or int(VDATE_mm) == 9:
    month_abbrv = month_name.lower()[0:4]
else:
    month_abbrv = month_name.lower()[0:3]

# Set input/output directories
mon_year_full_archive_dir = os.path.join(COMINhistoricarch, 'full_archive',
                                         VDATE_YYYY+'_data',
                                         month_abbrv+'_'+VDATE_YYYY[-2:])

# Get dev machine
cactus_match = re.match(re.compile(r"^clogin[0-9]{2}$"), hostname)
dogwood_match = re.match(re.compile(r"^dlogin[0-9]{2}$"), hostname)
if cactus_match:
    dev_machine = 'dlogin01'
elif dogwood_match:
    dev_machine = 'clogin01'

# Archive verm directory to full archive
files_to_copy_dict = {
    'sumac4_'+VDATE_YYYY+'/input_data': [
        'qcadp'+VDATE_mm.zfill(2), 'dsum'+VDATE_mm.zfill(2)
    ],
    'sumac4_'+VDATE_YYYY+'/wmo_reports': ['wmxrpt'+VDATE_mm.zfill(2)],
    'qcmon_'+VDATE_YYYY: ['qcmrpt'+VDATE_mm.zfill(2)],
    'cqcht_'+VDATE_YYYY+'/cqcht_'+VDATE_mm.zfill(2)+'/cqcht_reports': [
        'us96cqcht.txt', 'stnlist.txt', 'baseline.txt', 'us96cqcht.stn.txt',
        'all96.txt', 'canada96.txt', 'bias_fnl.txt',
        'montable'+VDATE_mm.zfill(2)+VDATE_YYYY[-2:],
        'saf96.txt'
     ],
    'anlver_'+VDATE_YYYY+'/wmo_reports': ['s1GFS500']
}
for archive_dir in list(files_to_copy_dict.keys()):
    for archive_filename in files_to_copy_dict[archive_dir]:
        save_file_to_archive = os.path.join(mon_year_full_archive_dir,
                                            archive_dir, archive_filename)
        if not os.path.exists(
                os.path.join(mon_year_full_archive_dir, archive_dir)
        ):
            os.makedirs(os.path.join(mon_year_full_archive_dir, archive_dir))
        verm_file = os.path.join(COMINverm, archive_dir, archive_filename)
        if archive_filename == 'qcmrpt'+VDATE_mm.zfill(2):
            verm_file = os.path.join(COMINverm, archive_dir,
                                     'wmo_reports', archive_filename)
        else:
            verm_file = os.path.join(COMINverm, archive_dir, archive_filename)
        print("Copying "+verm_file+" to full archive as "+save_file_to_archive)
        os.system('cp '+verm_file+' '+save_file_to_archive)

# Clean up daily directories
date_and_after_to_save_str = VDATE_YYYY+VDATE_mm+'01'
date_and_after_to_save = datetime.datetime.strptime(date_and_after_to_save_str,
                                                    '%Y%m%d')
COMINcurfv3gfs_list = glob.glob(COMINcurfv3gfs+'/gfs.*')
for COMINcurfv3gfs in COMINcurfv3gfs_list:
    COMINcurfv3gfs_date = COMINcurfv3gfs.rpartition('gfs.')[2]
    if (datetime.datetime.strptime(COMINcurfv3gfs_date, '%Y%m%d')
            < date_and_after_to_save):
        print("Removing directory "+COMINcurfv3gfs)
        os.system('rm -rf '+COMINcurfv3gfs)
        os.system('ssh '+user+'@'+dev_machine
                  +'"rm -rf '+COMINcurfv3gfs+'"')
verd_dir_list = glob.glob(COMINverf+'/verd.*')
for verd_dir in verd_dir_list:
    verd_dir_date = verd_dir.rpartition('verd.')[2]
    if (datetime.datetime.strptime(verd_dir_date, '%Y%m%d')
            < date_and_after_to_save):
        print("Removing directory "+verd_dir)
        os.system('rm -rf '+verd_dir)
        os.system('ssh '+user+'@'+dev_machine
                  +' "rm -rf '+verd_dir+'"')

# Send files to web
if SENDWEB == 'YES':
    sumac4_wmxrpt_file = os.path.join(COMINverm, 'sumac4_'+VDATE_YYYY,
                                      'wmo_reports', 
                                      'wmxrpt'+VDATE_mm.zfill(2))
    sumac4_wmxrpt_web_dir = os.path.join(WEBDIR, 's1', 'gfs_data', VDATE_YYYY)
    os.system('    ssh -q -l '+WEBUSER+' '+WEBHOST
              +' "mkdir -p '+sumac4_wmxrpt_web_dir+' "')
    print("Sending "+sumac4_wmxrpt_file+" to "+sumac4_wmxrpt_web_dir)
    os.system('scp '
              +sumac4_wmxrpt_file+' '
              +WEBUSER+'@'+WEBHOST+':'+sumac4_wmxrpt_web_dir+'/.')
    qcmon_qcmrp_file = os.path.join(COMINverm, 'qcmon_'+VDATE_YYYY,
                                    'wmo_reports',
                                    'qcmrpt'+VDATE_mm.zfill(2))
    qcmon_qcmrp_web_dir =  os.path.join(WEBDIR, 's1', 'qcmon_data', VDATE_YYYY)
    os.system('    ssh -q -l '+WEBUSER+' '+WEBHOST
              +' "mkdir -p '+qcmon_qcmrp_web_dir+' "')
    print("Sending "+qcmon_qcmrp_file+" to "+qcmon_qcmrp_web_dir)
    os.system('scp '
              +qcmon_qcmrp_file+' '
              +WEBUSER+'@'+WEBHOST+':'+qcmon_qcmrp_web_dir+'/.')
