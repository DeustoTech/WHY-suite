import pandas as pd
from datetime import timedelta, datetime
from glob import glob

src_path = "/home/ubuntu/carlos.quesada/disk/les/source/Loughborough Energy Study/UKDA-6583-excel/excel/meter_data/"

two_years = glob(src_path + '2008/*.csv') + glob(src_path + '2009/*.csv')
unique_households = {}
for entry in sorted(two_years):
    df = pd.read_csv(entry)
    household_id = entry.split('/')[-1].split('_')[1]
    if household_id not in unique_households:
        unique_households[household_id] = []

    for index, row in df.iterrows():
        #2008/01/04 18:55
        raw_datetime = row['DATETIME_LOCAL']
        date = raw_datetime.split(' ')[0].split('/')
        time = raw_datetime.split(' ')[1].split(':')
        reading_date = datetime(year=int(date[0]), month=int(date[1]), day=int(date[2]), hour=int(time[0]), minute=int(time[1]))
        unique_households[household_id].append([reading_date, row['IMPORT_KW']*(1/60)])

for household_id in unique_households:
    df_user = pd.DataFrame(unique_households[household_id])
    df_user.to_csv('../disk/les/raw/' + str(household_id) + '.csv', index=False, header=False, mode="a")