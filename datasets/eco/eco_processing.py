import pandas as pd
from datetime import datetime, timedelta
from glob import glob

base_path = "/home/ubuntu/carlos.quesada/disk/eco/source/datasets/*"

for household_path in sorted(glob(base_path)):
    household_number = household_path.split('/')[-1]
    household_cleaned_data = []
    for data_csv in sorted(glob(household_path + "/*.csv")):
        df = pd.read_csv(data_csv, header=None)
        date = data_csv.split('/')[-1].split('.csv')[0].split('-')
        date = datetime(year=int(date[0]), month=int(date[1]), day=int(date[2]), hour=0, minute=0)
        for index,row in df.iterrows():
            index_date = date + timedelta(seconds=index) 
            #Don't know for sure if thi is really W per second.
            w = row[0]
            #1 per second
            kwh = (w/1000)*(1/3600)
            household_cleaned_data.append([index_date, kwh])
    df_household = pd.DataFrame(household_cleaned_data)
    df_household.to_csv('../disk/eco/raw/' + str(household_number) + '.csv', index=False, header=False, mode="w")
