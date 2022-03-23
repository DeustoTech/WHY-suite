import pandas as pd
from datetime import datetime
from glob import glob

base_path = "/home/ubuntu/carlos.quesada/disk/umass_smart/source/apartment/*"

household_data = {}

for entry in sorted(glob(base_path)):
    year_path = entry + "/*.csv"
    for h_year_csv in sorted(glob(year_path)):
        apt_num = h_year_csv.split('/')[-1].split('_')[0]
        if apt_num not in household_data:
            household_data[apt_num] = []
        df = pd.read_csv(h_year_csv, header=None)
        for index,row in df.iterrows():
            date = row[0]
            kwh = float(row[1]) * (15/60)
            household_data[apt_num].append([date, kwh])

for household in household_data:
    df_user = pd.DataFrame(household_data[household])
    df_user.to_csv('../disk/umass_smart/raw/' + str(household) + '.csv', index=False, header=False, mode="a")