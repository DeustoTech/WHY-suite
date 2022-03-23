import pandas as pd
from datetime import timedelta, datetime
from glob import glob

src_path = "/home/ubuntu/carlos.quesada/disk/save/source/csv/raw/*.csv"

for entry in sorted(glob(src_path)):
    print(entry)
    unique_households = {}
    df = pd.read_csv(entry)
    for index, row in df.iterrows():
        if row['bmg_id'] not in unique_households:
            unique_households[row['bmg_id']] = []
        unique_households[row['bmg_id']].append(row)

    for user_id in unique_households:
        clean_readings_per_user = []    
        i = 0
        while i + 1  < len(unique_households[user_id]):
            energy_difference = (unique_households[user_id][i+1]['energy'] - unique_households[user_id][i]['energy'])/float(1000)
            date = datetime.utcfromtimestamp(unique_households[user_id][i+1]['recorded_timestamp']) 
            clean_readings_per_user.append([date, energy_difference])
            i=i+1
        df_user = pd.DataFrame(clean_readings_per_user)
        df_user.to_csv('../disk/save/raw/' + str(user_id) + '.csv', index=False, header=False, mode="a")
    #print("Number of households is " + str(len(unique_households)))