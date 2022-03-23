import pandas as pd
from datetime import datetime, timedelta

file_path = "/home/ubuntu/carlos.quesada/disk/sgsc/source/CD_INTERVAL_READING_ALL_NO_QUOTES.csv"

df_iterator = pd.read_csv(file_path, chunksize=100000, iterator=True)

for i, df_chunk in enumerate(df_iterator):
    readings_per_user = {}
    """for col in df_chunk.columns:
        print(col)"""
    for index, row in df_chunk.iterrows():
        if row['CUSTOMER_ID'] not in readings_per_user:
            readings_per_user[row['CUSTOMER_ID']] = []
            readings_per_user[row['CUSTOMER_ID']].append(row)
        else:
            readings_per_user[row['CUSTOMER_ID']].append(row)
    for user_id in readings_per_user:
        clean_readings_per_user = []
        for reading in readings_per_user[user_id]:
            reading_date = reading['READING_DATETIME']
            kwh = reading[' GENERAL_SUPPLY_KWH'] + reading[' CONTROLLED_LOAD_KWH']
            clean_readings_per_user.append([reading_date, kwh])
        df = pd.DataFrame(clean_readings_per_user)
        df.to_csv('../disk/sgsc/raw/' + str(user_id) + '.csv', index=False, header=False, mode="a")