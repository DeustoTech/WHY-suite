import pandas as pd
from datetime import timedelta, datetime

half_our = timedelta(minutes=30)
month_to_number = {"JAN": 1, "FEB": 2, "MAR": 3, "APR": 4, "MAY": 5, "JUN": 6, "JUL": 7, "AUG": 8, "SEP":9 , "OCT": 10, "NOV": 11, "DEC": 12 }
#TO create small portions of the dataset
#df = pd.read_csv(r"../disk/edrp/raw/7591elec/csv/edrp_elec.csv", nrows=10000)
#df.to_csv("../disk/edrp/raw/7591elec/csv/mini_edrp_elec.csv", index=False, header=True, mode='w', chunksize=10000)

#df = pd.read_csv(r"../disk/edrp/raw/7591elec/csv/edrp_elec.csv")
#df = pd.read_csv(r"../disk/edrp/raw/7591elec/csv/mini_edrp_elec.csv")
#print(df.columns.tolist())
#tfr = pd.read_csv(r"../disk/edrp/raw/7591elec/csv/edrp_elec.csv", chunksize=5000, iterator=True)
#df = pd.concat(tfr, ignore_index=True)

df_iterator = pd.read_csv(r"../disk/edrp/source/7591elec/csv/edrp_elec.csv", chunksize=100000, iterator=True)

for i, df_chunk in enumerate(df_iterator):
    readings_per_user = {}
    for index, row in df_chunk.iterrows():
        if row['ANON_ID'] not in readings_per_user:
            readings_per_user[row['ANON_ID']] = []
            readings_per_user[row['ANON_ID']].append(row)
        else:
            readings_per_user[row['ANON_ID']].append(row)



    for user_id in readings_per_user:
        clean_readings_per_user = []
        for reading in readings_per_user[user_id]:
            #print(reading['ADVANCEDATETIME'])
            day = reading['ADVANCEDATETIME'][0:2]
            month = reading['ADVANCEDATETIME'][2:5]
            year = reading['ADVANCEDATETIME'][5:7]
            complete_hour = reading['ADVANCEDATETIME'].split(':')[1:4]
            reading_date = datetime(year=2000 + int(year), month=month_to_number[month], day=int(day), hour=int(complete_hour[0]), minute=int(complete_hour[1]), second=int(complete_hour[2]))
            #print(reading_date)
            kwh = reading['ELECKWH']
            clean_readings_per_user.append([reading_date, kwh])
        df = pd.DataFrame(clean_readings_per_user)
        df.to_csv('../disk/edrp/raw/' + str(user_id) + '.csv', index=False, header=False, mode="a")
