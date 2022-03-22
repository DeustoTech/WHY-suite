import pandas as pd
from datetime import timedelta, datetime
from glob import glob
src_path = "/home/ubuntu/carlos.quesada/disk/nesemp/source/tab/csv/derived_data/*.csv"

for entry in glob(src_path):
    df = pd.read_csv(entry, header=None)
    filename = entry.split("/")[-1]
    print(filename)
    readings = []
    for index,row in df.iterrows():
        date = str(row[0])
        year = date[0:4]
        month = date[4:6]
        day = date[6:8]
        #to remove seconds
        time = row[1]
        time = str(int(time/100)) 
        if len(time) == 1 or len(time) == 2:
            minute = int(time)
            hour = 0
        elif len(time) == 3:
            minute = int(time[1:])
            hour = int(time[0])
        elif len(time) == 4:
            minute = int(time[2:])
            hour = int(time[0:2])           
        #print(row[0])
        #print(row[1])
        #print("Hour " + str(hour) + " . Minute: " + str(minute))
        reading_date = datetime(year=int(year), month=int(month), day=int(day), hour=hour, minute=minute)
        #print(reading_date)
        #print(row[2])
        kw = row[2]/float(1000)
        #print(kw)
        kwh = kw * float(5/60)
        #print(str(kwh))
        readings.append([reading_date, kwh])
    df = pd.DataFrame(readings)
    df.to_csv('../disk/nesemp/raw/' + filename, index=False, header=False, mode="a")
