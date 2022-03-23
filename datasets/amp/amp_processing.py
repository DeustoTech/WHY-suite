import pandas as pd
from datetime import datetime
from pytz import timezone
import pytz

#Column meanings: https://www.nature.com/articles/sdata201637/tables/5
#Paper: https://www.nature.com/articles/sdata201637
#KVAH to KWH https://www.quora.com/How-do-I-convert-KWH-to-KVAH    ¿??¿?
file_path = "/home/ubuntu/carlos.quesada/disk/amp/source/Electricity_WHE.csv"
df = pd.read_csv(file_path)
date_format='%m/%d/%Y %H:%M:%S %Z'
date_format_no_tz = '%Y/%m/%d %H:%M:%S'
readings = []
for index,row in df.iterrows():
    date = datetime.utcfromtimestamp(row['unix_ts'])
    v = row['V']
    i = row['I']/float(10)
    kw = (v*i)/float(1000)
    kwh = kw*(1/60)
    #print('Current date & time is:', date.strftime(date_format))
    date = date.astimezone(timezone('Canada/Pacific'))
    #print('Local date & time is  :', date.strftime(date_format))
    #print(date)
    readings.append([date.strftime(date_format_no_tz), kwh])

df_user = pd.DataFrame(readings)
df_user.to_csv('../disk/amp/raw/household_1.csv', index=False, header=False, mode="w")
