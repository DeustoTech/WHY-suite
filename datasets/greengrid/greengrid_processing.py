import pandas as pd
from glob import glob
from datetime import datetime

src_path = "/home/ubuntu/carlos.quesada/disk/greengrid/source/powerData/*.csv"

for entry in sorted(glob(src_path)):
    print(entry)
    household_data = []
    df = pd.read_csv(entry)
    current_datetime = None
    aggregated_datetime_data = 0
    household_id = None
    for index,row in df.iterrows():
        household_id = row['hhID']
        local_datetime = row['r_dateTime']
        local_datetime = datetime.strptime(local_datetime, '%Y-%m-%dT%H:%M:%SZ')
        if current_datetime == None:
            current_datetime = local_datetime
            aggregated_datetime_data  += row['powerW']
        elif current_datetime == local_datetime:
            aggregated_datetime_data  += row['powerW']
        elif current_datetime != local_datetime:
            aggregated_datetime_data = (aggregated_datetime_data/float(1000))*(1/60)
            household_data.append([current_datetime, aggregated_datetime_data])
            aggregated_datetime_data = row['powerW']
            current_datetime = local_datetime

    df = pd.DataFrame(household_data)
    df.to_csv('../disk/greengrid/raw/' + household_id + '.csv', index=False, header=False, mode="w")



