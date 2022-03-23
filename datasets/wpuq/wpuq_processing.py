import h5py
from datetime import datetime
import pandas as pd

base_path = "/home/ubuntu/carlos.quesada/disk/wpuq/source/"
filenames = ["2018_data_60min.hdf5", "2019_data_60min.hdf5", "2020_data_60min.hdf5"] 

data_per_household = {}

for filename in filenames:
    with h5py.File(base_path + filename, "r") as f:
        for d in f['NO_PV']:
            #In each d (house id) we have data about HOUSEHOLD and HEATPUMP
            if d not in data_per_household:
                data_per_household[d] = []
            for t in f['NO_PV'][d]['HOUSEHOLD']['table']:
                #Here the information afout electricity on each of d households. 
                if  not pd.isna(t['P_TOT']):
                    kwh = t['P_TOT']/float(1000)
                    #print(kwh)
                    date = datetime.utcfromtimestamp(t['index'])
                    #print(date)
                    #print(t['P_TOT']/float(1000))
                    #print(datetime.utcfromtimestamp(t['index']))
                    #print('-----------------------------')"""
                    data_per_household[d].append([date, kwh])


for d in data_per_household:
    df_user = pd.DataFrame(data_per_household[d])
    df_user.to_csv('../disk/wpuq/raw/' + str(d) + '.csv', index=False, header=False, mode="w")