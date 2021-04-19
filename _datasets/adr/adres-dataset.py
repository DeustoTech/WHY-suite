# -*- coding: utf-8 -*-
"""
ADRES DATASETS
"""
import scipy.io
import datetime as dt
import numpy as np
import pandas as pd

# Files
input_file = "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datos (raw)/ADRES/" + \
    "ADRES_Daten_120208.mat"
output_folder = "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datasets/adres/"

# Load data from .mat file
m = scipy.io.loadmat(input_file)
dataU  = np.array(m['Data'][0,0][0])
dataPQ = np.array(m['Data'][0,0][1])

# Generate dates
summ_base = dt.datetime(2011, 2, 7, 0, 0, 0)
summ_dates = [summ_base + dt.timedelta(seconds = x) for x in range(604800)]
wint_base = dt.datetime(2011, 8, 8, 0, 0, 0)
wint_dates = [summ_base + dt.timedelta(seconds = x) for x in range(604800)]
times = [wint_dates, summ_dates]

# Loop
nkey = "UPQ"
nseas = ["sum", "win"]
for rr, row in enumerate([(604800, 1209600), (0, 604800)]):
    for dd, data in enumerate([dataU, dataPQ]):
        for col in range(data.shape[1]):
            # Data extraction
            ts = data[row[0]:row[1], col]
            # File name generation
            n1 = nkey[dd * (col % 2 + 1)]
            n2 = str((col // (dd+1)) % 3 + 1)
            n3 = str(col // (3 * (dd+1)) + 1)
            n4 = nseas[rr]
            fname = n1 + n2 + "_h" + n3 + "_" + n4
            print(fname)
            # Build the dataframe
            df = pd.DataFrame({
                'time': times[rr],
                'data': ts
            })
            # Save the dataframe
            df.to_csv(
                output_folder + fname + '.csv',
                header = False,
                index = False
            ) 