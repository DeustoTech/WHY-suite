# -*- coding: utf-8 -*-
"""
Split all ISSDA data

@author: Quesada
"""
#import pickle
import os
from operator import itemgetter
import datetime

folder_in = "D:/Quesada/Documents/__TRABAJO/WHY (Deusto)/Datos/ISSDA/" + \
    "38_CER Electricity_Gas/CER Electricity Revised March 2012/"
#folder_in = "D:/Quesada/Documents/__TRABAJO/WHY (Deusto)/Datos/ENLITEN/"

## Create dictionary
for ii in range(6):
    # Initialize dict
    files_dict = {}
    # Select folder
    subfolder_in = folder_in + "File" + str(ii+1) + ".txt/"
    filename = "File" + str(ii+1) + ".txt"
    print("Analyzing " + filename)
    # Open file
    with open(subfolder_in + filename) as f1:
#        # Skip header
#        header = f1.readline()
        # Read file line by line
        while True: 
            line = f1.readline()
            # If there are no more lines to read, exit
            if not line: 
                break
            # Get all values of line 
            values = line.split()
            key_ID = int(values[0])
            values_list = [int(values[1]), float(values[2])]
            # Create key if it not exists
            if key_ID not in files_dict:
                files_dict[key_ID] = []
#                print(key_ID)
            # Append line
            files_dict[key_ID].append(values_list)
    f1.close()

    print("Writing outputs")
    # Output subfolder
    subfolder_out = folder_in + "output/"
#    # Create folder if not exists
#    if not os.path.exists(subfolder_out):
#        os.makedirs(subfolder_out)
    
    # Start date
    start_date = datetime.datetime(2009, 1, 1, 0, 0, 0)
    # Create a file for each key in dict
    for dict_key in files_dict.keys():
        # Sort values
        sorted_lines = sorted(files_dict[dict_key], key=itemgetter(0))
        
        with open(subfolder_out + str(dict_key) + ".csv", 'w+') as f3:
            for line in sorted_lines:
                # Get date
                days = int(str(line[0])[:3])
                minutes = int(str(line[0])[3:])*30
                new_date = start_date + \
                    datetime.timedelta(days=days) + \
                    datetime.timedelta(minutes=minutes)
                f3.write(str(new_date) + "," + str(line[1]) + "\n")
        f3.close()
        
## Save dictionary
#with open(folder_out + "dump.pkl", "wb") as f2:
#    pickle.dump(files_dict, f2, protocol=pickle.HIGHEST_PROTOCOL)

## Load from pickle
#file = open(folder_out + "dump.pkl",'rb')
#files_dict = pickle.load(file)
#file.close()