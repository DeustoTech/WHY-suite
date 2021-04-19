# -*- coding: utf-8 -*-
"""
Created on Fri Jun 26 12:39:27 2020
PerModAC data separator
@author: Quesada
"""
import datetime
import pandas as pd

root_folder = "D:/PerModAC/"
outp_folder = root_folder + "output/"
data_files = ['PL1.csv', 'PL2.csv', 'PL3.csv', 'QL1.csv', 'QL2.csv', 'QL3.csv']

# Creation of time file
print("CREATING TIME FILE")
time_file = [ 
    (datetime.datetime(2010,1,1,0,0,0) + 
     datetime.timedelta(seconds = ss)).strftime("%Y-%m-%d %H:%M:%S") 
     for ss in range(31536000)
]

for data_file in data_files:
    print(data_file)
    print("READING DATA FILE")
    df = pd.read_csv(root_folder + data_file, header=None)
    
    for nn in range(74):
        print("WRITING DATA FILE " + str(nn+1))
        filename = outp_folder + data_file[:3] + '-' + \
            str(nn+1) + data_file[3:]
        sub_df = pd.DataFrame({
            'time': time_file,
            'data': df[:][nn].tolist() })            
        sub_df.to_csv(filename, header=False, index=False)
            
#        with open(filename, mode='a', newline='') as wfile_descrp:
#            csv_writer = csv.writer(
#                wfile_descrp,
#                delimiter=',',
#                quotechar='"',
#                quoting=csv.QUOTE_MINIMAL)
#            csv_writer.writerow([
#                time_file, 
#                df[:][nn].tolist() ])

#for data_file in data_files:
#    print(data_file)
#    rows = []
#    with open(root_folder + data_file) as csv_file:
#        csv_reader = csv.reader(csv_file, delimiter=',')
#        for line_count, row in enumerate(csv_reader):
#            rows.append(row)
#            if (line_count + 1) % block == 0 :
#                print(round(100 * (line_count + 1) / 24, 2))
#                # SAVE IN SEPARATE FILES
#                for nn, nn_row in enumerate(row):
#                    filename = outp_folder + data_file[:3] + '-' + \
#                        str(nn+1) + data_file[3:]
#                    with open(filename, mode='a', newline='') as wfile_descrp:
#                        csv_writer = csv.writer(
#                            wfile_descrp,
#                            delimiter=',',
#                            quotechar='"',
#                            quoting=csv.QUOTE_MINIMAL)
#                        csv_writer.writerow([
#                            time_file[line_count], 
#                            [rr[nn] for rr in rows]
#                            ])
#                rows = []
                    