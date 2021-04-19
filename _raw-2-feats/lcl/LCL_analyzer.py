"""
Created on Mon Jun 22 08:47:34 2020
@author: Carlos Quesada, Universidad de Deusto
LOW CARBON LONDON
"""
import datetime
from operator import itemgetter

# ROOT FOLDER
root_folder = "D:/Quesada/Documents/__TRABAJO/WHY (Deusto)/Datos/" + \
    "Low Carbon London/"
# INPUT FOLDER
folder_in = root_folder + "halfhourly_dataset/"
# OUTPUT FOLDER
folder_out = root_folder + "output/"

# IMPORT ENERGY CONSUMPTION READINGS
for block in range(112):
    energy = []
    with open(folder_in + "block_" + str(block) + ".csv") as f:
        # Skip header
        _ = f.readline()
        while True:
            line = f.readline()[:-1]
            if not line:
                break
            values = line.split(",")
            values[1] = datetime.datetime.strptime( 
                values[1][:19], "%Y-%m-%d %H:%M:%S")
            # Check not null data
            if values[2] != "Null":
                values[2] = float(values[2])
                energy.append(values)                
            # Check if same customer
            if len(energy) > 1:
                if energy[-1][0] != energy[-2][0]:
                    # Sort lines
                    sorted_data = sorted(energy[:-1], key=itemgetter(1))
                    # Save to file
                    filename = sorted_data[0][0]
                    with open(folder_out + filename + ".csv", 'a') as f_out:
                        for line in sorted_data:
                            f_out.write(
                                line[1].strftime("%Y-%m-%d %H:%M:%S") + "," + \
                                str(line[2]) + "\n")
                    f_out.close()
                    energy = [energy[-1]]
    f.close()