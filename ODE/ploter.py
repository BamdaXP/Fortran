import matplotlib.pyplot as plt
import numpy as np
from scipy import interpolate
import os


def load_data(filepath):
    data_list = list()
    f = open(filepath)
    line = f.readline()
    while(line):
        try:
            data_list.append(float(line.strip()))
        except Exception as e:
            pass
        line = f.readline()

    f.close()
    return np.array(data_list)

def get_data_list(word):
    data_list = list()
    for filename in os.listdir("./data/"):
        fp = os.path.join("./data/", filename)
        if os.path.isfile(fp) and word in filename:
            data_list.append(load_data(fp))

    return data_list


def start_plot(op_str):
    y_data_list = get_data_list(op_str)

    
    fig, ax = plt.subplots()
    plt.title(op_str)
    i=1
    for y_data in y_data_list:
        txt = "step:"
        if i==1:
            txt+=str(0.025)
        elif i==2:
            txt+=str(0.033)
        elif i==3:
            txt += str(0.05)
        elif i ==4:
            txt += str(0.1)
            
            
        p_line = ax.plot(np.linspace(0,1.5,len(y_data)),y_data,label="Prediction"+txt)
        i+=1

    y_hat = 3/(1+np.linspace(0, 1.5, 100)**3)
    r_line = ax.plot(np.linspace(0, 1.5, 100),y_hat, label="Real")
    ax.legend()
    plt.show()

if __name__ == "__main__":
    for op in ["euler0","euler5","rk4"]:
        start_plot(op)
