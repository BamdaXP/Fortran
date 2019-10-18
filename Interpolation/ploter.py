import matplotlib.pyplot as plt
import numpy as np
from scipy import interpolate
import os

def load_data(filepath):
    data_list=list()
    f=open(filepath)
    line=f.readline()
    while(line):
        data_list.append(float(line.strip()))
        line=f.readline()
    
    f.close()
    return np.array(data_list)

def lagrange_plot():
    x_data=load_data("./lagrange_x.txt")
    y_data=load_data("./lagrange_y.txt")
    y_real_data=load_data("./lagrange_y_real.txt")
    
    fig, ax = plt.subplots()
    plt.title("Cubic Spline Interpolation")
    plt.errorbar(x_data, y_real_data, yerr=0,label='Predicted line')
    r_line=ax.plot(x_data,y_real_data,label="Real line")
    ax.legend()
    plt.show()
    pass

def newton_plot():
    x_data = load_data("./newton_x.txt")
    y_data = load_data("./newton_y.txt")
    y_real_data = load_data("./newton_y_real.txt")

    fig, ax = plt.subplots()
    plt.title("Newton Interpolation")
    plt.errorbar(x_data, y_data, yerr=abs(
        y_data-y_real_data), label='Predicted line')
    r_line = ax.plot(x_data, y_real_data, label="Real line")
    ax.legend()
    plt.show()
    pass
    

    import numpy as np


def coef(x, y):
    '''x : array of data points
       y : array of f(x)  '''
    n = len(x)
    a = []
    for i in range(n):
        a.append(y[i])

    for j in range(1, n):

        for i in range(n-1, j-1, -1):
            a[i] = float(a[i]-a[i-1])/float(x[i]-x[i-j])

    return np.array(a)  # return an array of coefficient


def Eval(a, x, r):
    ''' a : array returned by function coef()
        x : array of data points
        r : the node to interpolate at  '''
    n = len( a ) - 1
    temp = a[n] + (r - x[n])
    for i in range( n - 1, -1, -1 ):
        temp = temp * ( r - x[i] ) + a[i]
    return temp # return the y_value interpolation


def y(x):
    return 1.0/(1+x**2)
def x(i):
    return -5+10.0/15*i

if __name__ == "__main__":
    lagrange_plot()