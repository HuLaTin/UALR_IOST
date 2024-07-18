#!python3
# Written by Matthew McElhanon

import serial
import math
import datetime
import re

ser = serial.Serial('COM10')  # open serial port
csvtime = datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
header1 = 'Time,A,B,C,D,E,F,Average,X,Y,Radius,Deg,Radian,In Heat, In Humidity, Ex Heat, Ex Humidity'
Spod2_1 = open(f"spod2_{csvtime}.csv","a")
csvread = f"spod2_{csvtime}.csv"
Spod2_1.write(header1)
Spod2_1.close()
def spod2(t2):
    def config_vect(A_read,B_read,C_read,D_read,E_read,F_read):
        '''this function inputs six sensor signals at the vertices of a triangle and returns values of the x axis,
        y axis, hypotenuse and the angle in radians and degrees and average '''
        ang_A=math.radians(90)
        ang_B=math.radians(330)
        ang_C=math.radians(210)
        ang_D=math.radians(270)
        ang_E=math.radians(150)
        ang_F=math.radians(30)
        xo=A_read*math.cos(ang_A)+B_read*math.cos(ang_B)+C_read*math.cos(ang_C)+D_read*math.cos(ang_D)+E_read*math.cos(ang_E)+F_read*math.cos(ang_F)
        yo=A_read*math.sin(ang_A)+B_read*math.sin(ang_B)+C_read*math.sin(ang_C)+D_read*math.sin(ang_D)+E_read*math.sin(ang_E)+F_read*math.sin(ang_F)
        r=math.sqrt(x**2 +y**2)
        rad=math.acos(x/r)
        deg=math.degrees(rad)
        ave=(A_read + B_read + C_read + D_read + E_read + F_read)/6
        return(x,y,r,rad,deg,ave)
    internal_humidity = t2[0]
    internal_heat = t2[2]
    external_humidity = t2[4]
    external_heat = t2[6]
    VOC_A = t2[8]
    VOC_B = t2[9]
    VOC_C = t2[10]
    VOC_D = t2[11]
    VOC_E = t2[12]
    VOC_F = t2[13]

         #TASK7 - convert data to float and then input to configuration vector function
        #the data in the list are strings and we need to convert to float in order to do math 
        #print(type(VOC_A))
    n_VOC_A=float(VOC_A)
    n_VOC_B=float(VOC_B)
    n_VOC_C=float(VOC_C)
    n_VOC_D=float(VOC_D)
    n_VOC_E=float(VOC_E)
    n_VOC_F=float(VOC_F)
    #VOC calc
    output=config_vect(n_VOC_A,n_VOC_B,n_VOC_C,n_VOC_D,n_VOC_E,n_VOC_F)
    output=list(output)
    VOC_x=output[0]
    VOC_y=output[1]
    VOC_r=output[2]
    VOC_rad=output[3]
    VOC_deg=output[4]
    VOC_ave=output[5]
      
    time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    #NOTE: CONVERT VALUES BACK TO LIST FOR GOOGLE SHEETS
    values =f"{time},{n_VOC_A}, {n_VOC_B}, {n_VOC_C}, {n_VOC_D},{n_VOC_E},{n_VOC_F}, {VOC_ave}, {VOC_x}, {VOC_y}, {VOC_r}, {VOC_deg}, {VOC_rad}, {internal_heat}, {internal_humidity}, {external_heat}, {external_humidity}"

    # the following code makes backup file
    #TASK 9 dump data to local back file (csv)
    #note how we removed brackets from the list when we converted to strings
    Spod2 = open(f'{csvread}',"a")
    Spod2.write("\n")
    Spod2.write(str(values[0:-1]))
    Spod2.close()
while True:
    t=ser.readline()
    t1= str(f'{t.rstrip()}')
    t2 = re.findall(r'\d+',t1)
    print(t2)
    spod2(t2)
