#!python3
# Written by Matthew McElhanon

# possible changes that we could make
# more precision of timestamps
# time.sleep to add wait times?

import serial
import math
import datetime
import re
import time

def initialize_serial(port='COM10'):
    """Initialize and return a serial connection."""
    try:
        ser = serial.Serial(port, baudrate=9600, timeout=1)
        return ser
    except serial.SerialException as e:
        print(f"Error opening serial port: {e}")
        return None

def initialize_csv_file():
    """Initialize the CSV file with a header."""
    csvtime = datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
    header1 = 'Time,A,B,C,D,E,F,Average,X,Y,Radius,Deg,Radian,In Heat, In Humidity, Ex Heat, Ex Humidity'
    filename = f"spod2_{csvtime}.csv"
    with open(filename, "a") as file:
        file.write(header1 + '\n')
    return filename

def config_vect(A_read, B_read, C_read, D_read, E_read, F_read):
    """
    This function inputs six sensor signals at the vertices of a triangle and returns
    values of the x axis, y axis, hypotenuse and the angle in radians and degrees and average.
    """
    angles = [90, 330, 210, 270, 150, 30]
    angles_rad = [math.radians(angle) for angle in angles]
    x = sum(read * math.cos(ang) for read, ang in zip([A_read, B_read, C_read, D_read, E_read, F_read], angles_rad))
    y = sum(read * math.sin(ang) for read, ang in zip([A_read, B_read, C_read, D_read, E_read, F_read], angles_rad))
    r = math.sqrt(x**2 + y**2)
    rad = math.acos(x / r) if r != 0 else 0
    deg = math.degrees(rad)
    ave = sum([A_read, B_read, C_read, D_read, E_read, F_read]) / 6
    return x, y, r, rad, deg, ave

def spod2(t2, csv_filename):
    """Process sensor data and save to CSV."""
    try:
        internal_humidity = float(t2[0])
        internal_heat = float(t2[2])
        external_humidity = float(t2[4])
        external_heat = float(t2[6])
        VOC_A = float(t2[8])
        VOC_B = float(t2[9])
        VOC_C = float(t2[10])
        VOC_D = float(t2[11])
        VOC_E = float(t2[12])
        VOC_F = float(t2[13])

        # Calculate VOC values
        VOC_x, VOC_y, VOC_r, VOC_rad, VOC_deg, VOC_ave = config_vect(VOC_A, VOC_B, VOC_C, VOC_D, VOC_E, VOC_F)
        
        current_time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f')
        values = f"{current_time},{VOC_A},{VOC_B},{VOC_C},{VOC_D},{VOC_E},{VOC_F},{VOC_ave},{VOC_x},{VOC_y},{VOC_r},{VOC_deg},{VOC_rad},{internal_heat},{internal_humidity},{external_heat},{external_humidity}"

        # Write values to CSV
        with open(csv_filename, "a") as file:
            file.write(values + '\n')
    
    except (ValueError, IndexError) as e:
        print(f"Error processing data: {e}")

def main():
    ser = initialize_serial('COM10')
    if ser is None:
        return
    
    csv_filename = initialize_csv_file()
    
    while True:
        try:
            t = ser.readline().decode('utf-8').strip()
            t2 = re.findall(r'\d+\.\d+|\d+', t)  # Match both integer and float numbers
            print(t2)
            if t2:
                spod2(t2, csv_filename)
            time.sleep(0.1)  # Add a small delay between readings to avoid processing the same timestamp
        except serial.SerialException as e:
            print(f"Serial error: {e}")
            break
        except Exception as e:
            print(f"Unexpected error: {e}")
            break

if __name__ == "__main__":
    main()
