import pandas as pd

# Load the uploaded CSV file
file_path = 'SPOD\SPOD_R_SHINY\SPOD_R_Shiny_WebApp\Data\spod2_9.csv'
df = pd.read_csv(file_path)

# Display the first few rows of the dataframe to understand its structure
df.head()

###


# Load the CSV file with better error handling to identify the problematic rows
import csv

# Open the file and check for inconsistencies in the number of fields
with open(file_path, 'r', encoding='utf-8') as file:
    reader = csv.reader(file)
    rows = list(reader)

# Identify the row with the wrong number of columns
num_columns = max(len(row) for row in rows)
problematic_rows = [index for index, row in enumerate(rows) if len(row) != num_columns]

problematic_rows


###

# Load the CSV file while skipping rows with bad lines
df = pd.read_csv(file_path, error_bad_lines=False, warn_bad_lines=True)

# Display the first few rows of the dataframe to understand its structure
df.head()
