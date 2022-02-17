# arrhenius-stats
Code for calculating linear regressions on stream data

## Usage
### Input data
Toy data in the example format can be found at `test_data/test_data.csv`.
The data used in this project can be found at `final_data/Final_Data_Serena.csv`.

In general, input data must have 5 columns:
`SITE`, `DATE`, `STANDTEMP`, `InvGPP`, and `InvER`, where the `DATE` column is a string in MMDDYY format, and the last three columns are floats.

### Calculating regressions
The script `arrhenius_stats.R` is used for the heavy lifting in this analysis. Linear regressions are calculated for each data point (row) within a given day range, which in the original script are 5, 10 and 15 days on either side of the data point. The R script produces one `.csv` file for each dayrange, which are then passed to the python script.

This script is run from within RStudio, with the path to the data specified on the second to last line of code. This path should be changed to refelct the locaiton of the data.

### Merging outputs
I am much faster at Python than R, and could not get the merging of the three dataframes to work in R. I wrote this quick Python script to take care of it instead. The script merges a list of df's over common columns. The R script outputs 3, but this script generalizes to any number of dataframes (although it may get prohibitively slow at a certain number).

Example usage:
```
python unify_dfs.py <path to df 1> <path to df2> <path to df3> <save name, including path>
```
