This folder contains the following input data and GAMS model files:

## Rscripts
Folder with .csv files and R scripts (BCT_plots.R and Combined_BrownTrout.R) to preprocess and fit habitat suitability curves to data in the csv files.

## Excel Files
1. WASH_1yr_InputData.xls: Input data as an MS Excel file format for 1 year (2003). The first sheet in the file lists all the parameters that are used in WASH model and the subsequent sheets list the data that is used to populate the model results.
2. WASH_5yr_InputData.xlsx: Input data file for 5 years (2003 - 2007)
3. Flood_Frequency_Analysis.xlsm: Flood frequency analysis curves for several sites at the watershed

## GAMS Files
4. WASH.gms: The General Algebraic Modeling Systems (GAMS) model file
5. WASH-Data.gdx: The 1-year input data from the Excel input file in GAMS file format
6. baron.opt: Configuration file for the Baron solver
7. WASHModel.gpr: Project (directory) file. If opening and using GAMS, will need to create new project file to specify your local directory)
 
