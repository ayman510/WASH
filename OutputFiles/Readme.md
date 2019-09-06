This folder contains the output files of the WASH model.

## Folder Contents

1. WASH_1yr_OutputData.gdx : GAMS gdx file with all WASH model results for the single-year run. Generated when the WASH.gms model file is run in GAMS.

2. WASH_1yr_OutputData.xls : Excel version of the WASH model results in the gdx file.

3. WASH_Results.xlsx: The model output file. This file is developed to automatically update after every model run and generate new flow comparison and reservoir releases graphs. In addition, it arranges the data into new sheets ready to upload to a web map. 

4. WASH_Demand_Tradeoff.xlsx: The model output file that plots the WASH and Huamn Demand tradeoff curve. 

5. WASH_5yr_OutputData.gdx : GAMS gdx file with all WASH model results for the five-year run. Generated when the model is run in GAMS.

6. WASH_5yr_Results.xlsx : Excel file for the WASH model results for 5 years.

7. HyrumReservoir_Results.xlsx: Excel file to plot the results of Hyrum Reservoir.

8. VegetationPlots : Folder with R files and results to produce figures 6 and 10 in the manuscript.

## To reproduce specific figures in the manuscript

Figure 4. Monthly suitable aquatic, floodplain, and wetland habitat areas:
1. Copy the model output tables for the variables R (aquatic habitat), F (floodplain habitat), and W (wetland habitat) areas to their corresponding sheets in the WASH_1yr_OutputData.xlsx file. 
2. The figure in the sheet "Habitat_tradeoff" will be automatically updated.

Figure 5. Comparison between model recommended and current reservoir releases:
1. Copy the model output tables for RR (reservoir releases) into cells G3:G14 in sheet HydrumReservoirData in the HyrumReservoir_Results.xlsx file. 
2. The figrue in the sheet "Releases" will be automatically updated.

Figure 6. Vegetation cover and planting area:
1. In the VegetationPlots subfolder, open the PlotWASHVegetation.R script
2. If needed, edit line 89 of the script to point the R code to the location of the GDX file for the WASH results (WASH_1yr_OutputData.gdx)
3. Run the script to generate the figure

Figure 7. Model recommended improvements at the Bird Refuge:

Figure 7a:
1. Copy the model output tables for WSI (wetland suitability index) into sheet WSI in the WASH_1yr_OutputData.xlsx file.
2. The Figure in WSI sheet will be updated using the simulated (pre-generated) data.

Figure 7b:
1. Copy the model output tables for WASH (flow) into sheet Q in the WASH_1yr_OutputData.xlsx file.
2. The Figure in Q_Analysis_Cfs sheet will be updated using the simulated (pre-generated) data and the water right data for the refuge.

Figure 8. Tradeoff between WASH suitable area and delievery targets:
1. Go to the Tradeoff_Figure folder 
2. Use the WASH_Figure8.gms GAMS file to generate the figure 
3. Run the GAMS script to generate the WASH_Demand_Tradeoff.GDX file and spreadsheet for the tradeoff data.
4. Copy the WASH and dReq (for demand requirement) values into the WASH_1yr_Tradeoff_Output.xlsx spreadsheet.
5. Figure 8 will be generated in the sheet: "Analysis"

Figure 9. Comparison of 5-year (2003-2007) optimization vs simulation results:
1. Go to the 5Year_Results folder
2. Open the WASH_5yrs.gms GAMS file and run it to import the WASH_5yr_InputData.xls from the Input Folder 
3. The WASH_5yr_OutputData.GDX and WASH_5yr_OutputData.xlsx will be generated.
4. Run the script to populate results for 5-years as GDX and Excel files.
5. Copy the optimization results for R (suitable aquatic habitat area), RSI (aquatic habitat quality index), Q (flow) and RR (reservoir releases) for Hyrum only to their corresponding sheets. 
6. Figure 9 will be generated in the sheet: "Plot"

Figure 10. Shadow values of the vegetation growth:
1. In the VegetationPlots subfolder, open the PlotWASHVegetation.R script
2. Repeat Figure 6, Instruction #2
3. Run the script to generate the figure
