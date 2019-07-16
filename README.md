# Systems Modeling to Improve River, Riparian and Wetlands Habitat Quality and Area

Managing river systems requires an understanding of the interaction between stream ecology and hydrology. The suitability of a watershed physical habitat to support the livelihood and productivity of its biota primarily depends on managing flow. Habitat conservation and restoration efforts require an understanding and a measurement of the spatial and temporal ecological response to alterations in flow regimes. Managers use deterministic and qualitative measures such as habitat quality indexes to measure ecological response of particular species at certain locations and times to flow alterations. We propose improving habitat management decision-making by incorporating habitat quality indexes as objectives to maximize in a systems optimization model. The **Watershed Area of Suitable Habitat (WASH)** systems model uses habitat quality indexes to measure physically-available suitable habitat. WASH adds spatial and temporal functionality to quality indexes to recommend environmental flows for aquatic, floodplains, and wetland habitats while maintaining water for human beneficial uses. WASH also highlights promising restoration and conservation sites. In addition, WASH proposes and ranks alternatives for managers to restore and protect natural habitat. WASH formulation is generic and adaptable to other regulated river systems and for species of concern. We apply the model to the Lower Bear River, Utah as a case study to improve degraded habitat and communicate tradeoffs between human and ecosystem uses of water. Results show that the River is largely developed and appropriated for human water uses. However, increasing reservoirs winter releases and minimizing spring spill volumes can create additional suitable habitat area without harming human users. WASH results are displayed on an open-access web map that allows stakeholders to visualize opportunities to manage water and improve habitat quality and area


![alt text](http://bearriverfellows.usu.edu/wash/WatershedHabitat.jpg "Watershed Habitat")
**Figure 1**. River ecological habitat. Source: [Taylor 2008](http://www.versicolor.ca/kerr/Tantramar/3_CreaturesOfTheSaltMarsh/Creatures.html)  

## Objectives of WASH
* Quantify suitable habitat area within a watershed by embedding habitat quality indexes in a systems model, and
* Identify strategies to allocate scare natural (e.g. water) and management (e.g. financial) resources to improve habitat quality.

## Features of WASH
1. Allocates water to maximize ecological benefits at the watershed,
2. Considers multiple aquatic, floodplain, and wetlands habitats,
3. Accounts for competing ecological needs of water for multiple species and their life stage-water needs,
4. Considers temporal and spatial dependency between flow control structures at the watershed,
5. Meets human beneficial use of available water,
6. dentify sites in the watershed where managers can most improve ecosystem functions with limited water, and
7. Is generic and adaptable to other sites and species

## WASH Formulation
To improve river habitat quality, managers, first, identify watershed component targets that are in need of restoration. Here, we include the common areas of habitat management within a watershed: aquatic, floodplain, and impounded wetland habitats (Figure 2). Next, we select indicator species for each habitat component. Indicator species are organisms that (a) their presence can be monitored and their abundance can be measured, (b) have an ecological and managerial significance, and (c) their habitat requirements are directly impacted by flow regime conditions.  Then, we design habitat suitability indexes as functions of flow components, such as water depth and flood recurrence. These functions are site- and species-dependent and are based on the tolerance of species to changes in hydrologic and ecologic conditions. We measure hydrologic changes by controlling reservoir releases and storage volumes in addition to diversion volumes. We also control vegetation cover of indicator trees by revegetating the floodplains. These decision variables dictate the values of a group of state variables such as water depth, channel cross sectional area, reservoir surface area, storage and flood recurrence (Figure 1). We measure habitat performance by multiplying each suitability index [unitless] by an affected area. Habitat indicators are aggregated together using spatial and temporal weights to determine the overall WASH performance indicator value in unit area for the entire watershed. Expressing WASH in unit area (e.g. m2) helps communicate results to stakeholders and describe the implications of management actions over time. WASH model works with river systems as a group of nodes for demand sites and reservoirs and links for river reaches, diversions, and return flows.


![alt text](http://bearriverfellows.usu.edu/wash/ModelFormulation_Updated.jpg "Model Formulation")

**Figure 2**. A flow diagram of the WASH systems model components that shows the decision and state variables, model parameters, suitability indexes, performance indicators, objective function and constraints 

## Explanations of Repository Contents
1. [InputDataModel](https://github.com/ayman510/WASH/blob/master/InputDataModel/Readme.md) - Input data for GAMS model and R scripts to process input data
2. [Documentation](https://github.com/ayman510/WASH/blob/master/Documentation/Readme.md) - Manuscript preprint for article with final results
3. [OutputFiles](https://github.com/ayman510/WASH/blob/master/OutputFiles/Readme.md) - Model output files and scripts to process raw outputs and generate results including manuscript figures
4. [WebMap](https://github.com/ayman510/WASH/blob/master/Webmap/Readme.md) - Explanation of WASH webmap application and instructions to use the webmap.


## Instructions to Run WASH Model:
1. Download and install [GAMS](https://www.gams.com/) V.24.2.3 and [MS Excel](https://products.office.com/en-us/excel) to run the WASH tool. You will need a license to run GAMS solvers.
2. Download the following files from the GitHub repository: [GAMS Code file](https://github.com/ayman510/WASH/blob/master/Code/GAMScode.gms), [WASH Input Data File](https://github.com/ayman510/WASH/blob/master/InputData/WASH_1yr_InputData.xlsx), [WASH Output Data File](https://github.com/ayman510/WASH/blob/master/OutputFiles/WASH_1yr_OutputData.xlsx)
3. Create a new folder on your machine and save the above three files there.
4. Open GAMS software. Go to File-> Project -> New Project. Navigate to the folder you created and create a new project file with any name you want. 
5. In GAMS, go to File -> Options -> Solver and select your Nonlinear Problem (NLP) solver. The results generated in this [Output Data File](https://github.com/ayman510/WASH/blob/master/OutputFiles/WASH_1yr_OutputData.xlsx) were obtained using BARON global optimal solver. 
6. In GAMS, go to File -> Open and open the GAMScode.gms file. Read through the introduction and instructions to learn how to read and edit the code, if necessary.
7. To edit the input data, open the Input Data Excel Spreadsheet using MS Excel. Refer to the first sheet of the Excel file for a detailed description of the accepted data values and units
8. If applicable, change the name of the input data file in the code line: "$CALL GDXXRW.EXE input=InputData.xlsx" to your input data filename
9. Run the code using the Run botton or File -> Run. The code runs successfully if you get the message "*** Normal Completion ***" in the generated listing file
10. The model results are displayed on a listing file in GAMS. The results are also written to both GAMS Exchange (GDX) and Excel files. The downloaded output file: WASH_Results.xlsx will be updated with new results and will updated the graphs in the Excel file.
11. See [OutputFiles](https://github.com/ayman510/WASH/blob/master/OutputFiles/Readme.md) folder for further instructions to generate results shown in each manuscript figure.


## Recommended Citation
Ayman Alafifi and David E. Rosenberg (2019). "Systems Modeling to Improve River, Riparian, and Wetland Habitat Quality and Area." Submitted to Environmental Modeling and Software. July 2019.
