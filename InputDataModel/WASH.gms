$Title Watershed Area of Suitable Habitat (WASH) model and an application on the Lower Bear River Watershed

$OnText

####################
Developed by Ayman H. Alafifi

Dept. of Civil & Env. Engineering and Utah Water Research Lab
Utah State University
ayman.alafifi@gmail.com

Updated by David Rosenberg - April 12, 2018 to dump marginals of vegetation constraints
to GDX to read in by R to plot figures 6 and 10
####################

Introduction:
WASH is a system optimization model that maximizes water allocation to environmental needs while maintaining human and other beneficial uses.

WASH receommends allocation of water between users to improve habitat quality by incorporating habitat quality indexes as objectives to maximize
in a systems optimization model. WASH measures physically-available suitable habitat area in three main components of most watersheds, namely
aquatic, floodplain and wetland habitats. WASH highlights promising restoration and conservation sites that are in need of available water and proposes management alternatives
to protect habitat for priority species. WASH formulation is generic and adaptable to other regulated river systems and can accomodate multiple priority species of concern.

The code below desrcibes the WASH optimization model that includes a single objective function and several decision variables and constraints.
The code reads the  parameters that are required to run the model as GDX input data. The code writes the results to another GDX file for further processing.

More documentations on the model formulation are available here: https://github.com/ayman510/WASH

Citation:
Alafifi A., Rosenberg D.E., (2017), Systems Modeling to Improve River, Riparian, and Wetland Habitat Quality and Area, Journal of Environmental Modelling and Software (in prep.)


--------------------------------------------------------------------------
Instructions:
To run the code, follow the steps below:

1. Define the input data in an Excel Spreadsheet following the example given in the file: "InputData.xlsx". Refer to the first sheet for a detailed description of the accepted data values and units
2. Place the Excel Spreadsheet file in the same directory where this GAMS code is saved
3. If applicable, change the name of the input data file in the code line: "$CALL GDXXRW.EXE input=InputData.xlsx" to your input data filename
4. Run the code using the Run botton or File -> Run. The code runs successfully if you get the message "*** Normal Completion ***" in the generated listing file
5. The model results and outputs can be read from the listing file. GAMS will also write the results to a GAMS Exchange file (GDX) "WASH-solution.gdx" that can be passed to other software for further processing (e.g. R, Matlab, Excel, etc.)
6. Here, the results are also written to an Excel file "Model_Results.xlsx" that will display the results in US.Customary Units and will graph the results.


--------------------------------------------------------------------------
Licensing:

Copyright (c) 2016, Ayman H. Alafifi and David E. Rosenberg
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



$OffText

**** Model formulation begins here:

* The model formulation uses capital letters to indicate decision variables and lower-case letters to describe model parameters

*===============
*1. Declare sets
*===============

* The model divides a river network into nodes and links. Nodes are denoted by the set j. k is an aliase of j.
* The links in the model are denoted by a starting node and an end node. Q(j,k), for instance, desrcibes flow (Q) between nodes j and k.
* An example of a river network for the Lower Bear River, Utah that is represented in links and nodes can be found here: http://bearriverfellows.usu.edu/wash/2005/LBRNetwork.jpg


SETS     s       sub-indicators. Here s1=riverine habitat s2=floodplain connectivity and s3=impounded wetlands
         j       river netowrk nodes which lists all nodes in the network
         dem(j)  demand sites which are a subset of the model nodes (j)
         v(j)    reservoirs which are a subset of the model nodes (j)
         y       The type(s) of priorty fish species that the model uses as indicator species
         n       The type(s) of prioty vegetation species that the model uses as indicator species
         t       timesteps in months

         RA_par_indx    indexes to read the parameters for the reservoir volume elevation curves

         sf_par_indx    indexes to read the parameters for stage-flow relationships
         wf_par_indx    indexes to read the parameters for width-flow relationships
         wsi_par_indx   indexes to read the parameters for wetlands suitability index relationships

         rsi_indx      indexes to read the parameters for riverine suitability indicator equations
         fci_indx      indexes to read the floodplain connectivity indicator equations

         MassBalanceNodes(j)      Lists all the nodes that will balance the flow not including demands sites because a diffrent equation is used to describe their mass balance

         run        number of runs (iterations) for the allowable shortage to build a WASH-Demand pareto-optimal curve

         Qrun       number of runs (iterations) for flow values to test the model sensitivity and response to differnt flow values
;
*Define alias required to run the model and its loops
Alias (j,k)       ;
Alias (run,run2)  ;
Alias (Qrun,Qrun2);

*=================================
*Define Parameters and Scalars
*=================================
PARAMETERS
         LinkID(j,k)             A unqie ID to every link in the network to be used for GIS visulaization
         linkexist(j,k)          Describes if the link from node j to node k exists (1=yes and 0=no)
         envSiteExist(j,k)       Describe if the link is an environmental site or not (1=yes and 0=no). Environmental sites are were sensitive habitat is located and aquatic floodplain and wetland ecological indexes are calculated
         returnFlowExist(dem,k)  Defines if a return flow exists on a link from a demand site back to the river network  (1=yes and 0=no)
         DiversionExist(k, dem)  Defines if the link is a diversion from the network to a demand site  (1=yes and 0=no)
         WetlandsExist(j,k)      Defines if the link has impoounded wetlands (1=yes and 0=no)
         LinktoReservoir(j,v)    Defines if the link flows into a reservoir or not (1=yes and 0=no)
         LinkOutReservoir(v,j)   Defines if the link leave a reservoir or not and carry reservoir releases (1=yes and 0=no)
         wght(s,j,k,t)           The model spatial and temporal weights on every sub-indicator as set by stakeholders [0: no important - 1: important]
         reachGain(j,t)          Defines the flows that feed a node in the network - gain could be headflow or a tributary

         aw(j,k,t)               Total impounded wetlands area (when filled to maximum depth) [Mm^2]

         lss(j,k,t)              Net losses on link j entering k  as a [%] of flow. Losses could be due to seepage to the groundwater
         evap(v,t)               Evaporative losses in reservoirs [m per month]
         linkEvap(j,k,t)         Evaporative losses on links [m per month]

         cons(dem,t)             Consupmtive use fraction expressed as a [%] of all inflow received at a demand site
         lng(j,k)                Length of river links [m]
         minstor(v)              Inactive reservoir storage [Mm^3]
         maxstor(v)              Reservoir storage capacity [Mm^3]
         dReq(dem,t)             Demand requirements in a loop [Mm^3 per month]
         dreqBase(dem,t)         Base Demand requirements at demand sites [Mm^3 per month]
         dcap(k,dem)             Physical capacity of diversion links [Mm^3 per month] that feed demand sites
         instreamReq(j,k,t)      Minimum instream flow requirement as regulated by authories or required for other purposes such as hydropower or endangered species or water rights[m3 per month]
         cst(n)                  Unit cost of implementing the management objective of Revegetating species n [$]

         RA_par(v, RA_par_indx)  Reads the reservoir area volume curves parameters

         sf_par(j,k,sf_par_indx) Reads the stage-flow relationships parameters
         wf_par(j,k,wf_par_indx) Reads the width-flow relationships parameters
         wsi_par(j,k,t,wsi_par_indx)   Reads the wetlands suitability index monthly relationships parameters

         rsi_par(j,k,t,rsi_indx)     Reads the RSI equation parameters as a function of time for differnt life stages
         fci_par(j,k,t,fci_indx)     Reads the FCI equation parameters as a function of time for monthly flows


         InitSTOR(v)             Defines inital reservoir storage [Mm^3]
         InitD(j,k)              Defines inital river depth for the first time step [m]
         InitC(j,k)              Defines inital vegetation cover for the first time step [Mm^2]


         qmin(j,k,t)            Defines the physical and natural lower bounds of flow on links [Mm^3 per month] as defined by the channel geometry and historical records
         qmax(j,k,t)            Defines the physical and natural upper bounds of flow on links [Mm^3 per month] based on headflow received and reservoir capacity upstream of every link
         Qsim(j,k,t)            Flow values for simulation purposes [Mm^3 per month]
         CMax(j,k)              Maximum vegetation cover allowed based on site floodplain area and revegetation potentials [Mm2 per month]
         SimLink(j,k)           Defines the links where flow is fixed for the simulation case
         LinkExistSim(j,k)      Defines all other links where flow is not fixed for the simulation case

         g(j,k,t,n)             The natural growth area [Mm2 per month] of species n

         DValue(run)            Define parameters to run the model over multiple values of demand requirements to build a tradeoff curve

         QLoop(Qrun)            Define flow values for the flow sensivity analysis loop

         FlowMarginal(j,t)      A parameter defined to store the shadow values of flow mass balance

         h(j,k,t)               A decay value to define the rate of which the aquatic suitability index curve reaches a maximum of 1

         maxrv(j,k,t,n)         Maximum revegetation value
         minrv(j,k,t,n)         Minimum revegetation value
   ;


SCALARS
         b                       Total management budget to implement restoration actions [$]
*Parameters for unit conversion:
         Sqkm_ac                 Area unit conversion square km to acre /247.11 /
         Mm3mo_cfs               Flow unit conversion million cubic meter per month to cubic feet per second  /13.4289/
         Mm3_acft                Volume unit conversion million cuic meter to acre-feet / 810.714/
                       ;
*===================================================
* Read sets and parameter input values from Excel
*===================================================

*--------------------------------------------------------------------------------------------------------------------------------------------
*** Sample Network - use this to test the model. Includes 5 nodes, 1 reachgain, 1 reservoir, 1 demand site, a diversion a return flow

*$CALL GDXXRW.EXE input=WASH-SampleData_Feb2016.xlsx  output=WASH-sampleData.gdx Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  Set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1   Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=rsi_par rng=rsiEQ!A1 Rdim=1 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=1 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3    par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1 par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1 par=Qsim  rng=QSimulation!A1 Rdim=2  Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2  par=Qmax  rng=Qmax!A1 Rdim=2  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReq rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0  par=rv rng=Revegetate!A1 Rdim=3 Cdim=1  par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2
*$GDXIN WASH-sampleData.gdx
*--------------------------------------------------------------------------------------------------------------------------------------------


***Full network model with 3 Environmental Sites  (where field data are collected)
*$CALL GDXXRW.EXE input=WASH-Data-Apr2016.xlsx output=WASH-Data.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=1 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=1 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1 par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0  par=rv rng=Revegetate!A1 Rdim=3 Cdim=1  par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=MaxVegCover!A1 Rdim=3  Cdim=1 par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1


***Full network model with 19 Environmental Sites

*Difficulty loading last two parameters (minrv and maxrv). Maxrv is
*on a different worksheet (Vegetation). No minrv in the workbook.
*$CALL GDXXRW.EXE input=WASH_1yr_InputData.xlsx output=WASH-Data.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0   par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=CMax!A1 Rdim=2   par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1    set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1  par=g rng=NaturalGrowth!A1  Rdim=3  Cdim=1   par=maxrv  rng=RevegMax!A1 Rdim=3 Cdim=1   par=minrv  rng=RevegMin!A1 Rdim=3 Cdim=1
*Corrected
$CALL GDXXRW.EXE input=WASH_1yr_InputData.xlsx output=WASH-Data.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0   par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=CMax!A1 Rdim=2   par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1    set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1  par=g rng=NaturalGrowth!A1  Rdim=3  Cdim=1   par=maxrv  rng=Revegetate!A1 Rdim=3 Cdim=1   par=minrv
*Use to maximize aquatic objective
*$CALL GDXXRW.EXE input=WASH_1yr_InputDataMaxAquatic.xlsx output=WASH-Data.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0   par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=CMax!A1 Rdim=2   par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1    set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1  par=g rng=NaturalGrowth!A1  Rdim=3  Cdim=1   par=maxrv  rng=Revegetate!A1 Rdim=3 Cdim=1   par=minrv



*$CALL GDXXRW.EXE input=WASH_5yr_InputData.xlsx output=WASH-Data.gdx  Set=t rng=Month!A1:A100 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1 Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0   par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=Cmax!A1 Rdim=2   par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1    set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1  par=g rng=NaturalGrowth!A1  Rdim=3  Cdim=1   par=h   rng=h!A1   Rdim=3


*$CALL GDXXRW.EXE input=WASH_1site_InputData.xlsx output=WASH-Data_1site.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0  par=rv rng=Revegetate!A1 Rdim=3 Cdim=1  par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=Cmax!A1 Rdim=2   par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1    set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1  par=g rng=NaturalGrowth!A1  Rdim=3  Cdim=1



*$CALL GDXXRW.EXE input=Morton_InputData.xlsx  output=WASH-Data.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0  par=rv rng=Revegetate!A1 Rdim=3 Cdim=1  par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=MaxVegCover!A1 Rdim=3  Cdim=1 par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1    set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1  par=g rng=NaturalGrowth!A1  Rdim=3  Cdim=1


*For WASH Sensitivity Analysis
*$CALL GDXXRW.EXE input=SensitivityInput_Confluence_08312016.xlsx output=WASH-Data.gdx  Set=t rng=Month!A1:A12 Rdim=1  Set=s rng=SubInd!A1:A100 Rdim=1 set=y rng=FishSpp!A1:A10 Rdim=1 set=n rng=VegSpp!A1:A10 Rdim=1 Set=j rng=Nodes!A1:A100 Rdim=1 Set=wt rng=Wetlands!A1:A100 Rdim=1 Set=dem rng=Demand!A1:A100 Rdim=1  set=NodeNotDemandSite  rng=NodesNotDemand!A1:A100 Rdim=1 set=NodeNotHeadwater  rng=NodeNotHeadwater!A1:A100 Rdim=1  Set=v rng=Reservoirs!A1:A100 Rdim=1 Set=rsi_indx rng=rsiIndex!A1:A10 Rdim=1  Set=fci_indx rng=fciIndex!A1:A10 Rdim=1 par=LinkID  rng=LinkName!A1 Rdim=2   par=rsi_par rng=rsiEQ!A1 Rdim=3 Cdim=1  par=fci_par rng=fciEQ!A1 Rdim=3 Cdim=1 par=envSiteExist rng=EnvSite!A1 Rdim=1 Cdim=1  par=DiversionExist rng=Diversions!A1 Rdim=1 Cdim=1   par=returnFlowExist rng=ReturnFlow!A1 Rdim=1 Cdim=1 par=WetlandsExist rng=WetlandsSites!A1 Rdim=1 Cdim=1 par=LinktoReservoir rng=LinktoReservoir!A1 Rdim=1 Cdim=1 par=LinkOutReservoir rng=LinkOutReservoir!A1 Rdim=1 Cdim=1   Set=RA_par_indx rng=R_indx!A1 Rdim=1  Set=sf_par_indx rng=sf_indx!A1:A10 Rdim=1  Set=wf_par_indx rng=wf_indx!A1:A10 Rdim=1 Set=wsi_par_indx rng=wsi_indx!A1:A10 Rdim=1 par=reachGain rng=HeadFlow!A1 Rdim=1 Cdim=1   par=wght rng=weights!A1 Cdim=1 Rdim=3  par=aw rng=AW!A1 Rdim=3  par=lss rng=lss!A1 Rdim=2 Cdim=1 par=evap rng=evap!A1 Rdim=1 Cdim=1  par=linkEvap  rng=linkEvap!A1  Rdim=2  Cdim=1  par=RA_par rng=ResElevVol!A1 Rdim=1 Cdim=1  par=Qsim  rng=QSim!A1 Rdim=2 Cdim=1   par=Qmin  rng=Qmin!A1 Rdim=2 Cdim=1  par=Qmax  rng=Qmax!A1 Rdim=2 Cdim=1  par=Cons rng=Cons!A1 Rdim=1 Cdim=1 par=lng rng=Length!A1 Rdim=2   par=minstor rng=inactive!A1 Rdim=1  par=maxstor rng=capacity!A1 Rdim=1  par=dReqBase rng=demandReq!A1 Rdim=1 Cdim=1 par=dCap rng=divCap!A1 Rdim=2  par=instreamReq   rng=Instream!A1 Rdim=2 Cdim=1  par=cst rng=UnitCost!A1 Rdim=1  par=Linkexist rng=Connect!A1 Rdim=1 Cdim=1  par=sf_par rng=StageFlow!A1 Rdim=2 Cdim=1 par=wf_par rng=WidthFlow!A1 Rdim=2 Cdim=1   par=wsi_par rng=wp!A1 Rdim=3 Cdim=1  par=b rng=Budget!A1 Rdim=0  par=rv rng=Revegetate!A1 Rdim=3 Cdim=1  par=InitSTOR rng=InStor!A1 Rdim=1   par=InitD rng=InitD!A1 Rdim=2  par=InitC rng=InitC!A1 Rdim=2  par=CMax   rng=MaxVegCover!A1 Rdim=3  Cdim=1 par=SimLink  rng=SimLinks!A1 Rdim=2  par=LinkExistSim   rng=Connect_sim!A1 Cdim=1  Rdim=1  set= MassBalanceNodes rng=MassBalanceNodes!A1  Rdim=1  Set=run  rng=Runs!A1 Rdim=1   par=DValue  rng=DemandRuns!A1  Rdim=1  set=Qrun  rng=Qrun!A1  Rdim=1   par=QLoop   rng=QRunValues!A1  Rdim=1

*Write the input data into a GDX file
$GDXIN WASH-Data.gdx



*Load parameters and input data from the GDX file into the model
$LOAD s
$LOAD j
$LOAD dem
$LOAD v
$LOAD t
$LOAD y
$LOAD n
$LOAD LinkID
$LOAD envSiteExist
$LOAD linkexist
$LOAD DiversionExist
$LOAD returnFlowExist
$LOAD WetlandsExist
$LOAD LinktoReservoir
$LOAD LinkOutReservoir
$LOAD reachGain
$LOAD wght
$LOAD aw
$LOAD lss
$LOAD evap
$LOAD linkEvap
$LOAD RA_par_indx
$LOAD RA_par
$LOAD cons
$LOAD lng
$LOAD minstor
$LOAD maxstor
$LOAD dReqBase
$LOAD dCap
$LOAD instreamReq
$LOAD cst
$LOAD sf_par_indx
$LOAD sf_par
$LOAD wf_par_indx
$LOAD wf_par
$LOAD wsi_par_indx
$LOAD wsi_par
$LOAD rsi_indx
$LOAD fci_indx
$LOAD rsi_par
$LOAD fci_par
$LOAD b
$LOAD g
$LOAD InitSTOR
$LOAD InitD
$LOAD InitC
$LOAD Qmin
$Load Qmax
$LOAD QSim
$LOAD CMax
$LOAD SimLink
$LOAD LinkExistSim
$LOAD MassBalanceNodes
$LOAD Run
$LOAD DValue
$LOAD QRun
$LOAD QLoop
$LOAD maxrv
*$LOAD minrv
*$LOAD h

*Close GDX file
$GDXIN

*Set min vegetation removal to zero (could not load from GDX) - DER 9/25/2018
minrv(j,k,t,n) = 0;

* Display the input data values in the listing file. Go through these values to Check if the GDXXRW function reads and loads the parameters correctly into the model
Display s,j,dem, InitC, InitD, InitStor,b, wght ;
Display v,t,y, n, envSiteExist, linkexist              ;
Display DiversionExist, returnFlowExist, WetlandsExist      ;
Display LinktoReservoir, LinkOutReservoir, reachGain    ;
Display aw, lss, evap, linkEvap, RA_par_indx, RA_par       ;
Display cons,lng, minstor, maxstor, dReqBase, dCap, instreamReq, cst ;
Display sf_par_indx, sf_par, wf_par_indx, wf_par, wsi_par_indx, wsi_par, rsi_indx;
Display  fci_indx, fci_par, rsi_par, LinkID, Qmin, Qmax, QSim, CMax, SimLink, LinkExistSim, MassBalanceNodes, g ;
Display Run, DValue  ;
Display Qrun, QLoop, maxrv, minrv       ;

** HW#6 - Select an ecosystem objective to maximize. Turn off (set to zero) weights
**        for all other system components.
** Note: there will still be incidental ecosystem benefits for the other objectives
**       that the model does not see/count.
PARAMETER EcoCompToUse Ecosystem sytem component to optimize (1=riverine 2=floodplain 3=wetland 0=other) /0/;

*wght(s,j,k,t)$(ord(s) ne EcoCOmpToUse) = 1;


*===================
*Define Variables
*===================

VARIABLES
         Z             Objective function value of WASH that the model will try to maximize [Mm^2]
         Rsum          Sum of R values
         Ind(s,j,k,t)  Describes the summation of the model sub-indicators before applying any weights [Mm^2]
         Q(j,k,t)      Flow in links [Mm^3 per month]
         D(j,k,t)      Water depth in links [m]
         A(j,k,t)      River channel surface area in links [Mm^2]
         WD(j,k,t)     River channel width [m]
         C(j,k,t,n)    Vegetation cover of ripairn vegentation species n at environmental sites [Mm^2]
         RR(v,t)       Reservoir monthly releases volume [Mm^3 per month]
         STOR(v,t)     Reservoir monthly storage volume [Mm^3]
         RA(v,t)       Reservoir surface area as a function of reservoir storage [Mm^2]
         Shortage(dem,t) Demand shortage at each demand site [Mm^2]
         RV(j,k,t,n)     Defines revegetation areas of species n at environmental sites   [Mm^2]

         WSI(j,k,t)    Wetlands Suitability index   [unitless 0: poor -1: excellent]
         RSI(j,k,t,y)  Riverine Suitability Index   [unitless 0: poor -1: excellent]
         FCI(j,k,t,n)  Floodplain Suitability Index [unitless 0: poor -1: excellent]

         R(j,k,t)      Riverine habitat sub-indicator    [Mm^2]
         F(j,k,t)      Floodplain connectivity habitat sub-indicator  [Mm^2]
         W(j,k,t)      Wetlands habitat sub-indicator  [Mm^2]
;

*===============================================================
*Define initial values and bounds for the decision varaibles
*===============================================================

*-----------------
* Initial values
*-----------------

C.L(j,k,t,n)$envSiteExist(j,k)  =InitC(j,k);

*---------------------
* Bounds on Variables
*---------------------
* Constraints on Flow:        (COMMENT OUT for Model Simulation)

Q.UP(j,k,t)$LinkExist(j,k)= Qmax(j,k,t)$LinkExist(j,k) ;


* Constraints on vegetation cover
C.UP(j,k,t,n)$envSiteExist(j,k) = CMax(j,k);

* Contraints on other variables
Positive Variable  A, RV, WD, STOR, RA;



* To run the model as one instance without looping over demand  (comment out for demand loop)
dReq(dem,t) =    dreqBase(dem,t) ;

*----------------------------------------------------------------------------
* For Simulation: comment out the Q.UP line and use this line Q.FX instead
*----------------------------------------------------------------------------

*Q.FX(j,k,t)$SimLink(j,k)= QSim(j,k,t)$SimLink(j,k);





*============================
*Declare model equations
*============================

EQUATIONS
*-------------------
*Objective Function
*-------------------
         EQ1               Z     Objective function: Watershed Area of Suitable Habitat [Mm^2] and is calculated as the is composed of the weighted sum of the three sub-indicators (Ind): riparian [R] floodplain [F] and wetlands [W]
         EQ1a(s,j,k,t)     IND   Mapping the three perfomance indicators R and F and W variables into the s index [Mm^2]

         EQ2(j,k,t)        R     Riverine Habitat sub-indicator [Mm^2]
         EQ2a(j,k,t,y)     rsi   Riversine Suitability Index [unitless:0-1] as a function of flow for BCT
         EQ2b(j,k,t,y)     rsi   Riversine Suitability Index [unitless:0-1] as a function of flow for Browb Trout



         EQ3(j,k,t)        F     Floodplain habitat [Mm^2]
         EQ3a(j,k,t,n)     fci   floodplains suiability index [unitless:0-1] as a function of flow

         EQ4(j,k,t)        W     Impounded Wetland habitat [Mm^2]
         EQ4a(j,k,t)       wsi   Wetlands suitability index [unitless:0-1] as a function of flow

*-------------------
*Model Constraints
*-------------------

         EQ5(v,t)          Mass balance at reservoirs
         EQ5a(v,t)         Reservoir area-storage relationships
         EQ5b(v,t)         Reservoir releases equation that sums all flows leaving a reservoir
         EQ5c(v)           Storage balance for the last time step to be equal to inital storage
         EQ5d(v,t)        Initilize reservoir storage

         EQ6(j,t)          Mass balance equations at each node   - applies to all sites except demand sites
         EQ7(dem,t)        Mass balance at demand sites
         EQ7a(dem,t)       Demand Shortage equation

         EQ8(j,k,t)        Stage-flow relationships
         EQ9(j,k,t)        Width-flow relationships
         EQ10(j,k,t)       Channel surface area   [Mm^2]

         EQ11(j,k,t,n)     Vegetation cover mass balance
         EQ11a(j,k,t,n)    Constrains revegetation to growing season
         EQ11b(j,k,t,n)    Constrains revegetation to growing season
         EQ12(v,t)         Constrain reservoir storage to not go below inactive zone
         EQ12a(v,t)        Constrain rReservoir storage to not exceed reservoir capacity

         EQ13(dem,t)       Diversion flow volume to demand sites must meet demand delievery requirements
         EQ14(k,dem,t)     Diversions flow volume must not exceed diversion capacity

         EQ15(j,k,t)       Regulatory minimum instream flow requirements for ecological or hydropower or water rights purposes
         EQ16              All management actions should not exceed set budget

;


*============================
*Define Model Equations
*============================

EQ1..                                         Z =e= sum((s,j,k,t)$envSiteExist(j,k), wght(s,j,k,t) * IND(s,j,k,t))            ;


EQ1a(s,j,k,t)$envSiteExist(j,k)..             IND(s,j,k,t)$envSiteExist(j,k) =e= R(j,k,t)$(ord(s) eq 1) + F(j,k,t)$(ord(s) eq 2) +  W(j,k,t)$(ord(s) eq 3)  ;



EQ2(j,k,t)$(envSiteExist(j,k)and wght("riverine",j,k,t) )..               R(j,k,t)  =e=  prod((y), rsi(j,k,t,y) * A(j,k,t))                 ;

EQ2a(j,k,t,y)$(envSiteExist(j,k)and wght("riverine",j,k,t) and rsi_par(j,k,t,"Func_typ") eq 1 )..            rsi(j,k,t,y) =e= (rsi_par(j,k,t,"rsi_par1")+((rsi_par(j,k,t,"rsi_par2") - rsi_par(j,k,t,"rsi_par1")) / (1+exp((rsi_par(j,k,t,"rsi_par3") - D(j,k,t) )/rsi_par(j,k,t,"rsi_par4") ))))     ;
EQ2b(j,k,t,y)$(envSiteExist(j,k)and wght("riverine",j,k,t) and rsi_par(j,k,t,"Func_typ") eq 2 )..            rsi(j,k,t,y) =e= 1-(exp((-rsi_par(j,k,t,"rsi_par4"))*D(j,k,t))) ;




EQ3(j,k,t)$( envSiteExist(j,k)and wght("floodplain",j,k,t) )..             F(j,k,t) =e= sum((n),fci(j,k,t,n) * C(j,k,t,n))                    ;
EQ3a(j,k,t,n)$( envSiteExist(j,k)and wght("floodplain",j,k,t) )..          fci(j,k,t,n) =e= (fci_par(j,k,t,"fci_par1")+((fci_par(j,k,t,"fci_par2") - fci_par(j,k,t,"fci_par1")) / (1+exp((fci_par(j,k,t,"fci_par3") - Q(j,k,t) )/fci_par(j,k,t,"fci_par4") ))))     ;


EQ4(j,k,t)$(WetlandsExist(j,k) and wght("wetlands",j,k,t) )..              W(j,k,t) =e= wsi(j,k,t) * aw(j,k,t)                        ;
EQ4a(j,k,t)$(WetlandsExist(j,k) and wght("wetlands",j,k,t))..              wsi(j,k,t) =e=   wsi_par(j,k,t,"wsi_par1") * Q(j,k,t) + wsi_par(j,k,t,"wsi_par2")         ;

* Model Constraints


EQ5(v,t)..                                    STOR(v,t+1) =e= InitSTOR(v)$(ord(t) eq 1)+ STOR(v,t)$(ord(t) gt 1)+sum (j$LinktoReservoir(j,v), Q(j,v,t)*(1-lss(j,v,t))) - RR(v,t) - (evap(v,t)*RA(v,t)) ;

EQ5a(v,t)..                                   RA(v,t) =e= RA_par(v, "RA_par1") *(STOR(v,t)**2) +RA_par(v, "RA_par2")*STOR(v,t) + RA_par(v, "RA_par3")          ;
EQ5b(v,t)..                                   RR(v,t) =e= sum(j$LinkOutReservoir(v,j), Q(v,j,t) )  ;
EQ5c(v)..                                     sum(t$(ord(t) eq card(t)),STOR(v,t)) =e= InitSTOR(v) ;
EQ5d(v,t)..                                   STOR(v,t)$(ord(t) eq 1) =l= InitStor(v)  ;

EQ6(j,t)$(MassBalanceNodes(j))..            reachGain(j,t) + sum(k$linkexist(k,j), (Q(k,j,t) * (1-lss(k,j,t)))) - sum(k$linkexist(k,j), (A(k,j,t) * linkEvap(k,j,t))) =g= sum (k$linkexist(j,k), Q(j,k,t))  ;

EQ7(dem,t)..                                  sum (k$DiversionExist(k,dem),Q(k,dem,t) * (1-lss(k,dem,t))) - sum (k$DiversionExist(k,dem),Q(k,dem,t) * Cons(dem,t)) =g=  sum(k$returnFlowExist(dem,k), Q(dem,k,t))   ;
EQ7a(dem,t)..                                  Shortage(dem,t) =e=  dReq(dem,t) - sum(k$linkexist(k,dem), Q  (k,dem,t) ) ;


EQ8(j,k,t)$(envSiteExist(j,k) and (wght("riverine",j,k,t)) ) ..                D(j,k,t)=e= sf_par(j,k,"sf_par1")*Q(j,k,t) + sf_par(j,k,"sf_par2")                  ;
EQ9(j,k,t)$(envSiteExist(j,k) and (wght("riverine",j,k,t)) )..                 WD(j,k,t) =e= wf_par(j,k,"wf_par1")* Q(j,k,t) + wf_par(j,k,"wf_par2")            ;
EQ10(j,k,t)$(envSiteExist(j,k) and (wght("riverine",j,k,t)) )..                A(j,k,t)=e= (WD(j,k,t)* lng(j,k)) / 1000000          ;

EQ11(j,k,t,n)$envSiteExist(j,k)..             C(j,k,t,n) =e= InitC(j,k)$(ord(t) eq 1) + C(j,k,t-1,n)$(ord(t) gt 1) + RV(j,k,t,n)$(ord(t) gt 1) + g(j,k,t,n)$(ord(t) gt 1) ;
EQ11a(j,k,t,n)$envSiteExist(j,k)..            RV(j,k,t,n) =l= maxrv(j,k,t,n) ;
EQ11b(j,k,t,n)$envSiteExist(j,k)..            RV(j,k,t,n) =g= minrv(j,k,t,n) ;


EQ12(v,t)..                                   STOR(v,t) =g= minstor(v)                    ;
EQ12a(v,t)..                                  STOR(v,t) =l= maxstor(v)                    ;

EQ13(dem,t)..                                 sum(k$DiversionExist(k,dem), Q(k,dem,t)* (1 -lss(k,dem,t))) =g= dReq(dem,t);


EQ14(k,dem,t)$DiversionExist(k,dem)..         Q(k,dem,t) =l= dcap(k,dem)                           ;

EQ15(j,k,t)$LinkExist(j,k)..                  Q(j,k,t) =g= instreamReq(j,k,t)                                            ;

EQ16..                                        sum((j,k,t,n)$envSiteExist(j,k), cst(n) * RV(j,k,t,n)) =l= b                ;



*-------------------------------------------------------
*Control number of rows and columns in the listing file
*-------------------------------------------------------
option limrow = 100000  ;
option limcol = 100000  ;

*Control maximum running time (in seconds)
option reslim = 100000000  ;

*Set BARON's solution to global
option optcr = 0 ;



* instruct BARON to return numsol solutions
$onecho > baron.opt
numsol 100
gdxout multsol
$offecho


*=======================================================================
* Solve the model
*=======================================================================

*Define the model and solve all equations listed above
Model WASH /all/;



*WASH.optfile =1;
*option nlp=convert;

*sed -n -e "s:^ *\([exbi][0-9][0-9]*\) \(.*\):s/\1/\2/g:gp" dict.txt | sed -n '1!G;h;$p' > mod.txt
*sed -f mod.txt gams.gms




*Solve the model
Solve WASH maximizing Z using NLP;



*execute 'call ..\SobolFolder\SobolBatch2.bat'

*execute '=..\SobolFolder\SobolReadWrite.exe'




Display WASH.ModelStat, WASH.SolveStat, Z.L, WSI.L, W.L, FCI.L, F.L, RSI.L, R.L, Ind.L, D.l , Q.L, C.L, A.l, WD.L, RR.L, STOR.L, dreq, RV.L, RA.L, evap, wght ;

*execute_unload "testgdx.gdx";
*Convert and display results in U.S. units:


**Unit Coversion
Z.L  = Z.L  * Sqkm_ac ;
Q.L(j,k,t)  = Q.L(j,k,t)  * Mm3mo_cfs;
R.L(j,k,t)  = R.L(j,k,t)  * Sqkm_ac ;
F.L(j,k,t)  = F.L(j,k,t)  * Sqkm_ac ;
W.L(j,k,t)  = W.L(j,k,t)  * Sqkm_ac ;
RR.L(v,t) = RR.L(v,t) * Mm3mo_cfs;
STOR.L(v,t) = STOR.L(v,t) * Mm3_acft;

FlowMarginal(j,t)  = EQ6.m(j,t) ;

**** Save some vegetation equation outputs as parameters so can read into R
PARAMETERS EQ11_m(j,k,t,n) Equation 11 marginal (vegetation mass balance)
     EQ11a_m(j,k,t,n) Equation 11a marginal (upper bound on revegetation);

EQ11_m(j,k,t,n) = EQ11.m(j,k,t,n);
EQ11a_m(j,k,t,n) = EQ11a.m(j,k,t,n);


* Dump all input data and results to a GAMS gdx file
Execute_Unload "WASH_1yr_OutputData.gdx";
* Dump all the input data and results to an Excel file
Execute "gdx2xls WASH_1yr_OutputData.gdx"

*************************
*End of Code
*************************
