# Load and plot WASH model vegetation results to generate Figures 6 and 10
#
# David E. Rosenberg
# February 2, 2019
# Updated April 4, 2019
# david.rosenberg@usu.edu

# Draws from:
#Ayman Alafifi's code - https://github.com/ayman510/WASH/blob/master/UncertaintyAnalysis/2.ClusterAnalysis/cluster_all_par.R

# To RUN:
# Go here and download the package: http://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r

# Windows installation: In the R environment, navigate to the directory containing the binary package file 
#(see download links above), and, substituting the version number for a.b.c, type: install.packages("gdxrrw_a.b.c.zip") or, 
# depending on the version of R, install.packages("gdxrrw_a.b.c.zip",repos=NULL)


#Some help and examples here: http://ftp.gamsworld.org/presentations/informs2012_gdxrrw.pdf


# The overview of steps is:
# 1) Load in required libries
# 2) Read WASH model results from GAMS gdx file
# 3) Manipulate the data into form for plotting
# 4) Plot the figures
# 5) Also code to generate other figures not shown in the manuscript


rm(list = ls())  #Clear history

# Load all packages and libraries

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(gdxrrw)) { 
  download.file("http://support.gams.com/lib/exe/fetch.php?media=gdxrrw:gdxrrw_1.0.4_r351.zip","gdxrrw_1.0.4.zip") 
  install.packages("gdxrrw_1.0.4.zip",repos=NULL) 
  library(gdxrrw) 
}

if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) 
}


if (!require(ggplot2)) { 
  install.packages("ggplot2",repos="http://cran.r-project.org") 
  library(ggplot2) 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) 
}


if (!require(tidyr)) { 
  install.packages("tidyr",repos="http://cran.r-project.org") 
  library(tidyr) 
}


# Check the gams gdx interface is working
igdx("") 

###Set working directory
### Assume you are in the correct directory which is ../OutputFiles/VegetationPlots
#  The GAMS gdx file with results needs to be in ../OutputFiles

# Read in Node Names
dfNodeNames <- read.csv(file="NodeNames.csv", 
                        header=TRUE, 
                        #col.names = c("year", "journal", "title"), 
                        stringsAsFactors=FALSE,
                        #fileEncoding="UTF-8-BOM",
                        sep=",")

###############################################
## Read in Deterministic Model Results
###############################################
#Location of the GAMS gdx file with WASH results relative to current folder:
gdxfile <- "../WASH_1yr_OutputData.gdx"

# Output summary of gdx file contents to console
#gdxInfo(gdxName = gdxfile, dump=FALSE, returnList=TRUE, returnDF=TRUE)

#WASH
Z_det <- rgdx(gdxfile, list(name="Z", form = "full"))$val

#Aquatic
R_det <- rgdx(gdxfile, list(name="R", form = "full"))$val

#Floodplain
F_det <- rgdx(gdxfile, list(name="F", form = "full"))$val

#Wetlands
W_det <- rgdx(gdxfile, list(name="W", form = "full"))$val
#do.call(sum,W_det)
sum(W_det)

##ReachGain
ReachGain_det <- rgdx.param(gdxfile, "reachGain", names=c("River","Time","Value"),squeeze =FALSE)
#sum reach Gain
ReachGain_opt <- ReachGain_det %>%
  summarise(ReachGain_Mm3yr= sum(Value))
head(ReachGain_opt)

##Demand
Demand_det <- rgdx.param(gdxfile,"dReq", names=c("DemandSite","Time","Value"),squeeze =FALSE)
#sum Demand
Demand_opt <- Demand_det %>%
  summarise(Demand_Mm3yr= sum(Value))
head(Demand_opt)

#Reservoir Releases
Release_det <- rgdx(gdxfile,list(name="RR", form = "full"))$val
#filter releases
Release_det_hyrum <- matrix(Release_det[4,], nrow = 12, byrow = TRUE)
Release_det_porc <- matrix(Release_det[5,],  nrow = 12, byrow = TRUE)

#Vegetation and other reach data
Revegetate_det <- rgdx(gdxfile,list(name="RV", form = "full"))$val
#Cover
Cover_det <- rgdx(gdxfile,list(name="C", form = "full"))$val
#Maximum Cover
dfCmax <- rgdx.param(gdxfile, "CMax", names=c("j","k","Value"),squeeze =FALSE)
#Maximum revegetation
dfMaxRV <- rgdx.param(gdxfile, "maxRV", names=c("j","k","t","species","Value"),squeeze =FALSE)
# Flow data
Q_flow <- rgdx(gdxfile,list(name="Q", form = "full"))$val
#Pull in the shadow value (lagrange multiplier results for cover and revegetation, saved as parameters in GDX)
vegbal_eq <- rgdx(gdxfile,list(name="EQ11_m", form = "full"))$val
reveg_eq <- rgdx(gdxfile,list(name="EQ11a_m", form = "full"))$val

#Convert to dataframes (do not need for dfCMax and dfMaxRV (already dataframe))
dfReveg <- as.data.frame.table(Revegetate_det)
dfCover <- as.data.frame.table(Cover_det)
dfQflow <- as.data.frame.table(Q_flow)
dfFhsi <- as.data.frame.table(F_det)
dfVegBalEQ<- as.data.frame.table(vegbal_eq)
dfRevegEQ<- as.data.frame.table(reveg_eq)

#Make varaiables for each desired item
dfReveg$Reveg <- dfReveg$Freq
dfCover$Cover <- dfCover$Freq
dfMaxRV$MaxRV <- dfMaxRV$Value
dfCmax$Cmax <- dfCmax$Value
#Truncate to three decimal places
dfVegBalEQ$CoverMarg <- round(dfVegBalEQ$Freq,3)
dfRevegEQ$RevegMarg <- round(dfRevegEQ$Freq,3)
dfFhsi$Fhsi <- dfFhsi$Freq
dfQflow$Qflow <- dfQflow$Freq

# Define the variable names we'll use in a the composite data frame dfResults
cVarNames <- c("reach","reach.break", "t","Reveg","Cover","MaxRV","Cmax","CoverMarg","RevegMarg","Fhsi","Qflow")

#Merge all the data into one dataframe
dfResults <- merge(dfReveg,dfCover[,c("j","k","t","Cover")],by=c("j","k","t"), all.x=TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfMaxRV[,c("j","k","t","MaxRV")],by=c("j","k","t"), all.x=TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfCmax[,c("j","k","Cmax")],by=c("j","k"), all.x=TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfVegBalEQ[,c("j","k","t","CoverMarg")],by=c("j","k","t"), all.x=TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfRevegEQ[,c("j","k","t","RevegMarg")],by=c("j","k","t"), all.x=TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfFhsi[,c("j","k","t","Fhsi")],by=c("j","k","t"), all.x=TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfQflow[,c("j","k","t","Qflow")],by=c("j","k","t"), all.x=TRUE, sort=TRUE)

#Create Reach Names of the form River: Start to Finish (jX to jY)   [reach.break]
#Create Reach Names of the form River: Start to Finish (jX to jY)   [reach]
# Merge on Names to give a full reach name for the form River: Start to Finish
dfResults <- merge(dfResults,dfNodeNames, by=c("j"), all.x = TRUE, sort=TRUE)
dfResults <- merge(dfResults,dfNodeNames, by.x = "k", by.y="j", all.x = TRUE, sort=TRUE)
#dfResults$River.x.break <- paste(dfResults$River.x, "River\n", sep=" ")
#dfResults$River.x <- paste(dfResults$River.x, "River:", sep=" ")

#Without "River"
dfResults$River.x.break <- paste(dfResults$River.x, "\n", sep="")
dfResults$River.x <- paste(dfResults$River.x, ":", sep="")

dfResults$NodeName.x <- paste(dfResults$NodeName.x, "to", sep=" ")
# Node abbreviations
dfResults$reachAbr <- do.call(paste, c(dfResults[c("j","k")],sep = " to "))
dfResults$reachAbrPar <- paste("(",dfResults$reachAbr,")", sep="")
#Finally create the reach names, two versions (with and without a \n)
dfResults$reach <- do.call(paste, c(dfResults[c("River.x","NodeName.x","NodeName.y","reachAbrPar")],sep = " "))
dfResults$reach.break <- do.call(paste, c(dfResults[c("River.x.break","NodeName.x","NodeName.y","reachAbrPar")],sep = " "))

#Filter on rows 
#Find the reaches that have a non zero value for Revegetation or Cover in at least one time step
#Total by Reach
dfResults$TotCoverReveg <- dfResults$Cover + dfResults$Reveg

TotalCoverReveg <- dfResults[,c("reach","TotCoverReveg","River.x")] %>%
  #complete(t,reach, fill = list(TotCoverReveg = 0)) %>%
  group_by(reach,River.x) %>%
  summarise(TotalCover = sum(TotCoverReveg, na.rm=TRUE))

ReachesNonZero <- subset(TotalCoverReveg,TotalCover > 0)
#Find the rows that have one of the non zero reaches
dfResults$KeepRow <- apply(as.matrix(dfResults[,"reach"]), 1, function(r) any(r %in% ReachesNonZero$reach))

#Order the result rows by Reach and Time
dfResults <- dfResults[order(dfResults$reachAbrPar,dfResults$t), ]
#Filter on the rows that have at least one non-zero Cover or Revegetation
dfResultsFilt <- subset(dfResults,dfResults$KeepRow == "TRUE")

#Transform the dataframe so there are variable, value columns
dfResultsMelt <- melt(dfResultsFilt[,cVarNames],id.vars = c("reach","reach.break","t"))

#Axis plot ratios (for left-right plots)
AxisRatio <- max(dfResultsFilt$Cover)/max(dfResultsFilt$Qflow)
VegRatio = max(dfResultsFilt$Fhsi)/max(dfResultsFilt$Cover)

#Colors for plots
cFills <- c('#6baed6', '#3182bd', '#08519c') #Fill colors for bars. Light blue, medium blue, dark blue

### PLOTS
cMonths <- 1:12
cMonthsLabels <- month.abb[cMonths]

#Plot as a bar graph with Cover and Revegatation
#Filter and sort the dataset
dfPlotData <- dfResultsMelt %>% filter(variable %in% c("Cover","Reveg")) %>% arrange(reach,desc(variable))

ggplot() +
  geom_bar(data=dfPlotData, aes(x=t,y=value, fill=variable), stat="identity", position="identity", size=0.5) +
  scale_fill_manual(breaks=c("Cover","Reveg"), labels = c("Cover (left)","Planted Area (left)"), values= c("green","darkgreen")) +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Area (' ~Mm^2*')')) +
  theme(legend.title=element_blank()) + theme_bw()


#Plot RV and MaxRV as stacked bars
dfPlotData <- dfResultsMelt %>% filter(variable %in% c("MaxRV","Reveg")) %>% arrange(reach,desc(variable))

ggplot() +
  geom_bar(data=dfPlotData, aes(x=t,y=value, fill=variable), stat="identity", position = "identity", size=0.5) +
  scale_fill_manual(breaks=c("MaxRV","Reveg"), labels = c("Max Planting Area","Planted area"), values= c("green","darkgreen")) +
  theme_bw() +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Area (' ~Mm^2*')')) +
  theme(legend.title=element_blank())


#Plot both Shadow values as bars 
dfPlotData <- dfResultsMelt %>% filter(variable %in% c("CoverMarg","RevegMarg")) %>% arrange(reach,desc(variable))

ggplot() +
  geom_bar(data = dfPlotData, aes(x=t,y=value, fill=variable), stat="identity", position = "dodge", size=0.5) +
  scale_fill_manual(breaks=c("CoverMarg","RevegMarg"), labels = c("Vegetation balance (Eq. 11)","Planting constraint (Eq. 11a)"), values= c("green","darkgreen")) +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Shadow value (' ~Mm^2/Mm^2*')'), color="") +
  theme(legend.title=element_blank()) + theme_bw()


#Plot Vegetation growth marginals as lines

ggplot() +
  geom_line(data = dfResultsFilt, aes(x=t,y=abs(CoverMarg), color=reach, group=reach), stat="identity", position = "identity", size=1) +
  #scale_fill_manual(breaks=c("CoverMarg","RevegMarg"), labels = c("Vegetation balance (Eq. 11)","Planting constraint (Eq. 11a)"), values= c("green","darkgreen")) +
  #facet_wrap(~ reach, ncol=3) + 
  theme_bw() +
  labs(x="Month", y=bquote('Shadow value (' ~Mm^2/Mm^2*')'), color="Reach")
  #theme(legend.title="Reach") +
  

#Plot vegeation growth marginals as bars
ggplot() +
  geom_bar(data = dfResultsFilt, aes(x=t,y=abs(CoverMarg)), stat="identity", position = "identity", size=1) +
  #scale_fill_manual(breaks=c("CoverMarg","RevegMarg"), labels = c("Vegetation balance (Eq. 11)","Planting constraint (Eq. 11a)"), values= c("green","darkgreen")) +
  facet_wrap(~ reach, ncol=3) + 
  theme_bw() +
  labs(x="Month", y=bquote('Shadow value (' ~Mm^2/Mm^2*')'))

ggsave("VegetationMassBalanceMarginal.png", width=9, height = 6.5, units="in")
 


######### FIGURE 10 #################

#Plot vegetation growth marginals as stacked bars
dfPlotData <- dfResultsMelt %>% filter(reach %in% ReachesNonZero$reach)
dfPlotData <- dfPlotData %>% filter(variable %in% c("CoverMarg")) %>% arrange(reach,desc(variable))
dfPlotData$value <- abs(dfPlotData$value)
dfPlotData <- dfPlotData[order(dfPlotData$t,dfPlotData$value),]

#Sum by month
dfTotMarg <- dfPlotData %>%
  #complete(t,reach, fill = list(TotCoverReveg = 0)) %>%
  group_by(t) %>%
  summarise(TotalMarg = sum(value, na.rm=TRUE))

# Count reaches per river
dfRiverReaches <- ReachesNonZero %>%
  group_by(River.x) %>%
  summarize(count=n())

#Count rivers with more than one reach
NumRiversMultiReach <- length((dfRiverReaches[dfRiverReaches$count > 1,]$count))
NumReachesMulti <- sum(dfRiverReaches[dfRiverReaches$count > 1,]$count)
TotReaches <- sum(dfRiverReaches$count, na.rm = FALSE)
  
dfRiverReaches <- dfRiverReaches[order(-dfRiverReaches$count),]
cPals <- c("Blues","Greens", "Purples","Set1")
cPalComb <- vector()

## Construct the color pallete -- a stepped sequential scheme. Each river with multiple reaches gets a different color family
for (i in 1:NumRiversMultiReach){
  NumReaches <- as.numeric(dfRiverReaches[i,2])
  if (NumReaches > 9) { #ramp the colors
    getPalette = colorRampPalette(brewer.pal(9, cPals[i]))
    PalTemp <- getPalette(NumReaches+2) # avoid white
  } else { # Pull as is
    PalTemp <- brewer.pal(NumReaches+2, cPals[i])
  }
  
  print (NumReaches)
  print(PalTemp)
  print(PalTemp[2:(NumReaches+1)])
  cPalComb <- c(cPalComb,PalTemp[2:(NumReaches+1)])  
  #cPalComb <- c(cPalComb,PalTemp)  
  }

#Draw colors for remaining single reach rivers from Set1
ColOffset <- 4 # start on the third color
PalLast <- brewer.pal(TotReaches-NumReachesMulti+ColOffset,"Set1")
print(PalLast)
cPalComb <- c(cPalComb,PalLast[(ColOffset+1):(TotReaches-NumReachesMulti+1+ColOffset)])


ggplot(dfPlotData, aes(x=t,y=value,fill=reach)) +
  geom_bar(stat="identity") +
  theme_bw() +
  #scale_fill_manual(values = getPalette(colorCount)) +
  scale_fill_manual(values = cPalComb) +
  scale_x_discrete(labels= cMonthsLabels) +
  theme(legend.position = c(0.72,0.70), text = element_text(size=18), legend.text=element_text(size=11), legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  #labs(x="", y=bquote('Shadow value\n(Suitable area [' ~ Mm^2* ~ '] per Vegetated area [' ~ Mm^2* ~ '])'), fill="Reach")
  labs(x="", y='Shadow value\n(Suitable area per Vegetated area)', fill="River (color) and Reach (color intensity)")

ggsave("Fig10_MargVegValues.png", width=10, height = 8.5, units="in")
ggsave("Fig10_MargVegValues.pdf", width=10, height = 8.5, units="in")

############ END FIGURE 10 ###############


#Plot cover as bar and maximum cover as line
ggplot() +
  geom_bar(data=dfResultsFilt, aes(x=t,y=Cover, fill="green"), stat="identity", position = "identity", size=0.5) +
  geom_line(data = dfResultsFilt, aes(x=t, y=Cmax, color = "black", group=1), stat="identity", position = "identity", size=1.5) +
  
  scale_fill_manual(breaks=c("Cover"), labels = c("Cover"), values= c("green")) +
  scale_color_manual(breaks=c("Cmax"), labels = c("Maximum cover"), values= c("black")) +
  #scale_y_continuous(trans ='log10') +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Area (' ~Mm^2*')')) +
  scale_x_discrete(labels= cMonthsLabels) +
  theme(legend.title=element_blank()) +theme_bw()



#Plot Revegetated area and max revegetated area as lines
ggplot() +
  #Vegatation area
  geom_line(data=dfResultsFilt, aes(x=t,y=Reveg,group=1), stat="identity", position = "identity", size=1) +
  #Flow
  geom_line(data = dfResultsFilt, aes(x=t, y=MaxRV, group=1), stat="identity", position = "identity", size=2) +
  scale_color_manual(breaks=c("Reveg","MaxRV"), labels = c("Planted area","Max planting area"), values= c("green","darkgreen")) +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Area (' ~Mm^2*')')) +
  scale_x_discrete(labels= cMonthsLabels) +
  theme(legend.title=element_blank()) + theme_bw()


#Plot as a bar graph with Cover and Revegatation and line graph of flow and revegetated area
# All reaches!!
dfPlotData <- dfResultsMelt %>% filter(variable %in% c("Cover","Reveg")) %>% arrange(reach,desc(variable))
dfPlotData2 <- dfResultsMelt %>% filter(variable %in% c("Qflow")) %>% arrange(reach,desc(variable))
dfPlotData3 <- dfResultsMelt %>% filter(variable %in% c("Fhsi")) %>% arrange(reach,desc(variable))

#Set up the color scheme: light and dark green for the cover and Reveg. Blue for Flow and Red for suitable area
palGreens <- brewer.pal(7, "Greens")
palGreensUse <- palGreens[c(4,2)]

ggplot() +
    geom_bar(data=dfPlotData, aes(x=t,y=value, fill=variable), stat="identity", position = "identity", size=0.5) +
    #Habitat suitability
    geom_line(data=dfPlotData3, aes(x=t,y=value/(VegRatio/2), color="Suitable area (x30, left)", group=variable), stat="identity", position = "identity", size=1.5) +
  
  # Secondary axis of flow
    geom_line(data = dfPlotData2, aes(x=t, y=value*AxisRatio, color="Flow (right)", group=variable), stat="identity", position = "identity", size=1.5, linetype=2) +
 
    scale_y_continuous(sec.axis = sec_axis(~./AxisRatio, name = bquote('Flow (' ~Mm^3*'per month)'))) +
    scale_fill_manual(breaks=c("Cover","Reveg"), labels = c("Cover (left)","Planted Area (left)"), values= palGreensUse) +
    #scale_color_manual(breaks=c("Fhsi","Qflow"), labels = c("Suitable area (x30, left)","Flow (right)"), values= c("black",cFills[3])) +
    scale_color_manual(values= c(cFills[3],"red")) +
    scale_x_discrete(labels= cMonthsLabels) +
    theme_bw() +
      facet_wrap(~ reach, ncol=3) + labs(x="", y=bquote('Area (' ~Mm^2*')')) +
    theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=14)) 


###### FIGURE 6 #########################

#Plot as a bar graph with Cover and Revegatation and line graph of flow and revegetated area
# With filtered reaches

cReachFilts <- data.frame("reachAbr" = c("j1 to j4", "j20 to j21", "j24 to j7"))
#cReachFilts <- data.frame("reachAbr" = c("j24 to j7"))
cReachFiltsFull <- dfResultsFilt %>% filter(reachAbr %in% cReachFilts$reachAbr)
dfResultsMeltFilt <- dfResultsMelt %>% filter(reach %in% cReachFiltsFull$reach)

dfPlotData <- dfResultsMeltFilt %>% filter(variable %in% c("Cover","Reveg")) %>% arrange(reach,desc(variable))
dfPlotData2 <- dfResultsMeltFilt %>% filter(variable %in% c("Qflow")) %>% arrange(reach,desc(variable))
dfPlotData3 <- dfResultsMeltFilt %>% filter(variable %in% c("Fhsi")) %>% arrange(reach,desc(variable))

ggplot() +
  geom_bar(data=dfPlotData, aes(x=t,y=value, fill=variable), stat="identity", position = "identity", size=0.5) +
  #Habitat suitability
  geom_line(data=dfPlotData3, aes(x=t,y=value/(VegRatio/4), color="Suitable area (x30, left)", group=variable), stat="identity", position = "identity", size=2) +
  
  # Secondary axis of flow
  geom_line(data = dfPlotData2, aes(x=t, y=value*AxisRatio, color="Flow (right)", group=variable), stat="identity", position = "identity", size=2, linetype=2) +
  
  scale_y_continuous(sec.axis = sec_axis(~./AxisRatio, name = bquote('Flow (' ~Mm^3*' per month)'))) +
  scale_fill_manual(breaks=c("Cover","Reveg"), labels = c("Cover (left)","Planted Area (left)"), values= palGreensUse) +
  #scale_color_manual(breaks=c("Fhsi","Qflow"), labels = c("Suitable area (x30, left)","Flow (right)"), values= c("black",cFills[3])) +
  scale_color_manual(values= c(cFills[3],"red")) +
  scale_x_discrete(labels= cMonthsLabels) +
  theme_bw() +
  facet_wrap(~ reach.break, ncol=3,labeller = labeller(groupwrap = label_wrap_gen(10))) + labs(x="", y=bquote('Area (' ~Mm^2*')')) +
  theme(legend.position = c(0.84,0.85), text = element_text(size=18), legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  theme(text = element_text(size=20), legend.title=element_blank(), 
        legend.text=element_text(size=12),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        strip.text.x = element_text(size=10)) 

# These files don't save very well. Text is usally clipped or stuff is off
ggsave("Fig6_SuitableareaCoverPlantAreaFlow.pdf", width=9, height = 6.5, units="in")
ggsave("Fig6_SuitableareaCoverPlantAreaFlow.png", width=9, height = 6.5, units="in")

######## END OF FIGURE 6 #####################



#Plot as habitat suitability, cover, planted area, flow -- all as lines
dfPlotData <- dfResultsMelt %>% filter(variable %in% c("Cover")) %>% arrange(reach,desc(variable))
dfPlotData2 <- dfResultsMelt %>% filter(variable %in% c("Reveg")) %>% arrange(reach,desc(variable))
dfPlotData3 <- dfResultsMelt %>% filter(variable %in% c("Fhsi")) %>% arrange(reach,desc(variable))
dfPlotData4 <- dfResultsMelt %>% filter(variable %in% c("Qflow")) %>% arrange(reach,desc(variable))

ggplot() +
  #Habitat suitability
  geom_line(data=dfPlotData3, aes(x=t,y=value/VegRatio, color=variable, group = variable), stat="identity", position = "identity", size=1.5) +
  #Planted area
  geom_line(data=dfPlotData2, aes(x=t,y=value, color=variable, group = variable), stat="identity", position = "identity", size=1.5) +
  
  #Vegatation area
  geom_line(data=dfPlotData, aes(x=t,y=value, color=variable, group = variable), stat="identity", position = "identity", size=1.5) +
  #Flow
  geom_line(data = dfPlotData4, aes(x=t, y=value*AxisRatio, color = variable, group=variable), stat="identity", position = "identity", size=2) +
  scale_y_continuous(sec.axis = sec_axis(~./AxisRatio, name = bquote('Flow (' ~Mm^3*'per month)'))) +
  #scale_color_manual(breaks=c("Cover","Reveg","Freq"), labels = c("Cover (left)","Planted Area (left)", "Flow (right)"), values= c("darkgreen",cFills[3],"green")) +
  scale_color_manual(breaks=c("Fhsi", "Cover","Reveg","Qflow"), labels = c("Suitable area (left)","Cover (left)","Planted Area (left)","Flow (right)"), values= c("darkgreen","black",cFills[3],"green")) +
  theme_bw() +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Area (' ~Mm^2*')')) +
  theme(legend.title=element_blank()) 

ggsave("SuitabilityCoverPlantareaFlow.png")


#Plot the area and flow data as lines
ggplot() +
  #Planted area
  geom_line(data=dfPlotData2, aes(x=t,y=value, color=variable, group = variable), stat="identity", position = "identity", size=1.5) +
  
  #Vegatation area
  geom_line(data=dfPlotData, aes(x=t,y=value, color=variable, group = variable), stat="identity", position = "identity", size=1.5) +
  #Flow
  geom_line(data = dfPlotData4, aes(x=t, y=value*AxisRatio, color = variable, group=variable), stat="identity", position = "identity", size=2) +
  scale_y_continuous(sec.axis = sec_axis(~./AxisRatio, name = bquote('Flow (' ~Mm^3*'per month)'))) +
  #scale_color_manual(breaks=c("Cover","Reveg","Freq"), labels = c("Cover (left)","Planted Area (left)", "Flow (right)"), values= c("darkgreen",cFills[3],"green")) +
  scale_color_manual(breaks=c("Cover","Reveg","Qflow"), labels = c("Cover (left)","Planted Area (left)","Flow (right)"), values= c("darkgreen",cFills[3],"green")) +
  theme_bw() +
  facet_wrap(~ reach, ncol=3) + labs(x="Month", y=bquote('Area (' ~Mm^2*')')) +
  theme(legend.title=element_blank()) 

ggsave("CoverPlantareaFlow.png")

#Just plot the flow data
ggplot() +
  geom_line(data = dfResultsFilt, aes(x=t, y=Qflow, color=reach, group=reach), stat="identity", position = "identity", size=1) +
  theme_bw() +
  labs(x="Month", y=bquote('Flow (' ~Mm^3*')')) 

#Save the figure  
ggsave("CoverReveg.png")