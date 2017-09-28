

#or start here:


if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(gdxrrw)) { 
  download.file("http://support.gams.com/lib/exe/fetch.php?media=gdxrrw:gdxrrw_0.4.0.zip","gdxrrw_0.4.0.zip") 
  install.packages("gdxrrw_0.4.0.zip",repos=NULL) 
  library(gdxrrw) 
}

library(ggplot2)
library(dplyr)

igdx("") 

###Set working directory
setwd("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode")




##########################
#### DETERMINISTIC ANALYSIS
##########################

gdxfile <- "AllNetworkResults_Analysis_05192017.gdx"

#Budget
b_det<-rgdx.scalar(gdxfile,"b") 
head(b_det)

#WASH
Z_det <- rgdx(gdxfile, list(name="Z"))$val

Z_det

Z_det<- do.call(rbind, Z_det)%>%
  as.data.frame

##Reservoir Releases
RR_det <- rgdx(gdxfile, list(name="RR"))

class(RR_det)


RR_det<- do.call(rbind, RR_det)%>%
  as.data.frame


RR_det <- data.frame(Res=c(rep("j29",12),rep("j33",12)), 
                     time= rep(c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12"),2),
                     Value=c(0.1,0.6,0.8,1.2,4.23,6.6,6.45,5,1.26,1.54,0.486,0.232,0,0,0,0,3.77,4.2,3.189,2.5,1.05,0,0,1.06)
)


##########################
#### STOCASTIC ANALYSIS
##########################

gdxfile <- "WASH_MC_headflows.gdx"

### load parameters:  #squeeze = FALSE keeps all zeros and EPS values

###Budget
b<-rgdx.param(gdxfile,"b_sc",names=c("Run","Budget")) 
head(b)

###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")


#Create a bucket for feasible and infeasible scenarios and count them
group.state<- fes %>%
  group_by(isfeasible) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
group.state


###Load budget marginal values
bMar_temp<-rgdx.param(gdxfile,"EQ16marginal",names=c("Run","Value"),squeeze = FALSE) 
bMar <- left_join(bMar_temp, fes, by="Run")  #join with the feasible table
head(bMar)


#Load flow marginal values
QMar_temp<-rgdx.param(gdxfile,"EQ6marginal",names=c("Node","time","Run","Value"), squeeze = FALSE ) 
QMar <- left_join(QMar_temp, fes, by="Run")  #join with the feasible table
QMar <- mutate(QMar, Value = Value*-1)
head(QMar)

#Load reservoir releases
RR_temp<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
RR <- left_join(RR_temp, fes, by="Run")  #join with the feasible table
head(RR)

#Load reservoir storage
STOR_temp<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
STOR <- left_join(STOR_temp, fes, by="Run")  #join with the feasible table
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","Value"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)

#Load Aquatic habitat area
R_temp <- rgdx.param(gdxfile,"R_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
R <- left_join(R_temp, fes, by="Run")  #join with the feasible table
head(R)

R<- tbl_df(R) #filter out infeasible asnwers
filter(R,isfeasible =="feas")

#Load Floodplain habitat area
F_temp <-rgdx.param(gdxfile,"F_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
F <- left_join(F_temp, fes, by="Run")  #join with the feasible table
head(F)

#Load Wetlands habitat area
W_temp<-rgdx.param(gdxfile,"W_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
W <- left_join(W_temp, fes, by="Run")  #join with the feasible table
head(W)

#Load stageflow 
sf<-rgdx.param(gdxfile,"sf_par_sc",names=c("StartNode","EndNode","Run","Value"),squeeze =FALSE) 
head(sf)

#Load demand requirements
Dem_temp<-rgdx.param(gdxfile,"DemReq_sc",names=c("DemandSite","Time","Run","Value"),squeeze =FALSE) 
Dem <- left_join(Dem_temp, fes, by="Run")  #join with the feasible table
head(Dem)

#Load headflows
ReachGain_temp<-rgdx.param(gdxfile,"reachGain_sc",names=c("River","Time","Run","Value"),squeeze =FALSE) 
ReachGain <- left_join(ReachGain_temp, fes, by="Run")  #join with the feasible table
head(ReachGain)

#To convert:
#mutate(WASH, Value = Value * 247.11)



### Plotting 


options(scipen=10000)  #change scientific numbers to normal

### Plot the objective Function

WASH_feas <- WASH%>%
  filter(isfeasible == "feas")

WASH_flow <- data.frame(Z = WASH_feas$Value, Param=rep("Boundary Flows", dim(WASH_feas)[1] ))



##########################################################
#Demand
###Set working directory
setwd("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode")

gdxfile <- "WASH_MC_Demand.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values

###Budget
b<-rgdx.param(gdxfile,"b_sc",names=c("Run","Budget")) 
head(b)

###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")


#Create a bucket for feasible and infeasible scenarios and count them
group.state<- fes %>%
  group_by(isfeasible) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
group.state


###Load budget marginal values
bMar_temp<-rgdx.param(gdxfile,"EQ16marginal",names=c("Run","Value"),squeeze = FALSE) 
bMar <- left_join(bMar_temp, fes, by="Run")  #join with the feasible table
head(bMar)


#Load flow marginal values
QMar_temp<-rgdx.param(gdxfile,"EQ6marginal",names=c("Node","time","Run","Value"), squeeze = FALSE ) 
QMar <- left_join(QMar_temp, fes, by="Run")  #join with the feasible table
QMar <- mutate(QMar, Value = Value*-1)
head(QMar)

#Load reservoir releases
RR_temp<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
RR <- left_join(RR_temp, fes, by="Run")  #join with the feasible table
head(RR)

#Load reservoir storage
STOR_temp<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
STOR <- left_join(STOR_temp, fes, by="Run")  #join with the feasible table
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","Value"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)

#Load Aquatic habitat area
R_temp <- rgdx.param(gdxfile,"R_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
R <- left_join(R_temp, fes, by="Run")  #join with the feasible table
head(R)

R<- tbl_df(R) #filter out infeasible asnwers
filter(R,isfeasible =="feas")

#Load Floodplain habitat area
F_temp <-rgdx.param(gdxfile,"F_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
F <- left_join(F_temp, fes, by="Run")  #join with the feasible table
head(F)

#Load Wetlands habitat area
W_temp<-rgdx.param(gdxfile,"W_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
W <- left_join(W_temp, fes, by="Run")  #join with the feasible table
head(W)

#Load stageflow 
sf<-rgdx.param(gdxfile,"sf_par_sc",names=c("StartNode","EndNode","Run","Value"),squeeze =FALSE) 
head(sf)

#Load demand requirements
Dem_temp<-rgdx.param(gdxfile,"DemReq_sc",names=c("DemandSite","Time","Run","Value"),squeeze =FALSE) 
Dem <- left_join(Dem_temp, fes, by="Run")  #join with the feasible table
head(Dem)

#Load headflows
ReachGain_temp<-rgdx.param(gdxfile,"reachGain_sc",names=c("River","Time","Run","Value"),squeeze =FALSE) 
ReachGain <- left_join(ReachGain_temp, fes, by="Run")  #join with the feasible table
head(ReachGain)




WASH_feas <- WASH%>%
  filter(isfeasible == "feas")

WASH_demand <- data.frame(Z = WASH_feas$Value, Param=rep("Demand Requirements", dim(WASH_feas)[1] ))



#########################################

gdxfile <- "WASH_MC_budget.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values

###Budget
b<-rgdx.param(gdxfile,"b_sc",names=c("Run","Budget")) 
head(b)

###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")


#Create a bucket for feasible and infeasible scenarios and count them
group.state<- fes %>%
  group_by(isfeasible) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
group.state


###Load budget marginal values
bMar_temp<-rgdx.param(gdxfile,"EQ16marginal",names=c("Run","Value"),squeeze = FALSE) 
bMar <- left_join(bMar_temp, fes, by="Run")  #join with the feasible table
head(bMar)


#Load flow marginal values
QMar_temp<-rgdx.param(gdxfile,"EQ6marginal",names=c("Node","time","Run","Value"), squeeze = FALSE ) 
QMar <- left_join(QMar_temp, fes, by="Run")  #join with the feasible table
head(QMar)

#Load reservoir releases
RR_temp<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
RR <- left_join(RR_temp, fes, by="Run")  #join with the feasible table
head(RR)

#Load reservoir storage
STOR_temp<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
STOR <- left_join(STOR_temp, fes, by="Run")  #join with the feasible table
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","Value"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)




WASH_feas <- WASH%>%
  filter(isfeasible == "feas")

WASH_budget <- data.frame(Z = WASH_feas$Value, Param=rep("Budget", dim(WASH_feas)[1] ))


########################################
#BCT 10-45cm



###################################################
#  BCT 10 - 45 cm
###################################################

gdxfile <- "WASH_MC_BCT10_45.gdx"

#Load Objective Funcion
WASH10_45<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","Value"),squeeze =FALSE) 
head(WASH10_45)

#Load Aquatic habitat area
R10_45 <- rgdx.param(gdxfile,"R_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
head(R10_45)

#Load rsi EQ parameters 
rsi10_45<-rgdx.param(gdxfile,"rsi_EQ",names=c("StartNode","EndNode","Time", "Par","Run","Value"),squeeze =FALSE) 
head(rsi10_45)

#Load reservoir releases 
RR10_45<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze =FALSE) 
head(RR10_45)


FishType<- rep("BCT10_45", 50)
WASH10_45 <- cbind(WASH10_45, FishType)
head(WASH10_45)


WASH_BCT10_45 <- data.frame(Z = WASH10_45$Value, Param=rep("Water Depth 10-45cm", dim(WASH10_45)[1] ))

#################################
#BCT 30-75



### Load parameters from GDX file:

gdxfile <- "WASH_MC_BCT30_75.gdx"

#Load Objective Funcion
WASH30_75<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","Value"),squeeze =FALSE) 
head(WASH30_75)

#Load Aquatic habitat area
R30_75 <- rgdx.param(gdxfile,"R_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
head(R30_75)

#Load rsi EQ parameters 
rsi30_75<-rgdx.param(gdxfile,"rsi_EQ",names=c("StartNode","EndNode","Time", "Par","Run","Value"),squeeze =FALSE) 
head(rsi30_75)

#Load reservoir releases 
RR30_75<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze =FALSE) 
head(RR30_75)

FishType<- rep("BCT30_75", 101)
WASH30_75 <- cbind(WASH30_75, FishType)
head(WASH30_75)

WASH_BCT30_75 <- data.frame(Z = WASH30_75$Value, Param=rep("Water Depth 30-75cm", dim(WASH30_75)[1] ))
WASH_BCT30_75 <- WASH_BCT30_75 %>%
  filter(Z >510)

##########################
#Stage flow

gdxfile <- "WASH_MC_StageFlow.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values


###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")


#Create a bucket for feasible and infeasible scenarios and count them
group.state<- fes %>%
  group_by(isfeasible) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
group.state


###Load budget marginal values
bMar_temp<-rgdx.param(gdxfile,"EQ16marginal",names=c("Run","Value"),squeeze = FALSE) 
bMar <- left_join(bMar_temp, fes, by="Run")  #join with the feasible table
head(bMar)


#Load flow marginal values
QMar_temp<-rgdx.param(gdxfile,"EQ6marginal",names=c("Node","time","Run","Value"), squeeze = FALSE ) 
QMar <- left_join(QMar_temp, fes, by="Run")  #join with the feasible table
QMar <- mutate(QMar, Value = Value*-1)
head(QMar)

#Load reservoir releases
RR_temp<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
RR <- left_join(RR_temp, fes, by="Run")  #join with the feasible table
head(RR)

#Load reservoir storage
STOR_temp<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
STOR <- left_join(STOR_temp, fes, by="Run")  #join with the feasible table
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","Value"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)


WASH_StageFlow <- data.frame(Z = WASH$Value, Param=rep("Stage Flow Intercept", dim(WASH)[1] ))


#############
# Wetlands WSI

gdxfile <- "wet_aw_wsi.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values


###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")




#Load reservoir releases
RR<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
head(RR)

#Load reservoir storage
STOR<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","WASH_km2"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)


WASH_wsi <- data.frame(Z = WASH$WASH_km2, Param=rep("Wetlands Suitability Index", dim(WASH)[1] ))

WASH_wsi <- WASH_wsi %>%
  mutate(Z= replace(Z, Param=="Wetlands Suitability Index",Z-120))


#######################################
#Floodplain area


gdxfile <- "fld_rv.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values


###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")




#Load reservoir releases
RR<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
head(RR)

#Load reservoir storage
STOR<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","WASH_km2"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)


WASH_rv <- data.frame(Z = WASH$WASH_km2, Param=rep("Floodplain Area", dim(WASH)[1] ))



###############
#Wetland area


gdxfile <- "wet_aw.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values


###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")




#Load reservoir releases
RR<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
head(RR)

#Load reservoir storage
STOR<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
head(STOR)


#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","WASH_km2"),squeeze =FALSE) 
WASH <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH)


WASH_aw <- data.frame(Z = WASH$WASH_km2, Param=rep("Wetland Area", dim(WASH)[1] ))

WASH_aw <- WASH_aw %>%
  mutate(Z= replace(Z, Param=="Wetland Area",Z-140))

#################
#Floodplain index


gdxfile <- "fld_2yr_rv.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values


###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")




#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","WASH_km2"),squeeze =FALSE) 
WASH_2yr <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH_2yr)



gdxfile <- "fld_5yr_rv.gdx"


### load parameters:  #squeeze = FALSE keeps all zeros and EPS values


###Load and add lables to model Status 
fes<-rgdx.param(gdxfile,"ModelStat_sc",names=c("Run","Value")) 
isfeasible <-ifelse(fes$Value ==5, "infeas","feas") #define lables
fes<-cbind(fes,isfeasible) #append to dataframe

#override feasible value with text
fes<- fes %>%
  mutate(Value = isfeasible) %>%
  select(-Value)

head(fes)

#Sort values with feasible and infeasible scnearios 
infeas.stat <- fes %>%
  filter(isfeasible =="infeas")




#Load Objective Funcion
WASH_temp<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","WASH_km2"),squeeze =FALSE) 
WASH_5yr <- left_join(WASH_temp, fes, by="Run")  #join with the feasible table
head(WASH_5yr)


fld_merge <- rbind(WASH_2yr, WASH_5yr)


WASH_fld <- data.frame(Z = fld_merge$WASH_km2, Param=rep("Flood Recurrence ", dim(fld_merge)[1] ))




#WASH_aw <- WASH_aw %>%
#  mutate(Z= replace(Z, Param=="Wetland Area",Z-140))


############
#Plot box plot

WASH_boxplot <- rbind(WASH_flow, WASH_demand, WASH_budget, WASH_BCT10_45, WASH_BCT30_75, WASH_StageFlow, WASH_wsi, WASH_rv, WASH_aw, WASH_fld)
#BoxPlot


p <- ggplot(WASH_boxplot, aes(x=Param, y=Z))
p <- p + geom_boxplot( width=0.6, outlier.colour = "red", outlier.shape = 1) + coord_flip()
#P <- p+ geom_point(data=WASH_fld, aes(x=Param, y= Z),  color="red", size=4, shape=15)
p <- p+  labs(y=" WASH Objective Function (km2)", x="Model Parameters")
#p <- p+ ylim(c(520,570))
p <- p+  theme_classic(26)
#p <- p+ theme(axis.text.x = element_text(angle = 0, hjust = 1))
p <- p+ theme(axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black") )
p

