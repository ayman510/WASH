# Go here and download the package: http://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r

# Windows installation: In the R environment, navigate to the directory containing the binary package file 
#(see download links above), and, substituting the version number for a.b.c, type: install.packages("gdxrrw_a.b.c.zip") or, 
# depending on the version of R, install.packages("gdxrrw_a.b.c.zip",repos=NULL)


#Some help and examples here: http://ftp.gamsworld.org/presentations/informs2012_gdxrrw.pdf


#or start here:


rm(list = ls())  #Clear history

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

#To convert:
#mutate(WASH, Value = Value * 247.11)



### Plotting 


options(scipen=10000)  #change scientific numbers to normal

### Plot the objective Function


## Histogram of WASH
WASH %>% 
  filter(isfeasible == "feas") %>%
  ggplot() +
  aes(x= Value) +
  geom_histogram(col="red", fill="green", alpha=.3)+
  labs(x="WASH Objective Function (km2)") 
#  theme_classic(14)  
#  facet_grid(~isfeasible)
ggsave("WASH_hist.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/Demand")



###Demand

## Sum all demands per run and month:
Demand_sum <- Dem %>%
  group_by(Run,Time) %>%
  summarise(SumValue= sum(Value))
Demand_sum <- left_join(Demand_sum, fes, by="Run")  #join with the feasible table

head(Demand_sum)

#Plot Demands
Demand_sum %>%
  filter(isfeasible == "feas") %>%
  ggplot()+
  aes(x=Time, y=SumValue, color='Run' )+
  geom_line()+ stat_summary()+
  labs(x="Time (months)", y="Annual Demands(Mm3/mo)") +

  theme(axis.title.x = element_text(face="bold", size=16), 
        axis.title.y = element_text(face="bold", size=16),
        axis.text = element_text(color = "black", size = 13) )+
  theme_classic(14)
ggsave("Demand.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/Demand")


###WASH and Demand
## Sum all demand per run:
Demand_run <- Dem %>%
  group_by(Run) %>%
  summarise(SumValue= sum(Value))

head(Demand_run)

##plot WASH vs sum demand
WASH_demand <- left_join(WASH,Demand_run, by="Run")
head(WASH_demand)


WASH_demand %>%
  filter(isfeasible == "feas") %>%
  ggplot()+
  aes(x=SumValue, y=Value)+
  geom_point()+ stat_smooth()
  labs(x="Annual Urban and Ag Demand (Mm3/mo)", y="WASH Objective Function (km2)") +
  
    theme_classic(14)

ggsave("WASH_Demand.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/Demand")



        
### PLot reservoir releaases
RR_corr <- RR %>%
  mutate(Value = replace(Value, Res=="j29" & time=="t12", 0.05 *Value))

#Boxplot
RR_corr %>%
  filter(Res=="j29" ) %>%
  ggplot() +
  aes(x=time , y=Value, fill=Res)+
  geom_boxplot()+
  labs(y="Reservoir Releases (Mm3/month)", x="Month") + ylim(0,8)+
  theme_classic(14) 


#Multiline plot
RR_corr %>%
  filter(Res=="j29") %>%
  ggplot() +
  aes(x=time , y=Value, group=Run, fill=Res)+
  geom_line(alpha=0.3)+
  labs(y="Reservoir Releases (Mm3/month)", x="Month") + ylim(0,8)+
  theme_classic(14) 

ggsave("RR.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/Demand")







