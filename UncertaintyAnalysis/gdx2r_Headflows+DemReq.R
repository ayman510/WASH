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
library(tidyr)
library(reshape2)

function_path <- "./functions"
global_path <- "./global_func"

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)



igdx("") 

###Set working directory
setwd("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode")


## Deterministic Model
gdxfile <- "Optimization_Solution.gdx"



#WASH
Z_det <- rgdx(gdxfile, list(name="Z", form = "full"))$val

#Aquatic
R_det <- rgdx(gdxfile, list(name="R", form = "full"))$val

#Floodplain
F_det <- rgdx(gdxfile, list(name="F", form = "full"))$val
#sum wetlands area
W_opt <- W_det %>%
  summarise(Wetlands= sum(Value))
head(Wetlands_opt)



#Wetlands
W_det <- rgdx(gdxfile, list(name="W", form = "full"))$val
do.call(sum,W_det)
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


#Read in actual reservoir releases
RR_hyrum <- read.csv("Hyrum_releases.csv", header=TRUE)
RR_porc <- read.csv("Porc_releases.csv", header=TRUE)


#Combine with deterministic model and measure difference

#Combine with actual releases
RR_hyrum_det_diff <- cbind(RR_hyrum, Release_det_hyrum)
RR_porc_det_diff  <- cbind(RR_porc, Release_det_porc)

#Fix last end of time release value

RR_hyrum_det_diff <- RR_hyrum_det_diff%>%
  mutate(Release_det_hyrum = replace(Release_det_hyrum, j=="j29" & time=="t12", 0.46))



#find difference
RR_hyrum_det_diff$diff <-abs(RR_hyrum_det_diff$Release - RR_hyrum_det_diff$Release_det_hyrum)
RR_porc_det_diff$diff <-abs(RR_porc_det_diff$Release - RR_porc_det_diff$Release_det_porc)


#Clean the dataframe 
RR_hyrum_det_diff <- RR_hyrum_det_diff %>%
  select(-j, -Release_det_hyrum, -Release)

RR_porc_det_diff <- RR_porc_det_diff %>%
  select(-j, -Release_det_porc, -Release)


#Combine hyrum and procupine
RR_det_diff <- bind_rows(RR_hyrum_det_diff, RR_porc_det_diff)

#Sum all values
RR_det_sum <- RR_det_diff  %>%
  summarise(Reservoir_Release_Diff= sum(diff))



Det_model <- data.frame(Run="Opt", WASH_km2=Z_det, isfeasible= "feas", 
                        AquaticArea_km2= 175, FloodplainArea_km2 = 11.36, 
                        WetlandsArea_km2 = 343.63, Demand_Mm3yr= Demand_opt,
                        ReachGain_Mm3yr=ReachGain_opt,
                        Reservoir_Release_Diff = RR_det_sum
                        )
  



## Stocastic Model

gdxfile <- "WASH_MC_reachGain_DemReq.gdx"


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

#Load Aquatic habitat area
R <- rgdx.param(gdxfile,"R_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
head(R)



#Load Floodplain habitat area
Fld <-rgdx.param(gdxfile,"F_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
head(Fld)

#Load Wetlands habitat area
W<-rgdx.param(gdxfile,"W_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
head(W)


#Load demand requirements
Dem<-rgdx.param(gdxfile,"DemReq_sc",names=c("DemandSite","Time","Run","Value"),squeeze =FALSE) 
head(Dem)

#Load headflows
ReachGain<-rgdx.param(gdxfile,"reachGain_sc",names=c("River","Time","Run","Value"),squeeze =FALSE) 
head(ReachGain)


#Correct Reservoir Releases last value for December

RR_corr <- RR %>%
  mutate(Value = replace(Value, Res=="j29" & time=="t12", 0.05 *Value))


# Select only Hyrum and then porcupine reservoirs
RR_hyrum_runs <- RR_corr %>%
  filter(Res == "j29")

RR_porc_runs <- RR_corr %>%
  filter(Res == "j33")


#Combine with actual releases
RR_hyrum_diff <- full_join(RR_hyrum_runs, RR_hyrum, by="time")
RR_porc_diff  <- full_join(RR_porc_runs, RR_porc, by="time")

#find difference
RR_hyrum_diff$diff <-abs(RR_hyrum_diff$Value - RR_hyrum_diff$Release)
RR_porc_diff$diff <-abs(RR_porc_diff$Value - RR_porc_diff$Release)


#Clean the dataframe 
RR_hyrum_diff <- RR_hyrum_diff %>%
  select(-Res, -j, -Value, -Release)

RR_porc_diff <- RR_porc_diff %>%
  select(-Res, -j, -Value, -Release)

#Combine hyrum and procupine 
RR_diff <- bind_rows(RR_hyrum_diff, RR_porc_diff)

### Plotting 


options(scipen=10000)  #change scientific numbers to normal

### Plot the objective Function




###Group all parameteres by Run to plot on parallel coordinates: 

#Demand
Demand_run <- Dem %>%
  group_by(Run) %>%
  summarise(Demand_Mm3yr= mean(Value))
head(Demand_run)

#ReachGain
ReachGain_run <- ReachGain %>%
  group_by(Run) %>%
  summarise(ReachGain_Mm3yr= sum(Value))
head(ReachGain_run)


#Wetlands
W_run <- W %>%
  group_by(Run) %>%
  summarise(WetlandsArea_km2= sum(Value))
head(W_run)

#Aquatic
library(dplyr)
R_run <- R %>%
  group_by(Run)
  

head(R_run)
head(R)

#Floodplains
Fld_run <- Fld %>%
  group_by(Run) %>%
  summarise(FloodplainArea_km2= sum(Value))
head(Fld_run)


#Reservoir release difference
RR_diff_run <- RR_diff  %>%
  group_by(Run) %>%
  summarise(Reservoir_Release_Diff= sum(diff))
head(RR_diff_run)

## Combine all parameters into a single data frame:
Unc <- left_join(WASH,R_run, by="Run")
Unc <- left_join(Unc,W_run, by="Run")
Unc <- left_join(Unc,Fld_run, by="Run")
Unc <- left_join(Unc,Demand_run, by="Run")
Unc <- left_join(Unc,ReachGain_run, by="Run")
Unc <- left_join(Unc, RR_diff_run, by="Run")

#Add determinisitic model optimization answer
Unc <- bind_rows(Unc, Det_model)



# Add a cluster column to classify data by WASH value

#perform <- c()

#Loop through the objective function values (WASH) and define members of "perform' vector as either "better" or "worse" 
#for (n in seq(Unc1$Run) ) {
#    if (Unc1$WASH[n] > Z_det) {perform[n] = "better" }
#            else perform[n] = "worse" 
#}
#Unc1 <- data.frame(Unc1, perform=perform)

test_stat <- Unc$WASH > Z_det

### This works
Unc$perform <- NA
Unc$perform[test_stat] <- "better"
Unc$perform[!test_stat] <- "worse"



head(Unc)


####################################
# Examine Correlations in the Data
####################################
#First remove all infeasible records
mydata <- Unc%>%
  filter(isfeasible == "feas")

#Next get rid of all non numeric columns
mydata<- Unc[,-c(1,3,10)]

head(mydata)
library("PerformanceAnalytics")

chart.Correlation(mydata, histogram=T, pch=19, method="pearson", main="Correlation Matrix") #methods= kendall

ggsave("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/CorrelationMatrix.png")

#Write the data frame into excel for other software/if needed
write.csv(Unc, file= "Unc.csv")


#############################
## Parallel Plots:
#############################

library(parcoords)


parcoords(Unc, rownames= F, brushMode = "1d-axes" , autoresize = TRUE# 2d-strums are really neat
          , reorderable = TRUE, alpha = 0.7,  color=list( colorBy="WASH_km2", colorScale =htmlwidgets::JS('d3.scale.category20c()'))
                                                          
)

#Color schemes here: http://bl.ocks.org/aaizemberg/78bd3dade9593896a59d 

#Filter out infeasible runs


library(RColorBrewer)

Unc %>%
  filter(isfeasible == "feas") %>%
  select(-isfeasible) %>%
  parcoords(rownames= F, brushMode = "1d-axes-multi" ,autoresize = TRUE,  
           reorderable = TRUE, alpha=0.5, color=list(colorBy="perform", colorScale =htmlwidgets::JS('d3.scale.category10()')  )
)

Unc %>%
             filter(isfeasible == "feas") %>%
             select(-isfeasible) %>%
             parcoords(rownames= F, brushMode = "1d-axes" ,autoresize = TRUE,  
                       reorderable = TRUE, alpha = 1,  color=list(colorScale =htmlwidgets::JS('d3.scale.linear()
  .domain([480, 530])
  .range(["steelblue", "brown"])
  .interpolate(d3.interpolateLab)'), colorBy="WASH_km2" )
                       
)

Unc %>%
   filter(isfeasible == "feas") %>%
   select(-isfeasible) %>%
   parcoords(rownames= F, brushMode = "1d-axes" ,autoresize = TRUE,  
             reorderable = TRUE, alpha = 0.7,  color=list(colorby="WASH_km2", colorScale = htmlwidgets::JS(sprintf('d3.scale.threshold()
                                                                                                                   .domain(%s)
                                                                                                                   .range(%s)'
                                                                                                                   ,jsonlite::toJSON(seq(520,round(max(Unc$WASH_km2))))
                                                                                                                   ,jsonlite::toJSON(RColorBrewer::brewer.pal(3,"PuBuGn"))
             ))))




 
Unc %>%
   filter(isfeasible == "feas") %>%
   select(-isfeasible) %>%

   parcoords(rownames= F, brushMode = "1d-axes" ,autoresize = TRUE,  
             reorderable = TRUE, alpha = 0.7,  color=list(colorScale =htmlwidgets::JS(sprintf('d3.scale.threshold()
                                                                                              .domain(%s)
                                                                                              .range(%s)
                                                                                              ',jsonlite::toJSON(seq(530,round(max(Unc$WASH_km2))))
                                                                                              ,jsonlite::toJSON(RColorBrewer::brewer.pal(2,"PuBu")))), colorBy="WASH_km2" )
             )
 




#################################################
## Agglomorative Heirarchical Clustering
#################################################

require(ggrepel)
require(cluster)
require(ggdendro)
require(ade4)
library(dendextend)
require(ggsci)

#Reorder data in the column

colnames(Unc)

Unc <- Unc[c(1,3,10,2,4,5,6,7,8,9 )]

head(Unc)

#filter infeasible, and worse performing alternatives
Unc.feas <- Unc %>%
  filter(isfeasible == "feas") %>%
#  filter(perform == "worse") %>%
  select(-isfeasible, -perform)

head(Unc.feas)

#write to csv
write.csv(Unc.feas, file= "C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/clusterData.csv")

#Remove the runs column
Unc.clust<- Unc.feas[,-c(1)]


#Normalize / Standarize the data frame
# The daisy function in the cluster library will automatically perform standardization, but it doesn't give you complete control. 
# Scale has two optional vectors: 
# first: center: will be subtracted from every entry in that column
# second: scale: is used to divide the values in each column.
medians = apply(Unc.clust,2,median) #2 us ti aookt for columns (1 for rows)
mads = apply(Unc.clust,2,mad)
Unc.clust = scale(Unc.clust,center=medians,scale=mads)

# measure distance
clust.dist <- dist(Unc.clust)

#To examine the distance matrix
dist.matrix <- as.matrix(clust.dist)

#Generate hierarical clusters
run.hclust <- hclust (clust.dist, method = "ward.D2") 

# Plot the dendogram
plot(run.hclust, labels = Unc.feas$Run)



#Add boxes for number of clusters

rect.hclust(run.hclust, k=4, border = "red")

#Find out how many members are they in group
groups.4 <- cutree(run.hclust, 4)
table(groups.4)



#Table of means
G1 <- tapply(Unc.clust[,1], groups.4, mean) 
G2 <- tapply(Unc.clust[,2], groups.4, mean)

#Plot data with circles and means
library(plotrix)
plot(Unc.clust, col=groups.4)
points(G1, G2, col= 1:4, cex=2, pch=19)
for(i in 1:length(G1)) {  # draw circles
  draw.circle(G1[i], G2[i], 1, border=i,lty=3,lwd=3)
}

#For multiple group numbers
sapply(2:6,function(ncl)table(cutree(run.hclust,ncl)))


#To see members of each cluster
sapply(unique(groups.4), function(g)Unc.feas$Run[groups.4 == g])

#To examine statisics of original data
a3 <- aggregate(Unc.feas[,-c(1)], list(groups.4), mean )
data.frame(Cluster = a3[,1], Freq=as.vector(table(groups.4)), a3[,-1])

#export stats:
write.csv(a3, file="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/Stat.csv")

#Plot silhouette
plot(silhouette(cutree(run.hclust,3),clust.dist))

#Scree plot
wss<- (nrow(Unc.clust)-1)*sum(apply(Unc.clust,2,var))
for (i in 2:nrow(Unc.clust)-1) wss[i] <- sum(kmeans(Unc.clust, centers=i)$withinss)
plot(1:82, wss, type="b", xlab="Number of Clusters", ylab="Sum of Squares within Clusters")


############################
#K-mean Clustering
############################
kc <- kmeans(Unc.clust,4)
kc

plot(Unc.feas$WASH_km2~ Unc.feas$Reservoir_Release_Diff, col=kc$cluster, type="p", xlab="Reservoir Release Difference", ylab="Watershed Area Objective Function (km2)")

#Plot runs as text (use geom_text to activate check_overlap=TRUE)
ggplot(data=Unc.feas)+geom_label(aes(x=Reservoir_Release_Diff, y=WASH_km2, label=Unc.feas$Run, col=as.factor(kc$cluster)), check_overlap = TRUE)+  theme_classic(14)

pairs(~WASH_km2+AquaticArea_km2+FloodplainArea_km2+WetlandsArea_km2+Demand_Mm3yr+ReachGain_Mm3yr+Reservoir_Release_Diff,data=Unc.feas, col=kc$cluster,
      main="Simple Scatterplot Matrix")



library(fpc)
plotcluster(Unc.clust, kc$cluster)

clusplot(Unc.clust, kc$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


table(Unc.feas$Run, kc$cluster)



##########################################
#Find medoids
##########################################

#Use PAM function 
pamx<-pam(Unc.clust, 4)
summary(pamx)
pamx$medoids
pamx$id.med #get index for medoids

pamx$clustering 
plot(pamx)  #Generate multiple plots


#Combine runs with cluster number in a dataframe
Runs_clust <- data.frame(Run= Unc.feas$Run, cluster=pamx$clustering ) 

#Extract medoids
Run_med <- Runs_clust[pamx$id.med,]

#Combine with original data
Unc_med <- merge(Run_med, Unc, all.x=FALSE)

#Combine with the reservoir releases

#Merge reservoir releases data with clusters
to_merge <- data.frame(Run=Unc.feas$Run, Cluster=kc$cluster)

#Combine all reservoir releases
RR_plot <- merge(RR_corr, to_merge, by="Run")
colnames(RR_plot)[4] <- "Release"
colnames(RR_plot)[5] <- "cluster"

head(RR_plot)

#Extract Hyrum data
RR_plot_hyrum <- RR_plot %>%
  filter(Res== "j29")

head(RR_plot_hyrum)


#Remove old cluster column to add a hyrum-specific one
RR_plot_hyrum_med <-  RR_plot_hyrum %>%
  select(-cluster)

# Now merge clusters with runs for hyrum only
RR_med <- merge(RR_plot_hyrum_med, Run_med, by="Run")
RR_med$cluster <- as.factor(RR_med$cluster) 

#Plot reservoir releases for each cluster
p <- ggplot(data= RR_med, aes(x=time , y=Release, group=cluster, color=cluster))
p <- p+ geom_line(aes(linetype= cluster, color=cluster), size=2)
p <- p+ scale_x_discrete(limits=c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12"))
p <- p+  labs(y="Reservoir Releases (Mm3/month)", x="Month")
p <- p+  theme_classic(18) 
p


#############################################################
# Plot reservoir releases per cluster combined with medoids
#############################################################



RR_plot_hyrum$cluster <- as.factor(RR_plot_hyrum$cluster)


p <- ggplot(data= RR_plot_hyrum, aes(x=time , y=Release))
p <- p+ geom_line(alpha=0.2, aes(group=Run, color=cluster))
#Add medoid lines 
p <- p+ geom_line(data = RR_med, aes(group=cluster, color=cluster), size=2, linetype="dashed")
#Add actual releases 
#p <- p+ geom_line(data = RR_hyrum, aes(x=time , y=Release, group=1, colour="2003 Existing Releases"), color='black', size=1)
p <- p+ scale_x_discrete(limits=c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12"))
p <- p+  labs(y="Reservoir Releases (Mm3/month)", x="Month")
p <- p+  theme_classic(18) 
p


p + facet_wrap(~ cluster)






