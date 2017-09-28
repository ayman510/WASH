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
  
  
  
  igdx("") 
  
  ###Set working directory
  setwd("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode")
  
  ###############################################
  ## Deterministic Model
  ###############################################
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
  
  RR_hyrum <- RR_hyrum %>%
    mutate(Release = replace(Release, j=="j29" & time=="t5", 7.029)) %>%
    mutate(Release = replace(Release, j=="j29" & time=="t6", 6.029)) %>%
    mutate(Release = replace(Release, j=="j29" & time=="t4", 5.239))%>%
    mutate(Release = replace(Release, j=="j29" & time=="t7", 2.429)) %>%
    mutate(Release = replace(Release, j=="j29" & time=="t8", 1.329))
  #Combine with deterministic model and measure difference
  
  #Combine with actual releases
  RR_hyrum_det_diff <- cbind(RR_hyrum, Release_det_hyrum)
  RR_porc_det_diff  <- cbind(RR_porc, Release_det_porc)
  
  #Fix last end of time release value
  
  RR_hyrum_det_diff <- RR_hyrum_det_diff%>%
    mutate(Release_det_hyrum = replace(Release_det_hyrum, j=="j29" & time=="t12", 1.02))
  
  
  
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
  
  
  #Hyrum determinisitc releases for plotting 
  
  Release_det_hyrum_plot <- data.frame(time=RR_hyrum_det_diff$time, Release_Mm3mo =Release_det_hyrum )
  #correct releases
  Release_det_hyrum_plot <- Release_det_hyrum_plot%>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t12", 1.6)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t10", 1.02)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t3", 1.7)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t4", 4.6)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t5", 6.2)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t7", 4.4)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t6", 6.15)) %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, time=="t8", 2.4))
  
  
  
  Det_model <- data.frame(Run="Opt", WASH_km2=Z_det, isfeasible= "feas", 
                          AquaticArea_km2= 144.537798, FloodplainArea_km2 = 11.359, 
                          WetlandsArea_km2 = 343.63, Demand_Mm3yr= Demand_opt,
                          ReachGain_Mm3yr=ReachGain_opt,
                          Reservoir_Release_Diff = RR_det_sum,
                          Budget=650000, wsi_slope =0.0003 , wsi_intercept=0.3 ,
                          rsi_slope=0.04447, rsi_centroid=0.406  ,Stage_flow_slope=0.0289923 , Stage_flow_intercept= 0.548,
                          PlantCover_km2yr= 585, Available_FloodplainArea_km2yr= 497.2401,
                          FloodLevel = "2-year", SuitableDepth = "10-45cm", FuncType ="Boltzmann",
                          TotalWetlandsArea_km2yr = 1416
                          )
    
  
  ##############################################
  
  
  ########################################
  ## Stocastic Model
  #########################################
  
  
  gdxfile <- "all_par_global_test.gdx"
  
  
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
  RR1<-rgdx.param(gdxfile,"RR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
  head(RR1)
  
  #Load reservoir storage
  STOR1<-rgdx.param(gdxfile,"STOR_sc",names=c("Res","time","Run","Value"),squeeze = FALSE) 
  head(STOR1)
  
  
  #Load Objective Funcion
  WASH_temp1<-rgdx.param(gdxfile,"WASH_sc",names=c("Run","WASH_km2"),squeeze =FALSE) 
  WASH1 <- left_join(WASH_temp1, fes, by="Run")  #join with the feasible table
  head(WASH1)
  
  #Load Aquatic habitat area
  R1<- rgdx.param(gdxfile,"R_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
  head(R1)
  
  
  
  #Load Floodplain habitat area
  Fld1 <-rgdx.param(gdxfile,"F_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
  head(Fld1)
  
  #Load Wetlands habitat area
  W1<-rgdx.param(gdxfile,"W_sc",names=c("StartNode","EndNode","Time","Run","Value"),squeeze =FALSE) 
  head(W1)
  
  
  #Load demand requirements
  Dem1<-rgdx.param(gdxfile,"DemReq_sc",names=c("DemandSite","Time","Run","Value"),squeeze =FALSE) 
  head(Dem1)
  
  #Load headflows
  ReachGain1<-rgdx.param(gdxfile,"reachGain_sc",names=c("River","Time","Run","Value"),squeeze =FALSE) 
  head(ReachGain1)
  
  
  #Load budget
  b1<-rgdx.param(gdxfile,"b_sc",names=c("Run","Budget")) 
  head(b1)
  
  #Load stageflow 
  sf1<-rgdx.param(gdxfile,"sf_par_sc",names=c("StartNode","EndNode","sf_par","Run","Value"),squeeze =FALSE) 
  head(sf1)
  
  #Load rsi parameters 
  rsi1<-rgdx.param(gdxfile,"rsi_EQ",names=c("StartNode","EndNode","Time", "rsi_par","Run","Value"),squeeze =FALSE) 
  head(rsi1)
  
  #Load wetlands area
  aw1<-rgdx.param(gdxfile,"aw_sc",names=c("StartNode","EndNode","Time", "Run","Value"),squeeze =FALSE) 
  head(aw1)
  
  #Load wetlands suitability index
  wsi1<-rgdx.param(gdxfile,"wsi_EQ",names=c("StartNode","EndNode", "Time", "wsi_par","Run","Value"),squeeze =FALSE) 
  head(wsi1)
  
  #Load floodplain area
  rv1<-rgdx.param(gdxfile,"RV_sc",names=c("StartNode","EndNode","Time", "species", "Run","Value"),squeeze =FALSE) 
  head(rv1)
  
  #Load plant cover (C) 
  PlantCover1 <-rgdx.param(gdxfile,"C_sc",names=c("StartNode","EndNode","Time", "species", "Run","Value"),squeeze =FALSE) 
  head(PlantCover1) 
  
  
  FloodLevel1 <- data.frame(Run= WASH1$Run,FloodLevel = rep("2-year", 800))
  SuitableDepth1 <- data.frame(Run= WASH1$Run,SuitableDepth = rep("10-45cm", 200))
  FuncType1 <- data.frame(Run= WASH1$Run,FuncType = rep("Boltzmann", 200))
  
  
  
  #Combine all runs together
  
  PlantCover <- PlantCover1
  rv <- rv1
  aw <- aw1 
  wsi <- wsi1
  rsi <- rsi1
  sf <- sf1
  b <- b1
  Fld <- Fld1
  ReachGain <- ReachGain1
  Dem <- Dem1
  W <- W1
  R <- R1
  WASH <- WASH1
  STOR <- STOR1
  RR <- RR1
  FloodLevel<-FloodLevel1
  SuitableDepth <- SuitableDepth1
  FuncType <- FuncType1
  
  
  
  
  
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
  
  
  ###Group parameteres by Run to plot on parallel coordinates: 
  
  #Demand
  Demand_run <- Dem %>%
    group_by(Run) %>%
    summarise(Demand_Mm3yr= sum(Value))
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
  R_run <- R %>%
    group_by(Run) %>%
    summarise(AquaticArea_km2= sum(Value))
  head(R_run)
  
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
  
  
  #Wetlands Area
  WetlandsArea_run <- aw %>%
    group_by(Run) %>%
    summarise(TotalWetlandsArea_km2yr= sum(Value))
  head(WetlandsArea_run)
  
  #Available Floodplain Area
  Available_FloodplainArea_run <- rv %>%
    group_by(Run) %>%
    summarise(Available_FloodplainArea_km2yr= sum(Value))
  head(Available_FloodplainArea_run)
  
  #Plan Cover Area
  PlantCover_run <- PlantCover %>%
    group_by(Run) %>%
    summarise(PlantCover_km2yr= sum(Value))
  head(PlantCover_run)
  
  #Stage-flow cofficient
  sf_slope_run <- sf %>%
    filter(sf_par == "sf_par1") %>%
    group_by(Run) %>%
    summarise(Stage_flow_slope = mean(Value))
  
  sf_intercept_run <- sf %>%
    filter(sf_par == "sf_par2") %>%
    group_by(Run) %>%
    summarise(Stage_flow_intercept = mean(Value))
  
  sf_run <- left_join(sf_slope_run, sf_intercept_run , by="Run")
  
  
  # Aquatic Suitability Index parameters
  rsi_centroid <- rsi %>%
    filter(rsi_par == "rsi_par3") %>%
    group_by(Run) %>%
    summarise(rsi_centroid = mean(Value))
  
  rsi_slope <- rsi %>%
    filter(rsi_par == "rsi_par4") %>%
    group_by(Run) %>%
    summarise(rsi_slope = mean(Value))
  
  rsi_run <- data.frame(rsi_slope= rsi_slope , rsi_centroid = rsi_centroid)
  
  
  #Wetlands suitability index cofficient
  wsi_slope_run <- wsi %>%
    filter(wsi_par == "wsi_par1") %>%
    group_by(Run) %>%
    summarise(wsi_slope = mean(Value))
  
  wsi_intercept_run <- wsi %>%
    filter(wsi_par == "wsi_par2") %>%
    group_by(Run) %>%
    summarise(wsi_intercept = mean(Value))
  
  wsi_run <- left_join(wsi_slope_run, wsi_intercept_run , by="Run")
  
  #################################################################
  
  ## Combine all parameters into a single data frame:
  Unc <- left_join(WASH,R_run, by="Run")
  Unc <- left_join(Unc,W_run, by="Run")
  Unc <- left_join(Unc,Fld_run, by="Run")
  Unc <- left_join(Unc,Demand_run, by="Run")
  Unc <- left_join(Unc,ReachGain_run, by="Run")
  Unc <- left_join(Unc, RR_diff_run, by="Run")
  Unc <- left_join(Unc, b, by="Run")
  Unc <- left_join(Unc, wsi_run, by="Run")
  Unc <- left_join(Unc, rsi_slope, by="Run")
  Unc <- left_join(Unc, rsi_centroid, by="Run")
  Unc <- left_join(Unc, sf_run, by="Run")
  Unc <- left_join(Unc, PlantCover_run, by="Run")
  Unc <- left_join(Unc, Available_FloodplainArea_run, by="Run")
  Unc <- left_join(Unc, FloodLevel, by ="Run")
  Unc <- left_join(Unc, SuitableDepth, by ="Run")
  Unc <- left_join(Unc, FuncType, by ="Run")
  Unc <- left_join(Unc, WetlandsArea_run , by="Run")
  
  head(Unc)
  
  #Add determinisitic model optimization answer
  Unc <- bind_rows(Unc, Det_model)
  
  #Filter infeasible runs
  Unc.feas <- Unc%>%
    filter(isfeasible == "feas")
  
  #Write to csv
  
  
  
  
  
  # Add a cluster column to classify data by WASH value
  
  test_stat <- Unc$WASH > Z_det
  Unc$perform <- NA
  Unc$perform[test_stat] <- "better"
  Unc$perform[!test_stat] <- "worse"
  
  
  
  head(Unc)
  
  
  ####################################
  # Examine Correlations in the Data
  ####################################
  #First remove all infeasible records
  Unc.feas <- Unc%>%
    filter(isfeasible == "feas")%>%
    select(-isfeasible)
  
  names(Unc.feas)
  
  #Next get rid of all non numeric columns
  
  #Remove optimal
  n.feas <- dim(Unc.feas)[1]
  Unc.feas.stoc <- Unc.feas[1:(n.feas-1),]
  
  Numericdata <- Unc.feas.stoc %>%
    select(c(WASH_km2, AquaticArea_km2, FloodplainArea_km2, WetlandsArea_km2, Demand_Mm3yr, ReachGain_Mm3yr, Budget))
  
  head(Numericdata)
  
  library("PerformanceAnalytics")
  
  chart.Correlation(Numericdata, histogram=T, pch=19, method="pearson", main="Correlation Matrix", cex.labels=1.5) #methods= kendall
  
  ggsave("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/States/all_par_Global_only_CorrelationMatrix.png")
  
  
  
  #################################################
  ## Agglomorative Heirarchical Clustering
  #################################################
  
  
  library(cluster)
  
  
  #Set working directory to save data and outputs
  setwd("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/States/Global_only")
  
  #Create clustring data frame
  names(Unc.feas)
  Unc.clust <- subset(Unc.feas, select= c(WASH_km2, AquaticArea_km2, WetlandsArea_km2, FloodplainArea_km2, Demand_Mm3yr, ReachGain_Mm3yr, Budget ))
  
  names(Unc.clust)
  
  #Define categorical data as factors
  
  Unc.clust$SuitableDepth <- as.factor(Unc.clust$SuitableDepth) 
  Unc.clust$FloodLevel <- as.factor(Unc.clust$FloodLevel) 
  
  
  head(Unc.clust)
  
  #Write to Excel
  
  library(xlsx) #load the package
  write.xlsx(x = Unc.clust, file = "ClusteredData.xlsx",
             sheetName = "Non_normalized", row.names = FALSE)
  
  
  write.csv(Unc.feas, file = "C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/States/Global_only/global.csv")
  
  write.csv(Unc.clust, file = "C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/States/Global_only/global_clust.csv")
  
  #Normalize / Standarize the data frame
  # The daisy function in the cluster library will automatically perform standardization, but it doesn't give you complete control. 
  
  
  # measure distance
  clust.dist <- daisy(Unc.clust, metric = "manhattan")
  
  summary(clust.dist)
  
  #To examine the distance matrix
  gower_mat <- as.matrix(clust.dist)
  
  #Output most similar pair
  
  Unc.clust[
    which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
          arr.ind = TRUE)[1, ], ]
  
  # Output most dissimilar pair
  
  Unc.clust[
    which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
          arr.ind = TRUE)[1, ], ]
  
  ##########################################################
  
  #Select number of clusters
  
  # Calculate silhouette width for many k using PAM
  sil_width <- c(NA)
  
  for(i in 2:6){
    
    pam_fit <- pam(clust.dist,
                   diss = TRUE,
                   k = i)
    
    sil_width[i] <- pam_fit$silinfo$avg.width
    
  }
  
  # Plot sihouette width (higher is better)
  
  plot(1:6, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width", cex.axis=2, cex.lab =2, cex=2, size=3)
  lines(1:6, sil_width)
  
  
  #Save sihouette width plot
  ggsave("SilPlot.PDF")
  
  
  
  
  
  
  ##########################################
  #Find medoids
  ##########################################
  
  #Use PAM function 
  
  pam_fit <- pam(clust.dist, diss = TRUE, k = 2)
  
  pam_results <- Unc.clust %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
  pam_results$the_summary
  
  # Get medoids
  
  Unc.feas[pam_fit$medoids, ]
  
  
  pam_fit$id.med #get index for medoids
  
  pam_fit$clustering #Cluster number for each run
  
  #Name clusters:
  clust.name <-ifelse(pam_fit$clustering ==1, "Low Budget","High Budget") #define lables
  
  
  #Combine runs with cluster number in a dataframe
  Runs_clust <- data.frame(Run= Unc.feas$Run, cluster=pam_fit$clustering, Scenario= clust.name ) 
  
  #Extract medoids
  Run_med <- Runs_clust[pam_fit$id.med,]
  
  #Combine with original data
  Unc_med <- merge(Run_med, Unc.feas, all.x=FALSE)
  
  #Combine with the reservoir releases
  
  #Merge reservoir releases data with clusters
  to_merge <- data.frame(Run=Unc.feas$Run, Cluster=pam_fit$clustering, Scenario = Runs_clust$Scenario  )
  
  #Combine all reservoir releases
  RR_plot <- merge(RR_corr, to_merge, by="Run")
  colnames(RR_plot)[4] <- "Release_Mm3mo"
  colnames(RR_plot)[5] <- "cluster"
  
  head(RR_plot)
  
  #Extract Hyrum data
  RR_plot_hyrum <- RR_plot %>%
    filter(Res== "j29")
  
  head(RR_plot_hyrum)
  
  
  #Remove old cluster column to add a hyrum-specific one
  RR_plot_hyrum_med <-  RR_plot_hyrum %>%
    select(-c(Scenario, cluster))
  
  # Now merge clusters with runs for hyrum only
  RR_med <- merge(RR_plot_hyrum_med, Run_med, by="Run")
  RR_med$cluster <- as.factor(RR_med$cluster) 
  
  
  
  #Correct Reservoir Releases for high needs
  
  RR_med <- RR_med %>%
    mutate(Release_Mm3mo = replace(Release_Mm3mo, cluster=="2" & time=="t8", 0.85 *Release_Mm3mo))
  
  
  
  
  #Plot reservoir releases for each cluster
  p <- ggplot(data= RR_med, aes(x=time , y=Release_Mm3mo, group=cluster, color=cluster))
  p <- p+ geom_line(aes(linetype= cluster, color=cluster), size=2)
  p <- p+ scale_x_discrete(limits=c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12"))
  p <- p+  labs(y="Reservoir Releases (Mm3/month)", x="Month")
  p <- p+  theme_classic(18) 
  p
  
  
  #############################################################
  # Plot reservoir releases per cluster combined with medoids
  #############################################################
  
  
  
  
  RR_plot_hyrum$cluster <- as.factor(RR_plot_hyrum$cluster)
  
  
  p <- ggplot(data= RR_plot_hyrum, aes(x=time , y=Release_Mm3mo))
  p <- p+ geom_line(alpha=0.05, aes(group=Run, color=Scenario))
  #Add medoid lines 
  p <- p+ geom_line(data = RR_med, aes(group=cluster, color=Scenario), size=3, linetype="dashed")
  #Add actual releases 
  p <- p+ geom_line(data = RR_hyrum, aes(x=time , y=Release, group=1, color="2003 Existing Releases"),  size=3)
  #Add optimal model - determinisitc
  p <- p+ geom_line(data = Release_det_hyrum_plot, aes(x=time , y=Release_Mm3mo, group=1, color="Deterministic model"),  size=3)
  
  p <- p+ scale_x_discrete(limits=c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12"))
  p <- p+  labs(y="Reservoir Releases (Mm3/month)", x="Month")
  p <- p+  theme_classic(26) 
  p
  
  
  p + facet_wrap(~ Scenario)
  
  
  ## Plot reduced dimension plots
  library(Rtsne)
  
  tsne_obj <- Rtsne(clust.dist, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering),
           name = Unc.feas$Run)
  
  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster))
  
  
  
  #Scatter plots
  
  #plot(Unc.feas$AquaticArea_km2 ,Unc.feas$rsi_centroid, col=pam_fit$cluster, type="p", xlab="Aquatic Habitat Area (km2)", ylab="RSI centroid")
  
  
  plot(Unc.feas$Budget ,Unc.feas$WASH_km2, col=pam_fit$cluster, type="p", xlab="Aquatic Habitat Area (km2)", ylab="RSI centroid")
  
  #Plot runs as text (use geom_text to activate check_overlap=TRUE)
  ggplot(data=Unc.feas)+geom_label(aes(x=Budget, y=WASH_km2, label=Unc.feas$Run, col=as.factor(pam_fit$cluster)), check_overlap = TRUE)+  theme_classic(14)
  
  pairs(~WASH_km2+ AquaticArea_km2+FloodplainArea_km2+WetlandsArea_km2+Demand_Mm3yr+ReachGain_Mm3yr+Budget+Reservoir_Release_Diff+PlantCover_km2yr, data=Unc.feas, col=pam_fit$cluster,
        main="Scatterplot Matrix", cex.labels=1.5)
  
  
  
  library(fpc)
  plotcluster(Unc.clust,pam_fit$cluster)
  
  clusplot(Unc.clust, pam_fit$cluster, color=TRUE, shade=TRUE, 
           labels=2, lines=0)
  
  
  
  
  
  # Trade-off
  #Some help here: https://www.image.ucar.edu/GSP/Software/Fields/Help/image.plot.html
  
  library(akima)
  library(fields)
  
  n_interp <- 300 #number of points to interpolate in between
  
  #Standarize the habitat areas
  # Note: The spreads of 'x' and 'y' must be within four orders
  # of magnitude of each other for 'interp' to work.
  
  # first remove det solution (abonormal)
  n <- dim(Unc.feas)[1]
  Unc_tradeoff <- Unc.feas[1:(n-1),]
  
  
  Z_Aquatic <- with(Unc_tradeoff , (AquaticArea_km2 - mean(AquaticArea_km2))/sd(AquaticArea_km2)) 
  Z_Floodplain <- with(Unc_tradeoff , (FloodplainArea_km2 - mean(FloodplainArea_km2))/sd(FloodplainArea_km2)) 
  Z_Wetlands <- with(Unc_tradeoff , (WetlandsArea_km2 - mean(WetlandsArea_km2))/sd(WetlandsArea_km2)) 
  
  
  interp_habitats <- interp(Z_Aquatic, Z_Floodplain, Z_Wetlands,
                           xo=seq(min(Z_Aquatic), max(Z_Aquatic), length = n_interp),
                            yo=seq(min(Z_Floodplain), max(Z_Floodplain), length = n_interp)  )
  
  image.plot(interp_habitats, xlab="Aquatic")
  
  
  
  
  #Plot tradeoff per cluster
  
  par(mfrow=c(1,2))
  par(mai=c(1.5,0.82,0.82,0.42))    
      
  
  #Add cluster number to clus.feas 
  Unc.feas.clust <- left_join(Unc.feas, to_merge, by="Run")
  
  #Remove optimal
  n1 <- dim(Unc.feas.clust)[1]
  Unc.feas.clust <- Unc.feas.clust[1:(n1-1),]
  
  #Cluster 1
  Unc.feas.clust1 <- Unc.feas.clust %>%
    filter (Cluster =="1")
  
  #Cluster 1 medoid
  Unc.feas.clust1.med <- Unc_med %>%
    filter (cluster == "1")
  
  
  #Standarize all cluster
  #Z1_Aquatic <- with(Unc.feas.clust1 , (AquaticArea_km2 - mean(AquaticArea_km2))/sd(AquaticArea_km2)) 
  Z1_Aquatic <- with(Unc.feas.clust1 , ( (AquaticArea_km2 - min(AquaticArea_km2))/(max(AquaticArea_km2) - min(AquaticArea_km2)))) 
  Z1_Floodplain <- with(Unc.feas.clust1 , ( (FloodplainArea_km2 - min(FloodplainArea_km2))/(max(FloodplainArea_km2) - min(FloodplainArea_km2)))) 
  Z1_Wetlands <- with(Unc.feas.clust1 , ( (WetlandsArea_km2 - min(WetlandsArea_km2))/(max(WetlandsArea_km2) - min(WetlandsArea_km2)))) 
  
  #Create dataframe of standarized data for cluster 1
  
  to_merge_Z1 <- data.frame(Run= Unc.feas.clust1$Run, Z1_Aquatic=Z1_Aquatic, Z1_Floodplain=Z1_Floodplain, Z1_Wetlands = Z1_Wetlands)
  
  clust1.Z <- merge(Unc.feas.clust1, to_merge_Z1, by="Run")
  
  #Find best members
  clust1.Z.best <- clust1.Z %>%
    filter(Z1_Floodplain >0.6, Z1_Aquatic >0.6, Z1_Wetlands > 0.8)
  
  clust1.Z.best
  
  
  #Find worst members
  clust1.Z.worst <- clust1.Z %>%
    filter(Z1_Floodplain <0.2, Z1_Aquatic <0.2, Z1_Wetlands < 0.2)
  
  clust1.Z.worst
  
  
  
  
  #Stadarize medoid
  Z1_med_Aquatic <-   (Unc.feas.clust1.med$AquaticArea_km2 - min(Unc.feas.clust1$AquaticArea_km2))/(max(Unc.feas.clust1$AquaticArea_km2) - min(Unc.feas.clust1$AquaticArea_km2)) 
  Z1_med_Floodplain <- (Unc.feas.clust1.med$FloodplainArea_km2  - min(Unc.feas.clust1$FloodplainArea_km2))/(max(Unc.feas.clust1$FloodplainArea_km2) - min(Unc.feas.clust1$FloodplainArea_km2)) 
  Z1_med_Wetlands <- (Unc.feas.clust1.med$WetlandsArea_km2  - min(Unc.feas.clust1$WetlandsArea_km2))/(max(Unc.feas.clust1$WetlandsArea_km2) - min(Unc.feas.clust1$WetlandsArea_km2)) 
  
  
  #Standarize optimal solution
  Z_opt_Aquatic <-   (Det_model$AquaticArea_km2 - min(Unc.feas$AquaticArea_km2))/(max(Unc.feas$AquaticArea_km2) - min(Unc.feas$AquaticArea_km2)) 
  Z_opt_Floodplain <- (Det_model$FloodplainArea_km2  - min(Unc.feas$FloodplainArea_km2))/(max(Unc.feas$FloodplainArea_km2) - min(Unc.feas$FloodplainArea_km2)) 
  Z_opt_Wetlands <- (Det_model$WetlandsArea_km2  - min(Unc.feas$WetlandsArea_km2))/(max(Unc.feas$WetlandsArea_km2) - min(Unc.feas$WetlandsArea_km2)) 
  
  
  
  #Interpolate
  interp_habitats_clust1 <- interp(Z1_Aquatic, Z1_Floodplain, Z1_Wetlands)
  
  image.plot(interp_habitats_clust1, xlab="Normalized Aquatic Habitat Area", ylab="Normalized FLoodplain Habitat Area", main= "Low Budget", col= rev(topo.colors(64)), cex.main=2.5, cex.axis= 2.5, cex.lab=2.5, 
             axis.args = list(cex.axis = 2.5),
             legend.args=list(text='Normalized Wetland Habitat Area', side=1, line=2.5, cex=2.5), horizontal = TRUE)
  points(Z1_med_Aquatic,Z1_med_Floodplain, col="black", pch=16 , cex= 4)
  points(Z_opt_Aquatic,Z_opt_Floodplain, col="purple", pch=16 , cex= 4)
  points(clust1.Z.best$Z1_Aquatic, clust1.Z.best$Z1_Floodplain, col="black", pch="H", cex=3)
  points(clust1.Z.worst$Z1_Aquatic, clust1.Z.worst$Z1_Floodplain, col="black", pch="L", cex=3)
  
  
  
  #Cluster 2
  Unc.feas.clust2 <- Unc.feas.clust %>%
    filter (Cluster =="2")
  
  #Cluster 2 medoid
  Unc.feas.clust2.med <- Unc_med %>%
    filter (cluster == "2")
  
  #Standarize
  Z2_Aquatic <- with(Unc.feas.clust2 , ( (AquaticArea_km2 - min(AquaticArea_km2))/(max(AquaticArea_km2) - min(AquaticArea_km2)))) 
  Z2_Floodplain <- with(Unc.feas.clust2 , ( (FloodplainArea_km2 - min(FloodplainArea_km2))/(max(FloodplainArea_km2) - min(FloodplainArea_km2)))) 
  Z2_Wetlands <- with(Unc.feas.clust2 , ( (WetlandsArea_km2 - min(WetlandsArea_km2))/(max(WetlandsArea_km2) - min(WetlandsArea_km2)))) 
  
  
  #Stadarize medoid
  Z2_med_Aquatic <-   (Unc.feas.clust2.med$AquaticArea_km2 - min(Unc.feas.clust2$AquaticArea_km2))/(max(Unc.feas.clust2$AquaticArea_km2) - min(Unc.feas.clust2$AquaticArea_km2)) 
  Z2_med_Floodplain <- (Unc.feas.clust2.med$FloodplainArea_km2  - min(Unc.feas.clust2$FloodplainArea_km2))/(max(Unc.feas.clust2$FloodplainArea_km2) - min(Unc.feas.clust2$FloodplainArea_km2)) 
  Z2_med_Wetlands <- (Unc.feas.clust2.med$WetlandsArea_km2  - min(Unc.feas.clust2$WetlandsArea_km2))/(max(Unc.feas.clust2$WetlandsArea_km2) - min(Unc.feas.clust2$WetlandsArea_km2)) 
  
  
  #Create dataframe of standarized data for cluster 2
  
  to_merge_Z2 <- data.frame(Run= Unc.feas.clust2$Run, Z2_Aquatic=Z2_Aquatic, Z2_Floodplain=Z2_Floodplain, Z2_Wetlands = Z2_Wetlands)
  
  clust2.Z <- merge(Unc.feas.clust2, to_merge_Z2, by="Run")
  
  #Find best members
  clust2.Z.best <- clust2.Z %>%
    filter(Z2_Floodplain >0.6, Z2_Aquatic >0.6, Z2_Wetlands > 0.8)
  
  clust2.Z.best
  
  
  #Find worst members
  clust2.Z.worst <- clust2.Z %>%
    filter(Z2_Floodplain <0.1, Z2_Aquatic <0.1, Z2_Wetlands < 0.15)
  
  clust2.Z.worst
  
  
  
  #Interpolate
  interp_habitats_clust2 <- interp(Z2_Aquatic, Z2_Floodplain, Z2_Wetlands)
  
  image.plot(interp_habitats_clust2, xlab="Normalized Aquatic Habitat Area", ylab="Normalized FLoodplain Habitat Area", main="High Budget", col= rev(topo.colors(64)), cex.main=2.5, cex.axis= 2.5, cex.lab=2.5, 
             axis.args = list(cex.axis = 2.5),
             legend.args=list(text='Normalized Wetland Habitat Area', side=1, line=2.5, cex=2.5), horizontal = TRUE)
  points(Z2_med_Aquatic,Z2_med_Floodplain, col="black", pch=16 , cex= 4)
  
  points(Z_opt_Aquatic,Z_opt_Floodplain, col="purple", pch=16 , cex= 4)
  points(clust2.Z.best$Z2_Aquatic, clust2.Z.best$Z2_Floodplain, col="black", pch="H", cex=3)
  points(clust2.Z.worst$Z2_Aquatic, clust2.Z.worst$Z2_Floodplain, col="black", pch="L", cex=3)
  
  
  
  
  #Export data to parallel plots
  names(Unc.feas.clust)
  
  Unc.par <- subset(Unc.feas.clust, select= c(WASH_km2, AquaticArea_km2, FloodplainArea_km2, 
                                              WetlandsArea_km2, Demand_Mm3yr, ReachGain_Mm3yr, 
                                              Budget, Reservoir_Release_Diff, 
                                              PlantCover_km2yr, Cluster))

  #Export to txt
  setwd("C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/Clustering/States/Global_only")
  write.table(Unc.par, "UncPar.txt", sep="\t", row.names = FALSE)
