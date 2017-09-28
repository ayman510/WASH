###R code:

rm(list = ls())  #Clear history
library(ggplot2)
library(gdxrrw)
library(reshape2)

n_points <- 20

n_c <- 10
n_d <- 8

### Create a vector of Cs and Ds and then a dataframe with all combinations
C <- seq(0.35, 0.65, length.out = n_c)
D <- seq(0.01, 0.06, length.out = n_d)
nls_coef_df<- expand.grid(C=C, D=D)

### Loop through each combination
for (n in seq(1,dim(nls_coef_df)[1])){

### Extract C and D for this combination
C_temp <- nls_coef_df$C[n]
D_temp <- nls_coef_df$D[n]

### Generate random depths and find hsi values
X <- seq(0.3,0.9,length.out=n_points)  #depth

Y <- 0+((1-0)/(1+exp((C_temp-X)/D_temp)))  #hsi Boltzmann


### Create a datafrme from generated points
data_df <- data.frame(X=X, Y=Y, Run=n, C=C_temp, D=D_temp)

### Stick the coefficients back into a dataframe
### If it's the first time through, you need to create the dataframe to hold the results
### Otherwise, you just add the next line
if (n ==1) {
 line_results <- data_df
} else {
 line_results <- rbind(line_results, data_df)
}
}

X_op <- seq(0.3, 0.9, length.out =10)
Y_op <- 0+((1-0)/(1+exp((0.55-X_op)/0.0544)))
df_op <- data.frame(X=X_op, Y=Y_op)


### Plot all lines and use group to plot multiple lines
p <- ggplot(line_results, aes(x=X, y=Y  ))
p <- p + geom_line(alpha=0.7, aes(group = Run))
p <- p + geom_line(data=df_op, aes(x=X), size=2, linetype="dashed", color="red")
p <- p + theme_classic(20)
p <- p + labs(x="Water Depth (m) for brown trout", y="Aquatic Suitability Index")
p <- p +  ylim(0,1)
p
#ggsave("Coef_plots.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/HSI_Shape/BCT30_75/SampleCoef")


#require(viridis)

#p <- ggplot(line_results, aes(x=X, y=Y , colour=C, group = Run))
#p <- p + geom_line(alpha=0.7)
#p <- p + scale_color_viridis() 
#p <- p + theme_classic(14)
#p <- p + labs(x="Water Depth (m)", y="Aquatic Suitability Index")
#p <- p +  ylim(0,1)
#p
#ggsave("Coef_ColorC.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/HSI_Shape/BCT30_75/SampleCoef")


#p <- ggplot(line_results, aes(x=X, y=Y , colour=D, group = Run))
#p <- p + geom_line(alpha=0.7)
#p <- p + scale_color_viridis() 
#p <- p + theme_classic(14)
#p <- p + labs(x="Water Depth (m)", y="Aquatic Suitability Index")
#p <- p +  ylim(0,1)
#p
#ggsave("Coef_ColorD.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/HSI_Shape/BCT30_75/SampleCoef")


#p <- ggplot(line_results, aes(x=C, y=D))
#p<-  p + labs(x="Centroid of the hsi curve", y="Slope of the hsi curve")
#p <- p + stat_density_2d(aes(fill = ..level..), geom="polygon")
#p <- p + geom_point(col="red")
#p
#ggsave("Coef.jpeg", path="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/HSI_Shape/BCT30_75/SampleCoef")


#Write coefficients to csv file
#write.csv(nls_coef_df, file="C:/Users/Ayman/Box Sync/USU/Thesis/GAMS/GAMSCode/MonteCarlo/HSI_Shape/BCT30_75/SampleCoef/coef.csv")


#wgdx.lst('out',list(data_df))
