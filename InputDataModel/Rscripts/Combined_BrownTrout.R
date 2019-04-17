## Fry Brown Trout

source("theme_classic_correct.R")
library(ggplot2)

# fiting nonlinear curve using ggplot 


Fry50 <-read.csv("BT_Fry50.csv", header=TRUE)

head(Fry50)


#fit.fun = function(C,D) {(0+((1-0)/(1+exp((C-Depth_cm)/D))))}



## Fit the non-linear equation
#fit.nls <- nls(RSI ~ (0+((1-0)/(1+exp((C-Depth_cm)/D)))), start=list(C=.48,D=0.1), data=rsi80)

#fit.nls <- nls(RSI ~ (1/(1+exp(-k*(Depth_cm - C)))), data=rsi80, start=list(k=5, C=0.2))

fit.nls.F <- nls(RSI ~ (1-exp(-k*Depth_cm)), data=Fry50, start = list(k=2) )

## Examine the fit
summary(fit.nls.F)

## Create a new dataframe to hold results
nls.df.F <- Fry50
nls.df.F$Fit <- fitted(fit.nls.F)  # save fitted values
nls.df.F$Residuals <- residuals(fit.nls.F)  # save residuals
head(nls.df.F)

##Adult Brown Trout

# fiting nonlinear curve using ggplot 


Adult80 <-read.csv("BT_Adult80.csv", header=TRUE)

head(Adult80)


#fit.fun = function(C,D) {(0+((1-0)/(1+exp((C-Depth_cm)/D))))}



## Fit the non-linear equation
#fit.nls <- nls(RSI ~ (0+((1-0)/(1+exp((C-Depth_cm)/D)))), start=list(C=.48,D=0.1), data=rsi80)

#fit.nls <- nls(RSI ~ (1/(1+exp(-k*(Depth_cm - C)))), data=rsi80, start=list(k=5, C=0.2))

fit.nls.A <- nls(RSI ~ (1-exp(-k*Depth_cm)), data=Adult80, start = list(k=10) )

## Examine the fit
summary(fit.nls.A)

## Create a new dataframe to hold results
nls.df.A <- Adult80
nls.df.A$Fit <- fitted(fit.nls.A)  # save fitted values
nls.df.A$Residuals <- residuals(fit.nls.A)  # save residuals
head(nls.df.A)




#####################################
#Plot the curve


p <- ggplot(nls.df.A, aes(x=Depth_cm))
### Add the line
p <- p + geom_line(aes(y=nls.df.A$Fit, color= "Adult"), size =1.2) + geom_line(aes(y=nls.df.F$Fit, color="Fry"), linetype ="dashed", size=1.2)
#Add a legend title
p <- p + labs(color ="Brown Trout\nLife Stage")


### Apply formatting, set font to 12 pt
p <- p + theme_classic_correct(12)

#Change the order of legend items
#p <- p+ theme(legend.position = "top")


### Name the axes
p <- p + scale_y_continuous(name="Aquatic Habitat Suitability")
p <- p + scale_x_continuous(name="Water Depth (m)")

p <- p+scale_color_manual(values = c("red","black"))



p


ggsave("Brown_Trout_plot.png", p, width=6, height=4, dpi=600)
ggsave("Brown_Trout_plot.pdf", p, width=6, height=4)
ggsave("Brown_Trout_plot.svg", p, width=6, height=4)


