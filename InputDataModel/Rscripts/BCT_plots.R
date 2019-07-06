source("theme_classic_correct.R")

# fiting nonlinear curve using ggplot 

setwd("C:/Users/Ayman/Box Sync/USU/Thesis/Clean Data")

rsi80 <-read.csv("RSI80.csv", header=TRUE)

head(rsi80)


fit.fun = function(C,D) {(0+((1-0)/(1+exp((C-Depth)/D))))}



## Fit the non-linear equation
fit.nls <- nls(RSI ~ (0+((1-0)/(1+exp((C-Depth)/D)))), start=list(C=.48,D=0.1), data=rsi80)

## Examine the fit
summary(fit.nls)

## Create a new dataframe to hold results
nls.df <- rsi80
nls.df$Fit <- fitted(fit.nls)  # save fitted values
nls.df$Residuals <- residuals(fit.nls)  # save residuals
head(nls.df)

## Plot goodness of fit
plot(Fit ~ RSI, data = nls.df)
abline(a=0, b=1, col="red")

geom_point(data = rsi80)

ggplot(data=nls.df, aes(x=Depth, y=RSI))+geom_line()

plot(nls.df$Fit~rsi80$Depth )

attach(nls.df)

curve(fit.fun())


p <- ggplot(nls.df, aes(x=Depth))
### Add the line
p <- p + geom_line(aes(y=Fit))
### Add your points
p <- p + geom_point(aes(y=RSI), colour="red")
### Apply formatting, set font to 9 pt
p <- p + theme_classic_correct(9)
### Name the axes
p <- p + scale_y_continuous(name="Y axis likelihood?")
p <- p + scale_x_continuous(name="X axis something else")
p


### Save to pdf png or svg
ggsave("plot.png", p, width=6, height=4, dpi=600)
ggsave("plot.pdf", p, width=6, height=4)
ggsave("plot.svg", p, width=6, height=4)


