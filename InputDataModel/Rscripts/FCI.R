setwd("C:/Users/Ayman/Box Sync/USU/Thesis/Clean Data/FCI_csv")

FCIdata <-read.csv(file.choose(), header=TRUE)

head(FCIdata)

library("mosaic", lib.loc="~/R/win-library/3.3")


fFlow <- fitModel(FCI ~ (0+((1-0)/(1+exp((C-Flow)/D)))), data=FCIdata, start=list( C=284.5, D=13.935))

coef(fFlow)

plotPoints(FCI~Flow, data=FCIdata, col="white", xlab="Flow (Mm3/month)", ylab="Floodplain Connectivity Index [unitless]", main="Bear River at Corrine")

plotFun(fFlow(Flow)~Flow, FCI.lim=range(0,1), xlab="Flow (Mm3/month)", ylab="Floodplain Connectivity Index [unitless]", col="red", lwd=3, lty=1, add=TRUE )

abline(v=440, col=3)
