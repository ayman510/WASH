#Boltzmaann Sigmoidal function

library("mosaic", lib.loc="~/R/win-library/3.3")
HabitatData<-read.csv(file.choose(), header=TRUE)
head (HabitatData)


bolt <- function (Habitat,center,slope) {

 
  
  f <- fitModel(SI ~ (0+((1-0)/(1+exp((C-Att)/D)))), data=HabitatData, start=list(C=.4, D=.1))
  
  coef(f)
  
  plotFun(f(SI)~Att, y.lim=range(0,1), xlab="xlabel", ylab="ylabel", col="blue", lwd=1, lty=1 )
  
  
  
}