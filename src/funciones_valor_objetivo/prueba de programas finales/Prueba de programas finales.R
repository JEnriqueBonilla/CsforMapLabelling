setwd("C:/Users/Enrique/Desktop/Tesis/Programas Finales/")
source("MCSlabel.R")

data <- Data50
labels <- data$Labels
x <- data$x
y <- data$y

#Probar con parametros finales para MCS, y cs (4 algoritmos)
centro<-c(400,.01,.9,50,6,4,2,.05)
t <- 209302394
set.seed(t)
inisol<- sample(1:8,length(x),replace=T)

plot(x,y,col="red",pch=20,cex=1)

MCSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
SAmax=centro[4], InitialSol=inisol,Inef=centro[8])
