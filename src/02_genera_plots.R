##### las funciones estan en la carpeta de lib

#### inicializar parámetros
centro <- c(3000,.01,.975,100,9,3,2,.35)
t <- 209302394
data <- Data81.nom
labels <- data$nombres
x <- data$x
y <- data$y

######################################
######### algoritmo 1 ################
######################################

set.seed(t)
inisol<- sample(1:8,length(x),replace=T)

plot(x,y,col="red",pch=20,cex=1) + title("MCS")

MCSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
SAmax=centro[4], InitialSol=inisol,Inef=centro[8])


######################################
######### algoritmo 2 ################
######################################

set.seed(t)

plot(x,y,col="red",pch=20,cex=1) + title("Pointlabel (SANN)")

pointlabel2(x, y, labels = labels, cex = 1, method ="SANN", 
            allowSmallOverlap = F, trace = T, doPlot = T) 

######################################
######### algoritmo 3 ################
######################################

set.seed(t)
inisol<- sample(1:8,length(x),replace=T)

plot(x,y,col="red",pch=20,cex=1) + title("CS")

CSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
SAmax=centro[4], InitialSol=inisol,Inef=centro[8])

######################################
######### algoritmo 4 ################
######################################

set.seed(t)
inisol<- sample(1:8,length(x),replace=T)

plot(x,y,col="red",pch=20,cex=1) + title("CS con LS2")

CSlabells2(x, y, labels = labels, cex = 1, method = c("CS" 
), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
SAmax=centro[4], InitialSol=inisol,Inef=centro[8])


