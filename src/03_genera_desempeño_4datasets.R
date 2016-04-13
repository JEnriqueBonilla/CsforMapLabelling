########################### DataL100 con nombres de mujeres ###############

data <- DataL100.nom
labels <- data$nombres
x <- data$x
y <- data$y
seed <- c(124458, 115551, 119801, 116952, 122524)

######################################
######### algoritmo 1 #################
######################################
lista.datos <- list()
i <- 0

centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  
  set.seed(t)
  
  inisol<- sample(1:8,length(x),replace=T)
  
  
  plot(x,y,col="red",pch=20,cex=1, main = "MCS")
  
  
  mcssol <- MCSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol,Inef=centro[8])
  
  lista.datos[[i]] <- data.table(id = 1:length(mcssol), mcssol = mcssol)
  
}


data.mcs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(mcs = mean(mcssol))

######################################
######### algoritmo 2 #################
######################################
lista.datos <- list()
i <- 0
for(t in seed){ 
  i <- i + 1
  set.seed(t) 
  plot(x,y,col="red",pch=20,cex=1, main ="Pointlabel (SANN)")
  pointsol <- pointlabel2(x, y, labels = labels, cex = 1, method ="SANN", allowSmallOverlap = F, trace = T, doPlot = T) 
  
  
  
  lista.datos[[i]] <- data.table(id = 1:length(pointsol), pointsol = pointsol)
  
}


data.pointlabel <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(pointlabel = mean(pointsol))

# sol = 21.35766 

######################################
######### algoritmo 3 #################
######################################

lista.datos <- list()
i <- 0

for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS")
  
  cssol <- CSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(cssol), cssol = cssol)
  
}


data.cs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs = mean(cssol))


#14
######################################
######### algoritmo 4 #################
######################################
lista.datos <- list()
i <- 0

#centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS con LS2")
  
  csls2sol <- CSlabells2(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(csls2sol), csls2sol = csls2sol)
  
}


data.csls2 <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs_ls2 = mean(csls2sol))


# mcssol
# cssol
# csls2sol
# length(mcssol)
# length(cssol)
# length(csls2sol)
# data.graf <- data.table(id =1:length(csls2sol), mcssol =mcssol, cssol=cssol,  csls2sol= csls2sol)
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
data.graf <- data.cs %>%
  left_join(data.csls2) %>%
  left_join(data.mcs)  %>%
  left_join(data.pointlabel)
cache("data.graf")

punto <- as.numeric(data.graf[50,]$pointlabel)

(graf1 <- ggplot(data.graf, aes(x= id))+geom_line(aes(y = log(mcs,10), color = "MCS"))+
  scale_colour_brewer(name = "Algoritmos", palette="Set1") +
  geom_line(aes(y = log(cs,10), color = "CS"))+ 
  geom_line(aes(y =log(cs_ls2,10), color = "CS con LS2"))  +
  geom_line(aes(y =log(pointlabel,10), color = "Pointlabel")) +
  geom_point(aes(x=50, y = log(punto, 10)), color ="red")+
  geom_text(aes(x=50, y = log(punto, 10), label = "Punto de paro, Pointlabel"),hjust=-.03,just=0, size=5, fontface="plain")+
  # geom_text(aes(label=ifelse(log(pointlabel,10)==log(34.63116, 10), "sANN",'')),hjust=0,just=0) +
  xlab("Iteración") + ylab("Logaritmo base 10 de la función objetivo") + labs(title ="DataL100 con nombres de mujeres")) 

cache("graf1")

########################### Data100 con nombres de hombres ###############

data <- Data100.nom
labels <- data$nombres
x <- data$x
y <- data$y
seed <- c(124458, 115551, 119801, 116952, 122524)

######################################
######### algoritmo 1 #################
######################################
lista.datos <- list()
i <- 0

centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  
  set.seed(t)
  
  inisol<- sample(1:8,length(x),replace=T)
  
  
  plot(x,y,col="red",pch=20,cex=1, main = "MCS")
  
  
  mcssol <- MCSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol,Inef=centro[8])
  
  lista.datos[[i]] <- data.table(id = 1:length(mcssol), mcssol = mcssol)
  
}


data.mcs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(mcs = mean(mcssol))

######################################
######### algoritmo 2 #################
######################################
lista.datos <- list()
i <- 0
for(t in seed){ 
  i <- i + 1
  set.seed(t) 
  plot(x,y,col="red",pch=20,cex=1, main ="Pointlabel (SANN)")
  pointsol <- pointlabel2(x, y, labels = labels, cex = 1, method ="SANN", allowSmallOverlap = F, trace = T, doPlot = T) 
  
  
  
  lista.datos[[i]] <- data.table(id = 1:length(pointsol), pointsol = pointsol)
  
}


data.pointlabel <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(pointlabel = mean(pointsol))

# sol = 21.35766 

######################################
######### algoritmo 3 #################
######################################

lista.datos <- list()
i <- 0

for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS")
  
  cssol <- CSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(cssol), cssol = cssol)
  
}


data.cs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs = mean(cssol))


#14
######################################
######### algoritmo 4 #################
######################################
lista.datos <- list()
i <- 0

#centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS con LS2")
  
  csls2sol <- CSlabells2(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(csls2sol), csls2sol = csls2sol)
  
}


data.csls2 <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs_ls2 = mean(csls2sol))


# mcssol
# cssol
# csls2sol
# length(mcssol)
# length(cssol)
# length(csls2sol)
# data.graf <- data.table(id =1:length(csls2sol), mcssol =mcssol, cssol=cssol,  csls2sol= csls2sol)
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
data.graf.2 <- data.cs %>%
  left_join(data.csls2) %>%
  left_join(data.mcs)  %>%
  left_join(data.pointlabel)
cache("data.graf.2")

punto <- as.numeric(data.graf.2[50,]$pointlabel)

(graf2 <- ggplot(data.graf.2, aes(x= id))+geom_line(aes(y = log(mcs,10), color = "MCS"))+
   scale_colour_brewer(name = "Algoritmos", palette="Set1") +
   geom_line(aes(y = log(cs,10), color = "CS"))+ 
   geom_line(aes(y =log(cs_ls2,10), color = "CS con LS2"))  +
   geom_line(aes(y =log(pointlabel,10), color = "Pointlabel")) +
   geom_point(aes(x=50, y = log(punto, 10)), color ="red")+
   geom_text(aes(x=50, y = log(punto, 10), label = "Punto de paro, Pointlabel"),hjust=-.03,just=0, size=5, fontface="plain")+
   # geom_text(aes(label=ifelse(log(pointlabel,10)==log(34.63116, 10), "sANN",'')),hjust=0,just=0) +
   xlab("Iteración") + ylab("Logaritmo base 10 de la función objetivo") + labs(title ="Data100 con nombres de hombres")) 

cache("graf2")

########################### DataL75 con nombres y apellidos ###############

data <- DataL75.nom
labels <- data$nombres
x <- data$x
y <- data$y
seed <- c(124458, 115551, 119801, 116952, 122524)

######################################
######### algoritmo 1 #################
######################################
lista.datos <- list()
i <- 0

centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  
  set.seed(t)
  
  inisol<- sample(1:8,length(x),replace=T)
  
  
  plot(x,y,col="red",pch=20,cex=1, main = "MCS")
  
  
  mcssol <- MCSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol,Inef=centro[8])
  
  lista.datos[[i]] <- data.table(id = 1:length(mcssol), mcssol = mcssol)
  
}


data.mcs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(mcs = mean(mcssol))

######################################
######### algoritmo 2 #################
######################################
lista.datos <- list()
i <- 0
for(t in seed){ 
  i <- i + 1
  set.seed(t) 
  plot(x,y,col="red",pch=20,cex=1, main ="Pointlabel (SANN)")
  pointsol <- pointlabel2(x, y, labels = labels, cex = 1, method ="SANN", allowSmallOverlap = F, trace = T, doPlot = T) 
  
  
  
  lista.datos[[i]] <- data.table(id = 1:length(pointsol), pointsol = pointsol)
  
}


data.pointlabel <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(pointlabel = mean(pointsol))

# sol = 21.35766 

######################################
######### algoritmo 3 #################
######################################

lista.datos <- list()
i <- 0

for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS")
  
  cssol <- CSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(cssol), cssol = cssol)
  
}


data.cs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs = mean(cssol))


#14
######################################
######### algoritmo 4 #################
######################################
lista.datos <- list()
i <- 0

#centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS con LS2")
  
  csls2sol <- CSlabells2(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(csls2sol), csls2sol = csls2sol)
  
}


data.csls2 <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs_ls2 = mean(csls2sol))


# mcssol
# cssol
# csls2sol
# length(mcssol)
# length(cssol)
# length(csls2sol)
# data.graf <- data.table(id =1:length(csls2sol), mcssol =mcssol, cssol=cssol,  csls2sol= csls2sol)
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
data.graf.3 <- data.cs %>%
  left_join(data.csls2) %>%
  left_join(data.mcs)  %>%
  left_join(data.pointlabel)
cache("data.graf.3")

punto <- as.numeric(data.graf.3[50,]$pointlabel)

(graf3 <- ggplot(data.graf.3, aes(x= id))+geom_line(aes(y = log(mcs,10), color = "MCS"))+
   scale_colour_brewer(name = "Algoritmos", palette="Set1") +
   geom_line(aes(y = log(cs,10), color = "CS"))+ 
   geom_line(aes(y =log(cs_ls2,10), color = "CS con LS2"))  +
   geom_line(aes(y =log(pointlabel,10), color = "Pointlabel")) +
   geom_point(aes(x=50, y = log(punto, 10)), color ="red")+
   geom_text(aes(x=50, y = log(punto, 10), label = "Punto de paro, Pointlabel"),hjust=-.03,just=0, size=5, fontface="plain")+
   # geom_text(aes(label=ifelse(log(pointlabel,10)==log(34.63116, 10), "sANN",'')),hjust=0,just=0) +
   xlab("Iteración") + ylab("Logaritmo base 10 de la función objetivo") + labs(title ="DataL75 con nombres y apellidos")) 

cache("graf3")
########################### Nueva base de datos con apellidos ###############

data <- Data81.nom
labels <- data$nombres
x <- data$x
y <- data$y
seed <- c(124458, 115551, 119801, 116952, 122524)

######################################
######### algoritmo 1 #################
######################################
lista.datos <- list()
i <- 0

centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  
  set.seed(t)
  
  inisol<- sample(1:8,length(x),replace=T)
  
  
  plot(x,y,col="red",pch=20,cex=1, main = "MCS")
  
  
  mcssol <- MCSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol,Inef=centro[8])
  
  lista.datos[[i]] <- data.table(id = 1:length(mcssol), mcssol = mcssol)
  
}


data.mcs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(mcs = mean(mcssol))

######################################
######### algoritmo 2 #################
######################################
lista.datos <- list()
i <- 0
for(t in seed){ 
  i <- i + 1
  set.seed(t) 
  plot(x,y,col="red",pch=20,cex=1, main ="Pointlabel (SANN)")
  pointsol <- pointlabel2(x, y, labels = labels, cex = 1, method ="SANN", allowSmallOverlap = F, trace = T, doPlot = T) 
  
  
  
  lista.datos[[i]] <- data.table(id = 1:length(pointsol), pointsol = pointsol)
  
}


data.pointlabel <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(pointlabel = mean(pointsol))

# sol = 21.35766 

######################################
######### algoritmo 3 #################
######################################

lista.datos <- list()
i <- 0

for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS")
  
  cssol <- CSlabel(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(cssol), cssol = cssol)
  
}


data.cs <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs = mean(cssol))


#14
######################################
######### algoritmo 4 #################
######################################
lista.datos <- list()
i <- 0

#centro <- c(3000,.01,.975,100,9,3,2,.35)
for(t in seed){ 
  i <- i + 1
  set.seed(t)
  inisol<- sample(1:8,length(x),replace=T)
  
  plot(x,y,col="red",pch=20,cex=1, main = "CS con LS2")
  
  csls2sol <- CSlabells2(x, y, labels = labels, cex = 1, method = c("CS" 
  ), allowSmallOverlap = FALSE, trace = T, doPlot = T, Objective="dispersion", 
  NuClusters=centro[5], Taumax=centro[6], Bmax=centro[7], T0=centro[1], TE=centro[2], alpha=centro[3],
  SAmax=centro[4], InitialSol=inisol)
  
  
  lista.datos[[i]] <- data.table(id = 1:length(csls2sol), csls2sol = csls2sol)
  
}


data.csls2 <- lista.datos  %>% rbindlist %>% group_by(id)  %>% summarise(cs_ls2 = mean(csls2sol))


# mcssol
# cssol
# csls2sol
# length(mcssol)
# length(cssol)
# length(csls2sol)
# data.graf <- data.table(id =1:length(csls2sol), mcssol =mcssol, cssol=cssol,  csls2sol= csls2sol)
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
# data.graf.1 <- data.table(id =1:length(mcssol), resultado =mcssol, algoritmo = rep("mcsol", length(mcssol)))
data.graf.4 <- data.cs %>%
  left_join(data.csls2) %>%
  left_join(data.mcs)  %>%
  left_join(data.pointlabel)
cache("data.graf.4")

punto <- as.numeric(data.graf.4[50,]$pointlabel)

(graf4 <- ggplot(data.graf.4, aes(x= id))+geom_line(aes(y = log(mcs,10), color = "MCS"))+
   scale_colour_brewer(name = "Algoritmos", palette="Set1") +
   geom_line(aes(y = log(cs,10), color = "CS"))+ 
   geom_line(aes(y =log(cs_ls2,10), color = "CS con LS2"))  +
   geom_line(aes(y =log(pointlabel,10), color = "Pointlabel")) +
   geom_point(aes(x=50, y = log(punto, 10)), color ="red")+
   geom_text(aes(x=50, y = log(punto, 10), label = "Punto de paro, Pointlabel"),hjust=-.03,just=0, size=5, fontface="plain")+
   # geom_text(aes(label=ifelse(log(pointlabel,10)==log(34.63116, 10), "sANN",'')),hjust=0,just=0) +
   xlab("Iteración") + ylab("Logaritmo base 10 de la función objetivo") + labs(title ="Nueva base de datos con apellidos") ) 

cache("graf4")
