MCSlabel<-function (x, y = NULL, labels = seq(along = x), cex = 1, method = c("CS" 
), allowSmallOverlap = FALSE, trace = FALSE, doPlot = TRUE,Objective=c("dispersion","mncp","mncflp"), 
NuClusters=nuclusters, Taumax=taumax, Bmax=bmax, T0=t0, TE=te, alpha=ALPHA,
SAmax=samax, InitialSol=inisol,Inef=inef, ...) 
{
  ####una etiqueta a modificar
  #### se modifica el concepto de radio y se a?ade el concepto de lejan?a
  #### se conserva la modificaci?n del centro y no s?lo del gen
  if (!missing(y) && (is.character(y) || is.expression(y))) {
    labels <- y
    y <- NULL
  }
  labels <- as.graphicsAnnot(labels)
  boundary <- par()$usr
  xyAspect <- par()$pin[1]/par()$pin[2]
  toUnityCoords <- function(xy) {
    list(x = (xy$x - boundary[1])/(boundary[2] - boundary[1]) * 
           xyAspect, y = (xy$y - boundary[3])/(boundary[4] - 
                                                 boundary[3])/xyAspect)
  }
  toUserCoords <- function(xy) {
    list(x = boundary[1] + xy$x/xyAspect * (boundary[2] - 
                                              boundary[1]), y = boundary[3] + xy$y * xyAspect * 
           (boundary[4] - boundary[3]))
  }
  z <- xy.coords(x, y, recycle = TRUE)
  z <- toUnityCoords(z)
  x <- z$x      
  y <- z$y
  if (length(labels) < length(x)) 
    labels <- rep(labels, length(x))
  method <- match.arg(method)
  if (allowSmallOverlap) 
    nudgeFactor <- 0.02
  n_labels <- length(x)
  width <- (strwidth(labels, units = "figure", cex = cex) + 
              0.015) * xyAspect
  height <- (strheight(labels, units = "figure", cex = cex) + 
               0.015)/xyAspect
  gen_offset <- function(code) c(-1, -1, -1, 0, 0, 1, 1, 1)[code] * 
    (width/2) + (0+1i) * c(-1, 0, 1, -1, 1, -1, 0, 1)[code] * 
    (height/2)
  rect_intersect <- function(xy1, offset1, xy2, offset2) {
    w <- pmin(Re(xy1 + offset1/2), Re(xy2 + offset2/2)) - 
      pmax(Re(xy1 - offset1/2), Re(xy2 - offset2/2))
    h <- pmin(Im(xy1 + offset1/2), Im(xy2 + offset2/2)) - 
      pmax(Im(xy1 - offset1/2), Im(xy2 - offset2/2))
    w[w <= 0] <- 0
    h[h <= 0] <- 0
    w * h
  }
  nudge <- function(offset) {
    doesIntersect <- rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                    rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                    rectv[rectidx2]) > 0
    pyth <- abs(xy[rectidx1] + offset[rectidx1] - xy[rectidx2] - 
                  offset[rectidx2])/nudgeFactor
    eps <- 1e-10
    for (i in which(doesIntersect & pyth > eps)) {
      idx1 <- rectidx1[i]
      idx2 <- rectidx2[i]
      vect <- (xy[idx1] + offset[idx1] - xy[idx2] - offset[idx2])/pyth[idx1]
      offset[idx1] <- offset[idx1] + vect
      offset[idx2] <- offset[idx2] - vect
    }
    offset
  }
  objective <- function(gene) {
    offset <- gen_offset(gene)
    if (allowSmallOverlap) 
      offset <- nudge(offset)
    if (!is.null(rectidx1)){
      if(Objective=="dispersion"){
        area <- sum(rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                   rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                   rectv[rectidx2]))
      }else{if(Objective=="mncp"){area <- sum(rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                                             
                                                             rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                                             rectv[rectidx2])>0)
                                  
      }else{
        intersect<-rect_intersect(xy[rectidx1] + offset[rectidx1], 
                                  rectv[rectidx1], xy[rectidx2] + offset[rectidx2], 
                                  rectv[rectidx2])
        area<-0
        for(i in 1:n_labels){
          if(any(c(intersect[which(rectidx1==i)],intersect[which(rectidx2==i)])>0)){
            area<- 1+area}
        }
      }
      
      }
    }
    else{area<-0} 
    n_outside <- sum(Re(xy + offset - rectv/2) < 0 | Re(xy + 
                                                          offset + rectv/2) > xyAspect | Im(xy + offset - rectv/2) < 
                       0 | Im(xy + offset + rectv/2) > 1/xyAspect)
    if(Objective=="dispersion")
      res <- 1000 * area + n_outside else res<- area + n_outside
    
    res
  }
  xy <- x + (0+1i) * y
  rectv <- width + (0+1i) * height
  rectidx1 <- rectidx2 <- array(0, (length(x)^2 - length(x))/2)
  k <- 0
  for (i in 1:length(x)) for (j in seq(len = (i - 1))) {
    k <- k + 1
    rectidx1[k] <- i
    rectidx2[k] <- j
  }
  canIntersect <- rect_intersect(xy[rectidx1], 2 * rectv[rectidx1], 
                                 xy[rectidx2], 2 * rectv[rectidx2]) > 0
  rectidx1 <- rectidx1[canIntersect]
  rectidx2 <- rectidx2[canIntersect]
  if (trace) 
    cat("possible intersects =", length(rectidx1), "\n")
  if (trace) 
    cat("portion covered =", sum(rect_intersect(xy, rectv, 
                                                xy, rectv)), "\n")
  #M?todo CS 
  #######################################################################
  cs <- function() {
    # Se inicializan par?metros.
    LocalSearch<-function(geneCe){
      repeat{
        antscoreCe<-objective(geneCe)
        scoreCe<-antscoreCe
        newgeneCe<-geneCe
        for(i in 1:n_labels){
          for(j in 1:8){
            if(as.double(geneCe[i])!=j){
              newgeneCe[i] <- j
              newscoreCe<-objective(newgeneCe)
              if (newscoreCe < scoreCe) {
                geneCe <- newgeneCe
                scoreCe <- newscoreCe
                
              }else{newgeneCe[i]<-geneCe[i]}
            }
          }
          
          
        }
        if(antscoreCe<=scoreCe){
          break
        }
      }
      return(geneCe)
    }
    
    C<-list()
    scoreC<-rep(0,NuClusters)
    Tau<-rep(0,NuClusters)
    B<-rep(0,NuClusters)
    gene<-InitialSol
    score <- objective(gene)
    bestgene <- gene
    bestscore <- score
    per<-floor(n_labels*Inef)
    
    ###########################
    
    #se crean soluciones representantes del grupo
    for(i in 1:NuClusters){
      
      C[[i]] <- sample(1:8,n_labels,replace=T)
      scoreC[i]<-objective(as.double(C[[i]]))
    }
    ############################################
    
    lista<-c()
    while (T0 > TE) {
      lista <- c(lista,bestscore)
      bestscoreant<-bestscore #condici?n de no cambio 
      #Simulated Annealing
      for (j in 1:SAmax) {
        newgene <- gene
        newgene[sample(1:n_labels, 1)] <- sample(1:8, 
                                                 1)
        newscore <- objective(newgene)
        if (newscore <= score || runif(1) < exp((score - 
                                                   newscore)/T)) {
          
          score <- newscore
          gene <- newgene
        }
        #Nota: se le a?ade esto al c?digo del Paper (2014)
        if (score <= bestscore) {
          bestscore <- score
          bestgene <- gene
        }
        if (bestscore == 0) 
          break
        ###################################################
      }
      ####################
      
      T0 <- alpha * T0
      #distancia de Hamming respecto a S
      dist<-rep(0,NuClusters)
      for(i in 1:NuClusters){
        dist[i]<-sum(mapply(identical,as.integer(gene),C[[i]])) #typeof(gene)=double, typeof(c[[i]])=intenger
      }
      
      
        Pmax<-which(dist== max(dist)) #posici?n de el/los Centro(s) m?s cercano(s) a S
        Tau[Pmax[1]]<-Tau[Pmax[1]]+1
        
      if(score<=scoreC[Pmax[1]]) {#best center
          C[[Pmax[1]]]<-as.integer(gene)
          scoreC[Pmax[1]]<-score
        }
    
      if(Tau[Pmax[1]]==Taumax){
        Tau[Pmax[1]]<-0
        gene<-LocalSearch(as.double(C[[Pmax[1]]]))
        score<-objective(gene)
        
        #Nota: se le a?ade esto al c?digo del Paper (2014)
        if (score <= bestscore) {
          bestscore <- score
          bestgene <- gene
        }
        #################################################
        if(score==scoreC[Pmax[1]]){
          B[Pmax[1]]<-B[Pmax[1]]+1
          if(B[Pmax[1]]==Bmax){
            pos_change<-sample(1:n_labels,per)
            C[[Pmax[1]]][pos_change]<-sample(1:8,length(pos_change),replace=T)
            scoreC[Pmax[1]]<-objective(as.double(C[[Pmax[1]]]))
            C[[Pmax[1]]]<-as.integer(C[[Pmax[1]]])
            B[Pmax[1]]<-0
          }
          
        }else{
          B[Pmax[1]]<-0
          ##########################esto lo agrego yo#########################
          C[[Pmax[1]]]<-as.integer(gene)
          scoreC[Pmax[1]]<-score
          ####################################################################
        }
        
      }
      if (scoreC[Pmax[1]] <= bestscore) {
        bestscore <- scoreC[Pmax[1]]
        bestgene <- as.double(C[[Pmax[1]]])
      }  
      
      if (bestscore == 0) 
        break
      if (trace) 
        cat("overlap area =", bestscore, "\n")
    }
    
    
    if (trace) 
      cat("overlap area =", bestscore, "\n")
    nx <- Re(xy + gen_offset(bestgene))
    ny <- Im(xy + gen_offset(bestgene))
    list(x = nx, y = ny, l = lista)
    
  }

  xy <- cs()
  #xy <- toUserCoords(xy)
  if (doPlot) 
    text(xy, labels, cex = cex)
  invisible(xy)
  
  return(xy$l)

}