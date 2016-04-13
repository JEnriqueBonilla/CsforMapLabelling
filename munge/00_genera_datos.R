# paquete utilizado 
require(randomNames)

### instalación 
# install.packages("devtools")
# require(devtools)
# install_github("CenterForAssessment/randomNames")
# require(randomNames)

### variables para este paquete
# gender 1 = female 0 = male
# ethnicity 1:5 american india, asian, african, hispanic, white (non hispacic)

### nombres femeninos

set.seed(923023)

test.df = data.frame(genero = rep(1, 100)) 

nombres <- randomNames(n = 100, gender = test.df$genero , which.names = "first" )

dato <- data.frame( Labels = paste("etiqueta", 1:100, sep =""), nombres = nombres)

DataL100.nom <- DataL100 %>%
  left_join(dato)


### nombres masculinos

set.seed(923023)

test.df = data.frame(genero = rep(0, 100)) 

nombres <- randomNames(n = 100, gender = test.df$genero , which.names = "first" )

dato <- data.frame( Labels = paste("etiqueta", 1:100, sep =""), nombres = nombres)

Data100.nom <- Data100 %>%
  left_join(dato)

### nombres con apellido

set.seed(923023)

test.df = data.frame(genero = sample(c(0,1), 75, replace= T)) 

nombres <- randomNames(n = 75, gender = test.df$genero , which.names = "both", name.sep= ", " )

dato <- data.frame( Labels = paste("etiqueta", 1:75, sep =""), nombres = nombres)

DataL75.nom <- DataL75 %>%
  left_join(dato)

### nombres con apellido, nueva base

set.seed(923023)

nombres <- randomNames(n = 71, which.names = "last")


x1 <- seq(-4, 4, .2)
y1 <- x1*(x1-4)*(x1+4)

x2 <- rep(-2,15) + runif(15, -.5, .5)
y2 <- rep(-10,15) + runif(15, -3, 3)

x3 <- rep(2,15) + runif(15, -.5, .5)
y3 <- rep(10,15) + runif(15, -3, 3)

Data81.nom  <- data.frame(x = c(x1,x2,x3), y =c(y1,y2, y3), nombres )


