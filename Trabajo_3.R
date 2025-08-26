require(magrittr)
require(ggplot2)
require(readxl)
require(readr)
require(MASS)
require(boot)
require(xtable)
require(olsrr)
library(olsrr)
require(leaps)
library(Amelia)

# Punto 1:----------------------------------------------------------------------

HW3 <- read.csv("c:Datos\\data_HW3.csv",sep = ",",header = TRUE)
HW3 <- HW3[,2:3]
dim(HW3)

# A) Método: Conjunto de validación --------------------------------------------

# Seleccionando el conjunto de entrenamiento y prueba (70-30)%
set.seed(3713613)
train_id <- sample(101,0.7*nrow(HW3)) 
trainHW3 <- HW3[train_id,]
testHW3 <- HW3[-train_id,]

# Relación entre y Y x 

HW3 %>% ggplot(aes(x=x,y=y))+
  geom_point(col="#0BC9DF")+
  labs(x="X",y="Y",title = " Relación entre X y Y")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

# datos seleccionados para   entrenamiento 

HW3 %>% ggplot(aes(x=x,y=y,color="Datos de validacion"))+
  geom_point()+
  geom_point(data = trainHW3,aes(x=x,y=y,color="Datos de Entrenamiento"))+
  labs(color="Datos",x="X",y="Y",title = " Relación entre X y Y")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2, 0.2),)

# Ajuste delos polinomios de grado 1-10-----------------------------------------

MSE <- c()
for(i in 1:10){
  model <- lm(y~poly(x,i),data = trainHW3)
  pred <- predict(model,testHW3)
  MSE[i] <- mean((pred-testHW3$y)^2)
}
  
# solución A: Puntos MSE asociados al grado del polinomio-----------------------

plot(1:10,MSE, xlab="Grado", ylab="MSE",type="b",col=4)
grid()
title(main = expression("Puntos MSE asociados al grado del polinomio"))

# sub="test_MSE")


# Solución B: Método  LOOCV-----------------------------------------------------
# Ajuste de los 10 polinomios de grados 1-10
MSELOOCV <- c()
for(i in 1:10){
  modelglm <- glm(y~poly(x,i),data = HW3)
  cv.error <- cv.glm(HW3,modelglm)
  MSELOOCV[i] <- cv.error$delta[2]
}
 

plot(1:10,MSELOOCV, xlab="Grado", ylab="MSE Promedio",type="b",col=2)
grid()
title(main = expression("Puntos promedios del MSE del Método LOOCV \nsociados al grado del polinomio"))
      
# sub="test_MSE promedios")

# Solución C: k-fold cross validation-------------------------------------------
# Ajuste de los 10 polinomios de grado 1-10

MSEKfold <- c()
for(i in 1:10){
  modelglm <- glm(y~poly(x,i),data = HW3)
  cv.error.k <- cv.glm(HW3,modelglm,K = 10)
  MSEKfold[i] <- cv.error.k$delta[2]
}


plot(1:10,MSEKfold, xlab="Grado", ylab="MSE Promedio",type="b",col=3)
grid()
title(main = expression("Puntos promedios del MSE del Método K-fold \nasociados al grado del polinomio"))
    #sub="test_MSE promedios")


cbind(MSE,MSELOOCV,MSEKfold) %>% as.data.frame() %>% xtable(digits = 6)

# Punto 2:----------------------------------------------------------------------

dim(surgical)
surgical %>% names()
# los Datos están completos 
missmap(surgical)
# Mejor subconjunto ------------------------------------------------------------

chose.full <- regsubsets(y~., data=surgical, nvmax=8)
chose.full %>% summary()
with(summary(chose.full),data.frame(adjr2,rss,outmat))

# Resúmenes de los modelos con distintos subconjuntos de variables.

par(mfrow =c(2,2),plt = c(0.25, 0.95, 0.4, 0.95))
plot(summary(chose.full)$rss ,xlab="Number of Variables",ylab=" RSS",type="l", xlim=c(0,10))
a1<-which.min(summary(chose.full)$rss)
points (a1, summary(chose.full)$rss[a1], col ="red",cex =2, pch =20)

plot(summary(chose.full)$adjr2 ,xlab =" Number of Variables",ylab=" Adjusted RSq"
     ,type="l")
a2<-which.max(summary(chose.full)$adjr2)
points(a2, summary(chose.full)$adjr2[a2], col ="red",cex =2, pch =20)

plot(summary(chose.full)$cp ,xlab =" Number of Variables",ylab="Cp"
     ,type="l")
a3<-which.min(summary(chose.full)$cp)
points(a3, summary(chose.full)$cp[a3], col ="red",cex =2, pch =20)


plot(summary(chose.full)$bic ,xlab =" Number of Variables",ylab="BIC"
     ,type="l")
a4<-which.min(summary(chose.full)$bic)
points(a4, summary(chose.full)$bic[a4], col ="red",cex =2, pch =20)

dev.off()

# Por el criterio de cp (criterio de mallows)

plot(chose.full,scale = "Cp")
#plot(chose.full,scale = "bic")
#plot(chose.full,scale = "adjr2")
#plot(chose.full,scale = "r2")


# Con el criterio de Cp se seleccionan un subconjunto de 4 covariables 
# (el intercepto no se cuenta) y  son (bcs,pindex,enzyme_test,alc_heavy)

# Selección hacia adelante------------------------------------------------------

chose1.full <- regsubsets(y~.,data = surgical,nvmax = 8,method = "forward")
chose1.resu <- summary(chose1.full)

with(chose1.resu,data.frame(adjr2,rss,cp,bic,outmat))

par(mfrow =c(2,2),plt = c(0.25, 0.95, 0.4, 0.95))

plot(chose1.resu$rss ,xlab="Number of Variables",ylab=" RSS",type="l", xlim=c(0,9))
b1<-which.min(chose1.resu$rss)
points (b1, summary(chose1.full)$rss[b1], col ="red",cex =2, pch =20)

plot(chose1.resu$adjr2 ,xlab =" Number of Variables",ylab=" Adjusted RSq"
     ,type="l")
b2<-which.max(chose1.resu$adjr2)
points(b2, chose1.resu$adjr2[b2], col ="red",cex =2, pch =20)

plot(chose1.resu$cp ,xlab =" Number of Variables",ylab="Cp"
     ,type="l")
b3<-which.min(chose1.resu$cp)
points(b3, chose1.resu$cp[b3], col ="red",cex =2, pch =20)


plot(chose1.resu$bic ,xlab =" Number of Variables",ylab="BIC"
     ,type="l")
b4<-which.min(chose1.resu$bic)
points(b4, chose1.resu$bic[b4], col ="red",cex =2, pch =20)

par(mfrow =c(1,2),plt = c(0.1, 0.7, 0.1, 0.95))
plot(chose1.full,scale = "Cp")
plot(chose1.full,scale = "adjr2")

# Método hacia atrás ------------------------------------------------------------------------------

chose2.full <- regsubsets(y~.,data = surgical,nvmax = 8,method = "backward")
chose2.resu <- summary(chose2.full)


par(mfrow =c(2,2),plt = c(0.25, 0.95, 0.4, 0.95))

plot(chose2.resu$rss ,xlab="Number of Variables",ylab=" RSS",type="l", xlim=c(0,9))
c1 <- which.min(chose2.resu$rss)
points(c1, summary(chose2.full)$rss[c1], col ="red",cex =2, pch =20)

plot(chose2.resu$adjr2 ,xlab ="Number of Variables",ylab=" Adjusted RSq"
     ,type="l")
c2 <- which.max(chose2.resu$adjr2)
points(c2, chose2.resu$adjr2[c2], col ="red",cex =2, pch =20)

plot(chose2.resu$cp ,xlab =" Number of Variables",ylab="Cp"
     ,type="l")
c3 <- which.min(chose2.resu$cp)
points(c3, chose2.resu$cp[c3], col ="red",cex =2, pch =20)

plot(chose2.resu$bic ,xlab =" Number of Variables",ylab="BIC"
     ,type="l")
c4 <- which.min(chose2.resu$bic)
points(c4, summary(chose2.full)$bic[c4], col ="red",cex =2, pch =20)

par(mfrow =c(1,2),plt = c(0.1, 0.7, 0.1, 0.95))
plot(chose2.full,scale = "Cp")
plot(chose2.full,scale = "bic")

# Seleccion stepwise------------------------------------------------------------

chose3.full <- regsubsets(y~.,data = surgical,nvmax = 8,method = "seqrep")
chose3.resu <- summary(chose3.full)


par(mfrow =c(2,2),plt = c(0.25, 0.95, 0.4, 0.95))

plot(chose3.resu$rss ,xlab="Number of Variables",ylab=" RSS",type="l", xlim=c(0,9))
d1 <- which.min(chose3.resu$rss)
points(d1, chose3.resu$rss[d1], col ="red",cex =2, pch =20)

plot(chose3.resu$adjr2 ,xlab ="Number of Variables",ylab=" Adjusted RSq"
     ,type="l")
d2 <- which.max(chose3.resu$adjr2)
points(d2, chose3.resu$adjr2[d2], col ="red",cex =2, pch =20)

plot(chose3.resu$cp ,xlab =" Number of Variables",ylab="Cp"
     ,type="l")
d3 <- which.min(chose3.resu$cp)
points(d3, chose3.resu$cp[d3], col ="red",cex =2, pch =20)

plot(chose3.resu$bic ,xlab =" Number of Variables",ylab="BIC"
     ,type="l")
d4 <- which.min(chose3.resu$bic)
points(d4, summary(chose3.full)$bic[d4], col ="red",cex =2, pch =20)

par(mfrow =c(1,2),plt = c(0.1, 0.7, 0.1, 0.95))
plot(chose3.full,scale = "Cp")
plot(chose3.full,scale = "bic")


