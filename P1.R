library(sas7bdat)
library(nnet)
library(h2o)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(glmnet)
library(lmSupport)


cruzadaavnnet<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100)
    
  { 
    library(caret)
    library(dplyr)
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if  (listclass!=c(""))
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = TRUE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(error=mean(error))  
    
    return(medias)
    
  }

cruzadalin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5)
    
  { 
    library(caret)
    library(dplyr)
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if  (listclass!=c(""))
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    lineal<- train(formu,data=databis,
                   method="lm",trControl=control)
    
    print(lineal$results)
    
    preditest<-lineal$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(error=mean(error))  
    
    return(medias)
    
  }




#***********************************************************
#***********************************************************
#***********************************************************
#***********************************************************
#***********************************************************

# Lectura y esquema de variables
# Pasamos un archivo SAS a R
saratoga <- read.sas7bdat("C:/MASTER/2Cuatri/ML/SAS/ML/LIB/saratogar_train.sas7bdat")

# Nombramos las variables
dput(names(saratoga))
saratoga <- saratoga[,-18]


# c("price", "heating", "fuel", "waterfront", "newConstruction", 
#   "centralAir", "REP_pctCollege", "REP_rooms", "IMP_REP_age", "IMP_REP_bathrooms", 
#   "IMP_REP_bedrooms", "IMP_REP_fireplaces", "IMP_REP_landValue", 
#   "IMP_REP_livingArea", "LOG_IMP_REP_age", "SQR_IMP_REP_fireplaces", 
#   "EXP_REP_rooms")


continuas<-c("REP_pctCollege", "REP_rooms", "IMP_REP_age", "IMP_REP_bathrooms", 
             "IMP_REP_bedrooms", "IMP_REP_fireplaces", "IMP_REP_landValue", 
             "IMP_REP_livingArea", "LOG_IMP_REP_age", "SQR_IMP_REP_fireplaces", 
             "EXP_REP_rooms")
categoricas<-c("REP_heating", "fuel", "waterfront", "newConstruction", "centralAir")



# Pasar las categóricas a dummies
saratoga_dummies <- dummy.data.frame(saratoga, categoricas, sep=".")


# Estandarizar las variables continuas
# Calculo medias y dtipica de datos y estandarizo (solo las continuas)
means <- apply(saratoga_dummies[,  continuas], 2, mean) 
sds   <- sapply(saratoga_dummies[, continuas], sd) 



# Estandarizo solo las continuas y uno con las categoricas
saratoga_es <- scale(saratoga_dummies[, continuas], center=means, scale=sds) # escala (0,1)
numerocont  <- which(colnames(saratoga_dummies)%in%continuas)
saratoga_es <- cbind(saratoga_es, saratoga_dummies[, -numerocont])

dput(names(saratoga_es))


# NOTA: En los modelos pondremos solo k-1 dummies por cada categórica
c("REP_pctCollege", "REP_rooms", "IMP_REP_age", "IMP_REP_bathrooms", 
  "IMP_REP_bedrooms", "IMP_REP_fireplaces", "IMP_REP_landValue", 
  "IMP_REP_livingArea", "LOG_IMP_REP_age", "SQR_IMP_REP_fireplaces", 
  "EXP_REP_rooms", "price", "fuel.electric", "fuel.gas", "fuel.oil", 
  "waterfront.No", "waterfront.Yes", "newConstruction.No", "newConstruction.Yes", 
  "centralAir.No", "centralAir.Yes", "REP_heating.electric", "REP_heating.hot_air", 
  "REP_heating.hot_water_steam")


# ***************************
# SELECCION DE VARIABLES
# ***************************

set.seed(123123)

partitionIndex <- createDataPartition(saratoga_es$price, p=0.8, list=FALSE)
data_train <- saratoga_es[partitionIndex,]
data_test  <- saratoga_es[-partitionIndex,]


###########################################
## Seleccion de variables sin INTERACCIONES
# AIC
# Solo tiene un termino independiente
null <- lm(price~1, data=data_train)
# Lo tiene todo
full <- lm(price~., data=data_train)

modeloStepAIC <- step(null, scope=list(lower=null, upper=full), direction="both")

modeloStepAIC$coefficients



saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="REP_pctCollege"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="IMP_REP_bedrooms"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="IMP_REP_fireplaces"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="fuel.electric"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="fuel.gas"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="fuel.oil"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="waterfront.Yes"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="newConstruction.Yes"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="centralAir.Yes"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="REP_heating.electric"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="REP_heating.hot_air"]
saratoga_es <- saratoga_es[, !colnames(saratoga_es)=="REP_heating.hot_water_steam"]



View(saratoga_es)



# ***************************
# TUNING CON CARET
# ***************************

set.seed(321321)

# Validación cruzada una sola vez
control<-trainControl(method="cv", number=4, savePredictions="all") 

# Validación cruzada repetida
control<-trainControl(method="repeatedcv", number=5, repeats=10, savePredictions="all") 

# Training test una sola vez
control<-trainControl(method = "LGOCV",p=0.8,number=1,savePredictions = "all") 

# Training test repetido
control<-trainControl(method = "LGOCV", p=0.8, number=5, savePredictions = "all") 


# ***************************************************************
# avNNet: parámetros
# Number of Hidden Units (size, numeric)
# Weight Decay (decay, numeric)
# Bagging (bag, logical)
# ***************************************************************
avnnetgrid <-expand.grid(size=c(3, 4, 5, 6, 10, 15),
                         decay=c(0.01, 0.1, 0.001),
                         bag=FALSE)

redavnnet<- train(price~IMP_REP_livingArea+IMP_REP_landValue+IMP_REP_bathrooms+waterfront.No+
                    centralAir.No+EXP_REP_rooms+REP_rooms+newConstruction.No+LOG_IMP_REP_age+
                    IMP_REP_age+SQR_IMP_REP_fireplaces,
                  data=saratoga_es,
                  method="avNNet",
                  linout=TRUE,
                  maxit=100,
                  trControl=control,
                  repeats=5,
                  tuneGrid=avnnetgrid)

redavnnet



modelo1 <- cruzadaavnnet(data=saratoga_es,
                         vardep="price", 
                         listconti=c("IMP_REP_livingArea", "IMP_REP_landValue", "IMP_REP_bathrooms",
                                     "waterfront.No", "centralAir.No", "EXP_REP_rooms", "REP_rooms",
                                     "newConstruction.No", "LOG_IMP_REP_age", "IMP_REP_age", 
                                     "SQR_IMP_REP_fireplaces"),
                         listclass=c(""), grupos=5, sinicio=123123, repe=10,
                         size=c(4), decay=c(0.1), repeticiones=5, itera=100)

modelo1$modelo="avnnet1"

modelo2 <- cruzadaavnnet(data=saratoga_es,
                         vardep="price", 
                         listconti=c("IMP_REP_livingArea", "IMP_REP_landValue", "IMP_REP_bathrooms",
                                     "waterfront.No", "centralAir.No", "EXP_REP_rooms", "REP_rooms",
                                     "newConstruction.No", "LOG_IMP_REP_age", "IMP_REP_age", 
                                     "SQR_IMP_REP_fireplaces"),
                         listclass=c(""), grupos=5, sinicio=123123, repe=10,
                         size=c(5), decay=c(0.1), repeticiones=5, itera=100)

modelo2$modelo="avnnet2"

modelo3 <- cruzadaavnnet(data=saratoga_es,
                         vardep="price", 
                         listconti=c("IMP_REP_livingArea", "IMP_REP_landValue", "IMP_REP_bathrooms",
                                     "waterfront.No", "centralAir.No", "EXP_REP_rooms", "REP_rooms",
                                     "newConstruction.No", "LOG_IMP_REP_age", "IMP_REP_age", 
                                     "SQR_IMP_REP_fireplaces"),
                         listclass=c(""), grupos=5, sinicio=123123, repe=10,
                         size=c(6), decay=c(0.1), repeticiones=5, itera=100)

modelo3$modelo="avnnet3"

modelo4 <- cruzadaavnnet(data=saratoga_es,
                         vardep="price", 
                         listconti=c("IMP_REP_livingArea", "IMP_REP_landValue", "IMP_REP_bathrooms",
                                     "waterfront.No", "centralAir.No", "EXP_REP_rooms", "REP_rooms",
                                     "newConstruction.No", "LOG_IMP_REP_age", "IMP_REP_age", 
                                     "SQR_IMP_REP_fireplaces"),
                         listclass=c(""), grupos=5, sinicio=123123, repe=10,
                         size=c(10), decay=c(0.1), repeticiones=5, itera=100)

modelo4$modelo="avnnet4"

modelo5 <- cruzadaavnnet(data=saratoga_es,
                         vardep="price", 
                         listconti=c("IMP_REP_livingArea", "IMP_REP_landValue", "IMP_REP_bathrooms",
                                     "waterfront.No", "centralAir.No", "EXP_REP_rooms", "REP_rooms",
                                     "newConstruction.No", "LOG_IMP_REP_age", "IMP_REP_age", 
                                     "SQR_IMP_REP_fireplaces"),
                         listclass=c(""), grupos=5, sinicio=123123, repe=10,
                         size=c(15), decay=c(0.1), repeticiones=5, itera=100)


modelo5$modelo="avnnet5"


modelo6<-cruzadalin(data=saratoga_es,
                    vardep="price",listconti=c("IMP_REP_livingArea", "IMP_REP_landValue", "IMP_REP_bathrooms",
                                               "waterfront.No", "centralAir.No", "EXP_REP_rooms", "REP_rooms",
                                               "newConstruction.No", "LOG_IMP_REP_age", "IMP_REP_age", 
                                               "SQR_IMP_REP_fireplaces"),
                    listclass=c(""),grupos=5,sinicio=123123,repe=5)

modelo6$modelo="lineal"

union1<-rbind(modelo1,modelo2,modelo3,modelo4, modelo5, modelo6)

par(cex.axis=1)
boxplot(data=union1,error~modelo)




