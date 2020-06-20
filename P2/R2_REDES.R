#Carga librerias
library(caret)
library(sas7bdat)
library(pROC)
library(glmnet)

library(nnet)
library(h2o)
library(dummies)
library(MASS)
library(reshape)


clinical <- read.sas7bdat("C:\\MASTER\\2Cuatri\\ML\\SAS\\ML\\LIB\\set_r_train.sas7bdat")

clinical <- clinical[,-8] #borramos warn
dput(names(clinical))
summary(clinical)
dependiente<-c("status")
continuas<-c("minimum_age", "maximum_age", "n_collaborators", "n_countries", 
             "IMP_REP_enrollment", "IMP_number_of_arms", "TI_G_REP_group_type1", 
             "TI_G_REP_group_type2", "TI_G_REP_group_type3", "TI_G_REP_primary_purpose1", 
             "TI_G_REP_primary_purpose2", "TI_G_agency_class1", "TI_G_agency_class2", 
             "TI_G_country1", "TI_G_country2", "TI_G_country3", "TI_G_country4", 
             "TI_G_country5", "TI_G_country6", "TI_G_country7", "TI_G_gender1", 
             "TI_G_gender2", "TI_G_intervention_type1", "TI_G_intervention_type2", 
             "TI_G_intervention_type3", "TI_G_intervention_type4", "TI_G_masking1", 
             "TI_G_masking2", "TI_G_masking3", "TI_G_masking4", "TI_REP_allocation1", 
             "TI_REP_allocation2", "TI_REP_allocation3", "TI_REP_has_dmc1", 
             "TI_REP_has_dmc2", "TI_REP_has_dmc3", "TI_enrollment_type1", 
             "TI_enrollment_type2", "TI_healthy_volunteers1", "TI_healthy_volunteers2", 
             "TI_phase1", "TI_phase2")

# Vemos el reparto de 0s y 1s
prop.table(table(clinical$status))

clinical$status <-ifelse(clinical$status==1,"Yes","No")

clinical$status <- as.factor(clinical$status)

# Particion de datos
set.seed(12345)
partitionIndex <- createDataPartition(clinical$status, p=0.8, list=FALSE)
data_train <- clinical[partitionIndex,]
data_test  <- clinical[-partitionIndex,]



#*************************#
#** Seleccion variables **#
#*************************#
# aic
null<-glm(status~1,data=data_train,family=binomial)
full<-glm(status~.,data=data_train,family=binomial)
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")

modeloBackAIC$coefficients
modeloStepAIC$coefficients

# bic
modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward", k=log(nrow(data_train)))
modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",     k=log(nrow(data_train)))

modeloBackBIC$coefficients
modeloStepBIC$coefficients



#***********************#
#** CRUZADA LOGISTICA **#
#***********************#
cruzadalogistica <- function(data=data, vardep=NULL, listconti=NULL, listclass=NULL,
                             grupos=4, sinicio=1234, repe=5) {
  library(dummies)
  library(MASS)
  library(reshape)
  library(caret)
  library(dplyr)
  library(pROC)
  
  if (listclass != c("")) {
    for (i in 1:dim(array(listclass))) {
      numindi<-which(names(data)==listclass[[i]])
      data[,numindi]<-as.character(data[,numindi])
      data[,numindi]<-as.factor(data[,numindi])
    }
  }   
  
  data[,vardep]<-as.factor(data[,vardep])
  
  # Creo la formula para la logistica
  if (listclass!=c("")) {
    koko<-c(listconti,listclass)
  } else {
    koko<-c(listconti)
  }
  
  modelo<-paste(koko,sep="",collapse="+")
  formu<-formula(paste(vardep,"~",modelo,sep=""))
  
  formu 
  
  # Preparo caret   
  set.seed(sinicio)
  control<-trainControl(method="repeatedcv",
                        number=grupos,
                        repeats=repe,
                        savePredictions="all",
                        classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  regresion <- train(formu,
                     data=data,
                     trControl=control,
                     method="glm",
                     family=binomial(link="logit"))                  
  preditest<-regresion$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold  <- sapply(preditest$prueba, "[", 1)
  preditest$Rep   <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
    confu<-confusionMatrix(x,y)
    tasa<-confu[[3]][1]
    return(tasa)
  }
  
  # Aplicamos funciÃ³n sobre cada RepeticiÃ³n
  
  medias<-preditest %>% 
    group_by(Rep) %>% 
    summarize(tasa=1-tasafallos(pred,obs))
  
  # CalculamoS AUC  por cada RepeticiÃ³n de cv 
  # Definimnos funciÃ³n
  auc<-function(x,y) {
    curvaroc<-roc(response=x,predictor=y)
    auc<-curvaroc$auc
    return(auc)
  }
  
  # Aplicamos funciÃ³n sobre cada RepeticiÃ³n
  mediasbis<-preditest %>%
    group_by(Rep) %>%
    summarize(auc=auc(obs,Yes))
  
  # Unimos la info de auc y de tasafallos
  medias$auc<-mediasbis$auc
  
  return(medias)
  
}




# *********************************
# CRUZADA avNNet
# **************


cruzadaavnnetbin<-function(data=data, vardep="vardep", 
                           listconti="listconti", listclass="listclass", 
                           grupos=4, sinicio=1234, repe=5, size=c(5), 
                           decay=c(0.01), repeticiones=5, itera=100) { 
  
  # PreparaciÃ³n del archivo
  
  # b)pasar las categÃ³ricas a dummies
  
  if (listclass!=c("")) {
    databis<-data[,c(vardep,listconti,listclass)]
    databis<- dummy.data.frame(databis, listclass, sep = ".")
  } else {
    databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center=means, scale=sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  databis[,vardep]<-as.factor(databis[,vardep])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method="repeatedcv", number=grupos, repeats=repe,
                        savePredictions="all", classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  avnnetgrid<-expand.grid(size=size, decay=decay, bag=FALSE)
  
  avnnet<-train(formu, data=databis, method="avNNet", linout=FALSE, maxit=itera, 
                repeats=repeticiones, trControl=control, tuneGrid=avnnetgrid)
  
  print(avnnet$results)
  
  preditest<-avnnet$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold  <- sapply(preditest$prueba, "[", 1)
  preditest$Rep   <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
    confu<-confusionMatrix(x,y)
    tasa<-confu[[3]][1]
    return(tasa)
  }
  
  # Aplicamos funciÃ³n sobre cada RepeticiÃ³n
  
  medias<-preditest %>%
    group_by(Rep) %>%
    summarize(tasa=1-tasafallos(pred,obs))
  
  # CalculamoS AUC  por cada RepeticiÃ³n de cv 
  # Definimnos funciÃ³n
  
  auc<-function(x,y) {
    curvaroc<-roc(response=x,predictor=y)
    auc<-curvaroc$auc
    return(auc)
  }
  
  # Aplicamos funciÃ³n sobre cada RepeticiÃ³n
  
  mediasbis<-preditest %>%
    group_by(Rep) %>%
    summarize(auc=auc(obs,Yes))
  
  # Unimos la info de auc y de tasafallos
  
  medias$auc<-mediasbis$auc
  
  return(medias)
  
}



set1 <- c("minimum_age", "maximum_age", "n_countries", "IMP_REP_enrollment", 
          "TI_enrollment_type1", "TI_G_country1", "TI_G_country2", "TI_G_country3", 
          "TI_G_country4", "TI_G_country5", "TI_G_country6", "TI_G_gender1", 
          "TI_G_intervention_type1", "TI_G_intervention_type2", "TI_G_intervention_type3", 
          "TI_G_masking1", "TI_G_masking2", "TI_G_masking3", 
          "TI_G_REP_group_type1", "TI_G_REP_group_type2", "TI_G_REP_primary_purpose1", 
          "TI_healthy_volunteers1", "TI_phase1", 
          "TI_REP_allocation1", "TI_REP_allocation2", 
          "TI_REP_has_dmc1", "TI_REP_has_dmc2") 

set2 <- c("minimum_age", "maximum_age", "n_countries", "IMP_REP_enrollment", 
          "TI_enrollment_type1", "TI_G_country1", "TI_G_country2", "TI_G_country3", 
          "TI_G_country4", "TI_G_country5", "TI_G_country7", "TI_G_gender1", 
          "TI_G_intervention_type1", "TI_G_intervention_type4", 
          "TI_G_masking1", "TI_G_masking3", "TI_G_masking4", 
          "TI_G_REP_group_type3", "TI_G_REP_primary_purpose1", 
          "TI_healthy_volunteers1", "TI_phase1", 
          "TI_REP_allocation1", "TI_REP_allocation3", "TI_REP_has_dmc3") 
          
set3 <- c("minimum_age", "maximum_age", "n_countries", "IMP_REP_enrollment", 
          "TI_G_country2", "TI_G_country3", "TI_G_country4", "TI_G_country5", 
          "TI_G_country6", "TI_G_country7", "TI_G_gender1", "TI_G_intervention_type1", 
          "TI_G_masking1", "TI_G_masking2", "TI_G_masking3", "TI_G_REP_primary_purpose1", 
          "TI_REP_allocation1", "TI_REP_has_dmc1", "TI_REP_has_dmc2") 
          
set4 <- c("minimum_age", "maximum_age", "n_countries", "IMP_REP_enrollment", 
          "TI_G_country1", 
          "TI_G_country2", 
          "TI_G_country3", 
          "TI_G_country7", 
          "TI_G_gender1", 
          "TI_G_intervention_type1", 
          "TI_G_intervention_type4", 
          "TI_G_masking1", 
          "TI_G_masking3", 
          "TI_G_masking4", 
          "TI_G_REP_primary_purpose1", 
          "TI_REP_allocation1", 
          "TI_REP_has_dmc3") 
          
M1 <- cruzadalogistica(data=clinical, vardep=dependiente,
                       listconti=set1, listclass=c(""), 
                       grupos=5, sinicio=1234, repe=5)
M1$modelo="modeloBackAIC"


M2 <- cruzadalogistica(data=clinical, vardep=dependiente,
                       listconti=set2, listclass=c(""), 
                       grupos=5, sinicio=1234, repe=5)
M2$modelo="modeloStepAIC"


M3 <- cruzadalogistica(data=clinical, vardep=dependiente,
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=1234, repe=5)
M3$modelo="modeloBackBIC"

M4 <- cruzadalogistica(data=clinical, vardep=dependiente,
                       listconti=set4, listclass=c(""), 
                       grupos=5, sinicio=1234, repe=5)
M4$modelo="modeloStepBIC"

union_regresion <- rbind(M1, M2, M3, M4)

par(cex.axis=1)
boxplot(data=union_regresion,tasa~modelo,main="TASA FALLOS")
boxplot(data=union_regresion,auc~modelo,main="AUC")



red1<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(3), decay=c(0.1), repeticiones=5, itera=200)
red1$modelo="Red1"

red2<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(3), decay=c(0.001), repeticiones=5, itera=200)
red2$modelo="Red2"

red3<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(6), decay=c(0.1), repeticiones=5, itera=200)
red3$modelo="Red3"

red4<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(6), decay=c(0.001), repeticiones=5, itera=200)
red4$modelo="Red4"

red5<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(7), decay=c(0.1), repeticiones=5, itera=200)
red5$modelo="Red5"

red6<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(7), decay=c(0.001), repeticiones=5, itera=200)
red6$modelo="Red6"

red7<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(8), decay=c(0.1), repeticiones=5, itera=200)
red7$modelo="Red7"

red8<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(8), decay=c(0.001), repeticiones=5, itera=200)
red8$modelo="Red8"

red9<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(9), decay=c(0.1), repeticiones=5, itera=200)
red9$modelo="Red9"

red10<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(9), decay=c(0.001), repeticiones=5, itera=200)
red10$modelo="Red10"

red11<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=12345, repe=5,
                       size=c(12), decay=c(0.1), repeticiones=5, itera=200)
red11$modelo="Red11"

red12<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                        listconti=set3, listclass=c(""), 
                        grupos=5, sinicio=12345, repe=5,
                        size=c(12), decay=c(0.001), repeticiones=5, itera=200)
red12$modelo="Red12"

union_red <- rbind(red1, red2, red3, red4, red5, red6, red7, red8, red9, red10, red11, red12)

boxplot(data=union_red,tasa~modelo,main="TASA FALLOS")
boxplot(data=union_red,auc~modelo,main="AUC")




red9<-cruzadaavnnetbin(data=clinical, vardep=dependiente, 
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=54321, repe=5,
                       size=c(9), decay=c(0.1), repeticiones=5, itera=200)
red9$modelo="Red9"


M3 <- cruzadalogistica(data=clinical, vardep=dependiente,
                       listconti=set3, listclass=c(""), 
                       grupos=5, sinicio=54321, repe=5)
M3$modelo="modeloBackBIC"

union_final <- rbind(red9, M3)

boxplot(data=union_final,tasa~modelo,main="TASA FALLOS")
boxplot(data=union_final,auc~modelo,main="AUC")


