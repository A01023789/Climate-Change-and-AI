#Modelo de regresión para saber qué lleva a los países a tener un NDC de waste

#Especificamos de donde viene la información
dir.data<-"/Users/marianacornejo/Documents/GitHub/Climate-Change-and-AI/Data/Model//"
data<-read.csv(paste0(dir.data,"ModelData.csv"))
#Summary
summary(data)

#Limpiar columnas
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.03])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#Respuesta de interés, en este caso es WASTE
ids<-c("iso_code3","country")
response<-"m_waste"
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
predictors
length(predictors)
summary(data[,response])

threshold<- 0.5
#Guardar como un factor
data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
summary(data$response.binary)

# remove NA values in the response
#Quitar todos los NA de la variable de interés
data<-subset(data,is.na(response.binary)==FALSE)

#now look at how many high emission cases we have
#Resumen de la respuesta binaria
summary(data$response.binary)

data.model<-data[,c("response.binary",predictors)]
head(data.model)
#eliminar NA's
data.model<-data.model[complete.cases(data.model),]
summary(data.model)
dim(data.model)

#MODELO Lineal
#Estimación del modelo
#estimate full model
data.modelml<-data[,c(response,predictors)]
head(data.modelml)
#Modelo como una formula de las emisiones per capita en funcion de esta información
#Remover las variables altamente relacionadas
modelml<-as.formula(paste0(response,"~",paste(predictors[1:20],collapse="+")))



#Ya no salen NA's pero tampoco hay variables muy significativas
full.modelml <- lm(modelml, data = data.modelml)
summary(full.modelml)
dim(data.modelml)

#Al correr la regresión lineal se puede apreciar que las variables más significativas son a_agriculture, a_urban, m_agriculture, m_energy
#Se intentará correr un modelo más pequeño

library(leaps)

#Dividiremos la muestra en un conjunto de entrenamiento y uno de prueba para probar el modelo
set.seed (55555)
train <- sample (c(TRUE ,FALSE), nrow(data.modelml ),rep=TRUE)
train
test  <- (!train )
test

#Definiremos el largo del modelo, ya que se encontraron 3 variables muy significativas indicaremos que se creen modelo con máximo 5 variables
max.vars<-5

#Para la construcción del modelo se utilizarán 3 tecnicas para determinar cual es mejor, una selección completa, una a través de la tecnica forward y una a través de backward
#Selección completa
regfit.best <- regsubsets (modelml, data.modelml[train,], nvmax =max.vars,really.big = T) 

#Técnica forward stepwise selection
regfit.fwd <- regsubsets (modelml, data.modelml[train,], nvmax =max.vars,really.big = T, method = "forward") 

#Técnica backard stepwise selection
regfit.bwd <- regsubsets (modelml, data.modelml[train,], nvmax =max.vars,really.big = T, method = "backward")

#Exploraremos cada uno de ellos
msize<-5
coef(regfit.best ,msize)
coef(regfit.fwd , msize)
coef(regfit.bwd , msize)

#Observamos que con el método completo selecciona las variables a_energy, a_urban, a_water, m_buildings, m_lulucf
#Con el método forward stepwise: a_energy, a_urban, m_buildings, m_energy, m_lulucf
#Con el método backard stepwise: a_energy, a_urban, a_water, m_buildings, m_lulucf
#Se obtuvo que por dos técnicas tenemos que las mejores variables para la predicción son a_energy, a_urban, a_water, m_buildings, m_lulucf

#Para determinar cuál de lo tres modelos es mejor:
predict.regsubsets <-function (object, model ,newdata ,id ){
  object<-regfit.best
  newdata<-data.modelml[test ,]
  #Quiero el modelo con 4 predictores
  id<-5
  form<-modelml
  options(na.action='na.pass')
  #Dame una matriz con esta formula y estos datos
  mat<-model.matrix (form,newdata )
  dim(mat)
  coefi<-coef(object ,id=id)
  coefi
  #Quedarme con las variables x
  xvars<-names (coefi )
  pred<-mat[,xvars ]%*% coefi
  pred
  val.errors<- mean((newdata[,response]-pred)^2,na.rm=TRUE)
  val.errors
}

#Estimaremos la tasa de error de los tres modelos para saber cual tiene el menor error
cv.best<-data.frame(subset.type="best",
                    nvars=1,
                    test.mse=predict.regsubsets(regfit.best,modelml,data.modelml[test ,],1))
cv.best

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="best",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.best,modelml,data.modelml[test ,],i))
  cv.best<-rbind(cv.best,pivot)
  
}
cv.best

#best model
subset(cv.best,test.mse==min(test.mse))
#actual model
coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

#backward method
cv.bwd<-data.frame(subset.type="bwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.bwd,modelml,data.modelml[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="bwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.bwd,modelml,data.modelml[test ,],i))
  cv.bwd<-rbind(cv.bwd,pivot)
  
}

#best model
subset(cv.bwd,test.mse==min(test.mse))
#actual model
coef(regfit.bwd ,subset(cv.bwd,test.mse==min(test.mse))$nvars)










