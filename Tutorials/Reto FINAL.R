#Modelo de regresión para saber qué lleva a los países a tener un NDC de waste

#Especificamos de donde viene la información
dir.data<-"/Users/marianacornejo/Documents/GitHub/Climate-Change-and-AI/Data/Model//"
data<-read.csv(paste0(dir.data,"ModelData.csv"))
#Summary
summary(data)

#Limpiar columnas
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.50])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#Respuesta de interés, en este caso es WASTE
ids<-c("iso_code3","country")
response<-"m_waste"
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
predictors
length(predictors)

#Estimación del modelo
data.model<-data[,c(response,predictors)]
head(data.model)
#Modelo como una formula de la mitigación de residuos en funcion de esta información
model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
model

full.model <- lm(model, data = data.model)
summary(full.model)

#Al correr la regresión lineal se puede apreciar que las variables más significativas son a_agriculture, a_water, m_lulucf, migration_displacement, AG.SRF.TOTL.K2, DT.DOD.DECT.CD, NE.EXP.GNFS.ZS, NY.GDP.DEFL.KD.ZG, SH.DYN.MORT* y SP.ADO.TFRT
#Se intentará correr un modelo más pequeño

library(leaps)

#Dividiremos la muestra en un conjunto de entrenamiento y uno de prueba para probar el modelo
set.seed (55555)
train <- sample (c(TRUE ,FALSE), nrow(data.model ),rep=TRUE)
train
test  <- (!train )
test

#Definiremos el largo del modelo, ya que se encontraron 3 variables muy significativas indicaremos que se creen modelo con máximo 5 variables
max.vars<-5

#Para la construcción del modelo se utilizarán 3 tecnicas para determinar cual es mejor, una selección completa, una a través de la tecnica forward y una a través de backward
#Selección completa
regfit.best <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T) 

#Técnica forward stepwise selection
regfit.fwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "forward") 

#Técnica backard stepwise selection
regfit.bwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "backward")

#Exploraremos cada uno de ellos
msize<-5
coef(regfit.best ,msize)
coef(regfit.fwd , msize)
coef(regfit.bwd , msize)

#Observamos que con el método completo selecciona las variables a_agriculture, a_urban, a_water, m_agriculture, m_energy
#Con el método forward stepwise: a_coastal_zone, m_agriculture, m_energy, NE.GDI.TOTL.ZS y SP.DYN.LEOO.IN
#Con el método backard stepwise: a_agriculture, a_transport, a_water, m_agriculture, m_energy

#Para determinar cuál de lo tres modelos es mejor:
predict.regsubsets <-function (object, model ,newdata ,id ){
  object<-regfit.best
  newdata<-data.model[test ,]
  #Quiero el modelo con 4 predictores
  id<-5
  form<-model
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
                    test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],1))
cv.best

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="best",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],i))
  cv.best<-rbind(cv.best,pivot)
  
}
cv.best

#best model
subset(cv.best,test.mse==min(test.mse))
#actual model
coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)







