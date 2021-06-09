#Reto Modelo 2
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
#Generación binaria de una variable, la categoría High representa a los países con una alta mitigación de residuos mientras que Low aquellos con una baja mitigación
#Se encontró que 80 países tienen una alta mitigación mientras que 114 una baja.

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

#define the model we want to estimate
#Definir el modelo que queremos estimar
model<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
model

#ARBOL DE CLASIFICACIÓN
library(tree)
set.seed (55555)
#decir cuantos valores de eso 180 queremos utilizar para entrenar al modelo
train<-sample (1: nrow(data.model ), 100)
#Para computar la matriz de confusión se debe hacer un conjunto de prueba, valores reales de la variable en el conjunto de prueba
data.model.test<-data.model[-train ,]
response.binary.test<-data.model$response.binary[-train ]
tree.model <- tree(model,data.model ,subset =train )

#see the model
plot(tree.model)
text(tree.model, pretty=0)
#Nos dice que la variable m_energy es la más importante, después salud, 

#Probar que tan buen modelo para obtener una predicción del modelo de interes
tree.model.pred<-predict(tree.model ,data.model.test ,type ="class")
table(tree.model.pred ,response.binary.test)
#Tasa de error del (16+9)/80 del 31.25%
(16+9)/80

#We can use tree prunning to get a better classification tree
#Podar el árbol
set.seed (55555)
cv.data.model<-cv.tree(tree.model ,FUN=prune.misclass )
cv.data.model
#Se puede observar que un arbol de tamaño 8 sería el más adecuado, ya que presenta un error bajo y no es tan simple

#we can use prune.misclass() to obtain the best tree
#prune.misclass para obtener el mejor
prune.tree.model <- prune.misclass (tree.model ,best =8)
plot(prune.tree.model )
text(prune.tree.model ,pretty =0)

tree.pred<-predict (prune.tree.model , data.model.test ,type ="class")
table(tree.pred ,response.binary.test)

#ARBOL DE REGRESIÓN
#single best
library (MASS)
set.seed (55555)
train <- sample (1: nrow(data.model), 100)
Rtree.data.model <-tree(model,data.model ,subset =train)
summary (Rtree.data.model)
plot(Rtree.data.model  )
text(Rtree.data.model  ,pretty =0)

#prune tree
cv.Rtree.data.model <- cv.tree(Rtree.data.model )
cv.Rtree.data.model

#which is the best tree?

prune.Rtree.data.model <- prune.misclass(Rtree.data.model  ,best =2)
plot(prune.Rtree.data.model )
text(prune.Rtree.data.model ,pretty =0)

#Lo mejor fue con 2 ya que presenta el menor error

#RANDOM FOREST
library(randomForest)
set.seed (55555)
rf.data.model <- randomForest(model,
                              data=data.model ,
                              subset =train ,
                              mtry=round(length(predictors)^0.5),
                              importance =TRUE
)
rf.data.model

#base on many different trees, which are the most important drivers
importance (rf.data.model)
varImpPlot (rf.data.model )