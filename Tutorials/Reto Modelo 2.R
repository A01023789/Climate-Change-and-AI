#Reto Modelo 2
#Para este proceso se realizarán árboles de decisión

library(tree)

dir.data<-"/Users/marianacornejo/Documents/GitHub/Climate-Change-and-AI/Data/Model//"
dataModel<-read.csv(paste0(dir.data,"ModelData.csv"))
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

class(response)
responseF = as.factor(response)
class(responseF)

tree.waste<-tree(responseF~., data=dataModel)



