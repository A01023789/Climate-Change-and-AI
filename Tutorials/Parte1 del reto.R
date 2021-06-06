#Downloading data from different repositories
# we will first collect and explore the data available at https://www.climatewatchdata.org

#install the required libraries
install.packages(c("httr", "jsonlite"))

#load the libraries
library(httr)
library(jsonlite)

#NDC
 url <- "https://www.climatewatchdata.org/api/v1/data/ndc_content"

#make a get request

 resNDC <- GET(url)

#transform this to actual data

 NDCdata<-fromJSON(rawToChar(resNDC$content))

#NDCdata is list, let's explore its structure

 str(NDCdata)

#so we are getting a data frame with only 50 lines, this is obviously incomplete,
#let's learn more about this by exploring the structure of resNDC
 str(resNDC)

#we should also explore what the GET function is doing, try the following

?GET

# in this website we can learn about what parameters we can use to control the data request
#https://www.climatewatchdata.org/data-explorer/ndc-content?ndc-content-categories=unfccc_process&ndc-content-countries=All%20Selected&ndc-content-indicators=All%20Selected&ndc-content-sectors=All%20Selected&page=1
#So it looks like we can refine this, let's try the process again, but now using the countries field to access the data of the European Union
 resNDC<-GET(url,query= list(countries = "EUU", page=1))
 NDCdata<-fromJSON(rawToChar(resNDC$content))

#what sort of object NDCdata is ?
 class(NDCdata)

#what is the structure
 str(NDCdata)

#it looks we are only getting 50 observations per hit, let's look at the structure of resNDC again
 str(resNDC)

#so under headers, the field total says there 437 rows associated with this country, and the per page header says we are getting 50 rows per packages
#we will need a loop to get all this data, let's try the following
#first, let's set the parameters for the loop
 total<-as.numeric(resNDC$headers$total)
 per.page<-as.numeric(resNDC$headers['per-page'])
 pages<-ceiling(total/per.page)

#one way of doing this is using the for function
#intialize the loop
 resNDC<-GET(url,query= list(countries = "EUU", page=1))
 NDCdata<-fromJSON(rawToChar(resNDC$content))$data
for(i in 2:pages)
{
 resNDC<-GET(url,query= list(countries = "EUU", page=i))
 pivot<-fromJSON(rawToChar(resNDC$content))$data
 NDCdata<-rbind(NDCdata,pivot)
}

#check the dimmensions of this new object and see if the loop worked
 dim(NDCdata)

#it looks like it worked, but maybe we use too many lines to do this, we can do the same process with fewer lines using lapply
NDCdata.best<-lapply(c(1:pages),function(x){
                            resNDC<-GET(url,query= list(countries = "EUU", page=x));
                             fromJSON(rawToChar(resNDC$content))$data
                             }
                      )

NDCdata.best<-do.call(rbind,NDCdata.best)
# you can see we get the same object
dim(NDCdata.best)

#Next we need to do this for all countries and all available data in this repository
#let's explore the database
 colnames(NDCdata)
 names(NDCdata)
 summary(NDCdata)
#get unique values of variable
 unique(NDCdata$iso_code3)
#let's do all at once
 sapply(NDCdata,unique)
 sapply(NDCdata,max)

#let's do this for all countries

#primero tengo que obtenter un vector con las claves de los países
#segundo tengo que estimar cuantas páginas por país hay
#tercero en función de eso programar mi lapply para que haga rbind en todas las tables }
# guardar un csv de cada pais en mi carpeta
# este proceso tiene que ocurrir en todos los paises


#segunda opción jalo la base completa, y el loop corre 2003 .
#guardo el archivo

#Error en Names C, ir a asesoría para ver por qué sucede
 
NamesC<-
for (i in 1:length(NamesC))
{
NDCdata.best<-lapply(c(1:pages),function(x){
                            resNDC<-GET(url,query= list(countries = NamesC[i] , page=x));
                             fromJSON(rawToChar(resNDC$content))$data
                             }
                      )
NDCdata.best<-do.call(rbind,NDCdata.best)

}


#Tutorial 102
#load the libraries
library(httr)
library(jsonlite)


#NDC
url <- "https://www.climatewatchdata.org/api/v1/data/ndc_content"
resNDC <- GET(url)
total<-as.numeric(resNDC$headers$total)
per.page<-as.numeric(resNDC$headers['per-page'])
pages<-ceiling(total/per.page)


#-------------------------------------------------------------
#get all
#Tarda mucho en cargar
NDCdata.best<-lapply(c(1:pages),function(x){
        resNDC<-GET(url,query= list(page=x));
        fromJSON(rawToChar(resNDC$content))$data
}
)

NDCdata.best<-do.call(rbind,NDCdata.best)
# you can see we get the same object
dim(NDCdata.best)

#Get list of all countries included in the database
nations<-unique(NDCdata.best$iso_code3)

#--------------------------------------------------------------


#save all data in individual tables
root<-"/Users/sabrinaciscomani/Documents/GitHub/Climate-Change-and-AI/Data/NDC/Raw//"
for (i in 1:length(nations))
{
        pivot<-subset(NDCdata.best,iso_code3==nations[i])
        write.csv(pivot,paste0(root,nations[i],"_ndc.csv"),row.names=FALSE)
}

#create table fot nations codes
nations_names<-data.frame(iso_code3=unique(NDCdata.best$iso_code3),country=unique(NDCdata.best$country))
write.csv(nations_names,paste0(root,"nations_names_ndc.csv"),row.names=FALSE)


#TUTORIAL 103

#read all files store in one single location
root<-"/Users/marianacornejo/Documents/GitHub/Climate-Change-and-AI/Data/NDC/Raw//"

#first list all files and save these into a character vector
file.names <- list.files(path =root, pattern = ".csv")

#now read all files and save them in a list

NDCData<-lapply(file.names,function(x){read.csv(paste0(root,x))})
NDCData<-do.call("rbind",NDCData)

#firt we need to work on understanding what is inside this data
#let's begin by looking at the different indicators
#remember we need to find a way to quantitively describe an NDC, that is a huge challenge, let's be creative

indicators<-unique(NDCData$indicator_name)
indicators

#clean the names
indicators_names<-gsub("/","_",indicators)
indicators_names<-gsub(" ","_",indicators_names)
indicators_names<-gsub("\\(","_",indicators_names)
indicators_names<-gsub("\\)","_",indicators_names)
indicators_names<-gsub("%","",indicators_names)
indicators_names<-gsub(":","",indicators_names)

#there are 359 indicators, what is inside each one of them

#let's look at values in the first indicator
#Solo ver una base o indicador cambiar número para ver cuál es
i<-1
explore<-subset(NDCData,indicator_name==indicators[i])
explore$count<-1
explore<-aggregate(list(count=explore$count),list(
        global_category=explore$global_category,
        indicator_name=explore$indicator_name,
        value=explore$value
),sum)
explore<-explore[order(-explore$count),]


#what do we do from here? ideas?

#let's work first the null values to explore more
null.values<-c("No Document Submitted","No specified measure")
NDCData$value.numeric<-ifelse(NDCData$value==null.values[1],-1,1)
NDCData$value.numeric<-ifelse(NDCData$value==null.values[2],0,1)

#policy questions:
#which indicators receive the most global effort ?
explore<-aggregate(list(value.numeric=NDCData$value.numeric),list(
        global_category=NDCData$global_category,
        indicator_name=NDCData$indicator_name
),sum)
#with respect to mitigation
explore<-subset(explore,global_category=="Mitigation")   #Mitigation
explore<-explore[order(-explore$value.numeric),]
explore

#PAISES
#which countries are doing the most
explore<-aggregate(list(value.numeric=NDCData$value.numeric),list(
        global_category=NDCData$global_category,
        country=NDCData$country
),sum)
#with respect to mitigation
explore<-subset(explore,global_category=="Adaptation")   #Mitigation
explore<-explore[order(-explore$value.numeric),]
explore

## this is a test

# Now save all indactors in PC
#out<-"/Users/.." # for mac users
out<-"/Users/marianacornejo/Documents/GitHub/Climate-Change-and-AI/Data/NDC/Indicators\\"
for (i in 1:length(indicators))
{
        explore<-subset(NDCData,indicator_name==indicators[i])
        explore$count<-1
        explore<-aggregate(list(count=explore$count),list(
                global_category=explore$global_category,
                indicator_name=explore$indicator_name,
                value=explore$value
        ),sum)
        explore<-explore[order(-explore$count),]
        write.csv(explore,paste0(out,indicators_names[i],".csv"),row.names=FALSE)
}
#Hasta aquí el tutorial 3

