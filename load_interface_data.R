#load data and interface of all models
#and remove commas in the interface because commas are used for separating values

#don't forget to save files as tab-delimited, with utf-8 encoding because of Czech special characters 
dataDENTRO<-read.table("models/dataDENTRO.txt", fileEncoding = "UTF-8", encoding = "UTF-8",fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceDENTRO<-read.table("models/interfaceDENTRO.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceDENTRO<-interfaceDENTRO[!is.na(interfaceDENTRO$side),]
interfaceDENTRO[1:length(interfaceDENTRO)]<-lapply(interfaceDENTRO[1:length(interfaceDENTRO)], function(x) gsub(pattern=",", replacement=".", x=x))

dataSTA<-read.table("models/dataSTA.txt",  fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceSTA<-read.table("models/interfaceSTA.txt",  fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceSTA<-interfaceSTA[!is.na(interfaceSTA$side),]
interfaceSTA[1:length(interfaceSTA)]<-lapply(interfaceSTA[1:length(interfaceSTA)], function(x) gsub(pattern=",", replacement=".", x=x))

dataDECIDUOUS<-read.table("models/dataDECIDUOUS.txt",  fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceDECIDUOUS<-read.table("models/interfaceDECIDUOUS.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceDECIDUOUS<-interfaceDECIDUOUS[!is.na(interfaceDECIDUOUS$side),]
interfaceDECIDUOUS[1:length(interfaceDECIDUOUS)]<-lapply(interfaceDECIDUOUS[1:length(interfaceDECIDUOUS)], function(x) gsub(pattern=",", replacement=".", x=x))

dataSCSM<-read.table("models/dataSCSM.txt", fileEncoding = "UTF-8", encoding = "UTF-8", quote="", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceSCSM<-read.table("models/interfaceSCSM.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceSCSM<-interfaceSCSM[!is.na(interfaceSCSM$side),]
interfaceSCSM[1:length(interfaceSCSM)]<-lapply(interfaceSCSM[1:length(interfaceSCSM)], function(x) gsub(pattern=",", replacement=".", x=x))

dataCzech<-read.table("models/dataCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep=";", skipNul =TRUE, header=TRUE)
interfaceCzech<-read.table("models/interfaceCzech.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep=";", header=TRUE)
interfaceCzech<-interfaceCzech[!is.na(interfaceCzech$side),]
interfaceCzech[1:length(interfaceCzech)]<-lapply(interfaceCzech[1:length(interfaceCzech)], function(x) gsub(pattern=",", replacement=".", x=x))

dataJBOJP<-read.table("models/dataJBOJP.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceJBOJP<-read.table("models/interfaceJBOJP.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceJBOJP<-interfaceJBOJP[!is.na(interfaceJBOJP$side),]
interfaceJBOJP[1:length(interfaceJBOJP)]<-lapply(interfaceJBOJP[1:length(interfaceJBOJP)], function(x) gsub(pattern=",", replacement=".", x=x))

dataDEHM<-read.table("models/dataDEHM.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceDEHM<-read.table("models/interfaceDEHM.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceDEHM<-interfaceDEHM[!is.na(interfaceDEHM$side),]
interfaceDEHM[1:length(interfaceDEHM)]<-lapply(interfaceDEHM[1:length(interfaceDEHM)], function(x) gsub(pattern=",", replacement=".", x=x))

dataSUOMI<-read.table("models/dataSUOMI.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceSUOMI<-read.table("models/interfaceSUOMI.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceSUOMI<-interfaceSUOMI[!is.na(interfaceSUOMI$side),]
interfaceSUOMI[1:length(interfaceSUOMI)]<-lapply(interfaceSUOMI[1:length(interfaceSUOMI)], function(x) gsub(pattern=",", replacement=".", x=x))

dataUKguide<-read.table("models/dataUKguide.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE, na.strings="NaN")
interfaceUKguide<-read.table("models/interfaceUKguide.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceUKguide<-interfaceUKguide[!is.na(interfaceUKguide$side),]
interfaceUKguide[1:length(interfaceUKguide)]<-lapply(interfaceUKguide[1:length(interfaceUKguide)], function(x) gsub(pattern=",", replacement=".", x=x))

dataCH<-read.table("models/dataCH.txt", fileEncoding = "UTF-8", encoding = "UTF-8", fill=TRUE, sep="\t", skipNul =TRUE, header=TRUE)
interfaceCH<-read.table("models/interfaceCH.txt", fileEncoding = "UTF-8", encoding = "UTF-8",quote="", fill=TRUE, sep="\t", header=TRUE)
interfaceCH<-interfaceCH[!is.na(interfaceCH$side),]
interfaceCH[1:length(interfaceCH)]<-lapply(interfaceCH[1:length(interfaceCH)], function(x) gsub(pattern=",", replacement=".", x=x))

# In czech, there are empty spaces around words in some cells 
dataCzech <- data.frame(lapply(dataCzech, function(x) {if (is.character(x)) {return(trimws(x))} else {return(x)}}))
interfaceCzech <- data.frame(lapply(interfaceCzech, function(x) {if (is.character(x)) {return(trimws(x))} else {return(x)}}))

# if you need to further reformat the data, you can do it within the suitability_MODELNAME.txt file (because it will be specific to the model)