##############################################################################/
##############################################################################/
#Estimation of DL50 in winter 2018
##############################################################################/
##############################################################################/

#loading the libraries
library(drc)
library(plotrix)

#load the dataset
data2018<-read.table("data/datahiver2018.txt",header=TRUE,
                     sep="\t",stringsAsFactors=TRUE)
#aggregating the data for the different wells
data2018<-aggregate(cbind(dead,total)~dose+clone+date,
                    data=data2018,"sum")


##############################################################################/
#Regression analysis for clone 17-041-003 winter18####
##############################################################################/

data_41_003<-data2018[data2018$clone=="17-041-003",]
temp.m1<-drm(dead/total~dose,
             weights=total,
             data=data_41_003,
             curveid=date,
             fct=LN.3u(),type="binomial")
plot(temp.m1,ylim=c(0,1.1),xlim=c(0,100),
     main="17-041-003",legendPos=c(1.5,1))
compParm(temp.m1,"e")

tempdat<-dataSeal[dataSeal$ech_id==names(table(dataSeal$ech_id))[2],]
temp.m2<-drm(nb_mtot/(nb_mtot+nb_vi)~dose,
             weights=(nb_mtot+nb_vi),
             data=tempdat,
             curveid=ana_id,
             fct=LN.3u(),type="binomial")
plot(temp.m2,ylim=c(0,1.1),xlim=c(0,100),
     main=names(table(dataSeal$ech_id))[2])
compParm(temp.m2,"e")


##############################################################################/
#END
##############################################################################/