##############################################################################/
##############################################################################/
#comparisons with different plastic films to seal the plate
##############################################################################/
##############################################################################/

#loading the libraries
library(drc)
library(plotrix)

#load the dataset
dataSeal<-read.table("data/dys_comp_films.txt",header=TRUE,
                     sep=";",stringsAsFactors=TRUE)
#removing the unnecessary columns
dataSeal<-dataSeal[,c(1,2,4,6,18,19,20,21,22)]

dataSeal<-aggregate(cbind(nb_vi,nb_mb,nb_mt,nb_mtot)~dose+ana_id+ech_id,
                    data=dataSeal,"sum")

##############################################################################/
#Regression analysis of mycelial growth experiment scoring 20 or 21 days####
##############################################################################/

tempdat<-dataSeal[dataSeal$ech_id==names(table(dataSeal$ech_id))[1],]
temp.m1<-drm(nb_mtot/(nb_mtot+nb_vi)~dose,
             weights=(nb_mtot+nb_vi),
             data=tempdat,
             curveid=ana_id,
             fct=LN.3u(),type="binomial")
plot(temp.m1,ylim=c(0,1.1),xlim=c(0,100),
     main=names(table(dataSeal$ech_id))[1])
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
#comparison between repetitions####
##############################################################################/

tempdat<-dataSeal[dataSeal$ana_id==names(table(dataSeal$ana_id))[c(1,3)],]
temp.m3<-drm(nb_mtot/(nb_mtot+nb_vi)~dose,
             weights=(nb_mtot+nb_vi),
             data=tempdat,
             curveid=ana_id,
             fct=LN.3u(),type="binomial")
plot(temp.m3,ylim=c(0,1.1),xlim=c(0,100),
     main="clear seal")
compParm(temp.m3,"e")

tempdat<-dataSeal[dataSeal$ana_id==names(table(dataSeal$ana_id))[c(2,4)],]
temp.m4<-drm(nb_mtot/(nb_mtot+nb_vi)~dose,
          weights=(nb_mtot+nb_vi),
          data=tempdat,
          curveid=ana_id,
          fct=LN.3u(),type="binomial")
plot(temp.m4,ylim=c(0,1.1),xlim=c(0,100),
     main="porous seal")
compParm(temp.m4,"e")


##############################################################################/
#combined plot of the different comparisons####
##############################################################################/

op<-par(mfrow=c(2,2))
plot(temp.m1,ylim=c(0,1.1),xlim=c(0,100),
     main=names(table(dataSeal$ech_id))[1])
plot(temp.m2,ylim=c(0,1.1),xlim=c(0,100),
     main=names(table(dataSeal$ech_id))[2])
plot(temp.m3,ylim=c(0,1.1),xlim=c(0,100),
     main="clear seal")
plot(temp.m4,ylim=c(0,1.1),xlim=c(0,100),
     main="porous seal")
par(op)
#export to .pdf 8 x 8 inches


##############################################################################/
#END
##############################################################################/