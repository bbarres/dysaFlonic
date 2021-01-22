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
temp<-drm(nb_mtot/(nb_mtot+nb_vi)~dose,
             weights=(nb_mtot+nb_vi),
             data=tempdat,
             curveid=ana_id,
             fct=LN.3u(),type="binomial")
plot(temp,ylim=c(0,1.1),xlim=c(0,100),
     main="clear seal")
compParm(temp,"e")

tempdat<-dataSeal[dataSeal$ana_id==names(table(dataSeal$ana_id))[c(2,4)],]
temp<-drm(nb_mtot/(nb_mtot+nb_vi)~dose,
          weights=(nb_mtot+nb_vi),
          data=tempdat,
          curveid=ana_id,
          fct=LN.3u(),type="binomial")
plot(temp,ylim=c(0,1.1),xlim=c(0,100),
     main="porous seal")
compParm(temp,"e")



##############################################################################/
#END
##############################################################################/