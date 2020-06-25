###############################################################################
###############################################################################
#code for the dysaphis tests results and to chose a discriminant dose...
###############################################################################
###############################################################################

#loading the libraries
library(drc)
library(plotrix)


###############################################################################
#comparison between repetitions
###############################################################################

#-load the dataset
dys<-read.table("data/dldysaDat.txt",header=T,sep="\t",dec=".")
summary(dys)
head(dys)
str(dys)

#1-selectionner juste les valeurs des clones choisis, la référence sensible et 
#un des clones les plus résistant à ce jour
dataSR<-dys[dys$clone %in% c("16-042","17-041-003"),]

#2-pour faire la courbe 
#in order to obtain the same results than with priprobit, the Finney equivalent
#method, we have to remove the constrain on lowerlimit and chose a log-normal 
#model instead of a log-logistic model
modRS<-drm(dead/total~dose,weights=total,data=dataSR,fct=LN.3u(),curveid=clone,
                  type="binomial")
plot(modRS)
ED(modRS,50,interval="delta",reference="control")

#DL50 pour les différentes répétitions du clone sensible
modS<-drm(dead/total~dose,weights=total,data=dataSR[dataSR$clone=="16-042",],
          fct=LN.3u(),curveid=date,type="binomial")
plot(modS)
ED(modS,50,interval="delta",reference="control")

#DL50 pour les différentes répétitions du clone résistant
modR<-drm(dead/total~dose,weights=total,data=dataSR[dataSR$clone=="17-041-003",],
          fct=LN.3u(),curveid=date,type="binomial")
plot(modR)
ED(modR,50,interval="delta",reference="control")


#trouver une dose qui tue tous les sensibles mais pas trop de résistant
ED(modRS,97.5,interval="delta",reference="control")
ED(modRS,95,interval="delta",reference="control")
ED(modRS,19,interval="delta",reference="control")

