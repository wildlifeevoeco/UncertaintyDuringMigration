####### ## RUN kfold validation models ====
# Inputs: RSF DATA
# Outputs: kfold row values

library(lme4)
library(data.table)
library(Matrix)

source("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/scripts/functions/KFold-V1.1.R")

## NOTE - the function is called 'KfoldXVal'

##RSF DATA
Final_MR_migration_RSF_stop <- readRDS("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/Final_MR_migration_RSF_stop.RDS")
Final_MR_migration_RSF_mvt <- readRDS("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/Final_MR_migration_RSF_mvt.RDS")


### Stopover model ###

cbind(Randoms, PtID)~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
  scale(swe) + scale(prcp) + scale(Open_200)*scale(tmax) + scale(Open_200)*scale(swe) +
  scale(Open_200)*scale(prcp) + scale(Lichen_200)*scale(tmax) + scale(Lichen_200)*scale(swe) + scale(Lichen_200)*scale(prcp) +
  scale(Forest_200)*scale(tmax) + scale(Forest_200)*scale(swe) + scale(Forest_200)*scale(prcp)

kfold_stop<-KfoldXVal(data=Final_MR_migration_RSF_stop,
                        formula="cbind(Randoms, PtID)~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
  scale(swe) + scale(prcp) + scale(Open_200)*scale(tmax) + scale(Open_200)*scale(swe) +
  scale(Open_200)*scale(prcp) + scale(Lichen_200)*scale(tmax) + scale(Lichen_200)*scale(swe) + scale(Lichen_200)*scale(prcp) +
  scale(Forest_200)*scale(tmax) + scale(Forest_200)*scale(swe) + scale(Forest_200)*scale(prcp)",binVar = 'Animal_ID', k=5,modType="mclogit",resp="Randoms",modFam="binomial")
#view
kfold_stop ##  0.5405544  +- 0.3779214

write.csv(kfold_stop,"~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/tables/k-fold_RSFstop_caribou.csv")

##Movement model##

kfold_mvt<-KfoldXVal(data=Final_MR_migration_RSF_mvt,
                     formula="cbind(Randoms, PtID)~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
  scale(swe) + scale(prcp) + scale(Open_200)*scale(tmax) + scale(Open_200)*scale(swe) +
  scale(Open_200)*scale(prcp) + scale(Lichen_200)*scale(tmax) + scale(Lichen_200)*scale(swe) + scale(Lichen_200)*scale(prcp) +
  scale(Forest_200)*scale(tmax) + scale(Forest_200)*scale(swe) + scale(Forest_200)*scale(prcp)",binVar = 'Animal_ID', k=5,modType="mclogit",resp="Randoms",modFam="binomial")
#View
kfold_mvt ##  0.3124767 +- 0.1664861

write.csv(kfold_mvt,"~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/tables/k-fold_RSFmvt_caribou.csv")
