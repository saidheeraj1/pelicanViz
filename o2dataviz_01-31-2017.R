############# Import data from ReDCap


################################## Data Prep for Viz
library(reshape2)
library(plyr)
dataWide$sc1var1 <- NA
dataWide$sc1var2 <- NA
dataWide$sc1var3 <- NA
dataWide$sc1Repvar1 <- NA
dataWide$sc2var1 <- NA
dataWide$sc2var2 <- NA
dataWide$sc2var3 <- NA
dataWide$sc2Repvar1 <- NA
dataWide$pc1var1 <- NA
dataWide$pc1var2 <- NA
dataWide$pc1var3 <- NA
dataWide$pc1Repvar1 <- NA
dataWide$pc2var1 <- NA
dataWide$pc2var2 <- NA
dataWide$pc2var3 <- NA
dataWide$pc2Repvar1 <- NA
dataWide$tfvar1 <- NA
dataWide$tfvar2 <- NA
dataWide$tfvar3 <- NA
dataWide$otvar1 <- NA
dataWide$otvar2 <- NA
dataWide$otvar3 <- NA



########################### For tank adherence calculation
computeTankAdherence=function(data=dataWide, firstTimePoint="baseline", secondTimePoint="30"){
  suffixes=c("30"=".30", "60"=".60", "90"=".90")
  if(firstTimePoint=="baseline"){suffix0=""} else{suffix0=suffixes[firstTimePoint]}
  suffix1=suffixes[secondTimePoint]
  if(firstTimePoint=="baseline"){
    d0=data$bs_hmvs_dt_tm
    d0[nchar(d0) %in% 11]=paste0(d0[nchar(d0) %in% 11], "00:00")
    firstDate=as.POSIXct(d0)
  } else{
    d0=data[,paste0("out_o2_tank_dt", suffix0)]
    d0[nchar(d0) %in% 11]=paste0(d0[nchar(d0) %in% 11], "00:00")
    firstDate=as.POSIXct(d0)
  }
  d1=data[,paste0("out_o2_tank_dt", suffix1)]
  d1[nchar(d1) %in% 11]=paste0(d1[nchar(d1) %in% 11], "00:00")
  secondDate=as.POSIXct(d1)
  
  
  dateDiff=round(as.numeric(difftime(secondDate, firstDate, units="days")))
  
  tankRevalue=c("M-4 (a.k.a. A)"="103",
                "M-6 (a.k.a. B a.k.a. post valve 6)"="149",
                "D (a.k.a. post valve D)"="406",
                "E"="650",
                "M-L6"="170",
                "M-9 (a.k.a. C a.k.a. post valve 9)"="249",
                "M-L4 (a.k.a. invocare f2pcl4)"="113",
                "M"="3000",
                "MB08"="232",
                "M-2"="42",
                "M-7"="108",
                "Participant does not have oxygen tanks"=NA,
                "Participant does not have a second oxygen tank that is a different size"=NA,
                "Other"=NA)
  
  #Question: revalueing a NA?
  #Other tank sizes should return missing adherence, may be dichotomized later????
  
  
  dataWide[!is.na(dataWide$out_o2_tnksiz_othr_1.30) | !is.na(dataWide$out_o2_tnksiz_othr_1.60) | !is.na(dataWide$out_o2_tnksiz_othr_1.90) | 
             !is.na(dataWide$out_o2_tnksiz_othr_2.30) | !is.na(dataWide$out_o2_tnksiz_othr_2.60) | !is.na(dataWide$out_o2_tnksiz_othr_2.90) | 
             !is.na(dataWide$out_o2_tnksiz_othr_3.30) | !is.na(dataWide$out_o2_tnksiz_othr_3.60) | !is.na(dataWide$out_o2_tnksiz_othr_3.90), "visit_pelican_id"]
  
  tank1Liters=as.numeric(as.character(revalue(x=factor(data[, paste0("out_o2_tank_size_1", suffix1)]), replace=tankRevalue, warn_missing = F)))
  tank2Liters=as.numeric(as.character(revalue(x=factor(data[, paste0("out_o2_tank_size_2", suffix1)]), replace=tankRevalue, warn_missing = F)))
  tank3Liters=as.numeric(as.character(revalue(x=factor(data[, paste0("out_o2_tank_size_3", suffix1)]), replace=tankRevalue, warn_missing = F)))
  
  tank1Used=data[, paste0("out_o2_tanks_num1", suffix1)]*tank1Liters
  tank2Used=data[, paste0("out_o2_tanks_num2", suffix1)]*tank2Liters
  tank3Used=data[, paste0("out_o2_tanks_num3", suffix1)]*tank3Liters
  
  # Data is in L/min, converting to L/hour through a division by 60
  flowRate=data[, paste0("out_pt_reported_flow_rate", suffix1)]
  flowRate[is.na(flowRate)]=as.numeric(data[is.na(flowRate), paste0("out_o2_up", suffix1)])
  
  hrsUsed1=round(tank1Used/flowRate/60,2)
  hrsUsed2=round(tank2Used/flowRate/60,2)
  hrsUsed3=round(tank3Used/flowRate/60,2)
  
  tank1Adherence=round(hrsUsed1/dateDiff,2)
  tank2Adherence=round(hrsUsed2/dateDiff,2)
  tank3Adherence=round(hrsUsed3/dateDiff,2)
  
  
  adherence=apply(X=data.frame(tank1Adherence, tank2Adherence, tank3Adherence), MARGIN=1, FUN=function(x){sum(x, na.rm=T)})
  notank <- c(NA, "Participant does not have oxygen tanks", "Participant does not have a second oxygen tank that is a different size", "Other")
  adherence[is.na(tank1Adherence) |
              data[, paste0("out_o2_tank_size_1", suffix1)] %in% notank |
              data[, paste0("out_o2_tank_size_2", suffix1)] %in% notank |
              data[, paste0("out_o2_tank_size_3", suffix1)] %in% notank]=NA
  return(data.frame(tank_o2_adher=adherence, 
                    o2_1_cont=tank1Liters, o2_2_cont=tank2Liters, o2_3_cont=tank3Liters,
                    tot_lt_o2_1=tank1Used, tot_lt_o2_2=tank2Used, tot_lt_o2_3=tank3Used,
                    hrs_o2_1=hrsUsed1, hrs_o2_2=hrsUsed2, hrs_o2_3=hrsUsed3,
                    o2_1_adher=tank1Adherence, o2_2_adher=tank2Adherence, o2_3_adher=tank3Adherence,
                    out_o2_tank_days=dateDiff)
  )
  
}

tank30=computeTankAdherence(data=dataWide, firstTimePoint="baseline", secondTimePoint="30")
names(tank30)=paste0(names(tank30), "_30")
tank60=computeTankAdherence(data=dataWide, firstTimePoint="30", secondTimePoint="60")
names(tank60)=paste0(names(tank60), "_60")
tank90=computeTankAdherence(data=dataWide, firstTimePoint="60", secondTimePoint="90")
names(tank90)=paste0(names(tank90), "_90")
dataWide=dataWide[,!names(dataWide) %in% c(names(tank30), names(tank60), names(tank90))]
dataWide=data.frame(dataWide, tank30, tank60, tank90)

dataWide[is.na(dataWide$hrs_o2_1_30) | dataWide$hrs_o2_1_30 > 999999999, "hrs_o2_1_30"] <- 0
dataWide[is.na(dataWide$hrs_o2_1_60) | dataWide$hrs_o2_1_60 > 999999999, "hrs_o2_1_60"] <- 0
dataWide[is.na(dataWide$hrs_o2_1_90) | dataWide$hrs_o2_1_90 > 999999999, "hrs_o2_1_90"] <- 0

dataWide[is.na(dataWide$hrs_o2_2_30) | dataWide$hrs_o2_2_30 > 999999999, "hrs_o2_2_30"] <- 0
dataWide[is.na(dataWide$hrs_o2_2_60) | dataWide$hrs_o2_2_60 > 999999999, "hrs_o2_2_60"] <- 0
dataWide[is.na(dataWide$hrs_o2_2_90) | dataWide$hrs_o2_2_90 > 999999999, "hrs_o2_2_90"] <- 0

dataWide[is.na(dataWide$hrs_o2_3_30) | dataWide$hrs_o2_3_30 > 999999999, "hrs_o2_3_30"] <- 0
dataWide[is.na(dataWide$hrs_o2_3_60) | dataWide$hrs_o2_3_60 > 999999999, "hrs_o2_3_60"] <- 0
dataWide[is.na(dataWide$hrs_o2_3_90) | dataWide$hrs_o2_3_90 > 999999999, "hrs_o2_3_90"] <- 0

dataWide$tankHrs.30 <- apply(X= dataWide[,c("hrs_o2_1_30", "hrs_o2_2_30", "hrs_o2_3_30")], MARGIN=1, FUN = function(x){
  sum(x)
})

dataWide$tankHrs.60 <- apply(X= dataWide[,c("hrs_o2_1_60", "hrs_o2_2_60", "hrs_o2_3_60")], MARGIN=1, FUN = function(x){
  sum(x)
})

dataWide$tankHrs.90 <- apply(X= dataWide[,c("hrs_o2_1_90", "hrs_o2_2_90", "hrs_o2_3_90")], MARGIN=1, FUN = function(x){
  sum(x)
})


########################### To get dataframe for viz

### getTable function
source("https://github.com/saidheeraj1/pelicanViz/raw/master/tableforViz.R")

################ To get visualization

getViz(376)
