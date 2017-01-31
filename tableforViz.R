getTable <- function(id){
  coverage <- dataWide[dataWide$visit_pelican_id %in% id,]
  sc1Vars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_hmvs_st_o2_dt","out_st_o2_dt.30","out_st_o2_dt.60","out_st_o2_dt.90",
               "bs_hmvs_st_o2","out_st_o2_num.30","out_st_o2_num.60","out_st_o2_num.90","sc1var1","sc1var2","sc1var3")
  sc1RepVars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_hmvs_st_o2_dt","out_st_pl_dt1.30","out_st_pl_dt1.60","out_st_pl_dt1.90",
                  "sc1Repvar1","out_st_rep_num1.30", "out_st_rep_num1.60", "out_st_rep_num1.90",
                  "out_st_pl_num1.30", "out_st_pl_num1.60", "out_st_pl_num1.90")
  sc2Vars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_hmvs_st_o2_dt","out_st_o2_dt.30","out_st_o2_dt.60","out_st_o2_dt.90",
               "bs_st_o2_2_num","out_st_num2.30","out_st_num2.60","out_st_num2.90","sc2var1","sc2var2","sc2var3")
  sc2RepVars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_hmvs_st_o2_dt","out_st_pl_dt2.30","out_st_pl_dt2.60","out_st_pl_dt2.90",
                  "sc2Repvar1","out_st_rep_num2.30", "out_st_rep_num2.60", "out_st_rep_num2.90",
                  "out_st_pl_num2.30", "out_st_pl_num2.60", "out_st_pl_num2.90")
  pc1Vars <- c("visit_pelican_id", "bs_dt_nurse_call", "bs_chkn_port_dt", "out_port_con_dt.30", "out_port_con_dt.60", "out_port_con_dt.90",
               "bs_hmvs_port_hrs", "out_port_con_num.30", "out_port_con_num.60","out_port_con_num.90", "pc1var1","pc1var2","pc1var3")
  pc1RepVars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_chkn_port_dt","out_port_pl_dt1.30","out_port_pl_dt1.60","out_port_pl_dt1.90",
                  "pc1Repvar1","out_port_rep_num1.30", "out_port_rep_num1.60","out_port_rep_num1.90",
                  "out_port_pl_num1.30", "out_port_pl_num1.60", "out_port_pl_num1.90")
  pc2Vars <- c("visit_pelican_id", "bs_dt_nurse_call", "bs_chkn_port_dt", "out_port_con_dt.30", "out_port_con_dt.60", "out_port_con_dt.90",
               "bs_hmvs_port_hrs2", "out_port_num2.30", "out_port_num2.60","out_port_num2.90", "pc2var1","pc2var2","pc2var3")
  pc2RepVars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_chkn_port_dt","out_port_pl_dt2.30","out_port_pl_dt2.60","out_port_pl_dt2.90",
                  "pc2Repvar1","out_port_rep_num2.30", "out_port_rep_num2.60", "out_port_rep_num2.90",
                  "out_port_pl_num2.30", "out_port_pl_num2.60", "out_port_pl_num2.90")
  tfVars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_hmvs_trans_dt","out_hme_fill_dt.30","out_hme_fill_dt.60","out_hme_fill_dt.90",
              "bs_hmvs_trans_hrs","out_hme_fill_num.30","out_hme_fill_num.60","out_hme_fill_num.90","tfvar1","tfvar2","tfvar3")
  otVars <- c("visit_pelican_id", "bs_dt_nurse_call","bs_hmvs_tank_dt","out_o2_tank_dt.30","out_o2_tank_dt.60","out_o2_tank_dt.90",
              "bs_hmvs_tank_hrs","tankHrs.30", "tankHrs.60", "tankHrs.90", "otvar1", "otvar2", "otvar3")
  dataWide$bs_hmvs_tank_dt <- NA
  dataWide$bs_hmvs_tank_hrs <- NA
  
  sc1Row <- dataWide[dataWide$visit_pelican_id %in% id, c(sc1Vars)]
  sc1RepRow <- dataWide[dataWide$visit_pelican_id %in% id, c(sc1RepVars)]
  sc2Row <- dataWide[dataWide$visit_pelican_id %in% id, c(sc2Vars)]
  sc2RepRow <- dataWide[dataWide$visit_pelican_id %in% id, c(sc2RepVars)]
  pc1Row <-  dataWide[dataWide$visit_pelican_id %in% id, c(pc1Vars)]
  pc1RepRow <- dataWide[dataWide$visit_pelican_id %in% id, c(pc1RepVars)]
  pc2Row <- dataWide[dataWide$visit_pelican_id %in% id, c(pc2Vars)]
  pc2RepRow <- dataWide[dataWide$visit_pelican_id %in% id, c(pc2RepVars)]
  tfRow <- dataWide[dataWide$visit_pelican_id %in% id, c(tfVars)]
  otRow <- dataWide[dataWide$visit_pelican_id %in% id, c(otVars)]
  sc1Row$Equipment <- "Stationary1"
  sc1RepRow$Equipment <- "Stationary1 Rep"
  sc2Row$Equipment <- "Stationary2"
  sc2RepRow$Equipment <- "Stationary2 Rep"
  pc1Row$Equipment <- "Portable1"
  pc1RepRow$Equipment <- "Portable1 Rep"
  pc2Row$Equipment <- "Portable2"
  pc2RepRow$Equipment <- "Portable2 Rep"
  tfRow$Equipment <- "Transfill"
  otRow$Equipment <- "Tanks"
  vars =  c("PELICAN ID", "Hand off Date", "Baseline Reading Date", "30 Day Reading Date", "60 Day Reading Date", "90 Day Reading Date",
            "Baseline Reading", "30 Day Reading", "60 Day Reading","90 Day Reading",
            "Rep 30","Rep 60", "Rep 90", "Equipment")
  names(otRow) = names(tfRow) = names(pc1Row)  = names(sc1Row) =  names(pc2Row) = names(sc2Row) = names(pc1RepRow) = names(pc2RepRow) = names(sc1RepRow) = names(sc2RepRow) =vars
  outPut <- rbind(sc1Row,sc1RepRow, sc2Row, sc2RepRow, pc1Row,pc1RepRow, pc2Row, pc2RepRow, tfRow, otRow)
  return(outPut)
}


getViz <- function(id){
  idRow <- getTable(id)
  idRow$`Hand off Date` <- as.Date(idRow$`Hand off Date`)
  idRow$`Baseline Reading Date` <- as.Date(idRow$`Baseline Reading Date`)
  idRow$`30 Day Reading Date` <- as.Date(idRow$`30 Day Reading Date`)
  idRow$`60 Day Reading Date` <- as.Date(idRow$`60 Day Reading Date`)
  idRow$`90 Day Reading Date` <- as.Date(idRow$`90 Day Reading Date`)
  df <- melt(idRow[,c("Equipment","Baseline Reading Date",
                      "30 Day Reading Date", "60 Day Reading Date","90 Day Reading Date")], id.vars = "Equipment")
  #bmp("sample2.jpg", 1024, 1024, pointsize = 20)
  
  base_dates = df$value[which(df$variable == "Baseline Reading Date")]
  Day30_dates = df$value[which(df$variable == "30 Day Reading Date")]
  Day60_dates = df$value[which(df$variable == "60 Day Reading Date")]
  Day90_dates = df$value[which(df$variable == "90 Day Reading Date")]
  baseDate = max(base_dates, na.rm=T)
  Day30 = max(Day30_dates, na.rm=T)
  if(Day30 %in% c(NA, -Inf)){Day30 = as.Date(baseDate)+30}
  Day60 = max(Day60_dates, na.rm=T)
  if(Day60 %in% c(NA, -Inf)){Day60 = as.Date(Day30)+30}
  Day90 = max(Day90_dates, na.rm=T)
  if(Day90 %in% c(NA, -Inf)){Day90 = as.Date(Day60)+30}
  OriginalDates = c(baseDate, Day30, Day60, Day90)
  diff_1 = as.integer(Day30 - max(baseDate))
  diff_2 = as.integer(Day60 - Day30)
  diff_3 = as.integer(Day90 - Day60)
  diff_vector = c(diff_1, diff_2, diff_3, "")
  name_vector = c("Baseline", "30 Day", "60 Day", "90 Day")
  df$Equipment <- factor(df$Equipment)
  df$Equipment = factor(df$Equipment, levels = rev(c("Stationary1", "Stationary1 Rep","Stationary2","Stationary2 Rep",
                                                     "Portable1", "Portable1 Rep",  "Portable2", "Portable2 Rep", "Transfill","Tanks")))
  idRow$Equipment = factor(idRow$Equipment)
  idRow$Equipment = factor(idRow$Equipment, levels=levels(df$Equipment))
  
  readingPresent <- function(x){
    #toReturn=rep("No", length(x))
    #toReturn[!x %in% c(NA, 999999999)]="Yes"
    #return(toReturn)
    
    toReturn=sapply(X=x, FUN=function(x){
      if(x %in% c(NA,999999999)){"No"}
      else if(!x %in% c(NA,999999999)){"Yes"}
      else {"Error"}
    })
    return(toReturn)
  }
  
  getNA <- function(x){
    toReturn = sapply(X=x, FUN=function(x){
      if(x %in% c(999999999)){"Missing"}
      else {x}
    })
    return(toReturn)
  }
  readVars <- c("Baseline Reading", "30 Day Reading", "60 Day Reading", "90 Day Reading", "Rep 30", "Rep 60", "Rep 90")
  idRow[,paste0(readVars, "- reading present")]=apply(X=idRow[,readVars], MARGIN=2, FUN=readingPresent)
  idRow[, readVars] = apply(X=idRow[,readVars], MARGIN=2, FUN=getNA)
  plot(x=idRow$`Baseline Reading Date`, y=idRow$Equipment, type="p",
       pch = 19, main = paste0("ID - ", id), xlab = "", ylab = "",
       xaxt = 'n', yaxt = 'n', cex = 1.5, col =c("red", "darkgreen") [as.factor(idRow$`Baseline Reading- reading present`)],
       xlim=c(min(OriginalDates)-5, max(OriginalDates)+5),ylim = c(0.5,length(levels(idRow$Equipment))+0.5))
  abline(v = OriginalDates, lty = 3)
  points(x=idRow$`30 Day Reading Date`, y=idRow$Equipment, type="p",
         pch = 19, main = paste0("ID - ", id), xlab = "", ylab = "",
         xaxt = 'n', yaxt = 'n', cex = 1.5, col =c("red", "darkgreen") [as.factor(idRow$`30 Day Reading- reading present`)], ylim = c(0.5,length(levels(idRow$Equipment))+0.5))
  points(x=idRow$`60 Day Reading Date`, y=idRow$Equipment, type="p",
         pch = 19, main = paste0("ID - ", id), xlab = "", ylab = "",
         xaxt = 'n', yaxt = 'n', cex = 1.5, col =c("red", "darkgreen") [as.factor(idRow$`60 Day Reading- reading present`)], ylim = c(0.5,length(levels(idRow$Equipment))+0.5))
  points(x=idRow$`90 Day Reading Date`, y=idRow$Equipment, type="p",
         pch = 19, main = paste0("ID - ", id), xlab = "", ylab = "",
         xaxt = 'n', yaxt = 'n', cex = 1.5, col =c("red", "darkgreen") [as.factor(idRow$`90 Day Reading- reading present`)], ylim = c(0.5,length(levels(idRow$Equipment))+0.5))
  axis(2,at=1:10, labels=rev(c("SC1","SC1Rep","SC2","SC2Rep","PC1","PC1Rep","PC2", "PC2Rep", "TF","OT")), las = 2, cex.axis=0.75)
  dates = c(idRow$`Hand off Date`, idRow$`30 Day Reading Date`,
            idRow$`60 Day Reading Date`, idRow$`90 Day Reading Date`)
  axis(1, at=df$value, labels = format(dates, format = "%m/%d"), las = 2, cex.axis=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`Baseline Reading Date`, labels = idRow$`Baseline Reading`, pos=2, offset=0.5, cex=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`30 Day Reading Date`, labels = idRow$`30 Day Reading`, pos=2, offset=0.5, cex=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`30 Day Reading Date`, labels = idRow$`Rep 30`, pos=4, offset=0.5, cex=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`60 Day Reading Date`, labels = idRow$`60 Day Reading`, pos=2, offset=0.5, cex=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`60 Day Reading Date`, labels = idRow$`Rep 60`, pos=4, offset=0.5, cex=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`90 Day Reading Date`, labels = idRow$`90 Day Reading`, pos=2, offset=0.5, cex=0.75)
  text(y=factor(idRow$Equipment),x=idRow$`90 Day Reading Date`, labels = idRow$`Rep 90`, pos=4, offset=0.5, cex=0.75)
  mtext("|", side = 1, line = 3, at = OriginalDates)
  #mtext(as.numeric(diff_vector), side = 1, line = 3, at = c(as.integer(Day30 - max(baseDate))/2, as.integer(Day60-Day30)/2, as.integer(Day90-Day60)/2))
  mtext(as.numeric(diff_vector), side = 1, line = 3, at = c(max(baseDate)+10, Day30+10, Day60+10, Day90+10))
  mtext(name_vector, side = 1, line = 4, at = c(max(baseDate), Day30, Day60, Day90))
  
  #dev.off()
}