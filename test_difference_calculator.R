newCalculator <- function(used_dates) {
  sr_frame <- data.frame()
  dates <- unique(used_dates$newdate) #extract a vector of dates
  regions <- unique(used_dates$SR_name) #extract a vector of all security regions
  
  for (i in 1:(length(dates)-1)) {
    step1 <- filter(used_dates, newdate == dates[i]) #filter the data for date 1
    step2 <- filter(used_dates, newdate == dates[i+1]) #filter the data for the day after the current day
  
    for (j in 1:length(regions)) {
      r1_data <- filter(step1, SR_name == regions[j]) #filter all data for a given region on the first day
      r2_data <- filter(step2, SR_name == regions[j]) #filter all data for a given region on the second day
    
      df1 <- r1_data[!duplicated(r1_data[,"MN_name"]),] #delete all duplicate municipality rows
      df2 <- r2_data[!duplicated(r2_data[,"MN_name"]),] #delete all duplicate municipality rows on the second day
    
      increase <- sum(df2$MN_total.reported, na.rm = TRUE) - sum(df1$MN_total.reported, na.rm = TRUE)
      rna_flow <- sum(r2_data$TP_RNA.flow.per.100000)
      r_frame <- data.frame(Date = dates[i+1], Region = regions[j], Increase = increase, RNA.flow = rna_flow)
      sr_frame <- rbind(sr_frame, r_frame)
    }
  }
  non_na_frame <- sr_frame[complete.cases(sr_frame),]
  return(non_na_frame)
}

good_frame <- newCalculator(used_dates)
