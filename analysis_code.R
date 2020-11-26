library(dplyr)
library(tidyverse)
library()

dat <- readRDS("covid cases waste + tests.RDS")
str(dat)
?dat

dat2 <- distinct(dat)

step1 <- filter(dat2, newdate == "2020-10-25")
step2 <- filter(dat2, newdate == "2020-10-24")

df1 <- step1[!duplicated(step1[,"Municipality_name"]),]
df2 <- step2[!duplicated(step2[,"Municipality_name"]),]

sum(df1$Total_reported) - sum(df2$Total_reported)
sum(df1$Total_reported)
sum(df2$Total_reported)

mean1 <- dat2 %>%
  filter(newdate == "2020-10-25") %>%
  group_by(Municipality_name) %>%
  summarise(Total_reported2 = mean(Total_reported)) %>%
  summarise(Sum_total = sum(Total_reported2))

mean2 <- dat2 %>%
  filter(newdate == "2020-10-24") %>%
  group_by(Municipality_name) %>%
  summarise(Total_reported2 = mean(Total_reported)) %>%
  summarise(Sum_total = sum(Total_reported2))

mean1$Sum_total - mean2$Sum_total

unique(dat2$newdate)

dates <- unique(dat$newdate)
dates[1]
dates[2]

dates[-c(1:172)]

used_data <- dat[,dat$newdate %in% dates[-c(1:172)]]


used_data <- dat %>% filter(newdate %in% dates[-c(1:172)])
new_data <- used_data

new_data$inhabitants <- new_data$inhabitants * 1000

#Step 1
used_data <- dat %>% filter(newdate %in% dates[-c(1:178)])

differenceCalculator <- function(sewer_data) {
  dates <- unique(sewer_data$newdate) #create a list of dates
  differences <- numeric(length(dates)-1)
  for (i in 1:(length(differences))) {
    step1 <- filter(sewer_data, newdate == dates[i])
    step2 <- filter(sewer_data, newdate == dates[i+1])
    
    df1 <- step1[!duplicated(step1[,"Municipality_name"]),]
    df2 <- step2[!duplicated(step2[,"Municipality_name"]),]
    diff <- (sum(df2$Total_reported, na.rm = TRUE) - sum(df1$Total_reported, na.rm = TRUE))
    
    differences[i] <- diff
  }
  diff_frame <- as.data.frame(cbind(dates[-1], differences))
  colnames(diff_frame) <- c("Date", "Increase in reported infections")
  diff_frame$`Increase in reported infections` <- as.numeric(diff_frame$`Increase in reported infections`)
  return(diff_frame)
}

diff_frame <- differenceCalculator(used_data)
head(diff_frame)

diff2 <- differenceCalculator(dat)

#Step 2
used_data$inhabitants <- used_data$inhabitants * 1000
used_data$cases_per_100000 <- used_data$Total_reported * (100000/used_data$inhabitants)

differenceCalculator2 <- function(used_data) {
  dates <- unique(used_data$newdate) #create a list of dates
  differences <- numeric(length(dates)-1)
  for (i in 1:(length(differences))) {
    step1 <- filter(used_data, newdate == dates[i])
    step2 <- filter(used_data, newdate == dates[i+1])
    
    df1 <- step1[!duplicated(step1[,"Municipality_name"]),]
    df2 <- step2[!duplicated(step2[,"Municipality_name"]),]
    diff <- (mean(df2$cases_per_100000, na.rm = TRUE) - mean(df1$cases_per_100000, na.rm = TRUE))
    
    differences[i] <- diff
  }
  diff_frame <- as.data.frame(cbind(dates[-1], differences))
  colnames(diff_frame) <- c("Date", "Increase in reported infections per 100000")
  diff_frame$`Increase in reported infections per 100000` <- as.numeric(diff_frame$`Increase in reported infections per 100000`)
  return(diff_frame)
}

differenceCalculator2(used_data)

#Step 3
used_data$RNA_flow_per_100000 <- as.numeric(used_data$RNA_flow_per_100000)

virusCalculator <- function(used_data) {
  dates <- unique(used_data$newdate)
  means <- numeric(length(dates))
  for (i in 1:length(means)) {
    date_data <- filter(used_data, newdate == dates[i])
    df1 <- date_data[!duplicated(date_data[,"Municipality_name"]),]
    means[i] <- mean(df1$RNA_flow_per_100000, na.rm = TRUE)
  }
  means_frame <- as.data.frame(cbind(dates, means))
  colnames(means_frame) <- c("Date", "Mean RNA flow per 100000")
  means_frame$`Mean RNA flow per 100000` <- as.double(means_frame$`Mean RNA flow per 100000`)
  return(means_frame)
}

data_frame1 <- virusCalculator(used_data)

#Step 4

## We believe that the value in the column 'Percentage_in_security_region' corresponds with the percentage of sewage water that a RWZI processes that comes from the security region it is in. Therefore, a sensible way of implementing the weighting is to take that percentage and multiply it with the RNA particle flow within that RWZI. We subsequently take the mean per day and see how it has affected the mean RNA flow.

### FOR OUR GROUP: I'VE REMOVED ALL NA ROWS FOR NOW. THIS WOULD HAVE ALSO BEEN DONE IF NA.RM HAD BEEN IMPLEMENTED IN CALCULATING THE MEAN

rnaWeighting <- function(used_data) {
  options(digits = 22)
  dates <- unique(used_data$newdate) #create a list of all dates
  adjusted_means <- numeric(length(dates)) #create a vector of zeros that is equal in length to the number of dates
  
  non_na_data <- used_data[!is.na(used_data$RNA_flow_per_100000),] #just for now eliminate NA rows
  non_na_data2 <- non_na_data[!is.na(non_na_data$Percentage_in_security_region),]
  non_na_data2$Percentage_in_security_region <- as.double(non_na_data2$Percentage_in_security_region)
  non_na_data2$adjusted_RNA_flow <- (non_na_data2$RNA_flow_per_100000 * non_na_data2$Percentage_in_security_region) #adjust rna flow by multiplying it with the proportion of water from that security region processed by that RWZI
  for (i in 1:length(adjusted_means)) {
    day_data <- filter(non_na_data2, newdate == dates[i])
    df1 <- day_data[!duplicated(day_data[,"Municipality_name"]),]
    adjusted_means[i] <- mean(df1$adjusted_RNA_flow, na.rm = TRUE)
  }
  means_frame <- as.data.frame(cbind(dates, adjusted_means))
  colnames(means_frame) <- c("Date", "Adjusted mean RNA flow per 100000")
  means_frame$`Adjusted mean RNA flow per 100000` <- as.double(means_frame$`Adjusted mean RNA flow per 100000`)
  return(means_frame)
}

data_frame2 <- rnaWeighting(used_data)

mean(data_frame1$`Mean RNA flow per 100000`, na.rm = TRUE)
mean(data_frame2$`Adjusted mean RNA flow per 100000`, na.rm = TRUE)
