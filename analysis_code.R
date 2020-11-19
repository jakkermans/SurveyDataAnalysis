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
