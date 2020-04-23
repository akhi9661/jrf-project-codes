#--------------------------------------------------------------------------
# This R code calculates Drought Frequency and Drought Duration for
# 1. Drought i.e. SPEI < 0
# 2. Severe Drought i.e. SPEI b/w -1.5 to -2.0
# 3. Extreme Drought i.e. SPEI < -2.0
# 
# The input file contains 7 variables: 1 variable is Date and variables
# 2:7 is SPEI values at scale 3, 6, 9, 12, 15, and 24
#---------------------------------------------------------------------------

# libraries
install.packages("pacman")
library(pacman)
pacman::p_load(lubridate,dplyr,xlsx, install = TRUE)

# load file
filepath = "I:/something.xlsx"
file.dd = read_xlsx(filepath, sheet = "sheet1") #change the sheet name here


dd = file.dd[,1:2]
drought_data = list(file.dd[,c(1:2)],
                    file.dd[,c(1,3)],
                    file.dd[,c(1,4)],
                    file.dd[,c(1,5)],
                    file.dd[,c(1,6)],
                    file.dd[,c(1,7)])

dd.dur_list = list()
ddsev.dur_list = list()
ddext.dur_list = list()

for (i in 1:length(drought_data)) {
  dd = drought_data[[i]]
  
#-----------------------------------------------------------------------------------
# use lubridate and cut to create our yearly and 20-year variables 
# to group by later and create a column drought signifying if spei was negative.
#-----------------------------------------------------------------------------------

#create a column to group on by year and by 20-year
dd <- dd %>%
  mutate(year  = year(Date),
         year_20 = cut(year, breaks = c(2020,2040,2060,2080,2100), include.lowest = T,
                       labels = c("2021_2040", "2041_2060", "2061_2080", "2081_2100")))  %>%
  #column signifying if that month was a drought
  mutate(drought = ifelse(dd[,2]<0,1,0),
  dd_severe = ifelse(dd[,2]< -1.5 & dd[,2] > -2,1,0),
  dd_extreme = ifelse(dd[,2]< -2,1,0))


#----------------------------------------------------------------------------------------------------------
# use the group_by function to get frequency (or number of months with a drought) by year or 20-year period
#----------------------------------------------------------------------------------------------------------

#by year
dd_year <- dd %>%
  group_by(year) %>%
  summarise(year_freq = sum(drought, na.rm = T),
  year_sev = sum(dd_severe, na.rm = T),
  year_ext = sum(dd_extreme, na.rm = T)) %>%
  ungroup()

#by 20-year group
dd_year20 <- dd %>%
  group_by(year_20) %>%
  summarise(year20_freq = sum(drought, na.rm = T),
  year20_sev = sum(dd_severe, na.rm = T),
  year20_ext = sum(dd_extreme, na.rm = T)) %>%
  ungroup()

dd_year_list[i] = list(dd_year)
dd_yr20_list[i] = list(dd_year20)


#----------------------------------------------------------------------------------------------
# Calculating drought duration is a bit more complicated. It involves
# 1. identifying the first month of each drought
# 2. calculating the length of each drought
# 3. combining information from 1 and 2 together
# We can use lag to identify when a month changed from "no drought" to "drought". 
# In this case we want an index of where the value in row i is different from that in row i-1
#----------------------------------------------------------------------------------------------

# find index of where values change. 
change.ind <- dd$drought != lag(dd$drought)
ch.ind.sev <- dd$dd_severe != lag(dd$dd_severe)
ch.ind.ext <- dd$dd_extreme !=lag(dd$dd_extreme)

#use index to find drought start
drought.start <- dd[change.ind & dd$drought == 1,]
dd.start.sev <- dd[ch.ind.sev & dd$dd_severe ==1,]
dd.start.ext <- dd[ch.ind.ext & dd$dd_extreme ==1,]

#--------------------------------------------------------------------------------------------------------------------
# This results in a subset of the initial dataset, but only with the rows with the first month of a drought. 
# Then we can use rle to calculate the length of the drought. rle will calculate the length of every run of numbers, 
# so we will have to subset to only those runs where the value==1 (drought)
#--------------------------------------------------------------------------------------------------------------------

#calculate drought lengths
drought.lengths <- rle(as.vector(dd$drought))
dd.len.sev <- rle(as.vector(dd$dd_severe))
dd.len.ext <- rle(as.vector(dd$dd_extreme))

# we only want droughts (values = 1)
drought.lengths <- drought.lengths$lengths[drought.lengths$values==1]
dd.len.sev <- dd.len.sev$lengths[dd.len.sev$values==1]
dd.len.ext <- dd.len.ext$lengths[dd.len.ext$values==1]

#--------------------------------------------------------------------------------
# Now we can combine these two pieces of information together. 
# The first row is an NA because there is no value at i-1 to compare the lag to.
#--------------------------------------------------------------------------------

drought.dur <- cbind(drought.start, drought_length = drought.lengths)
dd.dur.sev <- cbind(dd.start.sev, drought_length = dd.len.sev)
dd.dur.ext <- cbind(dd.start.ext, drought_length = dd.len.ext)

dd.dur_list[i] = list(drought.dur)
ddsev.dur_list[i] = list(dd.dur.sev)
ddext.dur_list[i] = list(dd.dur.ext)

}

#------------------------------------------------------
# export all the data frames in a list as Excel file
#--------------------------------------------------------

master.list = list(dd_year_list, dd_yr20_list)
for (i in c(1:length(master.list))){
  write.xlsx(master.list[i], file = "Duration_years.xlsx", sheetName=paste(i), append=T)
  }

#--------------------------------------------------------------------
# since drought duration files have different number of rows, hence
# need to be exported separately
#--------------------------------------------------------------------

for (i in c(1:length(dd.dur_list))){
  dd.dur_list$id <- rownames(dd.dur_list) 
  melt(dd.dur_list)
  write.xlsx(dd.dur_list[i], file = "Drought_duration.xlsx", sheetName=paste(i), append=T)
}

for (i in c(1:length(ddext.dur_list))){
  ddext.dur_list$id <- rownames(ddext.dur_list) 
  melt(ddext.dur_list)
  write.xlsx(ddext.dur_list[i], file = "Severe_drought_duration.xlsx", sheetName=paste(i), append=T)
}

for (i in c(1:length(ddsev.dur_list))){
  ddsev.dur_list$id <- rownames(ddsev.dur_list) 
  melt(ddsev.dur_list)
  write.xlsx(ddsev.dur_list[i], file = "Extreme_drought_duration.xlsx", sheetName=paste(i), append=T)
}
