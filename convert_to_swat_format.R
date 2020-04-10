##    *********         Run each section of the code separetly           ********     ##
## **** change the starting date in each text output file to reflect the true date. Use notepad++ for that **** ##



##libraries ----
install.packages("pacman")
library(pacman)
pacman::p_load(tidyr,dplyr,data.table,finalfit, install = TRUE)


setwd("I:/folder/name")

## loading all files and organizing them separately----
#list all files
file.path = "I:/hurs"
pattern = c('tasmax', 'tasmin', 'pr', 'rsds', 'sfcWind', 'hurs', "*.txt")
temp.files = list.files(file.path, pattern = paste0(pattern, collapse = "|"), full = T)
files = grep(pattern = "*.nc", temp.files, invert = T, value = T)

#segregating similiar weather files together----
#for e.g. tasmin in one, tasmax in another etc.
hurs = list()
pr = list()
sfcWind = list()
tasmax = list()
tasmin = list()
rsds = list()

for (i in files) {
  hurs = grep(pattern = "hurs", files, value = T)
  pr = grep(pattern = "pr", files, value = T)
  sfcWind = grep(pattern = "sfcWind", files, value = T)
  tasmax = grep(pattern = "tasmax", files, value = T)
  tasmin = grep(pattern = "tasmin", files, value = T)
  rsds = grep(pattern = "rsds", files, value = T)
}


## formatting the data structure ----
newcolnames = c("t221850", "t221852", "t221855", "t221857", "t221860", "t221862", "t221865", "t221867", "t221870",
                "t221872", "t221875", "t223850", "t223852", "t223855", "t223857", "t223860", "t223862",	"t223865",
                "t223867", "t223870", "t223872", "t223875", "t226850", "t226852", "t226855", "t226857", "t226860", 
                "t226862", "t226865", "t226867", "t226870", "t226872", "t226875", "t228850", "t228852", "t228855", 
                "t228857", "t228860", "t228862", "t228865", "t228867", "t228870", "t228872", "t228875", "t231850", 
                "t231852", "t231855", "t231857", "t231860", "t231862", "t231865", "t231867", "t231870", "t231872", 
                "t231875", "t233850", "t233852", "t233855", "t233857", "t233860", "t233862", "t233865", "t233867", 
                "t233870", "t233872", "t233875")

## ********    working on tmp files:: this should be run separately ---- 


#1. adding a new column by combining lat and lon columns
for (i in 1:length(tasmax)) {
  
  tasmin_wide = as_tibble()
  tasmax_wide = as_tibble()
  tas = as_tibble()
  
  tempmin = as_tibble()
  tempmax = as_tibble()
  
  tempmin = read.table(tasmin[i])
  tempmax = read.table(tasmax[i])
  
  tempmin$latlon = paste(tempmin$V2, tempmin$V3, sep = ",")
  tempmax$latlon = paste(tempmax$V2, tempmax$V3, sep = ",")
  
  #2. removing lat and lon columns
  tempmin$V2 = NULL
  tempmin$V3 = NULL
  tempmax$V2 = NULL
  tempmax$V3 = NULL
  
  #3. creating a pivot table with latlon as columns and date as rows
  tasmin_wide = pivot_wider(tempmin, names_from = 'latlon', values_from = 'V4')
  tasmax_wide = pivot_wider(tempmax, names_from = 'latlon', values_from = 'V4')
  
  #test files with date
  test = tasmax_wide
  
  #4. remove first column containing date
  tasmax_wide[,1] = NULL
  tasmin_wide[,1] = NULL
  
  
  ## converting tasmax and tasmin into SWAT weather file format
  # adding a new column in one of the files
  # containing TASMAX and TASMIN separated by "," 
  # and containing three decimal points
  
  for (j in 1:ncol(tasmax_wide)) {
    for (k in 1:nrow(tasmax_wide)) {
        tas[k,j] = paste(finalfit::round_tidy(tasmax_wide[k,j], digits = 3), 
                       finalfit::round_tidy(tasmin_wide[k,j], digits = 3),
                       sep = ",")
      
    }
  }
  colnames(tas) = newcolnames
  tas.df = as.data.frame(tas)
  tas.df = rbind(c(19990101), tas.df)
 
  #Exporting the table as TEXT file
  for (c in names(tas.df)) {
    mainDir = file.path
    subDir = c(tools::file_path_sans_ext(basename(tasmin[i])))
    dir.create(file.path(mainDir, subDir), showWarnings = TRUE)
    setwd(file.path(mainDir, subDir))
    raw_file <- paste(c, ".txt", sep = "")
    write.table(tas.df[c], raw_file, 
                sep = "\t", quote = F, row.names = F, col.names = F, append = F)
  }
  #write.table(test, "test.txt", row.names = F, quote = F, sep = "\t")
  tas = as_tibble()
}


setwd(file.path)

## ********    working on pcp files:: this should be run separately ---- 

#1. adding a new column by combining lat and lon columns and resetting decimal points
for (i in 1:length(pr)) {
  
  pr_wide = as_tibble()
  temppr = as_tibble()
  
  temppr = read.table(pr[i])
  temppr$latlon = paste(temppr$V2, temppr$V3, sep = ",")
  temppr$V4 = finalfit::round_tidy(temppr$V4, digits = 3)
  
  #2. removing lat and lon columns
  temppr$V2 = NULL
  temppr$V3 = NULL
  
  #3. creating a pivot table with latlon as columns and date as rows
  pr_wide = pivot_wider(temppr, names_from = 'latlon', values_from = 'V4')
  
  #test files with date
  test = pr_wide
  
  #4. remove first column containing date
  pr_wide[,1] = NULL
  
  
  ## converting pr into SWAT weather file format
  
  colnames(pr_wide) = sub(pattern = "t", replacement = "p", newcolnames)
  pr.df = as.data.frame(pr_wide)
  pr.df = rbind(c(19990101), pr.df)
  #Exporting the table as TEXT file
  for (c in names(pr.df)) {
    mainDir = file.path
    subDir = c(tools::file_path_sans_ext(basename(pr[i])))
    dir.create(file.path(mainDir, subDir), showWarnings = TRUE)
    setwd(file.path(mainDir, subDir))
    raw_file <- paste(c, ".txt", sep = "")
    write.table(pr.df[c], raw_file, 
                sep = "\t", quote = F, row.names = F, col.names = F, append = F)
  }
  write.table(test, "test.txt", row.names = F, quote = F, sep = "\t")
}

setwd(file.path)

## ********    working on sfcWind files:: this should be run separately ---- 


#1. adding a new column by combining lat and lon columns and resetting decimal points
for (i in 1:length(sfcWind)) {
  
  sfcWind_wide = as_tibble()
  tempsfcWind = as_tibble()
  
  tempsfcWind = read.table(sfcWind[i])
  tempsfcWind$latlon = paste(tempsfcWind$V2, tempsfcWind$V3, sep = ",")
  tempsfcWind$V4 = finalfit::round_tidy(tempsfcWind$V4, digits = 3)
  
  #2. removing lat and lon columns
  tempsfcWind$V2 = NULL
  tempsfcWind$V3 = NULL
  
  #3. creating a pivot table with latlon as columns and date as rows
  sfcWind_wide = pivot_wider(tempsfcWind, names_from = 'latlon', values_from = 'V4')
  
  #test files with date
  test = sfcWind_wide
  
  #4. remove first column containing date
  sfcWind_wide[,1] = NULL
  
  
  ## converting sfcWind into SWAT weather file format
  
  colnames(sfcWind_wide) = sub(pattern = "t", replacement = "w", newcolnames)
  sfcWind.df = as.data.frame(sfcWind_wide)
  sfcWind.df = rbind(c(19990101), sfcWind.df)
  
  #Exporting the table as TEXT file
  for (c in names(sfcWind.df)) {
    mainDir = file.path
    subDir = c(tools::file_path_sans_ext(basename(sfcWind[i])))
    dir.create(file.path(mainDir, subDir), showWarnings = TRUE)
    setwd(file.path(mainDir, subDir))
    raw_file <- paste(c, ".txt", sep = "")
    write.table(sfcWind.df[c], raw_file, 
                sep = "\t", quote = F, row.names = F, col.names = F, append = F)
  }
  write.table(test, "test.txt", row.names = F, quote = F, sep = "\t")
}

setwd(file.path)

## ********    working on hurs files:: this should be run separately ---- 

#1. adding a new column by combining lat and lon columns and resetting decimal points
for (i in 1:length(hurs)) {
  
  hurs_wide = as_tibble()
  temphurs = as_tibble()
  
  temphurs = read.table(hurs[i])
  temphurs$latlon = paste(temphurs$V2, temphurs$V3, sep = ",")
  temphurs$V4 = finalfit::round_tidy(temphurs$V4, digits = 3)
  
  #2. removing lat and lon columns
  temphurs$V2 = NULL
  temphurs$V3 = NULL
  
  #3. creating a pivot table with latlon as columns and date as rows
  hurs_wide = pivot_wider(temphurs, names_from = 'latlon', values_from = 'V4')
  
  #4. remove first column containing date
  hurs_wide[,1] = NULL
  
  
  ## converting hurs into SWAT weather file format
  
  colnames(hurs_wide) = sub(pattern = "t", replacement = "rh", newcolnames)
  hurs.df = as.data.frame(hurs_wide)
  hurs.df = rbind(c(19990101), hurs.df)
  
  #Exporting the table as TEXT file
  for (c in names(hurs.df)) {
    mainDir = file.path
    subDir = c(tools::file_path_sans_ext(basename(hurs[i])))
    dir.create(file.path(mainDir, subDir), showWarnings = TRUE)
    setwd(file.path(mainDir, subDir))
    raw_file <- paste(c, ".txt", sep = "")
    write.table(hurs.df[c], raw_file, 
                sep = "\t", quote = F, row.names = F, col.names = F, append = F)
  }
}

setwd(file.path)

## ********    working on rsds files:: this should be run separately ---- 

#1. adding a new column by combining lat and lon columns and resetting decimal points
for (i in 1:length(rsds)) {
  
  rsds_wide = as_tibble()
  temprsds = as_tibble()
  
  temprsds = read.table(rsds[i])
  temprsds$latlon = paste(temprsds$V2, temprsds$V3, sep = ",")
  temprsds$V4 = finalfit::round_tidy(temprsds$V4, digits = 3)
  
  #2. removing lat and lon columns
  temprsds$V2 = NULL
  temprsds$V3 = NULL
  
  #3. creating a pivot table with latlon as columns and date as rows
  rsds_wide = pivot_wider(temprsds, names_from = 'latlon', values_from = 'V4')
  
  #test files with date
  test = rsds_wide
  
  #4. remove first column containing date
  rsds_wide[,1] = NULL
  
  
  ## converting rsds into SWAT weather file format
  
  colnames(rsds_wide) = sub(pattern = "t", replacement = "s", newcolnames)
  rsds.df = as.data.frame(rsds_wide)
  rsds.df = rbind(c(19990101), rsds.df)
  
  #Exporting the table as TEXT file
  for (c in names(rsds.df)) {
    mainDir = file.path
    subDir = c(tools::file_path_sans_ext(basename(rsds[i])))
    dir.create(file.path(mainDir, subDir), showWarnings = TRUE)
    setwd(file.path(mainDir, subDir))
    raw_file <- paste(c, ".txt", sep = "")
    write.table(rsds.df[c], raw_file, 
                sep = "\t", quote = F, row.names = F, col.names = F, append = F)
  }
  write.table(test, "test.txt", row.names = F, quote = F, sep = "\t")
}

setwd(file.path)
