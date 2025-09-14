#Code to create one large data fram from the raw licor files
#**Set working directory to a folder only containing the excel files related to this analysis**

#load libraries
library(dplyr)
library(purrr)
library(racir)
library(readxl)
library(tidyverse)

# Get the files in the directory
files <- list.files(full.names = TRUE)

processed_data <- lapply(files, function(file) {
  data <- read_excel(file, skip = 14)

  # Extract necessary information from the file name and add as columns
  individ <- as.numeric(gsub("mjc\\.(\\d+)\\..*", "\\1", basename(file)))
  speciesID <- gsub("mjc\\.\\d+\\.([A-Za-z]+).*", "\\1", basename(file))
  data$individ <- individ
  data$speciesID <- gsub("\\./", "", speciesID)  # Remove the leading './' in speciesID

  # Make column names unique
  colnames(data) <- make.unique(colnames(data), sep = "_")

  # Return the processed data frame
  return(data)
})


all_col_names <- lapply(processed_data, colnames)
all_names <- Reduce(union, all_col_names)

# Add missing columns as NA where necessary
processed_data_filled <- lapply(processed_data_filled, function(df) {
  if ("Water.pot" %in% names(df)) {
    df$Water.pot <- suppressWarnings(as.numeric(df$Water.pot))
  }
  if ("Measure.height" %in% names(df)) {
    df$Measure.height <- suppressWarnings(as.numeric(df$Measure.height))
  }
  if ("Air.temp" %in% names(df)) {
    df$Air.temp <- suppressWarnings(as.numeric(df$Air.temp))
  }
  if ("hhmmss...5" %in% names(df)) {
    df$hhmmss...5 <- suppressWarnings(as.numeric(df$hhmmss...5))
  }
  df
})

full.exin.data <- bind_rows(processed_data_filled)


#Exclude the units row from the file (the licor files have a units row below the column names row)
full.exin.data = full.exin.data%>%
  filter(AuxPower==0)

#Modify the dataframe:
full.exin.data = full.exin.data %>%   #Make a new column with full species names instead of initials
  mutate(species = case_when(
    speciesID == "ac" ~ "A. campestre",
    speciesID == "bp" ~ "B. papyrifera",
    speciesID == "cb" ~ "C. betulus",
    speciesID == "pm" ~ "P. menziesii",
    speciesID == "qg" ~ "Q. garryana",
    TRUE ~ NA_character_ )) %>%
  group_by(speciesID, individ) %>%
  mutate(individual = cur_group_id()) %>% #Make a unique individual name 1-25
  group_by(species,individ)%>%    #Make a branch column. This is unique for each branch that was resampled.
  mutate(branch = case_when(
    obs == c(1,2) ~ paste(speciesID, individual, "1", sep = "."),
    obs == c(3,4) ~ paste(speciesID, individual, "2", sep = "."),
    obs == c(5,6) ~ paste(speciesID, individual, "3", sep = "."),
    obs == c(7,8) ~ paste(speciesID, individual, "4", sep = "."),
    obs == c(9,10) ~ paste(speciesID, individual, "5", sep = "."),
    obs == c(11,12) ~ paste(speciesID, individual, "6", sep = "."),
    obs == c(13,14) ~ paste(speciesID, individual, "7", sep = "."),
    obs == c(15,16) ~ paste(speciesID,individual, "8", sep = ".")))%>%
  mutate(calc.wp=as.numeric(Water.pot) * (-0.00689))%>%   #Water potential was initially measured in psi, now it is in MPa
  mutate(time=case_when(
    obs=="1" ~ "08:50",obs=="2" ~ "08:50",
    obs=="3" ~ "09:50",obs=="4" ~ "09:50",
    obs=="5" ~ "10:50",obs=="6" ~ "10:50",
    obs=="7" ~ "11:50",obs=="8" ~ "11:50",
    obs=="9" ~ "12:50",obs=="10" ~ "12:50",
    obs=="11" ~ "13:50",obs=="12" ~ "13:50",
    obs=="13" ~ "14:50",obs=="14" ~ "14:50",
    obs=="15" ~ "15:50",obs=="16" ~ "15:50")) #%>%  #This made a new time column so we have nicely formatted times
  #select(-time...2, -TIME...11)%>%
  #rename(time_exact = hhmmss...5)
#write.csv(full.exin.data, "Cross_Exint.Data.csv")
