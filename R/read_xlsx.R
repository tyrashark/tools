# 1. read data file of xlsx
##install.packages("readxl")
library(readxl)
#setwd()

rawdata <- read_xlsx("data.xlsx", sheet = "phenotype_S1")

n = dim(rawdata)[1]

data_rep <- list()

## Before proceeding in this part, check the excel file to set a range.
data_rep[[1]] <- read_xlsx("data.xlsx", sheet = "phenotype_S1", range = paste0("A1:H", n+1), col_types = c("text", "text", "text", "text", rep("numeric", 4)))
data_rep[[2]] <- read_xlsx("data.xlsx", sheet = "phenotype_S1", range = paste0("J1:Q", n+1), col_types = c("text", "text", "text", "text", rep("numeric", 4)))
data_rep[[3]] <- read_xlsx("data.xlsx", sheet = "phenotype_S1", range = paste0("S1:Z", n+1), col_types = c("text", "text", "text", "text", rep("numeric", 4)))

### check the warning message.

## please check the each dataframe
data_rep[[1]]; data_rep[[2]]; data_rep[[3]]

data_df = NULL

## check column names
coln = colnames(data_rep[[1]])
coln

for(r in 1:3){
  ## arrange the columns for each dataframe if it has a different order.
  data_rep[[r]] = data_rep[[r]] %>% select(coln)
  
  ## stack all the dataframes in one dataframe.
  data_df = rbind(data_df, data_rep[[r]])
}

# 2. manipulate the data
##install.packages("tidyverse")
library(tidyverse)
