# Combine Belarus age data.tables, then write to raster
# Christopher Crawford, August 27th, 2020

# load libraries
# NOTE: One cannot install packages on individual cores, since the individual cores 
# do not have access to the internet. You have to install them on the head node. 
# You do this by opening R, and using the typical R commands, e.g. 
# install.packages("package") and library(package).

library(raster)
library(data.table)
library(devtools)
library(tictoc)
# devtools::install_github("ldemaz/dtraster")
library(dtraster)

update.packages()
tic("Full script")

# load custom filtering functions
source("scripts/util/_util_dt_filter_functions.R")

directory <- "/Users/christophercrawford/Google Drive/_Projects/abandonment_trajectories/data_derived/"

tic("load data.tables")
b1_age <- fread(input = paste0(directory, "belarus1_age.csv"))
b2_age <- fread(input = paste0(directory, "belarus2_age.csv"))
b3_age <- fread(input = paste0(directory, "belarus3_age.csv"))
b4_age <- fread(input = paste0(directory, "belarus4_age.csv"))
b5_age <- fread(input = paste0(directory, "belarus5_age.csv"))
b6_age <- fread(input = paste0(directory, "belarus6_age.csv"))
toc(log = TRUE)


# ------------------------------
# merge into single data.table

tic("rbind data.tables")
b_age <- rbindlist(list(b1_age, b2_age, b3_age, b4_age, b5_age, b6_age))
toc(log = TRUE)

tic("write out merged data.table")
fwrite(b_age, file = paste0(directory, "belarus_age.csv"))
toc(log = TRUE)


# final toc()
toc(log = TRUE)



# print times
tic.log()


