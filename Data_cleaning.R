## Load necessary packages
library(tidyverse)


## Import data

data_raw <- read_rds("G:/My Drive/IPSDS/Modern Workflow in Data Science/Assignments/Assignment-4/F00007762-WV6_Data_R_v20180912.rds")
country_code <- read.csv("G:/My Drive/IPSDS/Modern Workflow in Data Science/Assignments/Assignment-4/country_code.csv")

## Make a smaller dataset with only necessary variables

vars <- c("V2","V192", "V193", "V194", "V195", "V196", "V197",
          "V217", "V218", "V219", "V220", "V221", "V222", "V223", "V224",
          "V228A", "V228B", "V228C", "V228D", "V228E", "V228F", "V228G", "V228H", "V228I")

data_clean <- data_raw %>% 
    select(vars) %>% 
    filter_all(all_vars(. > 0)) %>% 
    mutate(V2 = as.integer(V2))


data_final <- full_join(data_clean, country_code, by = "V2") # Merge data clean with the country code
    

