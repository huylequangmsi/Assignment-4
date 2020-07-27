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
    mutate(V2 = as.integer(V2),
           V192 = as.numeric(V192))


data_final <- full_join(data_clean, country_code, by = "V2") # Merge data clean with the country code
save(data_final, file = "data_final.RData")
    
## Create objects for ShinyApp

country.name <- setNames(data_final$country, data_final$country) # country names

## Attitude to Democracy

democracy <- data_final %>% 
    group_by(country) %>% 
    summarise(m.V228A = mean(V228A, na.rm = TRUE),
              m.V228B = mean(V228B, na.rm = TRUE),
              m.V228C = mean(V228C, na.rm = TRUE),
              m.V228D = mean(V228D, na.rm = TRUE),
              m.V228E = mean(V228E, na.rm = TRUE),
              m.V228F = mean(V228F, na.rm = TRUE),
              m.V228G = mean(V228G, na.rm = TRUE),
              m.V228H = mean(V228H, na.rm = TRUE),
              m.V228I = mean(V228I, na.rm = TRUE))
head(democracy)

democracy[complete.cases(democracy),] %>% 
    ggplot(aes(x = country))+
    geom_point(aes(y = m.V228A, colour = "Votes are counted fairly"))+
    geom_point(aes(y = m.V228B, colour = "Opposition candidates are prevented from running"))+
    geom_point(aes(y = m.V228C, colour = "TV news favors the governing party"))+
    geom_point(aes(y = m.V228D, colour = "Voters are bribed"))+
    geom_point(aes(y = m.V228E, colour = "Journalists provide fair coverage of elections"))+
    geom_point(aes(y = m.V228F, colour = "Election officials are fair"))+
    geom_point(aes(y = m.V228G, colour = "Rich people buy elections"))+
    geom_point(aes(y = m.V228H, colour = "Voters are threatened with violence"))+
    geom_point(aes(y = m.V228I, colour = "Voters are offered a genuine choice"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          text = element_text(size = 9)) + 
    xlab("Country") + 
    ylab("Mean Reported Attitude to Science") + 
    labs(title = "Self-reported Attitude to Science", 
         subtitle = "(1: Very often, 4: Not at all often)",
         caption = "Source: World Values Survey, Wave 6, 2010-14")
    

## Attitude to Science 

science <- data_final %>% 
    group_by(country) %>% 
    summarise(m.V192 = mean(V192, na.rm = TRUE),
              m.V193 = mean(V193, na.rm = TRUE),
              m.V194 = mean(V194, na.rm = TRUE),
              m.V195 = mean(V195, na.rm = TRUE),
              m.V196 = mean(V196, na.rm = TRUE),
              m.V197 = mean(V197, na.rm = TRUE))

head(science)

science[complete.cases(science),] %>% 
           ggplot(aes(x=country)) +
    geom_point(aes(y = m.V192, colour = "Science makes our lives better"))+
    geom_point(aes(y = m.V193, colour = "Science brings more opportunities"))+
    geom_point(aes(y = m.V194, colour = "We depend too much on science"))+
    geom_point(aes(y = m.V195, colour = "Science breaks down people’s ideas of right and wrong"))+
    geom_point(aes(y = m.V196, colour = "Science is not important"))+
    geom_point(aes(y = m.V197, colour = "The world is better off thanks to science"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          text = element_text(size = 9)) + 
    xlab("Country") + 
    ylab("Mean Reported Attitude to Science") + 
    labs(title = "Self-reported Attitude to Science", 
         subtitle = "(1: Completely disagree, 10: Completely agree)",
         caption = "Source: World Values Survey, Wave 6, 2010-14")




