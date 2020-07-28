## Load necessary packages
library(tidyverse)
library(plyr)
library(data.table)


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
    dplyr::group_by(country) %>% 
    dplyr::summarise(m.V228A = mean(V228A, na.rm = TRUE),
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
          text = element_text(size = 12)) + 
    xlab("Country") + 
    ylab("Mean Reported Attitude to Democracy") + 
    labs(title = "Self-reported Attitude to Democracy", 
         subtitle = "(1: Very often, 4: Not at all often)",
         caption = "Source: World Values Survey, Wave 6, 2010-14",
         colour = "How often do the following things occur in this country’s elections?")


    
## News consumption

# To make a summary table

news1 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V217) %>% 
    dplyr::mutate(p.V217 = n/sum(n)) %>% 
    dplyr::select(-n)

news2 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V218) %>% 
    dplyr::mutate(p.V218 = n/sum(n)) %>% 
    dplyr::select(-n)

news3 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V219) %>% 
    dplyr::mutate(p.V219 = n/sum(n)) %>% 
    dplyr::select(-n)

news4 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V220) %>% 
    dplyr::mutate(p.V220 = n/sum(n)) %>% 
    dplyr::select(-n)

news5 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V221) %>% 
    dplyr::mutate(p.V221 = n/sum(n)) %>% 
    dplyr::select(-n)

news6 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V222) %>% 
    dplyr::mutate(p.V222 = n/sum(n)) %>% 
    dplyr::select(-n)

news7 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V223) %>% 
    dplyr::mutate(p.V223 = n/sum(n)) %>% 
    dplyr::select(-n)

news8 <- data_final %>%
    dplyr::group_by(country) %>% 
    dplyr::count(V224) %>% 
    dplyr::mutate(p.V224 = n/sum(n)) %>% 
    dplyr::select(-n)

news1 <- data.frame(news1)
news2 <- data.frame(news2)
news3 <- data.frame(news3)
news4 <- data.frame(news4)
news5 <- data.frame(news5)
news6 <- data.frame(news6)
news7 <- data.frame(news7)
news8 <- data.frame(news8)

news1$rn <- rownames(news1)
news2$rn <- rownames(news2)
news3$rn <- rownames(news3)
news4$rn <- rownames(news4)
news5$rn <- rownames(news5)
news6$rn <- rownames(news6)
news7$rn <- rownames(news7)
news8$rn <- rownames(news8)

news <- join_all(list(news1, news2, news3, news4,
                      news5, news6, news7, news8), 
                 by = 'rn', type = 'full')
news$rn <- NULL

head(news)

# To plot data

news_wide <- data_final %>% 
    select(country, V217, V218, V219, V220,
           V221, V222, V223, V224) %>% 
    dplyr::rename(Daily_newspaper = V217,
           Printed_magazines = V218,
           TV_news = V219,
           Radio_news = V220,
           Mobile_phone = V221,
           Email = V222,
           Internet = V223,
           Friends_Colleagues = V224)


news_long <- melt(news_wide, id.vars = "country")


news_long[complete.cases(news_long),] %>% 
    filter(country == "Algeria") %>% 
    ggplot(aes(x = variable, y = as.factor(value), fill = as.factor(value)))+
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          text = element_text(size = 12)) + 
    xlab("Media")+
    ylab("Proportion") + 
    labs(title = "News consumption", 
         subtitle = "(1: Daily, 2: Weekly, 3: Monthly, 4: < Monthly, 5: Never)",
         caption = "Source: World Values Survey, Wave 6, 2010-14",
         fill = "Frequency")


## Attitude to Science 

science <- data_final %>% 
    dplyr::group_by(country) %>% 
    dplyr::summarise(m.V192 = mean(V192, na.rm = TRUE),
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
          text = element_text(size = 12)) + 
    xlab("Country") + 
    ylab("Mean Reported Attitude to Science") + 
    labs(title = "Self-reported Attitude to Science", 
         subtitle = "(1: Completely disagree, 10: Completely agree)",
         caption = "Source: World Values Survey, Wave 6, 2010-14")




