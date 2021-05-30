library("tidyverse")
library("ggplot2")
library("dplyr")
library("zoo")
library("scales")
library("rjson")

setwd("/Users/shuang/Desktop/Spring 2021/PPHA 30560 Data Visualization/HW/Final/p1")

global<-read_csv("owid-covid-data.csv")
                 glo# col_types = cols(.default = "c"))

# P1 Data
setwd("/Users/shuang/Desktop/Spring 2021/PPHA 30560 Data Visualization/HW/Final/p1")
global.clean <- global %>% 
  select(location, date, total_cases, 
         new_cases, new_cases_smoothed,
         total_deaths, new_deaths, new_deaths_smoothed,
         people_vaccinated,total_vaccinations, new_vaccinations, 
         population,gdp_per_capita) %>% 
  subset(location == "World") %>% 
  arrange(date) %>% 
  mutate(date = as.Date(date),
         total_cases = as.numeric(total_cases),
         new_cases =  as.numeric(new_cases),
         new_cases_smoothed =  as.numeric(new_cases_smoothed),
         total_deaths =  as.numeric(total_deaths),
         new_deaths =  as.numeric(new_deaths),
         new_deaths_smoothed =  as.numeric(new_deaths_smoothed),
         people_vaccinated =  as.numeric(people_vaccinated),
         total_vaccinations =  as.numeric(total_vaccinations),
         new_vaccinations =  as.numeric(new_vaccinations),
         population =  as.numeric(population),
         gdp_per_capita =  as.numeric(gdp_per_capita)
         ) %>% 
  mutate(percent_vaccinated = people_vaccinated/population) %>%
  select(date, location, total_vaccinations, people_vaccinated, percent_vaccinated) %>% 
  drop_na() 

global.clean%>%
  write_csv("covid.csv")

# P2 Data
percent.clean <- global %>% 
  select(location, date, total_cases, 
         new_cases, new_cases_smoothed,
         total_deaths, new_deaths, new_deaths_smoothed,
         people_vaccinated,total_vaccinations, new_vaccinations, 
         people_fully_vaccinated,
         population, gdp_per_capita) %>% 
  # filter(location %in% c("United States", "China", "Canada", "India", "	Germany", "Mexico", "United Kingdom"))%>% 
  filter(location != "World" & location != "Qatar") %>%
  #arrange(date) %>% 
  mutate(date = as.Date(date),
         total_cases = as.numeric(total_cases),
         new_cases =  as.numeric(new_cases),
         new_cases_smoothed =  as.numeric(new_cases_smoothed),
         total_deaths =  as.numeric(total_deaths),
         new_deaths =  as.numeric(new_deaths),
         new_deaths_smoothed =  as.numeric(new_deaths_smoothed),
         people_vaccinated =  as.numeric(people_vaccinated),
         total_vaccinations =  as.numeric(total_vaccinations),
         new_vaccinations =  as.numeric(new_vaccinations),
         people_fully_vaccinated =  as.numeric(people_fully_vaccinated),
         population =  as.numeric(population),
         gdp_per_capita =  as.numeric(gdp_per_capita)
  ) %>% 
  mutate(percent_vaccinated = people_vaccinated/population,
         percent_fully =  people_fully_vaccinated/population) %>%
  select(date, location, people_vaccinated, percent_vaccinated, percent_fully, population,gdp_per_capita,total_cases  ) %>%
  filter(date == as.Date("2021-05-24")) %>% 
  drop_na() 

percent.clean$location

percent.clean%>%
  write_csv("covid_percent.csv")


# P3 Data
setwd("/Users/shuang/Desktop/Spring 2021/PPHA 30560 Data Visualization/HW/Final/p3")
manylines.clean <- global %>% 
  select(location, date, total_cases, 
         new_cases, new_cases_smoothed,
         total_deaths, new_deaths, new_deaths_smoothed,
         people_vaccinated,total_vaccinations, new_vaccinations, 
         people_fully_vaccinated,
         population, gdp_per_capita) %>% 
  filter(location %in% percent.clean$location | location =="World") %>% 
  #filter(location %in% c("United States", "China", "Canada", "India", "	Germany", "Mexico", "United Kingdom"))%>% 
  #filter(location != "World" & location != "Qatar") %>%
  #arrange(date) %>% 
  mutate(date = as.Date(date),
         total_cases = as.numeric(total_cases),
         new_cases =  as.numeric(new_cases),
         new_cases_smoothed =  as.numeric(new_cases_smoothed),
         total_deaths =  as.numeric(total_deaths),
         new_deaths =  as.numeric(new_deaths),
         new_deaths_smoothed =  as.numeric(new_deaths_smoothed),
         people_vaccinated =  as.numeric(people_vaccinated),
         total_vaccinations =  as.numeric(total_vaccinations),
         new_vaccinations =  as.numeric(new_vaccinations),
         people_fully_vaccinated =  as.numeric(people_fully_vaccinated),
         population =  as.numeric(population),
         gdp_per_capita =  as.numeric(gdp_per_capita)
  ) %>% 
  mutate(percent_vaccinated = people_vaccinated/population,
         percent_fully =  people_fully_vaccinated/population) %>%
  select(date, location, percent_vaccinated) %>%
  #filter(date == as.Date("2021-05-24")) %>% 
  drop_na() 

manylines.clean%>%
  write_csv("covid_manylines.csv")


# P4 Data
setwd("/Users/shuang/Desktop/Spring 2021/PPHA 30560 Data Visualization/HW/Final/p4")
pie.clean <- global %>% 
  select(location, date, total_cases, 
         new_cases, new_cases_smoothed,
         total_deaths, new_deaths, new_deaths_smoothed,
         people_vaccinated,total_vaccinations, new_vaccinations, 
         people_fully_vaccinated,
         population, gdp_per_capita) %>% 
  mutate(date=as.Date(date),
         total_cases = as.numeric(total_cases),
         new_cases =  as.numeric(new_cases),
         new_cases_smoothed =  as.numeric(new_cases_smoothed),
         total_deaths =  as.numeric(total_deaths),
         new_deaths =  as.numeric(new_deaths),
         new_deaths_smoothed =  as.numeric(new_deaths_smoothed),
         people_vaccinated =  as.numeric(people_vaccinated),
         total_vaccinations =  as.numeric(total_vaccinations),
         new_vaccinations =  as.numeric(new_vaccinations),
         people_fully_vaccinated =  as.numeric(people_fully_vaccinated),
         population =  as.numeric(population),
         gdp_per_capita =  as.numeric(gdp_per_capita)
  ) %>% 
  mutate(percent_fully =  people_fully_vaccinated/population,
         percent_vaccinated = people_vaccinated/population) %>%
  # mutate(need = 1- percent_fully) %>% 
  mutate(left = percent_vaccinated - percent_fully) %>% 
  select(date, location, percent_fully, left) %>%
  # filter(date == as.Date("2021-05-24")) %>%
  drop_na() 
 
  pie <- gather(pie.clean, condition, percentage, percent_fully:need, factor_key = TRUE)
    
  pie.clean %>%
    # filter(location %in% c("United States", "China", "Canada", "India", "	Germany", "Mexico", "United Kingdom"))%>%
    # filter(location == "World")%>%
    write_csv("covid_pie.csv")

  
  
  # P5 Data
  setwd("/Users/shuang/Desktop/Spring 2021/PPHA 30560 Data Visualization/HW/Final/p5")
  linemul.clean <- global %>% 
    select(location, date, total_cases, 
           new_cases, new_cases_smoothed,
           total_deaths, new_deaths, new_deaths_smoothed,
           people_vaccinated,total_vaccinations, new_vaccinations, total_vaccinations_per_hundred,
           people_fully_vaccinated,
           population, gdp_per_capita) %>% 
    filter(location %in% percent.clean$location | location =="World") %>% 
    #filter(location %in% c("United States", "China", "Canada", "India", "	Germany", "Mexico", "United Kingdom"))%>% 
    #filter(location != "World" & location != "Qatar") %>%
    #arrange(date) %>% 
    mutate(date = as.Date(date),
           total_cases = as.numeric(total_cases),
           new_cases =  as.numeric(new_cases),
           new_cases_smoothed =  as.numeric(new_cases_smoothed),
           total_deaths =  as.numeric(total_deaths),
           new_deaths =  as.numeric(new_deaths),
           new_deaths_smoothed =  as.numeric(new_deaths_smoothed),
           people_vaccinated =  as.numeric(people_vaccinated),
           total_vaccinations =  as.numeric(total_vaccinations),
           new_vaccinations =  as.numeric(new_vaccinations),
           total_vaccinations_per_hundred = as.numeric(total_vaccinations_per_hundred),
           people_fully_vaccinated =  as.numeric(people_fully_vaccinated),
           population =  as.numeric(population),
           gdp_per_capita =  as.numeric(gdp_per_capita)
    ) %>% 
    mutate(percent_vaccinated = people_vaccinated/population,
           percent_fully =  people_fully_vaccinated/population) %>%
    select(date, location, total_vaccinations_per_hundred) %>%
    # filter(location %in% c("United States", "China", "Canada", "India", "	Germany", "Mexico", "United Kingdom")) %>% 
    #filter(date == as.Date("2021-05-24")) %>% 
    drop_na() 
  
  linemul.clean%>%
    write_csv("covid_multilines.csv")

  unique(linemul.clean$location)
  
  
  # P1 Data
  setwd("/Users/shuang/Desktop/Spring 2021/PPHA 30560 Data Visualization/HW/Final/p6")
  global.clean <- global %>% 
    select(location, date, total_cases, 
           new_cases, new_cases_smoothed,
           total_deaths, new_deaths, new_deaths_smoothed,
           people_vaccinated,total_vaccinations, new_vaccinations, 
           population,gdp_per_capita) %>% 
    # subset(location == "World") %>% 
    arrange(date) %>% 
    mutate(date = as.Date(date),
           total_cases = as.numeric(total_cases),
           new_cases =  as.numeric(new_cases),
           new_cases_smoothed =  as.numeric(new_cases_smoothed),
           total_deaths =  as.numeric(total_deaths),
           new_deaths =  as.numeric(new_deaths),
           new_deaths_smoothed =  as.numeric(new_deaths_smoothed),
           people_vaccinated =  as.numeric(people_vaccinated),
           total_vaccinations =  as.numeric(total_vaccinations),
           new_vaccinations =  as.numeric(new_vaccinations),
           population =  as.numeric(population),
           gdp_per_capita =  as.numeric(gdp_per_capita)
    ) %>% 
    mutate(percent_vaccinated = people_vaccinated/population) %>%
    select(date, location, total_vaccinations, people_vaccinated, percent_vaccinated) %>% 
    drop_na() 
  
  global.clean%>%
    filter(date == as.Date("2021-05-26")) %>%
    write_csv("map.csv")
  