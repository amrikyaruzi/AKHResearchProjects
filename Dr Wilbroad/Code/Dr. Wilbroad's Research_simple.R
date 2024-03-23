#purrr::walk(c("Code", "Data", "Output"), dir.create)

rm(list = ls())

library(tidyverse)
library(here)
library(readxl)
library(janitor)

path <- here("./Data/NEW.xlsx")
sheets <- path %>% excel_sheets()


walk(.x = sheets,
     .f = ~{path %>% read_excel(sheet = .x) %>%
         assign(value = .,
                x = gsub(x = tolower(.x),
                         pattern = " |-",
                         replacement = "_"),
                envir = .GlobalEnv)})



# Function to clean all required dataframes

clean_data <- function(data, indicator){
  
  data <- data %>%
    row_to_names(row_number = 1) %>% clean_names() %>%
    select(-(any_of(c("total", "na", "name_of_facilities_2", "total_2", "na_2", "total_3"))))
  
  data <- data %>%
    filter(!is.na(name_of_facilities) & name_of_facilities != "Total")
  
  data <- data %>%
    pivot_longer(cols = !name_of_facilities, names_to = "month", values_to = "frequency")
  
  
  data <- data %>%
    mutate(year = case_when(
      
      month %in% str_to_lower(month.name) ~ 2019,
      str_detect(month, "_2") ~ 2020,
      str_detect(month, "_3") ~ 2021))
  
  
  data <- data %>%
    mutate(month = str_replace_all(month, "_\\d", ""))
  
  
  data <- data %>%
    mutate(month = str_replace_all(month, "_\\d", ""))
  
  
  data <- data %>%
    mutate(indicator = indicator) %>%
    select(name_of_facilities, month, year, indicator, frequency)
  
  return(data)
  
}


dfs <- list(fresh_stillbirths_monthly, live_birth, macerated_stillbirth_monthly, maternal_death_monthly,
            neonatal_death_monthly, number_deliveries_monthly, outcome_data_facility, vaccum_delivery_monthly)

names <- c("fresh_stillbirths_monthly", "live_birth", "macerated_stillbirth_monthly", "maternal_death_monthly",
           "neonatal_death_monthly", "number_deliveries_monthly", "outcome_data_facility", "vaccum_delivery_monthly")

data <- map2_df(.x = dfs,
                .y = names, clean_data)


# To take a look at the wide data format
# monthly_fresh_stillbirth %>% pivot_wider(names_from = month, values_from = frequency)


rm(list = setdiff(ls(), "data"))


# Further wrangling of the combined dataframe

data <- data %>% mutate(frequency = as.integer(frequency))

data1 <- data %>% group_by(year, month, indicator) %>% summarise(total = sum(frequency))

data2 <- data1 %>% mutate(
  
  indicator_refactored = case_when(
    indicator %in% c("monthly_fresh_stillbirth", "monthly_macerrated_stillbirth") ~ "monthly stillbirths",
    str_detect(indicator, "death") ~ str_replace(indicator, "death", "mortality"),
    TRUE ~ indicator),
  
indicator_refactored = str_replace_all(indicator_refactored, "_", " "))

data2 <- data2 %>% group_by(year, month, indicator_refactored) %>% summarise(total = sum(total))

data1 %>% mutate(metric = case_when(
  
  
  
))