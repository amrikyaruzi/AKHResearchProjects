#purrr::walk(c("Code", "Data", "Output"), dir.create)

library(tidyverse)
library(here)
library(readxl)
library(janitor)

path <- here("./Data/DATA.xlsx")
sheets <- path %>% excel_sheets()


walk(.x = sheets,
     .f = ~{path %>% read_excel(sheet = .x) %>%
         assign(value = .,
                x = gsub(x = tolower(.x),
                                pattern = " |-",
                                replacement = "_"),
                envir = .GlobalEnv)})



# Function to clean all required dataframes

clean_data <- function(data){
  
  #browser()
  
  indicator = as.character(substitute(data))
  #indicator = indicator
 
  
  data <- data %>% row_to_names(row_number = 1) %>% clean_names()
  
  data <- data %>%
    select(name_of_facilities:december, january_2:december_2, january_3:december_3)
  
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


monthly_number_of_delivery <- clean_data(monthly_number_of_delivery)
monthly_fresh_stillbirth <- clean_data(monthly_fresh_stillbirth)
monthly_macerrated_stillbirth <- clean_data(monthly_macerrated_stillbirth)
monthly_maternal_death <- clean_data(monthly_maternal_death)
monthly_neonatal_death <- clean_data(monthly_neonatal_death)

# To take a look at the wide data format
# monthly_fresh_stillbirth %>% pivot_wider(names_from = month, values_from = frequency)


rm(list = setdiff(ls(), c("monthly_number_of_delivery", "monthly_fresh_stillbirth", "monthly_macerrated_stillbirth",
                          "monthly_maternal_death", "monthly_neonatal_death")))

data_ <- bind_rows(monthly_number_of_delivery, monthly_fresh_stillbirth, monthly_macerrated_stillbirth,
                  monthly_maternal_death, monthly_neonatal_death)




