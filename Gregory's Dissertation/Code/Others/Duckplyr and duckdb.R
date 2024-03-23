library(duckdb)
library(duckplyr)
library(here)
library(tidyverse)

# OPTION 1- duckdb
con <- dbConnect(duckdb())

duckdb_read_csv(conn = con,
                files = here("./Data/Cleaned data period 2.csv"),
                name = "period_1")



# data1 <- dbReadTable(con, "period_1")

dbGetQuery(con, "SELECT COUNT(Parity) AS 'multipara_count' FROM period_1 WHERE Parity >= 1")

dbDisconnect(con)
rm(con)


# OPTION 2- duckplyr
data1 <- duckplyr_df_from_file(here("./Data/Cleaned data period 2.csv"),
                               "read_csv_auto")

data1 <- data1 %>% janitor::clean_names()

object.size(data1) %>% stringr::str_extract("\\d*") %>% as.numeric()/2^10 #in KB

colnames(data1)

data1 %>% distinct(miscarriage)
