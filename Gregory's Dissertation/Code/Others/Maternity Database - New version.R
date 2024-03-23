rm(list = ls())

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(readxl)
library(glue)
library(gt)
library(openxlsx)


what_months <- c("July", "August", "September") #"July", "August", "September"
what_year <- c(2023)


data <- list.files(here("./Data/New database/"), pattern = ".csv$", full.names = TRUE) %>%
  map_df(.x = ., .f = ~read.csv(colClasses = "character", file = .x))


dates <- list.files(here("./Data/New database/For Dates of Delivery"), pattern = ".xlsx$", full.names = TRUE) %>%
  read_excel()

dates <- dates %>% clean_names()
dates <- dates %>% distinct(across(.cols = "mr_no"), .keep_all = TRUE) %>%
  filter(!is.na(date_of_delivery)) %>%
  mutate(date_of_delivery = janitor::excel_numeric_to_date(date_of_delivery))


# Actual wrangling
data1 <- data %>% janitor::clean_names() #data <- data %>% janitor::clean_names()

data1 <- data1 %>% left_join(dates[,c("mr_no", "date_of_delivery")], by = c("mr_no" = "mr_no")) %>%
  relocate(date_of_delivery.y, .after = date_of_delivery.x) %>% select(-any_of(c("date_of_delivery.x", "column1")),
                                                                       "date_of_delivery" = "date_of_delivery.y")
rm(dates)


data1 <- data1 %>% mutate(mr_no = str_squish(mr_no)) %>%
  filter(!str_detect(patient_name, "BABY OF")) %>%
  distinct(across(.cols = "mr_no"), .keep_all = TRUE)

data1 <- data1 %>% mutate(
  labour_duration = case_when(labour_duration == "N/A" ~ NA_character_,
                              TRUE ~ labour_duration
                               ),
  
  labour_duration = str_replace(string = labour_duration,
                                pattern = "(\\d+).+",
                                replacement = "\\1"
                                ))


data1 <- data1 %>% mutate(across(c(age:number_of_foetuses, number_of_anc_visits, no_of_previous_csection,
                                   labour_duration, efw_by_last_uss_in_grams:baby_apgar_score_5th_minute),
                                 as.numeric))
  
data1 <- data1 %>% mutate(month_of_delivery = month(date_of_delivery, label = TRUE, abbr = FALSE),
                          year_of_delivery = year(date_of_delivery)) %>%
  
  relocate(month_of_delivery, year_of_delivery, .after = date_of_delivery)



data1 <- data1 %>% filter((month_of_delivery %in% what_months) & (year_of_delivery %in% what_year))



data1 <- data1 %>%
  mutate(across(.cols = everything(),
                .fns = str_squish)) %>%
  mutate(across(c(admission_no, age:number_of_foetuses), as.numeric)) %>%
  mutate(cs_surgeon = case_when(
    mr_no == "050-41-66" & cs_surgeon == "Jaffer. Ali" ~ "Jaiswal, Shweta",
    mr_no == "166-33-95" & cs_surgeon == "" ~ "Bakari, Rahma",
    TRUE ~ cs_surgeon
  )) %>% 
  mutate_if(is.character, str_to_lower)


theyear <- unique(data1$year_of_delivery)
thequarter <- unique(paste0("Q", quarter(data1$date_of_delivery)))

walk(c(glue(here("Output", {theyear})), glue(here("Output", {theyear}, {thequarter}))),
     ~dir.create(path = ., showWarnings = FALSE))





total_entered_per_month <- data1 %>% group_by(year_of_delivery, month_of_delivery) %>%
  summarise(Total_Frequency = n())

## Duplicates
### Duplicated rows in the entire data1 dataframe
duplicates <- data1 %>% group_by(mr_no, month_of_delivery, year_of_delivery) %>%
  mutate(duplicate = n() > 1) %>% filter(duplicate == TRUE) %>% arrange(mr_no) %>% select(-duplicate)

### Number of duplicates per month
duplicates_per_month <- duplicates %>% group_by(year_of_delivery, month_of_delivery) %>%
  summarise(Duplicates_Frequency = n())

unique_values_per_variable <- data1 %>%
  select(parity, no_of_previous_csection, onset_of_labour, number_of_foetuses, ga_weeks, presentation, lie) %>%
  map_df(.f = ~str_c(sort(unique(.x)), collapse = ", ")) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "unique_entries")


duplication_summary <- total_entered_per_month %>% inner_join(duplicates_per_month) %>%
  inner_join({data1 %>% distinct(mr_no, month_of_delivery, year_of_delivery, .keep_all = TRUE) %>%
      group_by(year_of_delivery, month_of_delivery) %>% summarise("Passed for analysis" = n())}) %>%
  mutate(non_duplicates = Total_Frequency - Duplicates_Frequency) %>%
  rename(Month = month_of_delivery, Year = year_of_delivery, Duplicates = Duplicates_Frequency,
         Non_Duplicates = non_duplicates) %>%
  
  relocate(Non_Duplicates, .before = "Passed for analysis")



list_datasets <- list("Summary" = duplication_summary,
                      "Duplicates" = duplicates,
                      "Unique entries" = data1 %>% distinct(mr_no, month_of_delivery, year_of_delivery,
                                                            .keep_all = TRUE),
                      "Unique Entries per Variable" = unique_values_per_variable)

# Exporting all of the above to an Excel worksheet in the Output folder (put in the sub folder for the year & quarter)
write.xlsx(list_datasets, here(glue("./Output/{theyear}/{thequarter}/Summary - Entered vs Unique.xlsx")))

# The line below will clean the global environment, keeping only data and data1 dataframes
rm(list = setdiff(ls(), c("data", "data1", "thequarter", "theyear")))

# Without any duplicates
data1 <- data1 %>% distinct(mr_no, .keep_all = TRUE)

# Putting each delivery into one of the 10 Robson groups based on the entered data (core variables)
# Month

#data1 <- data1 %>% filter(month_of_delivery == 6)

data1 <- data1 %>% mutate(onset_of_labour = case_when(
            onset_of_labour == "induction of labour" ~ "induced",
            onset_of_labour == "no labour (prelabor c/s)" ~ "no labour (pre labor c/s)",
            TRUE ~ onset_of_labour)) %>%
  
  mutate(group = case_when(
  parity == 0 & number_of_foetuses == 1 & presentation == "cephalic" & ga_weeks >= 37 &
    onset_of_labour == "spontaneous" ~ "1", #Group 1
  
  parity == 0 & number_of_foetuses == 1 & presentation == "cephalic" & ga_weeks >= 37 &
    (onset_of_labour == "induced" | onset_of_labour == "no labour (pre labor c/s)") ~ "2", #Group 2
  
  parity >= 1 & no_of_previous_csection == 0 & number_of_foetuses == 1 & presentation == "cephalic" & ga_weeks >= 37 &
    onset_of_labour == "spontaneous" ~ "3", #Group 3
  
  parity >= 1 & no_of_previous_csection == 0 & number_of_foetuses == 1 & presentation == "cephalic" & ga_weeks >= 37 &
    (onset_of_labour == "induced" | onset_of_labour == "no labour (pre labor c/s)") ~ "4", #Group 4
  
  parity >= 1 & no_of_previous_csection >= 1 & number_of_foetuses == 1 & presentation == "cephalic" &
    ga_weeks >= 37 ~ "5", #Group 5
  
  parity == 0 & number_of_foetuses == 1 & presentation == "breech" ~ "6", #Group 6
  
  parity >= 1 & number_of_foetuses == 1 & presentation == "breech" ~ "7", #Group 7
  
  number_of_foetuses > 1 ~ "8", #Group 8
  
  number_of_foetuses == 1 & (lie == "oblique" | lie == "transverse") ~ "9", #Group 9
  
  number_of_foetuses == 1 & presentation == "cephalic" & ga_weeks < 37 ~ "10", #Group 10
  
  TRUE ~ "Unclassifiable"
))



# In case of unclassifiables
data1 %>% filter(group == "Unclassifiable") %>%
  select(mr_no, admission_no, month_of_delivery, parity, number_of_foetuses, presentation, ga_weeks, onset_of_labour, group)


# In case of unclassifiables

data1 %>% filter(group == "Unclassifiable") %>%
  select(mr_no, admission_no, month_of_delivery, parity, no_of_previous_csection, number_of_foetuses, lie, presentation,
         ga_weeks, onset_of_labour, group) %>% openxlsx::write.xlsx("July - September 2023 unclassifiables.xlsx")

data1 %>% filter(group == "1" | group == "2") %>%
  select(mr_no, admission_no, month_of_delivery, parity, no_of_previous_csection, number_of_foetuses, lie, presentation,
         ga_weeks, onset_of_labour, group) %>% openxlsx::write.xlsx("July - September 2023 Group 1 & 2.xlsx")



data1 %>% filter(group == "Unclassifiable") %>%
  select(parity, no_of_previous_csection, onset_of_labour, number_of_foetuses, ga_weeks, presentation, lie) %>%
  map_df(.f = ~str_c(sort(unique(.x)), collapse = ", ")) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "unique_entries")


## Wangling continues
data1 <- data1 %>% mutate(group = factor(group,
                                         levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",  "Unclassifiable")))


data1 <- data1 %>% mutate(group_description = case_when(
  
  group == "1" ~ "Nulliparous women with a single cephalic pregnancy, ≥37 weeks gestation in spontaneous labour",
  
  group == "2" ~ "Nulliparous women with a single cephalic pregnancy, ≥37 weeks gestation who had labour induced or were delivered by CS before labour",
  
  group == "3" ~ "Multiparous women without a previous CS, with a single cephalic pregnancy, ≥37 weeks gestation in spontaneous labour",
  
  group == "4" ~ "Multiparous women without a previous CS, with a single cephalic pregnancy, ≥37 weeks gestation who had labour induced or were delivered by CS before labour",
  
  group == "5" ~ "All multiparous women with at least one previous CS, with a single cephalic pregnancy, ≥37 weeks gestation",
  
  group == "6" ~ "All nulliparous women with a single breech pregnancy",
  
  group == "7" ~ "All multiparous women with a single breech pregnancy including women with previous CS(s)",
  
  group == "8" ~ "All women with multiple pregnancies including women with previous CS(s)",
  
  group == "9" ~ "All women with a single pregnancy with a transverse or oblique lie, including women with previous CS(s)",
  
  group == "10" ~ "All women with a single cephalic pregnancy < 37 weeks gestation, including women with previous CS(s)",
  
  group == "Unclassifiable" ~ "Unclassifiable"
  
))


data1 <- data1 %>% mutate(mode_of_delivery = factor(mode_of_delivery),
                          mode_of_delivery_refactored = fct_collapse(mode_of_delivery,
                                                                     "Caesarean section" = c("emergency c / s",
                                                                                             "elective c / s",
                                                                                             "elective c/s",
                                                                                             "emergency c/s"),
                                                                     "Spontaneous Vertex Delivery" = "svd"))

data1 <- data1 %>% relocate(mode_of_delivery_refactored, .after = mode_of_delivery)



## Creating the backbone for the Robson classification summary table

group_size <- data1 %>% group_by(group, group_description) %>% #refactored emergency & elective cs as one
  summarise(Total = n()) %>% ungroup()

group_cs <- data1 %>% filter(mode_of_delivery_refactored == "Caesarean section") %>% group_by(group, group_description) %>% summarise(CS_no = n()) %>% ungroup()

table <- right_join(x = group_cs, y = group_size) %>% mutate(CS_no = replace_na(CS_no, 0)) #Important line. Helps with ensuring NA's (meaning no CSs in group) are replaced with 0's

table <- table %>% arrange(group) #since the arrangement will be disturbed if any of the groups has 0,s in CS 
rm(group_cs, group_size)


total_cs <- sum(table$CS_no)
total_patients <- sum(table$Total)


table1 <- table %>% mutate(group_size_perc = Total*100/total_patients,
                           group_cs_rate_perc = CS_no*100/Total,
                           absol_contribution_perc = CS_no*100/total_patients,
                           relat_contribution_perc = CS_no*100/total_cs) %>% 
  mutate(across(where(is.numeric), ~ifelse(.x != 100, round_half_up(.x, digits = 1), .))) %>%
  mutate(across(where(is.numeric), ~gsub(pattern = 100.00, replacement = 100, x = .x)))



# Further manipulation of the table continue

table2 <- table1 %>% mutate(robson_guidelines = case_when(
  
  group == "1" ~ "<10%",
  group == "2" ~ "20-35%",
  group == "3" ~ "No higher than 3%",
  group == "4" ~ "Rarely >15%",
  group == "5" ~ "50-60%",
  (group == "6" | group == "7") ~ "80-100%",
  group == "8" ~ "60%",
  group == "9" ~ "100%",
  group == "10" ~ "Around 30%",
  group == "Unclassifiable" ~ "-",
  TRUE ~ ""
  
)) %>% relocate(robson_guidelines, .after = group_cs_rate_perc)


# Reporting period for the data (months)
data1 <- data1 %>% mutate(date_of_delivery = ymd(date_of_delivery))
start_date <- format(unique(min(data1$date_of_delivery)), "%B %Y")
end_date <- (format(unique(max(data1$date_of_delivery)), "%B %Y"))



# Creating the gt formatted table (neat, beautiful looking!)

subtitle <- if_else(start_date != end_date, gt::html(glue::glue("<i>Data from {start_date} to {end_date}</i>")), gt::html(glue::glue("<i>Data for {start_date}</i>")))

graph_table <- table2 %>%
  
  gt() %>%
  
  tab_header(title = md("**AKH Dar es Salaam Caesarean Sections Classified according to the WHO Robson Classification System**"),
             subtitle = subtitle) %>%
  tab_footnote(footnote = paste("Total number of CS = ", as.character(total_cs)),
               locations = cells_column_labels(columns = CS_no)) %>%
  
  tab_footnote(footnote = paste("Total number of deliveries = ", as.character(total_patients)),
               locations = cells_column_labels(columns = Total)) %>%
  
  tab_footnote(footnote = "Group size (%) = n of women in the group / total N women delivered in the hospital x 100",
               locations = cells_column_labels(columns = group_size_perc)) %>%
  
  tab_footnote(footnote = "Group CS rate (%) = n of CS in the group / total N of women in the group x 100",
               locations = cells_column_labels(columns = group_cs_rate_perc)) %>%
  
  tab_footnote(footnote = "Absolute contribution (%) = n of CS in the group / total N of women delivered in the hospital x 100",
               locations = cells_column_labels(columns = absol_contribution_perc)) %>%
  
  tab_footnote(footnote = "Relative contribution (%) = n of CS in the group / total N of CS in the hospital x 100",
               locations = cells_column_labels(columns = relat_contribution_perc)) %>%
  cols_align(align = "center",
             columns = everything()) %>% 
  
  cols_align(align = "left",
             columns = c("group_description")) %>% #was center
  
  tab_footnote(footnote = gt::html("<i>Groups for which no data is displayed on the table (if any) had 0 deliveries falling under them</i><br><br>"), placement = "right") %>%
  
  cols_label(group = "Group",
             group_description = "Description",
             CS_no = "Number of CS in group",
             Total = "Number of women in group",
             group_size_perc = "Group Size (%)",
             group_cs_rate_perc = "Group CS rate (%)",
             robson_guidelines = "WHO (Robson) recommended CS rate",
             absol_contribution_perc = "Absolute group contribution to overall CS rate (%)",
             relat_contribution_perc = "Relative contribution of group to overall CS rate (%)")




gtsave(data = graph_table, here(glue("./Output/{theyear}/{thequarter}/{thequarter} {theyear} Robson Classification Table.html")))



## Reasons for operative delivery for group 1 & 2
## RCG Report Explanations

caesarean_deliveries <- data1 %>% filter(mode_of_delivery != "svd")

caesarean_deliveries %>%
  filter(group %in% c(1, 2)) %>%
  separate_longer_delim(indication_of_operative_delivery, delim = "|") %>%
  mutate(indication_of_operative_delivery = str_squish(indication_of_operative_delivery)) %>% 
  filter(indication_of_operative_delivery != "") %>%
  select(indication_of_operative_delivery, mr_no, cs_surgeon) %>%
  mutate(reason = case_when(
    
    indication_of_operative_delivery %in% c("none", "not applicable", "no", "na", "0", "n/a") ~ "not applicable",
    grepl(x = indication_of_operative_delivery, pattern = "request") ~ "maternal request",
    grepl(x = indication_of_operative_delivery, pattern = "induction") ~ "Failed induction",
    grepl(x = indication_of_operative_delivery, pattern = "nrfs|nrfs |fetal distress|reasuaring|reasuaring|status|reduced fetal movements|intrauterine hypoxia") ~ "fetal distress",
    
    grepl(x = indication_of_operative_delivery, pattern = "previous.*scar|scar|scars") ~ "uterine scar",
    grepl(x = indication_of_operative_delivery, pattern = "poor.*labo.*r|prolonged|obstructed|obstructes|abnormalities of forces of labour") ~ "poor progress of labour",
    grepl(x = indication_of_operative_delivery, pattern = "breech|breeech") ~ "Breech presentation",
    grepl(x = indication_of_operative_delivery, pattern = "twin|triplet") ~ "multiple gestation",
    grepl(x = indication_of_operative_delivery, pattern = "pelvi|big baby|disproportion") ~ "CPD",
    
    grepl(x = indication_of_operative_delivery, pattern = "bad") ~ "BOH",
    grepl(x = indication_of_operative_delivery, pattern = "oligohyndramnios|oligohydromnious") ~ "Oligohydramnios",
    grepl(x = indication_of_operative_delivery, pattern = "premature rupture of membranes") ~ "PROM",
    grepl(x = indication_of_operative_delivery, pattern = "labour and delivery complicated|maternal care for signs of fetal hypoxia") ~ "fetal distress",
    grepl(x = indication_of_operative_delivery, pattern = "labour and delivery complicated by fetal stress [distress] - [o68]") ~ "fetal distress",
    grepl(x = indication_of_operative_delivery, pattern = "pre-eclampsia") ~ "Pre-eclampsia",
    grepl(x = indication_of_operative_delivery, pattern = "severe pre-eclampsia - [o14.1]") ~ "Pre-eclampsia",
    
    grepl(x = indication_of_operative_delivery, pattern = "long labour|poor progress") ~ "poor progress of labour",
    grepl(x = indication_of_operative_delivery, pattern = "cardiomyopathy|mitral") ~ "cardiomyopathy",
    grepl(x = indication_of_operative_delivery, pattern = "previous 3rd degree tear|previous third degree perineal tear") ~ "previous perineal tear",
    # grepl(x = indication_of_operative_delivery, pattern = "") ~ "",
    # grepl(x = indication_of_operative_delivery, pattern = "") ~ "",
    
    TRUE ~ indication_of_operative_delivery
    
  )) %>%
  mutate(reason = case_when(
    str_detect(string = reason, pattern = "affected by complication") &
      mr_no == "166-25-18" ~ "fetal distress",
    str_detect(string = reason, pattern = "fetal abnormality and damage") &
      mr_no == "140-49-26" ~ "fetal distress",
    str_detect(indication_of_operative_delivery, "maternal complications of pregnancy") &
      mr_no == "166-14-21" ~ "fetal distress",
    TRUE ~ reason
  )) %>%
  select(cs_surgeon, reason) %>% 
  group_by(cs_surgeon, reason) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count),
         Percentage = Count*100/Total) %>%
  mutate_if(is.numeric, .funs = ~janitor::round_half_up(.x, digits = 1)) %>% 
  arrange(cs_surgeon, desc(Percentage)) %>% #knitr::kable()
  rename("Reason" = "reason") %>%
  mutate(cs_surgeon = case_when(!str_detect(cs_surgeon, "dr.") ~ str_replace_all(cs_surgeon,
                                                                                 pattern = "(\\w+).\\s(\\w+)",
                                                                                 replacement = "dr. \\2 \\1"),
                                TRUE ~ cs_surgeon),
         cs_surgeon = str_to_title(cs_surgeon)) %>%

  group_by(cs_surgeon) %>% arrange(desc(Percentage)) %>% nest() %>%
  rename(nested_data = data) %>%
  
  pwalk(
    .f = function(cs_surgeon, nested_data) {
      path <- file.path(glue(here("./Output/{theyear}/{thequarter}/By Consultant/Reasons")), 
                        "All_Surgeons.xlsx")
      if (!file.exists(path)) {
        wb <- createWorkbook()
      } else {
        wb <- loadWorkbook(path)
      }
      addWorksheet(wb, sheetName = cs_surgeon)
      
      # Set styles for the header
      headerStyle <- createStyle(
        textDecoration = "BOLD", fontColour = "#000000", fontSize = 12,
        fontName = "Arial Narrow", fgFill = "#9BBB59"
      )
      
      # Write data to the new sheet with styles
      writeData(wb, sheet = cs_surgeon, x = nested_data, headerStyle = headerStyle)
      
      # Set column widths to "auto"
      setColWidths(wb, sheet = cs_surgeon, cols = 1:ncol(nested_data), widths = "auto")
      
      # Save the Excel workbook
      saveWorkbook(wb, file = path, overwrite = TRUE)
      
    }
  )



### Per Consultant
# Tables per Consultant will be created based CS Surgeon, not by Primary Consultant

## Creating a folder for Consultants' tables
walk(.x = c(
  glue(here("Output", {theyear}, {thequarter}, "By Consultant")),
  glue(here("Output", {theyear}, {thequarter}, "By Consultant", "Reasons")) 
), .f = ~dir.create(.x, showWarnings = FALSE))


# glue(here("Output", {theyear}, {thequarter}, "By Consultant")) %>% dir.create(path = ., showWarnings = FALSE)
# glue(here("Output", {theyear}, {thequarter}, "By Consultant", "Reasons")) %>% dir.create(path = ., showWarnings = FALSE)

consultants <- unique(data1$cs_surgeon)

## Function to create tables per Consultant
table_per_surgeon <- function(consultant){
  
  #filtering data
  
  data2 <- data1 %>% filter(cs_surgeon == consultant)
  
  consultant <- str_to_title(consultant) %>% gsub(x = ., pattern = "Dr", replacement = "Dr\\.")
  
  ## Creating the backbone for the Robson classification summary table
  
  group_size <- data2 %>% group_by(group, group_description) %>% #refactored emergency & elective cs as one
    summarise(Total = n()) %>% ungroup()
  
  group_cs <- data2 %>% filter(mode_of_delivery_refactored == "Caesarean section") %>% group_by(group, group_description) %>% summarise(CS_no = n()) %>% ungroup()
  
  table <- right_join(x = group_cs, y = group_size) %>% mutate(CS_no = replace_na(CS_no, 0))
  rm(group_cs, group_size)
  
  
  # Data for total number of Caesarean sections in the reporting period overall and the total number of deliveries (CS + SVD)
  total_cs <- sum(table$CS_no)
  total_patients <- sum(table$Total)
  
  
  # Further manipulation of the Robson classification table
  
  table1 <- table %>% mutate(group_size_perc = Total*100/total_patients,
                             group_cs_rate_perc = CS_no*100/Total,
                             absol_contribution_perc = CS_no*100/total_patients,
                             relat_contribution_perc = CS_no*100/total_cs) %>% 
    mutate(across(where(is.numeric), ~ifelse(.x != 100, round_half_up(.x, digits = 1), .))) %>%
    mutate(across(where(is.numeric), ~gsub(pattern = 100.00, replacement = 100, x = .x)))
  
  
  # Further manipulation of the table continue
  
  table2 <- table1 %>% mutate(robson_guidelines = case_when(
    
    group == "1" ~ "<10%",
    group == "2" ~ "20-35%",
    group == "3" ~ "No higher than 3%",
    group == "4" ~ "Rarely >15%",
    group == "5" ~ "50-60%",
    (group == "6" | group == "7") ~ "80-100%",
    group == "8" ~ "60%",
    group == "9" ~ "100%",
    group == "10" ~ "Around 30%",
    group == "Unclassifiable" ~ "-",
    TRUE ~ ""
    
  )) %>% relocate(robson_guidelines, .after = group_cs_rate_perc)
  
  
  # Creating the gt formatted table (neat, beautiful looking!)
  
  graph_table <- table2 %>%
    
    gt() %>%
    
    tab_header(title = gt::html(glue::glue("<b>{consultant}'s Caesarean Sections Classified according to the WHO Robson Classification System</b>")),
               subtitle = gt::html(glue::glue("<i>Data from {start_date} to {end_date}</i>"))) %>%
    tab_footnote(footnote = paste("Total number of CS = ", as.character(total_cs)),
                 locations = cells_column_labels(columns = CS_no)) %>%
    
    tab_footnote(footnote = paste("Total number of deliveries = ", as.character(total_patients)),
                 locations = cells_column_labels(columns = Total)) %>%
    
    tab_footnote(footnote = "Group size (%) = n of women in the group / total N women delivered in the hospital x 100",
                 locations = cells_column_labels(columns = group_size_perc)) %>%
    
    tab_footnote(footnote = "Group CS rate (%) = n of CS in the group / total N of women in the group x 100",
                 locations = cells_column_labels(columns = group_cs_rate_perc)) %>%
    
    tab_footnote(footnote = "Absolute contribution (%) = n of CS in the group / total N of women delivered in the hospital x 100",
                 locations = cells_column_labels(columns = absol_contribution_perc)) %>%
    
    tab_footnote(footnote = "Relative contribution (%) = n of CS in the group / total N of CS in the hospital x 100",
                 locations = cells_column_labels(columns = relat_contribution_perc)) %>%
    cols_align(align = "center",
               columns = everything()) %>% 
    
    cols_align(align = "left",
               columns = c("group_description")) %>% #was center
    
    tab_footnote(footnote = gt::html("<i>Groups for which no data is displayed on the table (if any) had 0 deliveries falling under them</i><br><br>"), placement = "right") %>%
    
    cols_label(group = "Group",
               group_description = "Description",
               CS_no = "Number of CS in group",
               Total = "Number of women in group",
               group_size_perc = "Group Size (%)",
               group_cs_rate_perc = "Group CS rate (%)",
               robson_guidelines = "WHO (Robson) recommended CS rate",
               absol_contribution_perc = "Absolute group contribution to overall CS rate (%)",
               relat_contribution_perc = "Relative contribution of group to overall CS rate (%)")
  
  
  
  # Saving the table
  # The table will be saved to the output folder, in th sub folder for the particular year and quarter
  
  
  gtsave(data = graph_table, here(glue("./Output/{theyear}/{thequarter}/By Consultant/{consultant} - Robson Classification Table.html")))
  
}

walk(.x = consultants, .f = ~table_per_surgeon(consultant = .))


