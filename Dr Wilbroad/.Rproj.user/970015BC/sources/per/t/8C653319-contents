#purrr::walk(c("Code", "Data", "Output"), dir.create)

rm(list = ls())

library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(extrafont)
library(glue)
library(geomtextpath)
library(camcorder)
library(patchwork)
library(furrr)


plan(multisession(workers = length(availableWorkers())))

# Loading 2020-2022 data

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
      
      month %in% str_to_lower(month.name) ~ 2020,
      str_detect(month, "_2") ~ 2021))
  
  
  data <- data %>%
    mutate(month = str_replace_all(month, "_\\d", ""))
  
  
  data <- data %>%
    mutate(indicator = indicator) %>%
    select(name_of_facilities, month, year, indicator, frequency)
  
  return(data)
  
}


dfs <- list(fresh_stillbirths_monthly, live_birth, macerated_stillbirth_monthly, maternal_death_monthly,
            neonatal_death_monthly, number_deliveries_monthly, vaccum_delivery_monthly)

names <- c("fresh_stillbirths_monthly", "live_birth", "macerated_stillbirth_monthly", "maternal_death_monthly",
           "neonatal_death_monthly", "number_deliveries_monthly", "vaccum_delivery_monthly")

data <- map2_df(.x = dfs,
                .y = names, clean_data)


# To take a look at the wide data format
# monthly_fresh_stillbirth %>% pivot_wider(names_from = month, values_from = frequency)

# Loading 2019 data
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

clean_data_ <- function(data, indicator){
  
  data <- data %>%
    row_to_names(row_number = 1) %>% clean_names() %>%
    select(-(any_of(c("total", "na", "total_1", "na_1", "name_of_facilities_1",
                      "name_of_facilities_2", "total_2", "na_2", "total_3"))))
  
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
    mutate(indicator = indicator) %>%
    select(name_of_facilities, month, year, indicator, frequency)
  
  return(data)
  
}


dfs <- list(monthly_fresh_stillbirth, monthly_number_live_birth, monthly_macerrated_stillbirth, monthly_maternal_death,
            monthly_neonatal_death, monthly_number_of_delivery, monthly_vaccum_delivery)

names <- c("monthly_fresh_stillbirth", "monthly_number_live_birth", "monthly_macerrated_stillbirth", "monthly_maternal_death",
           "monthly_neonatal_death", "monthly_number_of_delivery", "monthly_vaccum_delivery")

data_ <- map2_df(.x = dfs,
                 .y = names, clean_data_)


# To take a look at the wide data format
# monthly_fresh_stillbirth %>% pivot_wider(names_from = month, values_from = frequency)

rm(list = setdiff(ls(), c("data", "data_")))

# Wrangling data_ further
data_ <- data_ %>% filter(year == 2019)

data_ <- data_ %>% mutate(
  indicator = case_match(
    .x = indicator,
    .default = indicator,
    
    "monthly_fresh_stillbirth" ~ "fresh_stillbirths_monthly",
    "monthly_number_live_birth" ~ "live_birth",
    "monthly_macerrated_stillbirth" ~ "macerated_stillbirth_monthly",
    "monthly_maternal_death" ~ "maternal_death_monthly",
    "monthly_neonatal_death" ~ "neonatal_death_monthly",
    "monthly_number_of_delivery" ~ "number_deliveries_monthly",
    "monthly_vaccum_delivery" ~ "vaccum_delivery_monthly"
    
  )
)

data <- data %>% bind_rows(data_)
rm(data_)

# Further wrangling of the combined dataframe

data <- data %>% mutate(frequency = as.integer(frequency))

data1 <- data %>% group_by(year, month, indicator) %>% summarise(total = sum(frequency))

data2 <- data1 %>% mutate(
  
  indicator_refactored = case_when(
    indicator %in% c("fresh_stillbirths_monthly", "macerated_stillbirth_monthly") ~ "monthly stillbirths",
    str_detect(indicator, "death") ~ str_replace(indicator, "death", "mortality"),
    TRUE ~ indicator),
  
  indicator_refactored = str_replace_all(indicator_refactored, "_", " "))

data2 <- data2 %>% group_by(year, month, indicator_refactored) %>% summarise(total = sum(total))

data2 <- data2 %>% mutate(quarter = case_when(
  month %in% tolower(month.name)[1:3] ~ "Q1",
  month %in% tolower(month.name)[4:6] ~ "Q2",
  month %in% tolower(month.name)[7:9] ~ "Q3",
  month %in% tolower(month.name)[10:12] ~ "Q4"
)) %>%
  relocate(quarter, .after = month)
# Need to work on # maternal death, neonatal death, still births, number of deliveries monthly = number of births


data2 <- data2 %>% rename(metric = indicator_refactored) %>%
  relocate(metric, .before = total)


data3 <- data2 %>% pivot_wider(names_from = metric, values_from = total)

## Monthly charts

data3 <- data3 %>%
  select(year, month, `number deliveries monthly`, `monthly stillbirths`, `maternal mortality monthly`,
         `neonatal mortality monthly`, `live birth`) %>%

rowwise() %>%
  
  mutate(
    
    `monthly still birth rate` = `monthly stillbirths` * 1000/`number deliveries monthly`,
    `monthly maternal mortality ratio` = `maternal mortality monthly` * 100000/`live birth`,
    `monthly neonatal mortality rate` = `neonatal mortality monthly` * 1000/`live birth`
    
  )



data3 <- data3 %>% select(year, month, `monthly number of births` = `number deliveries monthly`,
                          `monthly still birth rate`, `monthly maternal mortality ratio`,
                          `monthly neonatal mortality rate`) %>%
  pivot_longer(cols = c(`monthly number of births`:`monthly neonatal mortality rate`),
               names_to = "indicator", values_to = "value")



data3 <- data3 %>% mutate(month = substr(month, start = 1, stop = 3),
                          month_year = str_to_sentence(paste(month, year)))

data3 <- data3 %>% select(month_year, indicator, value)

levels <- map2_chr(.x = rep(str_to_sentence(month.abb), 3),
                   .y = c(rep(2019, 12), rep(2020, 12), rep(2021, 12)),
                   ~str_glue(.x, .y, .sep = " ")) %>%
  str_wrap(., width = 3)

data3 <- data3 %>% mutate(month_year = str_wrap(month_year, width = 3),
                          month_year = factor(month_year, levels = levels),
                          indicator = str_replace(indicator, "monthly ", "")) %>%
  mutate(across(.cols = indicator,
                .fns = ~{str_to_title(.x) %>% gsub(., pattern = "Of", replacement = "of")}),
         indicator = factor(indicator,
                            levels = c("Number of Births", "Still Birth Rate", "Maternal Mortality Ratio",
                                       "Neonatal Mortality Rate")))


## Quarterly charts

data4 <- data2 %>% pivot_wider(names_from = metric, values_from = total)

data4 <- data4 %>% group_by(year, quarter) %>%
  summarise(across(.cols = `live birth`:last_col(), .fns = sum))

data4 <- data4 %>%
  select(year, quarter, `number deliveries monthly`, `monthly stillbirths`, `maternal mortality monthly`,
         `neonatal mortality monthly`, `live birth`) %>%
  
  rowwise() %>%
  
  mutate(
    
    `monthly still birth rate` = `monthly stillbirths` * 1000/`number deliveries monthly`,
    `monthly maternal mortality ratio` = `maternal mortality monthly` * 100000/`live birth`,
    `monthly neonatal mortality rate` = `neonatal mortality monthly` * 1000/`live birth`
    
  )



data4 <- data4 %>% select(year, quarter, `monthly number of births` = `number deliveries monthly`,
                          `monthly still birth rate`, `monthly maternal mortality ratio`,
                          `monthly neonatal mortality rate`) %>%
  pivot_longer(cols = c(`monthly number of births`:`monthly neonatal mortality rate`),
               names_to = "indicator", values_to = "value")



data4 <- data4 %>% mutate(quarter_year = str_to_sentence(paste(quarter, year)))

data4 <- data4 %>% select(quarter_year, indicator, value)

data4_levels <- map2_chr(.x = rep(c("Q1", "Q2", "Q3", "Q4"), 3),
                   .y = c(rep(2019, 4), rep(2020, 4), rep(2021, 4)),
                   ~str_glue(.x, .y, .sep = " ")) %>%
  str_wrap(., width = 2)

data4 <- data4 %>% mutate(quarter_year = str_wrap(quarter_year, width = 2),
                          quarter_year = factor(quarter_year, levels = data4_levels),
                          indicator = str_replace(indicator, "monthly ", "")) %>%
  mutate(across(.cols = indicator,
                .fns = ~{str_to_title(.x) %>% gsub(., pattern = "Of", replacement = "of")}),
         indicator = factor(indicator,
                            levels = c("Number of Births", "Still Birth Rate", "Maternal Mortality Ratio",
                                       "Neonatal Mortality Rate")))

## t-test (independent a.k.a unpaired)
data4 <- data4 %>% mutate(group = case_when(str_detect(quarter_year, "2019") ~ "pre-covid",
                                            str_detect(quarter_year, "2020|2021") ~ "post-covid")) %>%
  relocate(group, .after = quarter_year)


data4 %>% nest(data5 = -indicator) %>%
  mutate(model = map(data5, ~{t.test(data = .x, value ~ group, paired = FALSE) %>% broom::tidy()})) %>%
  unnest(model)


## Chi-squared
data4 %>% nest(data5 = -indicator) %>%
  mutate(model = map(data5, ~{chisq.test(.x$value, .x$group) %>% broom::tidy()})) %>%
  unnest(model)


data4 %>% 
  group_by(indicator) %>%
  nest() %>%
  mutate(model = future_map(data, ~chisq.test(.$value, .$group, simulate.p.value=TRUE, B=1e7)),
         tidied_model = map(model, broom::tidy)) %>%
  unnest(tidied_model)

### Poisson regression - after getting rates/ ratios etc
data4 %>% nest(data5 = -indicator) %>%
  mutate(model = map(data5, ~{glm(data = .x, value ~ group, family = "poisson") %>% broom::tidy()})) %>%
  unnest(model)

### Anova
data4 %>% nest(data5 = -indicator) %>%
  mutate(model = map(data5, ~{aov(data = .x, value ~ group) %>% broom::tidy()})) %>%
  unnest(model)

# Helvetica belongs to the sans serif font

systemfonts::system_fonts() %>%
  filter(grepl("Microsoft Sans Serif", family)) %>%
  pull(name) %>%
  sort()


# Plotting the data
font_import()
loadfonts(device = "win")

# Initiate camcorder
# gg_record(dir = file.path(here("./Output/", "Plots")),
#           device = cairo_pdf,
#           width = 12, #10
#           height = 8)


# Function to plot data

plot_data <- function(df, x_axis, time_duration){
  
  
  x_axis = rlang::ensym(x_axis)
  
  
  ggplot(data = df,
         mapping = aes(x = !!x_axis, y = value)) +
    
    geom_line(group = 1) + geom_point(size = 1) +
    
    facet_wrap(~indicator, ncol = 1, scales = "free_y",
               labeller = as_labeller(c("Number of Births" = "Number of Births",
                                        "Still Birth Rate" = "Still Birth Rate (Still births per 1000 deliveries)",
                                        "Maternal Mortality Ratio" = "Maternal Mortality Ratio (per 100,000 live births)",
                                        "Neonatal Mortality Rate" = "Neonatal Mortality Rate (per 1000 live births)"))) +
    
    
    labs(
      caption = glue("<br>Figure 5: Timeline of Events and {time_duration}ly Outcomes Before and During the COVID-19 Era (2019 - 2021)<br>
                     <br>
                     <sup>1</sup>Government declared that there will be no lockdown in Tanzania (11 March)<br>
                     <sup>2</sup>Mandatory wearing of masks for all clients and visitors to the hospital (10 April onwards)<br>
                     <sup>3</sup>Hospital visitation by relatives limited to one person for only 30 minutes and sitting distance in ANC waiting areas (10 April onwards)<br>
                     <sup>4</sup>Travel restrictions (11 April - 18 May)<br>
                     <sup>5</sup>Government announced three days for prayers for God's protection and healing (17 April onwards)<br>
                     <sup>6</sup>Government declared that the pandemic was over in Tanzania and people should resume normal lives (4 July)<br>"),
      x = glue("<br>{time_duration}"),
      y = glue("<br>Value<br>")
    ) +
    
    
    theme(
      
      plot.title.position = "panel", #plot
      plot.title = element_text(hjust = 0),
      axis.line = element_line(),
      panel.background = element_rect(fill = "white"), #"gray96"
      strip.text = element_text(
        colour = "black"#, face = "italic"
      ),
      
      strip.background = element_rect(fill = "gray"), ##B2BEB5
      panel.grid.major.x = element_line(colour = "gray96"),
      panel.grid.minor.x = element_line(colour = "gray96", linetype = "dashed"),
      
      
      plot.caption = ggtext::element_markdown(hjust = 0),
      axis.title.x = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0.8),
      axis.title.y = ggtext::element_markdown(),
      
      
      text = element_text(family = "Comic Sans MS", size = 12) #"Comic Sans MS" #"Microsoft Sans Serif"
      
    ) +
    
    scale_y_continuous(
      breaks = scales::breaks_pretty()
    )
  
}


quarterly_data <- plot_data(data4, "quarter_year", "Quarter")
monthly_data <- plot_data(data3, "month_year", "Month")


print(quarterly_data)
print(monthly_data)


# quarterly_data + annotate(
#   geom = "text",
#   x = "Q1\n2020",
#   y = 10,
#   label = "1"
# )

suppressWarnings({

  quarterly_data + # Label 1- DONE
  
  geom_point(data = data4 %>% filter(indicator == last(indicator)),
             aes(x = "Q1\n2020", y = 8),
             shape = 1,
             size = 4.5) + 
  
  geomtextpath::geom_textpath(data = data4 %>% filter(indicator == last(indicator)),
                              x = "Q1\n2020",
                              y =8.1,
                              label = "1", size = 3) + #Label 2 - DONE
    
    geom_point(data = data4 %>% filter(indicator == last(indicator)),
               aes(x = "Q2\n2020", y = 13),
               shape = 1,
               size = 4.5) + 
    
    geomtextpath::geom_textpath(data = data4 %>% filter(indicator == last(indicator)),
                                x = "Q2\n2020",
                                y = 13.1,
                                label = "2", size = 3) + # Label 3
    
    geom_point(data = data4 %>% filter(indicator == last(indicator)),
               aes(x = "Q2\n2020", y = 10),
               shape = 1,
               size = 4.5) + 
    
    geomtextpath::geom_textpath(data = data4 %>% filter(indicator == last(indicator)),
                                x = "Q2\n2020",
                                y = 10.1,
                                label = "3", size = 3) + # Label 4
    
    geom_point(data = data4 %>% filter(indicator == last(indicator)),
               aes(x = "Q2\n2020", y = 8),
               shape = 1,
               size = 4.5) + 
    
    geomtextpath::geom_textpath(data = data4 %>% filter(indicator == last(indicator)),
                                x = "Q2\n2020",
                                y = 8.1,
                                label = "4", size = 3) + # Label 5
    
    geom_point(data = data4 %>% filter(indicator == last(indicator)),
               aes(x = "Q2\n2020", y = 6),
               shape = 1,
               size = 4.5) + 
    
    geomtextpath::geom_textpath(data = data4 %>% filter(indicator == last(indicator)),
                                x = "Q2\n2020",
                                y = 6.1,
                                label = "5", size = 3) + # Label 6
    
    geom_point(data = data4 %>% filter(indicator == last(indicator)),
               aes(x = "Q3\n2020", y = 8),
               shape = 1,
               size = 4.5) + 
    
    geomtextpath::geom_textpath(data = data4 %>% filter(indicator == last(indicator)),
                                x = "Q3\n2020",
                                y = 8.1,
                                label = "6", size = 3)
})



quarterly_data +
  geomtextpath::geom_textsegment(label = "1",
                              hjust = 0.5,
                              vjust = 0.5,
                              data = data4 %>% filter(indicator == last(indicator)),
                              x = "Q2\n2020", xend = "Q2\n2020",
                              y = 0, yend = 10)

# gg_stop_recording()