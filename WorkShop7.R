#load libraries 
library(tidyverse)
library(tidymodels)
library(skimr)

#upload data 
volcano_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

#get overview of data
skim(volcano_raw)

volcano_raw%>%filter(last_eruption_year!="Unknown")

volcano_raw%>%count(primary_volcano_type,sort = TRUE)


volcano_df <- volcano_raw %>%
  transmute(volcano_type = case_when(str_detect(primary_volcano_type, "Stratovolcano") ~ "Stratovolcano",
                                     str_detect(primary_volcano_type, "Shield") ~ "Shield",
                                     TRUE ~ "Other"),
            volcano_number, latitude, longitude, elevation, 
            tectonic_settings, major_rock_1) %>%
  mutate_if(is.character, factor)













volcano_rec <- recipe(volcano_type ~ ., data = volcano_df) %>%
  update_role(volcano_number, new_role = "Id") %>%
  step_other(tectonic_settings) %>%
  step_other(major_rock_1) %>%
  step_dummy(tectonic_settings, major_rock_1) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_smote(volcano_type)





rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")





