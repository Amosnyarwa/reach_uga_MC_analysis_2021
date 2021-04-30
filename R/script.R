library(tidyverse)
devtools::install_github("zackarno/butteR")
library(readxl)
library(butteR)
library(srvyr)
3

# read data
dap <- read_csv("inputs/analysis_dap_MC_ev_MT.csv")
view(dap)
df_data <- read_xlsx("inputs/BRIDGE Endline Survey - GM2.xlsx")

# data cleaning for some numeric variables coming 

df_data$crop_production_kgs %>% 
  sort()
df_data$crop_production_kgs <- as.numeric(df_data$crop_production_kgs)

view(df_data)

# select variables from DAP that are in the dataset
variables_to_analyse <- dap$variable[dap$variable %in% colnames(df_data)]

butteR::survey_collapse()

# Convert df to survey
df_svy <- as_survey(df_data)
df_svy

df_svy$variables

overall_analysis <- butteR::survey_collapse(df= df_svy, 
                                            vars_to_analyze = variables_to_analyse)
# Disagregating by population status
by_population_status <- butteR::survey_collapse(df= df_svy, 
                                            vars_to_analyze = variables_to_analyse, disag = "participant_category")

# Combine analysis outputs

overall_analysis %>% 
  mutate(
    analysis_level= "overall"
  )

by_population_status %>% 
  mutate(
    by_population_status= "overall"
  )

bind_rows(by_population_status, overall_analysis)

combined_analysis <- bind_rows(by_population_status, overall_analysis)

# trick to combine disagregation analysis faster
res <- list()

res$overall_analysis <- butteR::survey_collapse(df= df_svy, 
                                            vars_to_analyze = variables_to_analyse)
# Disagregating by population status
res$by_population_status <- butteR::survey_collapse(df= df_svy, 
                                                vars_to_analyze = variables_to_analyse, disag = "participant_category")

combined_analysis_with_list <- bind_rows(res)


