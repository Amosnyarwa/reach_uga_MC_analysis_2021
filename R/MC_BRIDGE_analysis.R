# Loading required packages
library(tidyverse)
library(scales)

# Average monthly income (in UGX) of target groups before and after the main implementation phase
df_income <- tibble(Status= c("Host", "Refugee", "Host", "Refugee"),
                    Average_monthly_income= c(126808, 68647, 138666, 100470),
                    Implementation_phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_income %>% 
  ggplot(aes(fill= Implementation_phase, x= Status, y= Average_monthly_income)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(labels= scales::comma)+
  scale_y_continuous(breaks= NULL)+
  geom_text(aes(label= format(Average_monthly_income, nsmall=0, big.mark=","), vjust= -.3, hjust= .5),
            position = position_dodge2(width = .5))+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 13), legend.title= element_blank(), axis.text.y = element_blank())+
  # ylim(0, 150000)
  labs(title=paste("Average monthly income (in UGX) of target groups (n baseline: 654, n endline:517)"), x= element_blank(), y= element_blank())


# Type of agricultural enterprises or agricultural activities

df_agric_enterprise <- tibble(Response= c("Crop_farming", "Crop_farming", "Vegetable_growing", "Vegetable_growing", "Agricultural_produce_selling", "Agricultural_produce_selling", "Livestock_keeping", "Livestock_keeping", "Buying_and_selling_livestock", "Buying_and_selling_livestock", "Poultry_keeping", "Poultry_keeping", "Maize_milling", "Maize_milling", "Others", "Others"),
                              Percentage_respondents= c(41, 44, 24, 25, 13, 12, 8, 8, 3, 3, 6, 4, 3, 1, 3, 1),
                              Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))    


df_agric_enterprise %>%
  mutate(
    Response= str_replace_all(Response, "_", " "),
    Response= str_to_sentence(Response)
  ) %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percentage_respondents) , y= Percentage_respondents)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0, 50)+
  labs(title=paste("Type of Agricultural Enterprises or Agricultural Activities practiced (n baseline: 654, n endline:517)"), x= element_blank(), y= "% of respondents" )
  
  

# Level of practicing agricultural production Stacked grph

df_agric_production_level <- tibble(Response= c("Commercial", "Subsistence", "Commercial", "Subsistence"),
                                    Percentage_respondents= c(31, 69, 25, 75),
                                    Phase= c("Baseline", "Baseline", "Endline", "Endline"))


df_agric_production_level %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percentage_respondents) , y= Percentage_respondents)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(breaks= NULL)+
  geom_text(aes(label= scales::percent(Percentage_respondents/100, accuracy = 1, trim = TRUE), vjust= -.3, hjust= .5),
            position = position_dodge2(width = .5))+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 80)+
  labs(title=paste("Level of practicing of Agricultural Production (n baseline: 654, n endline:517)"), x= element_blank(), y= element_blank())
  # labs(x= element_blank(), y= "% of respondents")
  
# Average total amount (in Kg) of agricultural products produced by target groups

df_amount_agric_products_produced <- tibble(Response= c("Simsim", "Simsim", "Groundnuts", "Groundnuts", "Maize", "Maize", "Beans", "Beans", "Others", "Others"),
                                   Average_amount_kgs= c(93.3, 248.7, 139.3, 179.3, 84.6, 91.8, 48.3, 50, 118.6, 123.2),
                                   Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline" ))

df_amount_agric_products_produced %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Average_amount_kgs) , y= Average_amount_kgs)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 250)+
  labs(title=paste("Average total amount (in Kgs) of Agricultural products produced (n baseline: 654, n endline:517)"), x= element_blank(), y= "Average amount in Kgs")
  

# Average total amount (in UGX) of agricultural products sold to the market by target groups

df_amount_agric_products_sold <- tibble(Agric_product= c("Simsim", "Simsim", "Groundnuts", "Groundnuts", "Maize", "Maize", "Beans", "Cotton"),
                                        Average_amount_UGX= c(65293, 86150, 52501, 71121, 49211, 63214, 105000, 97875),
                                        Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Endline", "Endline"))

df_amount_agric_products_sold %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Agric_product, -Average_amount_UGX) , y= Average_amount_UGX)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(labels = comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 120000)+
  labs(title=paste("Average total amount (in UGX) of Agricultural products sold to the market (n baseline: 654, n endline:517)"), x= element_blank(), y= "Average amount in UGX")
  # labs(x= element_blank() , y= "Average amount in UGX")

# Employment status of surveyed respondents stacked-create 

df_employment_status <- tibble(Gender= c("Male", "Male", "Female", "Female"),
                               Percent_employed= c(24.5, 26.9, 18.1, 20.4),
                               Phase= c("Baseline", "Endline", "Baseline", "Endline"))

df_employment_status %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Gender, -Percent_employed) , y= Percent_employed)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(breaks= NULL)+
  geom_text(aes(label= scales::percent(Percent_employed/100, accuracy = 1, trim = TRUE), vjust= -.3, hjust= .5),
            position = position_dodge2(width = .5))+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 30)+
  labs(title=paste("Employment status of survey respondents (n baseline: 654, n endline:517)"), x= element_blank(), y= element_blank())
  

# Satisfaction level of surveyed respondents with current employment activity -stacked

df_satisfaction_level <- tibble(level_satisfaction= c("Very unsatisfied", "Very unsatisfied", "Unsatisfied", "Unsatisfied", "Neutral", "Neutral", "Satisfied", "Satisfied", "Highly satisfied", "Highly satisfied" ),
                                Percent_satisfaction= c(9, 5, 18, 16, 19, 29, 50, 48, 4, 3),
                                Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_satisfaction_level %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(level_satisfaction, -Percent_satisfaction) , y= Percent_satisfaction)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.6)+
  scale_y_continuous(breaks= NULL)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 60)+
  labs(title=paste("Satisfaction level of respondents with current employment activity (n baseline: 654, n endline:517)"), x= element_blank(), y= "% satisfaction with current employment")
  # labs(x= element_blank() , y= "% satisfaction with current employment")

# Employment status by type of employment-convert to %

df_employment_type <- tibble(employment_type= c("formally_employed", "Self_employed", "formally_employed", "Self_employed" ),
                             Number_employed_by_type= c(45, 92, 70, 103),
                             Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_employment_type %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(employment_type, -Number_employed_by_type) , y= Number_employed_by_type)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 120)+
  labs(x= element_blank() , y= "# of respondents by type of employment ")

# Type of engagement of formally employed respondents-check data

df_engagement_type <- tibble(type_of_engagement= c("Full_time", "Short_term", "Apprenticeship", "Full_time", "Short_term", "Part_time"),
                             Percent_engagement_type= c(7, 38, 55, 37, 39, 24  ),
                             Phase= c("Baseline", "Baseline", "Baseline", "Endline", "Endline", "Endline"))

df_engagement_type %>%
  ggplot(aes(fill= Phase, x= fct_reorder(type_of_engagement, -Percent_engagement_type) , y= Percent_engagement_type)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 60)+
  labs(x= element_blank() , y= "% of respondents by type of engagement")

# Ways of acquiring jobs

df_acquire_jobs <- tibble(Way_acquire_job= c("Recommended_by_friend", "Recommended_by_friend", "Recommended_by_relative", "Recommended_by_relative", "Through_apply_for_various_positions", "Through_apply_for_various_positions", "Head_hunting", "Head_hunting", "Others", "Others"),
                          Percent_way_acquire_jobs= c(19, 23, 8, 12, 18, 35, 29, 21, 26, 9),
                          Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"  ))

df_acquire_jobs %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Way_acquire_job, -Percent_way_acquire_jobs) , y= Percent_way_acquire_jobs)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 40)+
  labs(x= element_blank() , y= "% of channels of acquiring jobs")

# Sectors of employment 

df_employment_sector <- tibble(sector_of_employment= c("Manufacturing", "Manufacturing", "Food_processing", "Food_processing", "Agriculture", "Agriculture", "Services", "Services", "Retail_and_wholesale", "Retail_and_wholesale", "Hospitality", "Hospitality", "Other", "Other"),
                               Percent_employment_sector= c(6, 8, 9, 6, 37, 26, 15, 31, 5, 13, 4, 3, 24, 14),
                               Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_employment_sector %>%
  ggplot(aes(fill= Phase, x= fct_reorder(sector_of_employment, -Percent_employment_sector) , y= Percent_employment_sector)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 40)+
  labs(x= element_blank() , y= "% showing sectors where respondents were employed")

# Number of surveyed respondents that have received training on business management

df_respondents_received_training <- tibble(Responses= c("Yes", "Yes", "No", "No"),
                                           Number_responses= c(174, 170, 97, 140),
                                           Phase= c("Baseline", "Endline", "Baseline", "Endline"))

df_respondents_received_training %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Responses, -Number_responses) , y= Number_responses)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 200)+
  labs(x= element_blank() , y= "# of respondents that received training")

# Type of training provided to manage business

df_training_type <- tibble(type_of_training= c("Business_record_keeping", "Business_record_keeping", "Making_business_plans", "Making_business_plans", "Financial_literacy", "Financial_literacy", "Savings", "Savings", "Business_group_formation", "Business_group_formation", "Business_selection", "Business_selection", "Others", "Others"),
                           Number_trained_by_type= c(123, 138, 102, 121, 75, 62, 109, 95, 54, 52, 45, 54, 2, 1),
                           Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_training_type %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(type_of_training, -Number_trained_by_type) , y= Number_trained_by_type)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 150)+
  labs(x= element_blank() , y= "# of respondents who participated in the training")

# Percentage of respondents who got support/cash grant to establish IGAs

df_beneficiaries_cash_grant <- tibble(Received_cash_grant= c("Yes", "No", "Yes", "No"),
                                      Percent_received_cash_grant= c(37, 56, 42, 54 ),
                                      Phase= c("Baseline", "Baseline", "Endline", "Endline" ))

df_beneficiaries_cash_grant %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Received_cash_grant, -Percent_received_cash_grant) , y= Percent_received_cash_grant)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 60)+
  labs(x= element_blank() , y= "# of respondents who received support/cash grant to establish IGAs")

# Number of respondents who were trained on how to develop business plans

df_business_plan_training <- tibble(Status= c("Host", "Host", "Refugees", "Refugees"),
                                    Number_trained_business_plan= c(13, 53, 89, 68),
                                    Phase= c("Baseline", "Endline", "Baseline", "Endline"))

df_business_plan_training %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Status, -Number_trained_business_plan) , y= Number_trained_business_plan)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 100)+
  labs(x= element_blank() , y= "# of respondents who received training on developing business plans")

# Level of application of skills gained in training
df_application_skills <- tibble(Response= c("Yes", "No", "Yes", "No" ),
                                Percent_application_skills= c(90, 10, 96, 4),
                                Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_application_skills %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_application_skills) , y= Percent_application_skills)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 100)+
  labs(x= element_blank() , y= "% of level of application of skills gained in trainings by target groups")

# Effects of the trainings on the businesses of the target groups
df_training_effect <- tibble(Effects_training= c("Increased_profits","Increased_profits", "Beter_book_keeping", "Beter_book_keeping", "Business_is_more_organized_than_before", "Business_is_more_organized_than_before", "Others", "Others"),
                             Percent_effect_training= c(56, 41, 20, 34, 22, 24, 2, 1),
                             Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_training_effect %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Effects_training, -Percent_effect_training) , y= Percent_effect_training)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 80)+
  labs(x= element_blank() , y= "% effect of the training on businesses of the target groups")

# Percentage of respondents with mobile phones
df_phone_ownership <- tibble(Status=c("Host", "Refugee", "Host", "Refugee"),
                             Percent_phone_ownership= c(66, 61, 65, 71),
                             Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_phone_ownership %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Status, -Percent_phone_ownership) , y= Percent_phone_ownership)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 100)+
  labs(x= element_blank() , y= "% of respondents with mobile phones")

# Key factors that prevent use of internet
df_key_factor_limit_internet_usage <- tibble(Key_factors= c("Cannot_afford_regular_internet_use", "Cannot_afford_regular_internet_use", "No_limited_electricity", "No_limited_electricity", "No_limited_access_to_computers_phones", "No_limited_access_to_computers_phones"),
                                             Percent_limited_internet= c(19, 34, 29, 45, 43, 51),
                                             Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_key_factor_limit_internet_usage %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Key_factors, -Percent_limited_internet) , y= Percent_limited_internet)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 60)+
  labs(x= element_blank() , y= "% of key factors preventing limited internet use")

# I can always manage to solve difficult problems if I try hard enough
df_solve_problems <- tibble(Response= c("Prefer_not_to_answer", "Prefer_not_to_answer", "True", "True", "Somewhat_true", "Somewhat_true", "Somewhat_false", "Somewhat_false", "False", "False"),
                            Percent_response= c(4, 0, 50, 29, 21, 49, 7, 10, 18, 12),
                            Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_solve_problems %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_response) , y= Percent_response)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 60)+
  labs(x= element_blank() , y= "% of ability to solve difficult problems")

# Felt little interest or pleasure in doing things
df_interest_pleasure <- tibble(Response= c("Prefer_not_to_answer", "Prefer_not_to_answer", "Nearly_every_day", "Nearly_every_day", "More_than_half_the_days", "More_than_half_the_days", "Several_days", "Several_days", "Not_at_all", "Not_at_all"),
                               Percent_response= c(2, 1, 8, 17, 17, 15, 24, 34, 49, 33),
                               Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_interest_pleasure %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_response) , y= Percent_response)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 60)+
  labs(x= element_blank() , y= "% of interest or pleasure in doing things")

# Felt tired or had little energy
df_tired_little_energy <- tibble(Response= c("Prefer_not_to_answer", "Prefer_not_to_answer", "Nearly_every_day", "Nearly_every_day", "More_than_half_the_days", "More_than_half_the_days", "Several_days", "Several_days", "Not_at_all", "Not_at_all"),
                                 Percent_response= c(1, 1, 8, 20, 14, 14, 27, 32, 50, 33),
                                 Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_tired_little_energy %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_response) , y= Percent_response)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  ylim(0, 60)+
  labs(x= element_blank() , y= "% of respondents who felt tired or had little energy")






