# Loading required packages
library(tidyverse)
library(scales)

# Average monthly income (in UGX) of target groups
df_income <- tibble(Status= c("Host", "Refugee", "Host", "Refugee"),
                    Average_monthly_income= c(126808, 68647, 138666, 100470),
                    Implementation_phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_income %>% 
  ggplot(aes(fill= Implementation_phase, x= Status, y= Average_monthly_income)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(labels= scales::comma)+
  # scale_y_continuous(breaks= NULL)+
  geom_text(aes(label= format(Average_monthly_income, nsmall=0, big.mark=","), vjust= -.3, hjust= .5),
            position = position_dodge2(width = 0.5))+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.y = element_blank())+
  # ylim(0, 150000)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(title= "Average monthly income (in UGX) of target groups", subtitle = "n baseline: 650, n endline:517", x= element_blank(), y= element_blank())


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
  geom_bar(position = "dodge", stat = "identity", width = 0.8)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0, 50)+
  labs(title= "Percentage of respondents by agricultural enterprises or activities practiced", subtitle = "n baseline: 326, n endline:527", x= element_blank(), y= "% of respondents")
  

# Level of practicing agricultural production

df_agric_production_level <- tibble(Response= c("Commercial", "Subsistence", "Commercial", "Subsistence"),
                                    Percentage_respondents= c(31, 69, 25, 75),
                                    Phase= c("Baseline", "Baseline", "Endline", "Endline"))



df_agric_production_level %>%
  ggplot(aes(fill= Response, x= fct_reorder(Phase, -Percentage_respondents) , y= Percentage_respondents)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percentage_respondents,"%")),
            position = position_stack(vjust = 0.5), size = 5)+
  scale_y_continuous(breaks= NULL)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 80)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(title= "Proportion of respondents by type of agricultural production", subtitle = "n baseline: 326, n endline:527", x= element_blank(), y= element_blank())

  
# Average total amount (in Kg) of agricultural products produced by target groups

df_amount_agric_products_produced <- tibble(Response= c("Simsim", "Simsim", "Groundnuts", "Groundnuts", "Maize", "Maize", "Beans", "Beans", "Others", "Others"),
                                   Average_amount_kgs= c(93.3, 248.7, 139.3, 179.3, 84.6, 91.8, 48.3, 50, 118.6, 123.2),
                                   Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline" ))

df_amount_agric_products_produced %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Average_amount_kgs), y= Average_amount_kgs)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.8)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 250)+
  labs(title= "Average total amount (in Kgs) of Agricultural products produced", subtitle = "n baseline: 213, n endline:365", x= element_blank(), y= "Average amount in Kgs")
  
  

# Average total amount (in UGX) of agricultural products sold to the market by target groups

df_amount_agric_products_sold <- tibble(Agric_product= c("Simsim", "Simsim", "Groundnuts", "Groundnuts", "Maize", "Maize", "Beans", "Beans", "Cotton", "Cotton"),
                                        Average_amount_UGX= c(65293, 86150, 52501, 71121, 49211, 63214, 0, 105000, 0, 97875),
                                        Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_amount_agric_products_sold %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Agric_product, -Average_amount_UGX) , y= Average_amount_UGX)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.8)+
  scale_y_continuous(labels = comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 120000)+
  labs(title= "Average total amount (in UGX) of Agricultural products sold to the market", subtitle = "n baseline: 211, n endline:365", x= element_blank(), y= "Average amount in UGX")
  
  

# Employment status of surveyed respondents 

df_employment_status <- tibble(Gender= c("Male", "Female", "Male", "Female"),
                               Percent_employed= c(24.5, 18.1, 26.9, 20.4),
                               Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_employment_status %>%
  ggplot(aes(fill= Gender, x= fct_reorder(Phase, -Percent_employed) , y= Percent_employed)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_employed,"%")),
            position = position_stack(vjust = 0.5), size = 5)+
  scale_y_continuous(breaks= NULL)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 30)+
  labs(title= "Proportion of respondents'employment status by gender", subtitle = "n baseline: 212, n endline:365", x= element_blank(), y= element_blank())

# Satisfaction level of surveyed respondents with current employment activity -stacked

df_satisfaction_level <- tibble(level_satisfaction= c("Very unsatisfied", "Very unsatisfied", "Unsatisfied", "Unsatisfied", "Neutral", "Neutral", "Satisfied", "Satisfied", "Highly satisfied", "Highly satisfied" ),
                                Percent_satisfaction= c(9, 5, 18, 16, 19, 29, 50, 48, 4, 3),
                                Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_satisfaction_level %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(level_satisfaction, -Percent_satisfaction) , y= Percent_satisfaction)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.8)+
  scale_y_continuous(breaks= NULL)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 60)+
  labs(title= "Proportion of respondents' satisfaction level with current employment activity", subtitle = "n baseline: 43, n endline:93", x= element_blank(), y= "% satisfaction with current employment" )
  # labs(title=paste("Satisfaction level of respondents with current employment activity (n baseline: 654, n endline:517)"), x= element_blank(), y= "% satisfaction with current employment")
  

# Employment status by type of employment-convert to %

df_employment_type <- tibble(employment_type= c("Formally employed", "Self employed", "Formally employed", "Self employed" ),
                             Percent_employed_by_type= c(32.8, 67.2, 40.5, 59.5),
                             Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_employment_type %>% 
  ggplot(aes(fill= employment_type, x= fct_reorder(Phase, -Percent_employed_by_type) , y= Percent_employed_by_type)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_employed_by_type, "%")),
            position = position_stack(vjust = 0.5), size = 5)+
  scale_y_continuous(breaks= NULL)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 120)+
  labs(title= "Proportion of respondents' employment status by type of employment activity", subtitle = "n baseline: 212, n endline:93", x= element_blank(), y= element_blank())
  # labs(x= element_blank() , y= "# of respondents by type of employment ")

# Type of engagement of formally employed respondents

df_engagement_type <- tibble(type_of_engagement= c("Full time", "Short term", "Apprenticeship", "Part time", "Full time", "Short term", "Part time", "Apprenticeship"),
                             Percent_engagement_type= c(24.4, 20, 55.6, 0, 34.3, 38.6, 27.1, 0),
                             Phase= c("Baseline", "Baseline", "Baseline", "Baseline", "Endline", "Endline", "Endline", "Endline"))

df_engagement_type %>%
  ggplot(aes(fill= Phase, x= fct_reorder(type_of_engagement, -Percent_engagement_type) , y= Percent_engagement_type)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 60)+
  labs(title= "Proportion of formally employed respondents by type of engagement", subtitle = "n baseline: 45, n endline:70", x= element_blank(), y= "% of respondents by type of engagement")
  # labs(x= element_blank() , y= "% of respondents by type of engagement")

# Ways of acquiring jobs

df_acquire_jobs <- tibble(Way_acquire_job= c("Recommended by friend", "Recommended by friend", "Recommended by relative", "Recommended by relative", "Through apply for various positions", "Through apply for various positions", "Head hunting", "Head hunting", "Others", "Others"),
                          Percent_way_acquire_jobs= c(19, 23, 8, 12, 18, 35, 29, 21, 26, 9),
                          Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"  ))

df_acquire_jobs %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Way_acquire_job, -Percent_way_acquire_jobs) , y= Percent_way_acquire_jobs)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.8)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 40)+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "Proportion of respondents' ways of acquiring jobs", subtitle = "n baseline: 45, n endline:70", x= element_blank(), y= "% channels of acquiring jobs")
  # labs(x= element_blank() , y= "% channels of acquiring jobs")

# Sectors of employment 

df_employment_sector <- tibble(sector_of_employment= c("Manufacturing", "Manufacturing", "Food processing", "Food processing", "Agriculture", "Agriculture", "Services", "Services", "Retail and wholesale", "Retail and wholesale", "Hospitality", "Hospitality", "Other", "Other"),
                               Percent_employment_sector= c(6, 8, 9, 6, 37, 26, 15, 31, 5, 13, 4, 3, 24, 14),
                               Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_employment_sector %>%
  ggplot(aes(fill= Phase, x= fct_reorder(sector_of_employment, -Percent_employment_sector) , y= Percent_employment_sector)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 40)+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "Proportion of respondents' sectors of employment", subtitle = "n baseline: 45, n endline:70", x= element_blank(), y= "% respondents' sectors of employment")
  # labs(x= element_blank() , y= "% showing sectors where respondents were employed")

# Number of surveyed respondents that have received training on business management

df_respondents_received_training <- tibble(Responses= c("Yes", "Yes"),
                                           Percent_responses= c(64.2, 54.8),
                                           Phase= c("Baseline", "Endline"))

df_respondents_received_training %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Responses, -Percent_responses) , y= Percent_responses)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_responses,"%")),
            position = position_dodge2(width = 0.5), size = 5, vjust= -0.5)+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())+
  # ylim(0, 200)+
  labs(title= "Proportion of respondents that received training on business management", subtitle = "n baseline: 271, n endline:310", x= element_blank(), y= element_blank())
  # labs(x= element_blank() , y= "# of respondents that received training")

# Type of training provided to manage business

df_training_type <- tibble(type_of_training= c("Business record keeping", "Business record keeping", "Making business plans", "Making business plans", "Financial literacy", "Financial literacy", "Savings", "Savings", "Business group formation", "Business group formation", "Business selection", "Business selection", "Others", "Others"),
                           Number_trained_by_type= c(24.1, 26.4, 20, 23.1, 14.7, 11.9, 21.4, 18.2, 10.6, 9.9, 8.8, 10.3, 0.4, 0.2),
                           Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_training_type %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(type_of_training, -Number_trained_by_type) , y= Number_trained_by_type)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0, 30)+
  labs(title= "Proportion of respondents that received training by type", subtitle = "n baseline: 510, n endline:523", x= element_blank(), y= "% of respondents that received training by type")
  

# Percentage of respondents who got support/cash grant to establish IGAs

df_beneficiaries_cash_grant <- tibble(Received_cash_grant= c("Yes", "Yes"),
                                      Percent_received_cash_grant= c(37, 42),
                                      Phase= c("Baseline", "Endline"))

df_beneficiaries_cash_grant %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Received_cash_grant, -Percent_received_cash_grant) , y= Percent_received_cash_grant)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_received_cash_grant,"%")),
            position = position_dodge2(width = 0.5), size = 5, vjust= -0.5)+
  # scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  ylim(0, 50)+
  labs(title= "Proportion of respondents who received support/cash grant to establish IGAs ", subtitle = "n baseline: 510, n endline:523", x= element_blank(), y= "% of respondents who received support/cash grant to establish IGAs")
  # labs(x= element_blank() , y= "# of respondents who received support/cash grant to establish IGAs")

# % of respondents who were trained on how to develop business plans

df_business_plan_training <- tibble(Status= c("Host", "Refugees", "Host", "Refugees"),
                                    Percent_trained_business_plan= c(12.7, 87.3, 43.8, 56.2),
                                    Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_business_plan_training %>%
  ggplot(aes(fill= Status, x= fct_reorder(Phase, -Percent_trained_business_plan) , y= Percent_trained_business_plan)) +
  geom_bar(position = "stack", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_trained_business_plan,"%")),
            position = position_stack(vjust = 0.5), size = 5)+
  # scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 100)+
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(title= "Proportion of respondents trained on how to develop business plans", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= element_blank())
  

# Level of application of skills gained in training
df_application_skills <- tibble(Response= c("Yes","Yes"),
                                Percent_application_skills= c(90, 96),
                                Phase= c("Baseline", "Endline"))

df_application_skills %>%
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_application_skills) , y= Percent_application_skills)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_application_skills,"%")),
            position = position_dodge2(width = 0.5), size = 5, vjust= -0.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  # ylim(0, 100)+
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(title= "Proportion of respondents by level of application of the skills gained in training", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= element_blank())
  

# Effects of the trainings on the businesses of the target groups
df_training_effect <- tibble(Effects_training= c("Increased profits","Increased profits", "Beter book keeping", "Beter book keeping", "Business is more organized than before", "Business is more organized than before", "Others", "Others"),
                             Percent_effect_training= c(56, 41, 20, 34, 22, 24, 2, 1),
                             Phase= c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"))

df_training_effect %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Effects_training, -Percent_effect_training) , y= Percent_effect_training)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0, 60)+
  labs(title= "Proportion of respondents by effects of trainings on businesses", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= "% effect of the training on businesses")
  # labs(x= element_blank() , y= "% effect of the training on businesses of the target groups")

# Percentage of respondents with mobile phones
df_phone_ownership <- tibble(Status=c("Host", "Refugees", "Host", "Refugees"),
                             Percent_phone_ownership= c(66, 61, 65, 71),
                             Phase= c("Baseline", "Baseline", "Endline", "Endline"))

df_phone_ownership %>%
  ggplot(aes(fill= Status, x= Phase, y= Percent_phone_ownership)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_phone_ownership,"%")),
            position = position_dodge2(width = 0.5), size = 5, vjust= -0.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  # ylim(0, 80)+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  labs(title= "Proportion of respondents with mobile phones", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= element_blank())
  # labs(x= element_blank() , y= "% of respondents with mobile phones")

# Key factors that prevent use of internet
df_key_factor_limit_internet_usage <- tibble(Key_factors= c("Cannot afford regular internet use", "Cannot afford regular internet use", "No limited electricity", "No limited electricity", "No limited access to computers phones", "No limited access to computers phones"),
                                             Percent_limited_internet= c(19, 34, 29, 45, 43, 51),
                                             Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_key_factor_limit_internet_usage %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Key_factors, -Percent_limited_internet) , y= Percent_limited_internet)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5)+
  geom_text(aes(label = paste0(Percent_limited_internet,"%")),
            position = position_dodge2(width = 0.5), size = 5, vjust= -0.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 11), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  # ylim(0, 60)+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())+
  labs(title= "Proportion of respondents by key factors that prevent use of internet", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= element_blank())
 

# I can always manage to solve difficult problems if I try hard enough
df_solve_problems <- tibble(Response= c("Prefer not to answer", "Prefer not to answer", "True", "True", "Somewhat true", "Somewhat true", "Somewhat false", "Somewhat false", "False", "False"),
                            Percent_response= c(4, 0, 50, 29, 21, 49, 7, 10, 18, 12),
                            Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_solve_problems %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_response) , y= Percent_response)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 60)+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "Proportion of respondents by ability to solve difficult problems", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= "% of ability to solve difficult problems")
  # labs(x= element_blank() , y= "% of ability to solve difficult problems")

# Felt little interest or pleasure in doing things
df_interest_pleasure <- tibble(Response= c("Prefer not to answer", "Prefer not to answer", "Nearly every day", "Nearly every day", "More than half the days", "More than half the days", "Several days", "Several days", "Not at all", "Not at all"),
                               Percent_response= c(2, 1, 8, 17, 17, 15, 24, 34, 49, 33),
                               Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_interest_pleasure %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_response) , y= Percent_response)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank())+
  ylim(0, 50)+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "Proportion of respondents by pleasure in doing things", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= "% of interest or pleasure in doing things")
  # labs(x= element_blank() , y= "% of interest or pleasure in doing things")

# Felt tired or had little energy
df_tired_little_energy <- tibble(Response= c("Prefer not to answer", "Prefer not to answer", "Nearly every day", "Nearly every day", "More than half the days", "More than half the days", "Several days", "Several days", "Not at all", "Not at all"),
                                 Percent_response= c(1, 1, 8, 20, 14, 14, 27, 32, 50, 33),
                                 Phase= c("Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline"))

df_tired_little_energy %>% 
  ggplot(aes(fill= Phase, x= fct_reorder(Response, -Percent_response) , y= Percent_response)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.title = element_blank())+
  scale_fill_manual(values= c("#303434", "#00aeef"))+
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.5)), text= element_text(size= 12), legend.title= element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0, 60)+
  labs(title= "Proportion of respondents by level of feeling tired or little energy", subtitle = "n baseline: 102, n endline:121", x= element_blank(), y= "% of respondents who felt tired or had little energy")
  





