library(tidyverse)
library(readxl)
library(janitor)
library(plotly)


data <- read_csv("2023-10-01.csv") %>% 
  clean_names() %>% 
  select(time, payee,transaction_type, description, up_category=category,total=total_aud) %>% 
  mutate(category = case_when(
    transaction_type == "Salary" ~ "Income",
    up_category == "Groceries" ~ "Need",
    up_category == "Rent & Mortgage" ~ "Need",
    up_category == "Rates & Insurance" ~ "Need",
    up_category == "Utilities" ~ "Need",
    up_category == "Public transport" ~ "Need",
    up_category == "Car Insurance, Rego & Maintenance" ~ "Need",
    TRUE ~ "Want"
  ))

data_test <- data %>% 
  filter(transaction_type == "Transfer")


################################################################################
################################ CHART DATA ####################################
################################################################################


income <- data %>% 
  filter(category == "Income") %>% 
  summarise(income = sum(total)) %>% 
  pull(income)

needs <- data %>% 
  filter(category == "Need") %>% 
  summarise(needs = sum(total) * -1) %>% 
  pull(needs)

wants <- data %>% 
  filter(category == "Want") %>% 
  summarise(wants = sum(total) * -1) %>% 
  pull(wants)

# savings <- data %>% 
#   filter(category == "Savings") %>% 
#   summarise(wants = sum(total) * -1) %>% 
#   pull(wants)

# savings_residual <- data.frame(income,needs,wants,savings) %>% 
#   mutate(residuals = income - needs - wants - savings)

chart_data <- data.frame(income,needs,wants) %>% 
  mutate(savings = income - needs - wants) %>%
  pivot_longer(everything()) %>% 
  mutate(share = round(value / income * 100)) %>% 
  mutate(name = as_factor(name)) %>% 
  mutate(name = fct_relevel(name, c("income", "needs", "wants","savings"))) %>% 
  arrange(name) %>% 
  mutate(target_value = case_when(
    
    name == "income" ~ NA_real_,
    name == "needs" ~ income * 0.5,
    name %in% c("wants", "savings") ~ income * 0.25
    
  ))
  
ggplot(chart_data, aes(x = reorder(name, -share), share, fill = name)) + 
  geom_col() + theme_minimal() 

################################################################################
############################# TABLES ###########################################
################################################################################


needs_table_data <- data %>% 
  filter(category == "Need") %>% 
  group_by(up_category, payee) %>% 
  summarise(total = sum(total)) %>% 
  arrange(total)

wants_table_data <- data %>% 
  filter(category == "Want") %>% 
  group_by(payee) %>% 
  summarise(total = sum(total)) %>% 
  arrange(total)



