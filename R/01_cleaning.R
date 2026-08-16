library(tidyverse)

data <- read.csv("data/credit_risk_dataset.csv")
               
view(data)

#Check Na's
colSums(is.na(data))

#-----------------------------------------------------
# Create feature to distinguish NA types
#-----------------------------------------------------
temp_data <-
  data |> mutate(
    type = case_when(
      is.na(loan_int_rate) & !is.na(person_emp_length) ~ "loan_int_rate NA",
      !is.na(loan_int_rate) & is.na(person_emp_length) ~ "person_emp_length NA",
      is.na(loan_int_rate) & is.na(person_emp_length) ~ "Both NA",
      !is.na(loan_int_rate) & !is.na(person_emp_length) ~ "Neither NA",
    )
  )

temp_data|> 
  group_by(type) |> 
  summarise(
    n = n(),
    prop = n / nrow(data) * 100
  ) |> 
  arrange(desc(prop))


temp_data <- data |> 
  mutate(
    type = case_when(
      is.na(loan_int_rate) & !is.na(person_emp_length) ~ "loan_int_rate NA",
      #!is.na(loan_int_rate) & is.na(person_emp_length) ~ "person_emp_length NA",
      is.na(loan_int_rate) & is.na(person_emp_length) ~ "Both NA",
      !is.na(loan_int_rate) & !is.na(person_emp_length) ~ "Neither NA",
      )
    ) |> 
  filter(!is.na(type))

temp_data |> 
  ggplot(aes(x = person_age, color = type, fill = type, alpha = 0.2)) +
  geom_density()
  
temp_data |> 
  ggplot(aes(x = type, color = factor(loan_status), fill = factor(loan_status))) +
  geom_bar(position = "fill")

temp_data |> 
  group_by(type, loan_status) |> 
  count()
