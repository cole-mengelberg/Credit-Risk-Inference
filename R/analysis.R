library(tidyverse)

data <- read.csv("data/credit_risk_dataset.csv")
               
view(data)
#-----------------------------------------------------
# Data Cleaning
#-----------------------------------------------------

#Check Na's
colSums(is.na(data))
sum(is.na(data)) / count(data) * 100

#-----------------------------------------------------
# Try to find pattern to fill Na's
#-----------------------------------------------------

# Create feature to distinguish NA types
temp_data <-
  data |>
    mutate(
      type = case_when(
        is.na(loan_int_rate) & !is.na(person_emp_length) ~ "loan_int_rate NA",
        !is.na(loan_int_rate) & is.na(person_emp_length) ~ "person_emp_length NA",
        is.na(loan_int_rate) & is.na(person_emp_length) ~ "Both NA",
        TRUE ~ "Neither NA"
      )
  )

temp_data|> 
  group_by(type) |> 
  summarise(
    n = n(),
    prop = n / nrow(data) * 100
  ) |> 
  arrange(desc(prop))

get_mode <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

options(tibble.width = Inf)
temp_data |> 
  group_by(type) |> 
  summarise(
    median_age = median(person_age),
    median_income = median(person_income),
    mode_home_ownership = get_mode(person_home_ownership),
    mode_loan_intent = get_mode(loan_intent),
    mode_loan_grade = get_mode(loan_grade),
    median_loan_amount = median(loan_amnt),
    median_loan_percent_income = median(loan_percent_income),
    mode_default_on_file = get_mode(cb_person_default_on_file),
    median_hist_length = median(cb_person_cred_hist_length)
  ) |> 
  view()
# Plot NA vs numerical features

temp_data |> 
  ggplot(aes(x = person_age, color = type, fill = type, alpha = 0.2)) +
  geom_density()

temp_data |> 
  ggplot(aes(x = person_income, color = type, fill = type, alpha = 0.2)) +
  geom_density()

temp_data |> 
  ggplot(aes(x = loan_amnt, color = type, fill = type, alpha = 0.2)) +
  geom_density()

temp_data |> 
  ggplot(aes(x = loan_percent_income, color = type, fill = type, alpha = 0.2)) +
  geom_density()

temp_data |> 
  ggplot(aes(x = cb_person_cred_hist_length, color = type, fill = type, alpha = 0.2)) +
  geom_density()

# Plot NA vs categorical features

temp_data |> 
  ggplot(aes(x = type, color = factor(person_home_ownership), fill = factor(person_home_ownership))) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = factor(loan_intent), fill = factor(loan_intent))) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = factor(loan_status), fill = factor(loan_status))) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = factor(loan_grade), fill = factor(loan_grade))) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = factor(cb_person_default_on_file), fill = factor(cb_person_default_on_file))) +
  geom_bar(position = "fill")

