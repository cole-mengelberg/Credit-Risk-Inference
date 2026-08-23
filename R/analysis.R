# =====================================================
# 1. Load Data & Packages
# =====================================================

library(tidyverse)
data <- read.csv("data/credit_risk_dataset.csv")

# =====================================================
# 2. Data Cleaning
# =====================================================

view(data)

#Check Na's
colSums(is.na(data))
sum(duplicated(data)) # 165 duplicates
data <- unique(data)
sum(!complete.cases(data)) / nrow(data) * 100 # 12% of rows have at least one missing value
sum(is.na(data)) / (nrow(data) * ncol(data)) * 100 # 1% of the data is empty

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

# Distribution of NA combinations
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

# Plot NA vs numerical features

temp_data |> 
  ggplot(aes(x = person_age, color = type, fill = type, alpha = 0.2)) +
  geom_density()

temp_data |> 
  ggplot(aes(x = person_income, color = type, fill = type, alpha = 0.2)) +
  geom_density()

temp_data |> 
  filter(type != "Both NA") |> 
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

# Statistics of NA feature groups
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

temp_data |> 
  group_by(type) |> 
  count(person_home_ownership) |> 
  mutate(proportion = n / sum(n) * 100) |> 
  arrange(type, desc(proportion)) |> 
  view()

temp_data |> 
  group_by(type) |> 
  count(loan_intent) |> 
  mutate(proportion = n / sum(n) * 100) |> 
  arrange(type, desc(proportion)) |> 
  view()

temp_data |> 
  group_by(type) |> 
  count(loan_grade) |> 
  mutate(proportion = n / sum(n) * 100) |> 
  arrange(type, desc(proportion)) |> 
  view()

temp_data |> 
  group_by(person_home_ownership) |> 
  summarize(
    median_int_rate = median(loan_int_rate, na.rm = TRUE),
    median_emp_length = median(person_emp_length, na.rm = TRUE)
  )

temp_data |> 
  group_by(loan_grade) |> 
  summarize(
    median_int_rate = median(loan_int_rate, na.rm = TRUE),
    median_emp_length = median(person_emp_length, na.rm = TRUE)
  )

temp_data |> 
  group_by(loan_intent) |> 
  summarize(
    median_int_rate = median(loan_int_rate, na.rm = TRUE),
    median_emp_length = median(person_emp_length, na.rm = TRUE),
  )

# Use linear regression to impute the missing data

lm_data <- temp_data |> 
  filter(type != "Both NA")

# Model for loan interest rate
int_rate_model <- lm(
  loan_int_rate ~
    person_age +
    person_income +
    person_home_ownership +
    loan_intent +
    loan_grade +
    loan_amnt +
    loan_percent_income +
    cb_person_cred_hist_length +
    cb_person_default_on_file +
    person_emp_length,
  data = lm_data
)

# Model for employment length
emp_model <- lm(
  person_emp_length ~
    person_age +
    person_income +
    person_home_ownership +
    loan_intent +
    loan_grade +
    loan_amnt +
    loan_percent_income +
    cb_person_cred_hist_length +
    cb_person_default_on_file +
    loan_int_rate,
  data = lm_data
)

# Fill missing values

data <- data |>
  mutate(
    loan_int_rate = if_else(
      is.na(loan_int_rate) & !is.na(person_emp_length),
      predict(
        int_rate_model,
        newdata = data
      ),
      loan_int_rate
    )
  )

data <- data |>
  mutate(
    person_emp_length = if_else(
      is.na(person_emp_length) & !is.na(loan_int_rate),
      predict(
        emp_model,
        newdata = data
      ),
      person_emp_length
    )
  )

data <- data |>
  mutate(
    loan_int_rate = if_else(
      is.na(loan_int_rate),
      median(loan_int_rate, na.rm = TRUE),
      loan_int_rate
    ),
    person_emp_length = if_else(
      is.na(person_emp_length),
      median(person_emp_length, na.rm = TRUE),
      person_emp_length
    )
  )
