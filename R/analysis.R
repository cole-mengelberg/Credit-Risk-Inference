# =====================================================
# 1. Load Data & Packages
# =====================================================

library(tidyverse)
library(Metrics)
library(DescTools)
data <- read.csv("data/credit_risk_dataset.csv")

set.seed(42)
# =====================================================
# 2. Data Cleaning
# =====================================================

View(data)
glimpse(data)

# Change Data Types
data <- data |> 
  mutate(
    person_home_ownership = factor(person_home_ownership),
    loan_intent = factor(loan_intent),
    loan_grade = factor(loan_grade),
    cb_person_default_on_file = factor(cb_person_default_on_file),
    loan_status = factor(loan_status)
  )

# =====================================================
# 3. Outliers
# =====================================================

graph_boxplot <- function(df, col) {
  df |> 
    ggplot(aes(y = {{col}})) +
    geom_boxplot()
}

remove_outlier <- function(df, col, cutoff) {
  df |> 
    filter(is.na({{col}}) | {{col}} < cutoff)
}

graph_boxplot(data, person_age) +
  geom_hline(yintercept = 100)
data <- remove_outlier(data, person_age, 100)

graph_boxplot(data, person_income) +
  geom_hline(yintercept = 1000000)
data <- remove_outlier(data, person_income, 1000000)

graph_boxplot(data, person_emp_length) +
  geom_hline(yintercept = 50)
data <- remove_outlier(data, person_emp_length, 50)

graph_boxplot(data, loan_amnt)
graph_boxplot(data, loan_int_rate)
graph_boxplot(data, loan_percent_income)
graph_boxplot(data, cb_person_cred_hist_length)

# =====================================================
# 4. Missing Values
# =====================================================

#Check Na's
sum(is.na(data)) # 4008 NA values
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
  ggplot(aes(x = type, color = person_home_ownership, fill = person_home_ownership)) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = loan_intent, fill = loan_intent)) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = loan_status, fill = loan_status)) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = loan_grade, fill = loan_grade)) +
  geom_bar(position = "fill")

temp_data |> 
  ggplot(aes(x = type, color = cb_person_default_on_file, fill = cb_person_default_on_file)) +
  geom_bar(position = "fill")

# Statistics of NA feature groups
temp_data |> 
  group_by(type) |> 
  summarise(
    median_age = median(person_age, na.rm = TRUE),
    median_income = median(person_income, na.rm = TRUE),
    mode_home_ownership = Mode(person_home_ownership, na.rm = TRUE),
    mode_loan_intent = Mode(loan_intent, na.rm = TRUE),
    mode_loan_grade = Mode(loan_grade, na.rm = TRUE),
    median_loan_amount = median(loan_amnt, na.rm = TRUE),
    median_loan_percent_income = median(loan_percent_income, na.rm = TRUE),
    mode_default_on_file = Mode(cb_person_default_on_file, na.rm = TRUE),
    median_hist_length = median(cb_person_cred_hist_length, na.rm = TRUE)
  ) |> 
  View()

temp_data |> 
  group_by(type) |> 
  count(person_home_ownership) |> 
  mutate(proportion = n / sum(n) * 100) |> 
  arrange(type, desc(proportion)) |> 
  View()

temp_data |> 
  group_by(type) |> 
  count(loan_intent) |> 
  mutate(proportion = n / sum(n) * 100) |> 
  arrange(type, desc(proportion)) |> 
  View()

temp_data |> 
  group_by(type) |> 
  count(loan_grade) |> 
  mutate(proportion = n / sum(n) * 100) |> 
  arrange(type, desc(proportion)) |> 
  View()

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

# =====================================================
# 5. Imputing Using Linear Regression
# =====================================================

lm_data <- temp_data |> 
  filter(type == "Neither NA")

train_index <- sample(
  seq_len(nrow(lm_data)),
  size = 0.8 * nrow(lm_data)
)

train_data <- lm_data[train_index, ]
test_data <- lm_data[-train_index, ]

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
  data = train_data
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
  data = train_data
)

# Evaluating the linear regression models
summary(int_rate_model) # R^2 of about 0.9
summary(emp_model) # R^2 of about 0.1

# Check RMSE, MAE for the loan interest rate model
predictions <- predict(
  int_rate_model,
  newdata = test_data
)

rmse(test_data$loan_int_rate, predictions) # error of 1.011172
mae(test_data$loan_int_rate, predictions) # error of 0.7893877

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

sum(!complete.cases(data)) / nrow(data) * 100
data <- data |> 
  drop_na()
sum(!complete.cases(data)) / nrow(data) * 100
