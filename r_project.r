# Load required libraries
library(readr)
library(janitor)
library(dplyr)

# Avoid scientific notation for large numbers
options(scipen = 999)

# Read the dataset
data <- read_csv("data/salaries.csv")

# Clean and format the column names
clean_data <- data %>%
  clean_names()

# Add employee_id and move it to the front
clean_data <- clean_data %>%
  mutate(employee_id = row_number()) %>%
  select(employee_id, everything())

# Remove rows with missing values
clean_data <- na.omit(clean_data)

# Preview data
head(data)
str(data)
summary(data)

# Impute missing salary values in original dataset (not clean_data)
mean_salary <- mean(clean_data$salary, na.rm = TRUE)
data$salary <- ifelse(is.na(data$salary), mean_salary, data$salary)

# Convert column types appropriately
clean_data <- clean_data %>%
  mutate(
    work_year = as.numeric(work_year),
    experience_level = as.factor(experience_level),
    employment_type = as.factor(employment_type),
    job_title = as.factor(job_title),
    salary = as.numeric(salary),
    salary_currency = as.factor(salary_currency),
    salary_in_usd = as.numeric(salary_in_usd),
    employee_residence = as.factor(employee_residence),
    remote_ratio = as.numeric(remote_ratio),
    company_location = as.factor(company_location),
    company_size = as.factor(company_size)
  )

# Show structure of cleaned data
str(clean_data)

# Remove duplicates
clean_data <- clean_data[!duplicated(clean_data), ]

# Compute standard deviation of salaries
for (col_name in c("salary")) {
  std_dev <- sd(clean_data[[col_name]])
  cat("Standard deviation of", col_name, "is:", round(std_dev, 2), "\n")
}

# Find top 5 lowest salaries
lowest_salaries <- clean_data[order(clean_data$salary), ]
top5_lowest <- head(lowest_salaries, 5)

cat("\nTop 5 Lowest Salaries:\n")
for (i in seq_len(nrow(top5_lowest))) {
  cat(
    top5_lowest$job_title[i], "- $",
    formatC(top5_lowest$salary[i], format = "f", big.mark = ",", digits = 0),
    "\n"
  )
}

# Find top 5 highest salaries
highest_salaries <- clean_data[order(-clean_data$salary), ]
top5_highest <- head(highest_salaries, 5)

cat("\nTop 5 Highest Salaries:\n")
for (i in seq_len(nrow(top5_highest))) {
  cat(
    top5_highest$job_title[i], "- $",
    formatC(top5_highest$salary[i], format = "f", big.mark = ",", digits = 0),
    "\n"
  )
}

# Calculate top 5 employee locations by percentage
location_counts <- table(clean_data$employee_residence)
sorted_locations <- sort(location_counts, decreasing = TRUE)
top5_locations <- head(sorted_locations, 5)

total_employees <- sum(location_counts)
percentages <- round((top5_locations / total_employees) * 100, 2)

cat("Top 5 Employee Locations by Percentage:\n")
for (i in seq_along(percentages)) {
  cat(names(percentages)[i], ":", percentages[i], "%\n")
}

# Additional analysis: average salary by company size
avg_salary_by_company_size <- clean_data %>%
  group_by(company_size) %>%
  summarize(avg_salary = round(mean(salary, na.rm = TRUE), 2)) %>%
  arrange(desc(avg_salary))

cat("\nAverage Salary by Company Size:\n")
print(as.data.frame(avg_salary_by_company_size))

# Additional analysis: average salary by job title
top_avg_job_titles <- clean_data %>%
  group_by(job_title) %>%
  summarize(avg_salary = mean(salary, na.rm = TRUE)) %>%
  arrange(desc(avg_salary)) %>%
  head(5)

cat("\nTop 5 Job Titles by Average Salary:\n")
print(as.data.frame(top_avg_job_titles))

# Save cleaned data to new CSV file
write.csv(clean_data, "data/cleaned_salaries.csv", row.names = FALSE)
