# Import readr, janitor, and dplyr to use in code

library(readr)
library(janitor)
library(dplyr)

# Keeps the numeric data from using scientific notation

options(scipen = 999)

# Read CSV file into a variable named data

data <- read_csv("data/salaries.csv")

# Create a clean copy of data

clean_data <- data
clean_data <- clean_names(clean_data)

# Add new column named employee_id and add it to the front of the table

clean_data <- clean_data %>%
    mutate(employee_id = row_number()) %>%
    select(employee_id, everything())

# Remove rows with missing data by using na.omit

clean_data <- na.omit(clean_data)

# Quick summary of the first rows, the structure of the dataframe, and the summary of the numerical statistics
head(data)
str(data)
summary(data)

# Imput mean values if salary is NA is present for any of the rows

mean_salary <- mean(clean_data$salary, na.rm = TRUE)
data$salary <- ifelse(is.na(data$salary), mean(data$salary, na.rm = TRUE), data$salary)

# Convert data types depending on the data itself
# Those may include: date, factor, and numeric 

clean_data$work_year <- as.numeric(clean_data$work_year)
clean_data$experience_level <- as.factor(clean_data$experience_level)
clean_data$employment_type <- as.factor(clean_data$employment_type)
clean_data$job_title <- as.factor(clean_data$job_title)
clean_data$salary <- as.numeric(clean_data$salary)
clean_data$salary_currency <- as.factor(clean_data$salary_currency)
clean_data$salary_in_usd <- as.numeric(clean_data$salary_in_usd)
clean_data$employee_residence <- as.factor(clean_data$employee_residence)
clean_data$remote_ratio <- as.numeric(clean_data$remote_ratio)
clean_data$company_location <- as.factor(clean_data$company_location)
clean_data$company_size <- as.factor(clean_data$company_size)

#Print changes so they are visible to the user
str(clean_data)

# Remove any duplicates of data that could affect the integrity

clean_data <- clean_data[!duplicated(clean_data), ]

# Loops through the data to find the standard deviation of the salaries

for (col_name in c("salary")) {
    std_dev <- sd(clean_data[[col_name]])
    cat("Standard deviation of", col_name, "is:", round(std_dev, 2), "\n")
    }

# Loops through the data to find the lowest salaries

# Sort dataframe by salary (ascending)
lowest_salaries <- clean_data[order(clean_data$salary), ]

# Get top 5
top5_lowest <- head(lowest_salaries, 5)

# Display
cat("\nTop 5 Lowest Salaries:\n")
for (i in 1:nrow(top5_lowest)) {
    cat(top5_lowest$job_title[i], "- $", formatC(top5_lowest$salary[i], format = "f", big.mark = ",", digits = 0), "\n")
    }

# Loops through the data to find the top highest salaries

# Sort dataframe by salary (descending)
highest_salaries <- clean_data[order(-clean_data$salary), ]

# Get top 5
top5_highest <- head(highest_salaries, 5)

# Display
cat("\nTop 5 Highest Salaries:\n")
for (i in 1:nrow(top5_highest)) {
    cat(top5_highest$job_title[i], "- $", formatC(top5_highest$salary[i], format = "f", big.mark = ",", digits = 0), "\n")
    }

# Get the top 5 employee locations by %

# Count number of employees per country
location_counts <- table(clean_data$employee_residence)

# Sort in descending order
sorted_locations <- sort(location_counts, decreasing = TRUE)

# Get top 5
top5_locations <- head(sorted_locations, 5)

# Convert to percentage
total_employees <- sum(location_counts)
percentages <- round((top5_locations / total_employees) * 100, 2)

# Display
cat("Top 5 Employee Locations by Percentage:\n")
for (i in 1:length(percentages)) {
    cat(names(percentages)[i], ":", percentages[i], "%\n")
    }

# Save clean file to a new csv file with updated information

write.csv(clean_data, "data/cleaned_salaries.csv", row.names = FALSE)
