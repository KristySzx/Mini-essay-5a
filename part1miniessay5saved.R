# Load required libraries
library(tibble)
library(dplyr)
library(ggplot2)

# Simulation Parameters
num_days <- 100
names <- c("Matt", "Ash", "Jacki", "Rol", "Mike")
couple <- c("Matt", "Jacki")
couple_correlation <- 0.8

# Simulate Data with tibble
simulated_data <- tibble(
  Day = rep(1:num_days, length(names)),
  Reader = rep(names, each = num_days),
  Pages_Read = ifelse(Reader %in% couple,
                      rnorm(num_days, mean = 20, sd = 5),
                      rnorm(num_days, mean = 15, sd = 3))
)

# Ensure positive correlation for the couple
if ("Matt" %in% couple && "Jacki" %in% couple) {
  simulated_data$Pages_Read[simulated_data$Reader == "Jacki"] <-
    couple_correlation * simulated_data$Pages_Read[simulated_data$Reader == "Matt"] +
    sqrt(1 - couple_correlation^2) * rnorm(num_days, mean = 0, sd = 1)
}
# Plot the data
library(ggplot2)
ggplot(simulated_data, aes(x = Day, y = Pages_Read, color = Reader)) +
  geom_line() +
  labs(title = "Pages Read Over 100 Days",
       x = "Day",
       y = "Pages Read",
       color = "Reader") +
  theme_minimal()

# Tests
# Test 1: Boundary Conditions
# Check if the number of days is exactly 100
if (nrow(simulated_data) == 100) {
  cat("Test 1 (Boundary Conditions): Number of observations is 100.\n")
} else {
  cat("Test 1 (Boundary Conditions): Number of observations is not 100.\n")
}

# Test 2: Classes
# Check if the data types are as expected
if (all(sapply(simulated_data, class) == c("integer", "character", "numeric"))) {
  cat("Test 2 (Classes): Data types are as expected.\n")
} else {
  cat("Test 2 (Classes): Data types are not as expected.\n")
}

# Test 3: Missing Data
# Check for missing values
if (sum(is.na(simulated_data)) == 0) {
  cat("Test 3 (Missing Data): No missing values.\n")
} else {
  cat("Test 3 (Missing Data): Missing values present.\n")
}

# Test 4: Duplicates
# Check for duplicate rows
if (any(duplicated(simulated_data))) {
  cat("Test 4 (Duplicates): Duplicates present.\n")
} else {
  cat("Test 4 (Duplicates): No duplicates.\n")
}

# Test 5: Regression Results
# Perform linear regression between Matt and Jacki
lm_result <- lm(Pages_Read ~ Reader, data = simulated_data %>% filter(Reader %in% couple))
summary(lm_result)
