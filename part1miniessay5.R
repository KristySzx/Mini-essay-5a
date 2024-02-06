# Load required library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Simulate the dataset
days <- 1:100
readers <- c("Matt", "Ash", "Jacki", "Rol", "Mike")

# Simulate correlated data for Matt and Jacki
matt_pages <- rnorm(100, mean = 20, sd = 5)
jacki_pages <- matt_pages + rnorm(100, mean = 5, sd = 2)

# Simulate independent data for Ash, Rol, and Mike
ash_pages <- rnorm(100, mean = 15, sd = 3)
rol_pages <- rnorm(100, mean = 10, sd = 2)
mike_pages <- rnorm(100, mean = 18, sd = 4)

# Create the simulated dataset
simulated_data <- data.frame(
  Day = rep(days, 5),
  Reader = rep(readers, each = 100),
  Pages_Read = c(matt_pages, ash_pages, jacki_pages, rol_pages, mike_pages)
)

# Plot the data
library(ggplot2)
ggplot(simulated_data, aes(x = Day, y = Pages_Read, color = Reader)) +
  geom_line() +
  labs(title = "Pages Read Over 100 Days",
       x = "Day",
       y = "Pages Read",
       color = "Reader") +
  theme_minimal()

# Test for correlation between Matt and Jacki
correlation <- cor(simulated_data$Pages_Read[simulated_data$Reader == "Matt"],
                   simulated_data$Pages_Read[simulated_data$Reader == "Jacki"])
cat("Correlation between Matt and Jacki:", correlation, "\n")

# Test for independence between Ash, Rol, and Mike
independence_test <- cor.test(simulated_data$Pages_Read[simulated_data$Reader == "Ash"],
                              simulated_data$Pages_Read[simulated_data$Reader == "Rol"])
cat("Independence test between Ash and Rol:\n", independence_test, "\n")

# Calculate average pages read by each student
avg_pages_read <- simulated_data %>%
  group_by(Reader) %>%
  summarise(avg_pages = mean(Pages_Read))
print(avg_pages_read)


