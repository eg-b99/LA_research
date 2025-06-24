# BIVARIATE ANALYSIS

# Install required packages (run only if not installed)
install.packages("tidyverse")
install.packages("zoo")
install.packages("psych")
install.packages("flextable")
install.packages("GGally")

# Load required libraries
library(GGally)
library(tidyverse)
library(gtsummary)
library(gt)
library(zoo)
library(psych)
library(flextable)

# Set working directory (adjust path as needed)
setwd("/Users/macbookpro/Documents/Statistics project")

# Load cleaned datasets
gini_data <- read.csv("cleaned_interpolated_gini.csv")
democratization_data <- read.csv("cleaned_democratization.csv")

# Merge the datasets on common columns (e.g., Country and Year)
bivariate_data <- inner_join(gini_data, democratization_data, by = c("Country.Name" = "Country.Name", "Year" = "Year"))

# Rename columns for clarity
bivariate_data <- bivariate_data %>%
  rename(
    GINI_Index = GINI_Index,
    Democratization_Index = Democratization_Index
  )

# Step 0: Handle missing data
# Remove rows with NA values in key variables
bivariate_data <- bivariate_data %>% drop_na(GINI_Index, Democratization_Index)

# Step 1: Compute correlation coefficient by country
correlations_by_country <- bivariate_data %>%
  group_by(Country.Name) %>%
  summarise(
    Correlation = cor(GINI_Index, Democratization_Index, use = "complete.obs", method = "pearson")
  )
print(correlations_by_country)

# Step 2: Perform regression analysis for each country
regression_results <- bivariate_data %>%
  group_by(Country.Name) %>%
  summarise(Model = list(lm(Democratization_Index ~ GINI_Index, data = .)))

# View regression summaries for each country
for (i in seq_along(regression_results$Country.Name)) {
  country <- regression_results$Country.Name[i]
  model <- regression_results$Model[[i]]
  cat("\n---", country, "---\n")
  print(summary(model))
}

# Step 3: Create scatter plots with trend lines for each country
for (i in seq_along(regression_results$Country.Name)) {
  country <- regression_results$Country.Name[i]
  country_data <- bivariate_data %>% filter(Country.Name == country)  # Filter data for the country
  
  ggplot(country_data, aes(x = GINI_Index, y = Democratization_Index)) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    theme_minimal() +
    labs(
      title = paste("Relationship between GINI Index and Democratization Index in", country),
      x = "GINI Index",
      y = "Democratization Index"
    ) +
    print()  # Ensure the plot renders
}

# Step 4: Compute overall correlation coefficient for Latin America
overall_correlation <- cor(bivariate_data$GINI_Index, bivariate_data$Democratization_Index, use = "complete.obs", method = "pearson")
print(paste("Overall Pearson correlation for Latin America: ", round(overall_correlation, 2)))

# Step 5: Perform regression analysis for all of Latin America
overall_model <- lm(Democratization_Index ~ GINI_Index, data = bivariate_data)
summary(overall_model)

# Step 6: Create scatter plot with trend line for all Latin America
ggplot(bivariate_data, aes(x = GINI_Index, y = Democratization_Index)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Relationship between GINI Index and Democratization Index in Latin America",
    x = "GINI Index",
    y = "Democratization Index"
  )

# Step 7: Diagnostic plots for each country's regression
for (i in seq_along(regression_results$Country.Name)) {
  country <- regression_results$Country.Name[i]
  model <- regression_results$Model[[i]]
  
  png(paste0("diagnostic_plot_", country, ".png"))  # Save each plot
  par(mfrow = c(2, 2))
  plot(model, main = paste("Diagnostics for", country))
  par(mfrow = c(1, 1))  # Reset plot layout
  dev.off()
}


# Step 8: Diagnostic plots for overall regression
png("overall_model_diagnostics.png")  # Save diagnostic plots as a PNG
par(mfrow = c(2, 2))  # Arrange plots in 2x2 grid
plot(overall_model, main = "Diagnostics for Overall Model")
par(mfrow = c(1, 1))  # Reset plot layout
dev.off()  # Close the graphics device


# Step 9: Save merged and cleaned dataset for future reference
write.csv(bivariate_data, "bivariate_gini_democratization_cleaned.csv", row.names = FALSE)
