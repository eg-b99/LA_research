
install.packages("tidyverse")
install.packages("zoo")
install.packages("psych")
install.packages("flextable")


library(tidyverse)
library(gtsummary)
library(gt)
library(zoo)
library(psych)
library(flextable)


setwd("/Users/macbookpro/Documents/Statistics project")


data <- read.csv("gini_world.csv", header = TRUE, skip = 4, sep = ",")


colnames(data) <- make.names(colnames(data), unique = TRUE)


latin_america_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Cuba", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Uruguay", "Venezuela"
)


gini_latin_america <- data %>%
  filter(Country.Name %in% latin_america_countries)


gini_latin_america_long <- gini_latin_america %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GINI_Index"
  ) %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year))  # Convert Year column to numeric
  )

# Interpolating missing values
gini_latin_america_long <- gini_latin_america_long %>%
  group_by(Country.Name) %>%
  arrange(Year) %>%
  mutate(GINI_Index = zoo::na.approx(GINI_Index, na.rm = FALSE))  

# Excluding countries with no valid data
gini_latin_america_long_clean <- gini_latin_america_long %>%
  group_by(Country.Name) %>%
  filter(any(!is.na(GINI_Index)))


descriptive_stats <- gini_latin_america_long_clean %>%
  summarise(
    Mean_GINI = mean(GINI_Index, na.rm = TRUE),
    Median_GINI = median(GINI_Index, na.rm = TRUE),
    SD_GINI = sd(GINI_Index, na.rm = TRUE),
    Min_GINI = min(GINI_Index, na.rm = TRUE),
    Max_GINI = max(GINI_Index, na.rm = TRUE),
    Skewness_GINI = psych::skew(GINI_Index, na.rm = TRUE),
    Kurtosis_GINI = psych::kurtosi(GINI_Index, na.rm = TRUE)
  )


print(descriptive_stats)


ggplot(gini_latin_america_long_clean, aes(x = Country.Name, y = GINI_Index)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  coord_flip() +  
  labs(title = "GINI Index by Country", x = "Country", y = "GINI Index")


ggplot(gini_latin_america_long_clean, aes(x = GINI_Index)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density Plot of GINI Index", x = "GINI Index", y = "Density")


ggplot(gini_latin_america_long_clean, aes(x = GINI_Index)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  theme_minimal() +
  facet_wrap(~ Country.Name, scales = "free_y") +
  labs(title = "GINI Index Distribution by Country", x = "GINI Index", y = "Frequency")


ggplot(gini_latin_america_long_clean, aes(x = Year, y = GINI_Index, color = Country.Name, group = Country.Name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "GINI Index Over Time by Country", x = "Year", y = "GINI Index") +
  theme(legend.position = "bottom")


ggplot(gini_latin_america_long_clean, aes(x = Year, y = GINI_Index, color = Country.Name, group = Country.Name)) +
  geom_line(size = 1.2) +                             
  geom_point(size = 2, shape = 21, fill = "white") +  
  theme_minimal() +                                   
  labs(
    title = "Trends in GINI Index Across Latin American Countries",
    subtitle = "Data interpolated for missing years",
    caption = "Source: World Bank, GINI Index Dataset",
    x = "Year",
    y = "GINI Index"
  ) +
  scale_color_brewer(palette = "Dark2") +             
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),            
    plot.caption = element_text(size = 10, face = "italic"),         
    legend.position = "bottom",                                      
    legend.title = element_blank(),                                  
    legend.text = element_text(size = 10),                           
    axis.text.x = element_text(angle = 45, hjust = 1),               
    axis.title.x = element_text(size = 12, face = "bold"),           
    axis.title.y = element_text(size = 12, face = "bold"),           
    panel.grid.major = element_line(color = "gray90")                
  ) +
  guides(
    color = guide_legend(ncol = 3)                                   
  )



write.csv(gini_latin_america_long_clean, "cleaned_interpolated_gini.csv", row.names = FALSE)




# DEMOCRATIZATION 

setwd("/Users/macbookpro/Documents/Statistics project")


democratization_data <- read.csv("index_democratization.csv", sep = ";", dec = ",")


democratization_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Dominican Republic", "Ecuador", "El Salvador", "Guatemala",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Uruguay", "Venezuela"
)

democratization_filtered <- democratization_data %>%
  filter(cname %in% democratization_countries)


democratization_long <- democratization_filtered %>%
  select(cname, year, van_index) %>%
  rename(Country.Name = cname, Year = year, Democratization_Index = van_index) %>%
  mutate(Year = as.numeric(Year))


democratization_long_clean <- democratization_long %>%
  group_by(Country.Name) %>%
  arrange(Year)


democratization_stats <- democratization_long_clean %>%
  summarise(
    Mean_Index = mean(Democratization_Index, na.rm = TRUE),
    Median_Index = median(Democratization_Index, na.rm = TRUE),
    SD_Index = sd(Democratization_Index, na.rm = TRUE),
    Min_Index = min(Democratization_Index, na.rm = TRUE),
    Max_Index = max(Democratization_Index, na.rm = TRUE),
    Skewness_Index = psych::skew(Democratization_Index, na.rm = TRUE),
    Kurtosis_Index = psych::kurtosi(Democratization_Index, na.rm = TRUE)
  )


print(democratization_stats)



ggplot(democratization_long_clean, aes(x = Country.Name, y = Democratization_Index)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Democratization Index by Country", x = "Country", y = "Democratization Index")


ggplot(democratization_long_clean, aes(x = Democratization_Index)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density Plot of Democratization Index", x = "Democratization Index", y = "Density")


ggplot(democratization_long_clean, aes(x = Democratization_Index)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  theme_minimal() +
  facet_wrap(~ Country.Name, scales = "free_y") +
  labs(title = "Democratization Index Distribution by Country", x = "Democratization Index", y = "Frequency")


ggplot(democratization_long_clean, aes(x = Year, y = Democratization_Index, color = Country.Name, group = Country.Name)) +
  geom_line(size = 1) +                             
  geom_point(size = 3, shape = 22, fill = "yellow", color = "black") +  
  theme_minimal() +                                 
  labs(
    title = "Trends in Democratization Index Across Latin American Countries",
    subtitle = "Detailed yearly trends",
    caption = "Source: Democratization Dataset",
    x = "Year",
    y = "Democratization Index"
  ) +
  scale_color_manual(values = rainbow(length(unique(democratization_long_clean$Country.Name)))) +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),             
    plot.caption = element_text(size = 10, face = "italic"),          
    legend.position = "right",                                       
    legend.title = element_blank(),                                   
    legend.text = element_text(size = 8),                             
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),     
    axis.text.y = element_text(size = 10),                            
    axis.title.x = element_text(size = 12, face = "bold"),            
    axis.title.y = element_text(size = 12, face = "bold"),            
    panel.grid.major = element_line(color = "gray90")                 
  ) +
  guides(
    color = guide_legend(ncol = 2)                                    
  )


write.csv(democratization_long_clean, "cleaned_democratization.csv", row.names = FALSE)


#EDUCATION SPENDINGS AS GDP PERCENTAGE


education_data <- read.csv("Education_expenditure_pGDP.csv", sep = ";", dec = ",")


latin_america_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Dominican Republic", "Ecuador", "El Salvador", "Guatemala",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Uruguay", "Venezuela"
)

education_latin_america <- education_data %>%
  filter(cname %in% latin_america_countries) %>%
  select(cname, year, wdi_expedu) %>%
  rename(Country = cname, Year = year, Education_Expenditure = wdi_expedu)


descriptive_stats <- education_latin_america %>%
  group_by(Country) %>%
  summarise(
    Mean_Expenditure = mean(Education_Expenditure, na.rm = TRUE),
    Median_Expenditure = median(Education_Expenditure, na.rm = TRUE),
    SD_Expenditure = sd(Education_Expenditure, na.rm = TRUE),
    Min_Expenditure = min(Education_Expenditure, na.rm = TRUE),
    Max_Expenditure = max(Education_Expenditure, na.rm = TRUE)
  )


print(descriptive_stats)



ggplot(education_latin_america, aes(x = Country, y = Education_Expenditure)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Education Expenditure by Country", x = "Country", y = "Education Expenditure (% of GDP)")


ggplot(education_latin_america, aes(x = Education_Expenditure)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density Plot of Education Expenditure", x = "Education Expenditure (% of GDP)", y = "Density")


ggplot(education_latin_america, aes(x = Education_Expenditure)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "Education Expenditure Distribution by Country", x = "Education Expenditure (% of GDP)", y = "Frequency")


ggplot(education_latin_america, aes(x = Year, y = Education_Expenditure, color = Country, group = Country)) +
  geom_line(size = 1) +                             
  geom_point(size = 3, shape = 22, fill = "yellow", color = "black") +  
  theme_minimal() +                                 
  labs(
    title = "Trends in Education Expenditure Across Latin American Countries",
    subtitle = "Detailed yearly trends",
    caption = "Source: Education Expenditure Dataset",
    x = "Year",
    y = "Education Expenditure (% of GDP)"
  ) +
  scale_color_manual(values = rainbow(length(unique(education_latin_america$Country)))) +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),             
    plot.caption = element_text(size = 10, face = "italic"),          
    legend.position = "right",                                       
    legend.title = element_blank(),                                   
    legend.text = element_text(size = 8),                             
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),     
    axis.text.y = element_text(size = 10),                            
    axis.title.x = element_text(size = 12, face = "bold"),            
    axis.title.y = element_text(size = 12, face = "bold"),            
    panel.grid.major = element_line(color = "gray90")                 
  ) +
  guides(
    color = guide_legend(ncol = 2)                                    
  )


custom_colors <- c(
  "Argentina" = "#1f77b4", "Bolivia" = "#ff7f0e", "Brazil" = "#2ca02c",
  "Chile" = "#d62728", "Colombia" = "#9467bd", "Costa Rica" = "#8c564b",
  "Dominican Republic" = "#e377c2", "Ecuador" = "#7f7f7f",
  "El Salvador" = "#bcbd22", "Guatemala" = "#17becf",
  "Honduras" = "#393b79", "Mexico" = "#637939", "Nicaragua" = "#8c6d31",
  "Panama" = "#843c39", "Paraguay" = "#ad494a", "Peru" = "#d6616b",
  "Uruguay" = "#6b6ecf", "Venezuela" = "#ce6dbd"
)


ggplot(education_latin_america, aes(x = Year, y = Education_Expenditure, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 22, fill = "yellow", color = "black") +
  theme_minimal() +
  labs(
    title = "Trends in Education Expenditure Across Latin American Countries",
    subtitle = "Detailed yearly trends",
    caption = "Source: Education Expenditure Dataset",
    x = "Year",
    y = "Education Expenditure (% of GDP)"
  ) +
  scale_color_manual(values = custom_colors) +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90")
  ) +
  guides(
    color = guide_legend(ncol = 2)
  )

write.csv(education_latin_america, "cleaned_education_expenditure_new.csv", row.names = FALSE)


#GDP PER CAPITA



install.packages("tidyverse")
install.packages("psych")
install.packages("ggplot2")
install.packages("zoo")
install.packages("flextable")


library(tidyverse)
library(gtsummary)
library(gt)
library(zoo)
library(psych)
library(flextable)
library(ggplot2)

setwd("/Users/macbookpro/Documents/Statistics project")


gdp_data <- read.csv("GDP_per_Capita_current_prices.csv", sep = ";", dec = ",")


latin_america_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Dominican Republic", "Ecuador", "El Salvador", "Guatemala",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Uruguay", "Venezuela"
)

gdp_latin_america <- gdp_data %>%
  filter(cname %in% latin_america_countries) %>%
  select(cname, year, gle_cgdpc) %>%
  rename(Country = cname, Year = year, GDP_per_Capita = gle_cgdpc)


descriptive_stats <- gdp_latin_america %>%
  group_by(Country) %>%
  summarise(
    Mean_GDP = mean(GDP_per_Capita, na.rm = TRUE),
    Median_GDP = median(GDP_per_Capita, na.rm = TRUE),
    SD_GDP = sd(GDP_per_Capita, na.rm = TRUE),
    Min_GDP = min(GDP_per_Capita, na.rm = TRUE),
    Max_GDP = max(GDP_per_Capita, na.rm = TRUE)
  )


print(descriptive_stats)

ggplot(gdp_latin_america, aes(x = Country, y = GDP_per_Capita)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "GDP per Capita by Country", x = "Country", y = "GDP per Capita (Current Prices)")


ggplot(gdp_latin_america, aes(x = GDP_per_Capita)) +
  geom_density(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Density Plot of GDP per Capita", x = "GDP per Capita (Current Prices)", y = "Density")


ggplot(gdp_latin_america, aes(x = GDP_per_Capita)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  theme_minimal() +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "GDP per Capita Distribution by Country", x = "GDP per Capita (Current Prices)", y = "Frequency")


ggplot(gdp_latin_america, aes(x = Year, y = GDP_per_Capita, color = Country, group = Country)) +
  geom_line(size = 1) +   
  geom_point(size = 3, shape = 22, fill = "yellow", color = "black") +
  theme_minimal() +                                 
  labs(
    title = "Trends in GDP per Capita Across Latin American Countries",
    subtitle = "Detailed yearly trends",
    caption = "Source: GDP per Capita Dataset",
    x = "Year",
    y = "GDP per Capita (Current Prices)"
  ) +
  scale_color_manual(values = rainbow(length(unique(gdp_latin_america$Country)))) + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),       
    plot.caption = element_text(size = 10, face = "italic"),        
    legend.position = "right",                          
    legend.title = element_blank(),                           
    legend.text = element_text(size = 8),                          
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),    
    axis.text.y = element_text(size = 10),                  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "gray90")
  ) +
  guides(
    color = guide_legend(ncol = 2)                                    
  )

write.csv(gdp_latin_america, "cleaned_gdp_per_capita.csv", row.names = FALSE)

# BIVARIATE ANALYSIS


install.packages("tidyverse")
install.packages("zoo")
install.packages("psych")
install.packages("flextable")
install.packages("GGally")


library(tidyverse)
library(gtsummary)
library(gt)
library(zoo)
library(psych)
library(flextable)
library(GGally)


setwd("/Users/macbookpro/Documents/Statistics project")


gini_data <- read.csv("cleaned_interpolated_gini.csv")
democratization_data <- read.csv("cleaned_democratization.csv")


bivariate_data <- inner_join(gini_data, democratization_data, by = c("Country.Name" = "Country.Name", "Year" = "Year"))


bivariate_data <- bivariate_data %>%
  rename(
    GINI_Index = GINI_Index,
    Democratization_Index = Democratization_Index
  )


# Removing rows with N/A values
bivariate_data <- bivariate_data %>% drop_na(GINI_Index, Democratization_Index)


correlations_by_country <- bivariate_data %>%
  group_by(Country.Name) %>%
  summarise(
    Correlation = cor(GINI_Index, Democratization_Index, use = "complete.obs", method = "pearson")
  )
print(correlations_by_country)


regression_results <- bivariate_data %>%
  group_by(Country.Name) %>%
  summarise(Model = list(lm(Democratization_Index ~ GINI_Index, data = .)))


for (i in seq_along(regression_results$Country.Name)) {
  country <- regression_results$Country.Name[i]
  model <- regression_results$Model[[i]]
  cat("\n---", country, "---\n")
  print(summary(model))
}


ggplot(bivariate_data, aes(x = GINI_Index, y = Democratization_Index)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  facet_wrap(~ Country.Name) +
  labs(
    title = "Relationship between GINI Index and Democratization Index by Country",
    x = "GINI Index",
    y = "Democratization Index"
  )


overall_correlation <- cor(bivariate_data$GINI_Index, bivariate_data$Democratization_Index, use = "complete.obs", method = "pearson")
print(paste("Overall Pearson correlation for Latin America: ", round(overall_correlation, 2)))


overall_model <- lm(Democratization_Index ~ GINI_Index, data = bivariate_data)
summary(overall_model)


ggplot(bivariate_data, aes(x = GINI_Index, y = Democratization_Index)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Relationship between GINI Index and Democratization Index in Latin America",
    x = "GINI Index",
    y = "Democratization Index"
  )


par(mfrow = c(2, 2))  
plot(overall_model, main = "Diagnostics for Overall Model")
par(mfrow = c(1, 1))  

write.csv(bivariate_data, "bivariate_gini_democratization_cleaned.csv", row.names = FALSE)

install.packages("tidyverse")
install.packages("zoo")


library(tidyverse)
library(zoo)

setwd("/Users/macbookpro/Documents/Statistics project")


# MULTIVARIATE REGRESSION ANALYSIS

gini <- read.csv("interpolated_gini_latin_america.csv")  
democratization <- read.csv("cleaned_interpolated_democratization.csv") 
education <- read.csv("cleaned_education_expenditure_new.csv") 
gdp <- read.csv("cleaned_gdp_per_capita.csv")  


gini <- gini %>% select(-2)  


colnames(gini) <- c("Country", "Year", "GINI_Index")
colnames(democratization) <- c("Country", "Year", "Democratization_Level")
colnames(education) <- c("Country", "Year", "Education_Expenditure")
colnames(gdp) <- c("Country", "Year", "GDP_per_Capita")


gini <- gini %>% mutate(Year = as.numeric(gsub("[^0-9]", "", Year)))
democratization <- democratization %>% mutate(Year = as.numeric(Year))
education <- education %>% mutate(Year = as.numeric(Year))
gdp <- gdp %>% mutate(Year = as.numeric(Year))


common_years <- intersect(intersect(unique(gini$Year), unique(democratization$Year)),
                          intersect(unique(education$Year), unique(gdp$Year)))

gini <- gini %>% filter(Year %in% common_years)
democratization <- democratization %>% filter(Year %in% common_years)
education <- education %>% filter(Year %in% common_years)
gdp <- gdp %>% filter(Year %in% common_years)


merged_data <- gini %>%
  left_join(democratization, by = c("Country", "Year")) %>%
  left_join(education, by = c("Country", "Year")) %>%
  left_join(gdp, by = c("Country", "Year"))



merged_data <- merged_data %>%
  mutate(GINI_Index = ifelse(GINI_Index == 0, NA, GINI_Index))


merged_data <- merged_data %>%
  group_by(Country) %>%
  mutate(
    GINI_Index = zoo::na.approx(GINI_Index, na.rm = FALSE),
    Education_Expenditure = zoo::na.approx(Education_Expenditure, na.rm = FALSE),
    GDP_per_Capita = zoo::na.approx(GDP_per_Capita, na.rm = FALSE)
  ) %>%
  ungroup()


merged_data <- merged_data %>%
  drop_na(GINI_Index, Education_Expenditure, GDP_per_Capita)


write.csv(merged_data, "final_merged_dataset.csv", row.names = FALSE)


summary(merged_data)
head(merged_data)



install.packages("ggiraphExtra")


library(car)
library(tidyverse)
library(car)
library(ggplot2)
library(broom)
library(ggiraphExtra)
library(stargazer)
library(lmtest)
library(corrplot)




merged_data <- read.csv("final_merged_dataset.csv")


merged_data <- merged_data %>%
  mutate(
    GINI_Index = as.numeric(GINI_Index),
    Democratization_Level = as.numeric(Democratization_Level),
    Education_Expenditure = as.numeric(Education_Expenditure),
    GDP_per_Capita = as.numeric(GDP_per_Capita),
    Year = as.numeric(Year)
  )



# Null Model
null_model <- lm(Democratization_Level ~ 1, data = merged_data)
summary(null_model)


ggplot(merged_data, aes(x = Country, y = Democratization_Level)) +
  geom_segment(aes(xend = Country, yend = mean(Democratization_Level)), color = "darkgray") +
  geom_point() +
  geom_hline(aes(yintercept = mean(Democratization_Level)), color = "deeppink") +
  labs(title = "Null Model Intercept and Prediction Error",
       x = NULL,
       y = "Democratization Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

# Simple Model
simple_model <- lm(Democratization_Level ~ GINI_Index, data = merged_data)
summary(simple_model)


ggplot(merged_data, aes(x = GINI_Index, y = Democratization_Level)) +
  geom_segment(aes(xend = GINI_Index, yend = predict(simple_model)), color = "darkgray") +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Simple Regression Model",
       x = "GINI Index",
       y = "Democratization Level")

# Multivariatye Regression Model
multiple_model <- lm(Democratization_Level ~ GINI_Index + Education_Expenditure + GDP_per_Capita + Year, 
                     data = merged_data)
summary(multiple_model)


vif_values <- vif(multiple_model)
print(vif_values)


par(mfrow = c(2, 2))
plot(multiple_model)


bp_test <- bptest(multiple_model)
print(bp_test)


shapiro_test <- shapiro.test(residuals(multiple_model))
print(shapiro_test)


cooks_distance <- cooks.distance(multiple_model)
merged_data$cooksd <- cooks_distance
ggplot(merged_data, aes(x = seq_along(cooksd), y = cooksd)) +
  geom_col() +
  geom_hline(yintercept = 4 / nrow(merged_data), color = "deeppink", linetype = "dashed") +
  labs(title = "Cook's Distance for Multiple Regression",
       x = "Index", y = "Cook's Distance")




stargazer(null_model, simple_model, multiple_model, type = "text",
          star.cutoffs = c(0.05, 0.01, 0.001))


sink("model_summary.txt")
summary(multiple_model)
sink()



# GINI Index Effect
ggplot(merged_data, aes(x = GINI_Index, y = Democratization_Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "GINI Index and Democratization Level",
       x = "GINI Index",
       y = "Democratization Level") +
  theme_minimal()

# Education Expenditure Effect
ggplot(merged_data, aes(x = Education_Expenditure, y = Democratization_Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Education Expenditure and Democratization Level",
       x = "Education Expenditure (% of GDP)",
       y = "Democratization Level") +
  theme_minimal()

# GDP per Capita Effect
ggplot(merged_data, aes(x = GDP_per_Capita, y = Democratization_Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(title = "GDP per Capita and Democratization Level",
       x = "GDP per Capita (USD)",
       y = "Democratization Level") +
  theme_minimal()


