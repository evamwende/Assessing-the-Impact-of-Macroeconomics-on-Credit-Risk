
# Descriptive Analysis ----------------------------------------------------


# Install and load all necessary packages

# install.packages(c("tidyverse", "readxl", "writexl", "skimr", "ggplot2", "corrplot"))

library(tidyverse)
library(readxl)
library(writexl)
library(skimr)
library(ggplot2)  
library(corrplot)

# Set the full path to input Excel file
file_path <- "/Users/evamwendekiio/Desktop/Dissertation!/Analysis/All_Data.xlsx"

# paths for output files in the same directory
merged_data_path <- file.path(dirname(file_path), "Final_Panel_Data.xlsx")
descriptive_stats_path <- file.path(dirname(file_path), "Descriptive_Statistics.xlsx")
group_summary_path <- file.path(dirname(file_path), "Group_Summary_Statistics.xlsx")
correlation_heatmap_path <- file.path(dirname(file_path), "Correlation_Heatmap.png")

# List of relevant sheet names
sheets <- c("NPLs", "GDP", "Unemployment", "HICP", "Lending_Rate")

# Function to read and reshape each sheet
read_and_pivot <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  df_long <- df %>%
    pivot_longer(cols = -Country, names_to = "Quarter", values_to = sheet_name) %>%
    mutate(Quarter = as.character(Quarter))
  return(df_long)
}

# Read and merge all sheets into one dataset
final_panel_data <- sheets %>%
  map(read_and_pivot) %>%
  reduce(full_join, by = c("Country", "Quarter")) %>%
  arrange(Country, Quarter) 


# A) Overall Summary Statistics
descriptive_stats_table <- final_panel_data %>%
  select(`NPLs`, `GDP`, `Unemployment`, `HICP`, `Lending_Rate`) %>%
  skim() %>%
  yank("numeric") %>%
  select(variable = skim_variable, n_missing, mean, sd, p0, p50, p100) %>%
  rename(Variable = variable, Missing_Values = n_missing, Mean = mean, Std_Dev = sd, Min = p0, Median = p50, Max = p100)

# B) Group-Level Summary Statistics
grouped_data <- final_panel_data %>%
  mutate(Group = case_when(
    Country %in% c("Germany", "France", "Netherlands", "Austria", "Belgium") ~ "Core Economies",
    Country %in% c("Spain", "Italy", "Portugal", "Greece") ~ "Southern Periphery",
    Country %in% c("Ireland", "Estonia", "Latvia", "Lithuania") ~ "Dynamic & Converging Economies"
  ))

group_summary_table <- grouped_data %>%
  group_by(Group) %>%
  summarise(Average_NPL_Ratio = mean(NPLs, na.rm = TRUE), .groups = 'drop')

countries_in_group <- tribble(
  ~Group, ~Countries_in_Group,
  "Core Economies", "Germany, France, Netherlands, Austria, Belgium",
  "Southern Periphery", "Spain, Italy, Portugal, Greece",
  "Dynamic & Converging Economies", "Ireland, Estonia, Latvia, Lithuania"
)
final_group_summary_table <- left_join(group_summary_table, countries_in_group, by = "Group")

# C) Correlation Matrix and Heatmap
independent_vars <- final_panel_data %>%
  select(`GDP`, `Unemployment`, `HICP`, `Lending_Rate`)

correlation_matrix <- cor(independent_vars, use = "complete.obs")

# Create the heatmap
png(correlation_heatmap_path, width = 800, height = 800) 
corrplot(correlation_matrix, method = "color", type = "upper",
         addCoef.col = "black",  
         number.cex = 1.8, 
         tl.col = "black",                    
         tl.srt = 45,                         
         tl.cex = 1.8,                        
         mar = c(1, 1, 1, 1))                
dev.off() 

# SAVE ALL OUTPUTS 
write_xlsx(final_panel_data, path = merged_data_path)
write_xlsx(descriptive_stats_table, path = descriptive_stats_path)
write_xlsx(final_group_summary_table, path = group_summary_path)

# PRINT CONFIRMATIONS
print(paste("Successfully saved the full merged data to:", merged_data_path))
print(paste("Successfully saved the descriptive statistics table to:", descriptive_stats_path))
print(paste("Successfully saved the group summary table to:", group_summary_path))
print(paste("Successfully saved the correlation heatmap to:", correlation_heatmap_path))
print("--- Correlation Matrix ---")
print(round(correlation_matrix, 2))


# Stationary Analysis -----------------------------------------------------

#Install and load the necessary package 
# install.packages("plm")
library(plm) # package for Panel Linear Models


# Load Data
file_path <- "/Users/evamwendekiio/Desktop/Dissertation!/Analysis/Final_Panel_Data.xlsx"
panel_data <- read_excel(file_path)

# Declare data as a panel dataset
pdata <- pdata.frame(panel_data, index = c("Country", "Quarter"))


# Perform the Levin-Lin-Chu (LLC) Stationary test...

# list of the variables we want to test
variables_to_test <- c("NPLs", "GDP", "Unemployment", "HICP", "Lending_Rate")

# This will store the results of our tests
test_results <- list()

# Loop through each variable and perform the test
for (variable in variables_to_test) {
  formula <- as.formula(paste(variable, "~ 1"))
  test <- purtest(formula, data = pdata, test = "levinlin", exo = "intercept", lags = "AIC")
  test_results[[variable]] <- summary(test)
}


# Print Results
print("--- Panel Unit Root Test Results (Levin-Lin-Chu) ---")
print("Null Hypothesis (H0): The series has a unit root (is non-stationary)")
print("A small p-value (< 0.05) means we reject H0, so the series IS stationary.")

# Print the results for each variable
for (variable in names(test_results)) {
  cat("\n--- Variable:", variable, "---\n")
  print(test_results[[variable]])
}


# Estimate the two main types of panel regression models (FE and RE) --------

# Define the regression equation 
model_formula <- NPLs ~ GDP + Unemployment + HICP + `Lending_Rate`


# A) Fixed Regression Model
print("--- Estimating Fixed Effects (FE) Model ---")
fe_model <- plm(model_formula, data = pdata, model = "within")

# Print the summary of the FE model results
print("--- Fixed Effects Model Results ---")
summary(fe_model)


# -Random Effects (RE) model
print("--- Estimating Random Effects (RE) Model ---")
re_model <- plm(model_formula, data = pdata, model = "random")

# Print the summary of the RE model results
print("--- Random Effects Model Results ---")
summary(re_model)


# HAUSMAN TEST 
print("--- Performing Hausman Test ---")
hausman_test_result <- phtest(fe_model, re_model)

# Print the results of the Hausman test
print("--- Hausman Test Results ---")
print(hausman_test_result)




# COVID IMPACT ANALYSIS: STRUCTURAL BREAKS AND PRE/POST-COMPARISONS ==========================================================================


# Create Year and Quarter_Num columns for filtering
panel_data_with_dates <- final_panel_data %>%
  separate(Quarter, into = c("Year", "Quarter_Num"), sep = "Q", remove = FALSE, convert = TRUE)

# Create the pre-COVID dataset (2015-Q1 to 2019-Q4)
pre_covid_data <- panel_data_with_dates %>%
  filter(Year < 2020)

# Create the during/post-COVID dataset (2020-Q1 onwards)
post_covid_data <- panel_data_with_dates %>%
  filter(Year >= 2020)

# Declare each subset as a panel data frame
pdata_pre_covid <- pdata.frame(pre_covid_data, index = c("Country", "Quarter"))
pdata_post_covid <- pdata.frame(post_covid_data, index = c("Country", "Quarter"))

# The formula 
model_formula <- NPLs ~ GDP + Unemployment + HICP + `Lending_Rate`


# ESTIMATE THE FIXED EFFECTS MODEL FOR EACH PERIOD ---

# A) Pre-COVID Period Model
print("--- Estimating Fixed Effects Model for Pre-COVID Period (2015-2019) ---")
fe_model_pre_covid <- plm(model_formula, data = pdata_pre_covid, model = "within")
summary(fe_model_pre_covid)

# B) During/Post-COVID Period Model
print("--- Estimating Fixed Effects Model for During/Post-COVID Period (2020 onwards) ---")
fe_model_post_covid <- plm(model_formula, data = pdata_post_covid, model = "within")
summary(fe_model_post_covid)

# CREATE AND DISPLAY THE COMPARISON TABLE
library(stargazer) 
print("--- Comparison of Fixed Effects Models: Pre- vs. Post-COVID ---")

stargazer(fe_model_pre_covid, fe_model_post_covid,
          type = "text", # Prints a text table to the console
          title = "Comparison of Fixed Effects Models: Pre- vs. Post-COVID",
          column.labels = c("Pre-COVID (2015-2019)", "Post-COVID (2020-2024)"),
          dep.var.labels.include = FALSE, # Hides the default "Dependent variable:" label
          covariate.labels = c("GDP Growth", "Unemployment", "HICP Inflation", "Lending Rate"), # Clean variable names
          notes = "Significance levels: *** p<0.001, ** p<0.01, * p<0.05",
          align = TRUE)


# HYBRID MODEL: WITH COVID INTERACTION TERMS ------------------------------------------

# Create COVID dummy and interaction terms
panel_data_with_dates$COVID <- ifelse(panel_data_with_dates$Year >= 2020, 1, 0)
panel_data_with_dates$GDP_COVID <- panel_data_with_dates$GDP * panel_data_with_dates$COVID
panel_data_with_dates$Unemployment_COVID <- panel_data_with_dates$Unemployment * panel_data_with_dates$COVID
panel_data_with_dates$HICP_COVID <- panel_data_with_dates$HICP * panel_data_with_dates$COVID
panel_data_with_dates$LendingRate_COVID <- panel_data_with_dates$`Lending_Rate` * panel_data_with_dates$COVID

# Declare the updated dataset as a panel
pdata_hybrid <- pdata.frame(panel_data_with_dates, index = c("Country", "Quarter"))

# Estimate the Hybrid Fixed Effects Model
print("--- Estimating Hybrid Fixed Effects Model with COVID Interaction Terms ---")
hybrid_model <- plm(NPLs ~ GDP + Unemployment + HICP + `Lending_Rate` +
                      COVID + GDP_COVID + Unemployment_COVID +
                      HICP_COVID + LendingRate_COVID,
                    data = pdata_hybrid, model = "within")

# Summary of Hybrid Model
print("--- Hybrid Model Results ---")
summary(hybrid_model)

# Compare Base FE vs Hybrid FE Model
print("--- Comparison Table: Base FE vs Hybrid FE Model with COVID Interactions ---")

stargazer(fe_model, hybrid_model,
          type = "text",
          title = "Comparison of Base and Hybrid Fixed Effects Models",
          column.labels = c("Base FE Model", "Hybrid FE Model (COVID Interactions)"),
          covariate.labels = c(
            "GDP Growth", "Unemployment Rate", "HICP Inflation", "Lending Rate",
            "COVID Dummy", "GDP x COVID", "Unemployment x COVID",
            "HICP x COVID", "Lending Rate x COVID"
          ),
          dep.var.labels = "Non-Performing Loans (NPLs)",
          omit.stat = c("f", "ser"),
          notes = "Significance levels: *** p<0.001, ** p<0.01, * p<0.05",
          align = TRUE)

#  COEFFICIENT PLOT ----------

# library(ggplot2)
library(dplyr)

# Extract the results from the model summary
model_summary <- summary(hybrid_model)
coefficients <- as.data.frame(model_summary$coefficients)

# Prepare the data
coefficients <- coefficients %>%
  rownames_to_column(var = "Variable") %>%
  rename(Coefficient = Estimate, Std_Error = `Std. Error`, p_value = `Pr(>|t|)`) %>%
  mutate(
    Lower_CI = Coefficient - 1.96 * Std_Error,
    Upper_CI = Coefficient + 1.96 * Std_Error
  )

# The plot
coefficient_plot <- ggplot(coefficients, aes(x = Coefficient, y = fct_reorder(Variable, Coefficient))) +
  geom_point(size = 3.5, color = "darkblue") +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.25, color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Coefficient Estimate (Impact on NPL Ratio)",
    y = "Variable",
    # title = "Estimated Coefficients from Hybrid Fixed Effects Model"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.margin = unit(c(1, 2, 1, 1), "cm") 
  )

# Print
print(coefficient_plot)

# Save the plot
ggsave("Coefficient_Plot_Hybrid_Model.png", plot = coefficient_plot, width = 10, height = 7)





