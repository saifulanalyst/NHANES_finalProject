library(haven)

## Q2 . a Demographics, Questionnaire, laboratory, and examination 19  variables "HIQ011", "SMQ040", "ALQ121", "PAD680", "SLD012","BPQ020","DIQ010","MCQ220","MCQ160C","MCQ160F","BPQ150","DIQ050","BPQ101D","HSQ590","DPQ020","DPQ030"

demographics <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Demo/DEMO_L.XPT")
exam_bmxmbi <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Examinat/BMX_L.XPT")
laborat_lbxglu <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Laboratory/GLU_L.XPT")
laborat_lbxtc <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Laboratory/TCHOL_L.XPT")
quest_hiq011 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/HIQ_L.XPT")
quest_smq040 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/SMQ_L.XPT")
quest_alq121 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/ALQ_L.XPT")
quest_pad680 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/PAQ_L.XPT")
quest_sld012 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/SLQ_L.XPT")
quest_bpq020 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/BPQ_L.XPT")
quest_diq010 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/DIQ_L.XPT")
quest_mcq220 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/MCQ_L.XPT")
quest_hsq590 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/HSQ_L.XPT")
quest_dpq020 <- read_xpt("/Users/mamoon/Downloads/5208_DM_finalProject/NHANES_Questionnaire/DPQ_L.XPT")

View(quest_24_vars)



# Combine the datasets if necessary or extract variables directly
# List of datasets
quest_new_datasets <- list(demographics, exam_bmxmbi,laborat_lbxglu,laborat_lbxtc, quest_hiq011, quest_smq040, quest_alq121, quest_pad680, quest_sld012,
                           quest_bpq020, quest_diq010, quest_mcq220, quest_hsq590, quest_dpq020)

# List of variables to extract summaries for
quest_24_vars <- c("RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "INDFMPIR","BMXBMI", "LBXTC", "LBXGLU","HIQ011","SMQ040","ALQ121","PAD680","SLD012",
                   "BPQ020","DIQ010","MCQ220","MCQ160C","MCQ160F",
                   "BPQ150","DIQ050","BPQ101D","HSQ590","DPQ020","DPQ030") # Replace with actual variable names

View(quest_new_datasets)

# Using Loop function through each dataset and extract summaries for selected variables
for (i in 1:length(quest_new_datasets)) {
  # Filter only the selected variables that exist in each dataset
  dataset <- quest_new_datasets[[i]]
  available_vars <- intersect(quest_24_vars, names(dataset))
  
  # Display summary and first 5 rows if variables are available in this dataset
  if (length(available_vars) > 0) {
    cat("Dataset", i, "Summary:\n")
    print(summary(dataset[ , available_vars]))
    cat("First 5 Rows:\n")
    print(head(dataset[ , available_vars], 5))
    cat("\n-------------------\n")
  } else {
    cat("Dataset", i, "does not contain any of the selected variables.\n")
  }
}

# Q2 b. # Perform an initial exploration to understand the dataset. Check for and document any missing data
# in each variable. Summarize the percentage of missing data by variable and consider the potential impact of missing data on future analyses. 


library(dplyr) # Loading dplyr

# Function to calculate missing data percentage
calculate_missing_data <- function(dataset, vars) {
  # Ensure dataset is a data frame
  if (!is.data.frame(dataset)) {
    stop("Dataset must be a data frame.")
  }
  
  # Identify overlapping variables
  available_vars <- intersect(vars, names(dataset))
  
  # Calculate missing percentage if variables are found
  if (length(available_vars) > 0) {
    missing_data <- sapply(dataset[ , available_vars, drop = FALSE], function(x) sum(is.na(x)) / length(x) * 100)
    return(missing_data)
  } else {
    return(NULL)  # Return NULL if no variables are found
  }
}

# Initialize a list to store missing data results
missing_data_results <- list()

# Loop through each dataset and calculate missing data percentage
for (i in seq_along(quest_new_datasets)) {
  dataset <- quest_new_datasets[[i]]
  missing_data <- calculate_missing_data(dataset, quest_24_vars)
  
  if (!is.null(missing_data)) {
    cat("Dataset", i, "Missing Data Percentage:\n")
    print(missing_data)
    cat("\n-------------------\n")
    missing_data_results[[paste0("Dataset_", i)]] <- missing_data
  } else {
    cat("Dataset", i, "does not contain any of the selected variables.\n")
  }
}

# Combine results into a summary table if needed
missing_data_summary <- do.call(rbind, lapply(names(missing_data_results), function(name) {
  data.frame(Dataset = name, Variable = names(missing_data_results[[name]]), 
             Missing_Percentage = missing_data_results[[name]])
}))

# Print a combined summary (optional)
print(missing_data_summary)

#Q2. c. Review and document the dataset’s structure, noting aspects such as the number of observations,
# variable types, and overall completeness.


# Function to review and document dataset structure
review_dataset_structure <- function(dataset, dataset_name) {
  # Ensure the dataset is a data frame
  if (!is.data.frame(dataset)) {
    cat(dataset_name, "is not a valid data frame.\n")
    return(NULL)
  }
  
  # Number of observations and variables
  num_obs <- nrow(dataset)
  num_vars <- ncol(dataset)
  
  # Variable types
  var_types <- sapply(dataset, class)
  
  # Percentage of missing values for each variable
  missing_percent <- sapply(dataset, function(x) sum(is.na(x)) / length(x) * 100)
  
  # Combine the structure information into a summary table
  structure_summary <- data.frame(
    Variable = names(var_types),
    Type = var_types,
    Missing_Percentage = round(missing_percent, 2),
    stringsAsFactors = FALSE
  )
  
  # Print the overview
  cat("\nDataset:", dataset_name, "\n")
  cat("Number of Observations:", num_obs, "\n")
  cat("Number of Variables:", num_vars, "\n")
  cat("Overall Missing Data Percentage:", round(mean(missing_percent), 2), "%\n")
  cat("\nVariable Summary:\n")
  print(head(structure_summary, 10))  # Display the first 10 rows
  cat("\n-------------------\n")
  
  return(structure_summary)
}

# Initialize a list to store all summaries
all_summaries <- list()

# Loop through each dataset and review structure
for (i in seq_along(quest_new_datasets)) {
  dataset <- quest_new_datasets[[i]]
  dataset_name <- paste0("Dataset_", i)
  
  summary_table <- review_dataset_structure(dataset, dataset_name)
  if (!is.null(summary_table)) {
    all_summaries[[dataset_name]] <- summary_table
  }
}

# Combine all summaries into a single table for documentation
combined_summary <- do.call(rbind, lapply(names(all_summaries), function(name) {
  cbind(Dataset = name, all_summaries[[name]])
}))

# Print the combined summary
print(head(combined_summary))


-------------
#Q2. C for 24 variables
  
  # Function to review and document dataset structure for specific variables
  review_selected_variables <- function(dataset, dataset_name, selected_vars) {
    # Ensure the dataset is a data frame
    if (!is.data.frame(dataset)) {
      cat(dataset_name, "is not a valid data frame.\n")
      return(NULL)
    }
    
    # Filter only the selected variables present in the dataset
    available_vars <- intersect(selected_vars, names(dataset))
    
    if (length(available_vars) == 0) {
      cat(dataset_name, "does not contain any of the selected variables.\n")
      return(NULL)
    }
    
    # Number of observations and variables
    num_obs <- nrow(dataset)
    num_vars <- length(available_vars)
    
    # Variable types
    var_types <- sapply(dataset[ , available_vars, drop = FALSE], class)
    
    # Percentage of missing values for each variable
    missing_percent <- sapply(dataset[ , available_vars, drop = FALSE], function(x) sum(is.na(x)) / length(x) * 100)
    
    # Combine the structure information into a summary table
    structure_summary <- data.frame(
      Variable = available_vars,
      Type = var_types,
      Missing_Percentage = round(missing_percent, 2),
      stringsAsFactors = FALSE
    )
    
    # Print the overview
    cat("\nDataset:", dataset_name, "\n")
    cat("Number of Observations:", num_obs, "\n")
    cat("Number of Selected Variables:", num_vars, "\n")
    cat("Overall Missing Data Percentage (Selected Variables):", round(mean(missing_percent), 2), "%\n")
    cat("\nVariable Summary (Selected):\n")
    print(structure_summary)
    cat("\n-------------------\n")
    
    return(structure_summary)
  }

# Initialize a list to store all summaries
all_summaries <- list()

# List of datasets and selected variables
selected_vars <- quest_24_vars  # Use only the selected variables

# Loop through each dataset and review structure
for (i in seq_along(quest_new_datasets)) {
  dataset <- quest_new_datasets[[i]]
  dataset_name <- paste0("Dataset_", i)
  
  summary_table <- review_selected_variables(dataset, dataset_name, selected_vars)
  if (!is.null(summary_table)) {
    all_summaries[[dataset_name]] <- summary_table
  }
}

------------
# Q2. D Conduct checks to confirm that the data is logically consistent (e.g., values fall within expected
#  ranges for continuous variables and categories are valid for categorical variables) and make sure that you fixed any inconsistencies. 

  # Define expected ranges and valid categories for selected variables
  expected_values <- list(
    # Continuous variables: specify ranges
    RIDAGEYR = c(0, 120),  # Age range: 0-120 years
    BMXBMI = c(10, 80),    # BMI range: 10-80
    LBXTC = c(50, 400),    # Total Cholesterol range: 50-400 mg/dL
    LBXGLU = c(40, 500),   # Glucose range: 40-500 mg/dL
    
    # Categorical variables: specify valid categories
    RIAGENDR = c(1, 2),    # Gender: 1 = Male, 2 = Female
    RIDRETH1 = c(1, 2, 3, 4, 5), # Race/Ethnicity categories
    HIQ011 = c(1, 2, 7, 9), # Health Insurance: 1 = Yes, 2 = No, 7/9 = Refused/Don't know
    SMQ040 = c(1, 2, 7, 9), # Smoker status: 1 = Every day, 2 = Some days, etc.
    ALQ121 = c(1, 2, 7, 9), # Alcohol consumption: 1 = Yes, 2 = No, etc.
    BPQ020 = c(1, 2, 7, 9)  # Blood Pressure: 1 = Yes, 2 = No, etc.
  )

# Function to check logical consistency
check_logical_consistency <- function(dataset, dataset_name, expected_values) {
  issues <- list()  # Initialize list to store inconsistencies
  
  cat("\nChecking Dataset:", dataset_name, "\n")
  
  # Loop through expected values to check ranges and valid categories
  for (var in names(expected_values)) {
    if (var %in% names(dataset)) {
      values <- dataset[[var]]  # Extract variable
      
      # Check if it's a range or a category
      if (is.numeric(expected_values[[var]])) { 
        # Continuous variable: check range
        out_of_range <- which(values < expected_values[[var]][1] | values > expected_values[[var]][2])
        if (length(out_of_range) > 0) {
          cat("Variable", var, "has", length(out_of_range), "out-of-range values.\n")
          issues[[var]] <- out_of_range
        }
      } else {
        # Categorical variable: check valid categories
        invalid_values <- which(!values %in% expected_values[[var]])
        if (length(invalid_values) > 0) {
          cat("Variable", var, "has", length(invalid_values), "invalid values.\n")
          issues[[var]] <- invalid_values
        }
      }
    }
  }
  
  cat("Check complete for:", dataset_name, "\n-------------------\n")
  return(issues)
}

# Function to fix inconsistencies (optional)
fix_inconsistencies <- function(dataset, issues, expected_values) {
  for (var in names(issues)) {
    if (is.numeric(expected_values[[var]])) {
      # Continuous variable: replace out-of-range with NA
      dataset[[var]][issues[[var]]] <- NA
    } else {
      # Categorical variable: replace invalid values with NA
      dataset[[var]][issues[[var]]] <- NA
    }
  }
  return(dataset)
}

# List to store all datasets with issues
datasets_with_issues <- list()

# Loop through datasets to check logical consistency
for (i in seq_along(quest_new_datasets)) {
  dataset <- quest_new_datasets[[i]]
  dataset_name <- paste0("Dataset_", i)
  
  # Check logical consistency
  issues <- check_logical_consistency(dataset, dataset_name, expected_values)
  
  # Fix inconsistencies if issues are found
  if (length(issues) > 0) {
    datasets_with_issues[[dataset_name]] <- issues
    quest_new_datasets[[i]] <- fix_inconsistencies(dataset, issues, expected_values)
  }
}

# Save datasets with fixed inconsistencies (optional)
for (i in seq_along(quest_new_datasets)) {
  write.csv(quest_new_datasets[[i]], paste0("fixed_dataset_", i, ".csv"), row.names = FALSE)
}

# Print summary of datasets with issues
if (length(datasets_with_issues) > 0) {
  cat("\nSummary of Datasets with Issues:\n")
  print(names(datasets_with_issues))
} else {
  cat("\nNo logical inconsistencies found in the datasets.\n")
}

-------------------------------
#Q3 Choose 6 variables for recoding
# Two variables from “Quantitative to Categorical”: For example, recode BMI into categories

library(dplyr)

# Assuming `exam_bmxmbi` contains the BMI variable and `demographics` contains Age
# Modify the variable names if needed

# Recode BMI into categories
exam_bmxmbi <- exam_bmxmbi %>%
  mutate(BMI_Category = case_when(
    BMXBMI < 18.5 ~ "Underweight",
    BMXBMI >= 18.5 & BMXBMI < 24.9 ~ "Normal Weight",
    BMXBMI >= 25 & BMXBMI < 29.9 ~ "Overweight",
    BMXBMI >= 30 ~ "Obese",
    TRUE ~ NA_character_ # Assign NA for missing or invalid values
  ))

# Print the first few rows to verify
print(head(exam_bmxmbi %>% select(BMXBMI, BMI_Category)))

# Recode Age into categories (example)
demographics <- demographics %>%
  mutate(Age_Group = case_when(
    RIDAGEYR < 18 ~ "Child",
    RIDAGEYR >= 18 & RIDAGEYR < 30 ~ "Young Adult",
    RIDAGEYR >= 30 & RIDAGEYR < 60 ~ "Adult",
    RIDAGEYR >= 60 ~ "Senior",
    TRUE ~ NA_character_ # Assign NA for missing or invalid values
  ))

# Print the first few rows to verify
print(head(demographics %>% select(RIDAGEYR, Age_Group)))

# Two variables from “Quantitative to Binary”: For example, creating a “Senior” (≥65) vs. “Non-Senior” (<65) indicator from the age variable.

# Recode Age into a binary "Senior" indicator
demographics <- demographics %>%
  mutate(Senior_Indicator = ifelse(RIDAGEYR >= 65, "Senior", "Non-Senior"))

# Print the first few rows to verify
print(head(demographics %>% select(RIDAGEYR, Senior_Indicator)))

# Example for another variable: Binary Indicator for High BMI (≥30 vs. <30)
exam_bmxmbi <- exam_bmxmbi %>%
  mutate(High_BMI_Indicator = ifelse(BMXBMI >= 30, "High BMI", "Normal BMI"))

# Print the first few rows to verify
print(head(exam_bmxmbi %>% select(BMXBMI, High_BMI_Indicator)))

# iii Two variables from “Categorical to Binary”: Simplify a categorical variable like education level into two categories (e.g., "High School or Less" vs. "More than High School").

# DMDEDUC2: 1 = <9th grade, 2 = 9–11th grade, 3 = High school/GED, 4 = Some college/AA, 5 = College graduate or above

# Recode education level into two categories
demographics <- demographics %>%
  mutate(Education_Binary = case_when(
    DMDEDUC2 %in% c(1, 2, 3) ~ "High School or Less",  # Categories 1, 2, 3
    DMDEDUC2 %in% c(4, 5) ~ "More than High School",   # Categories 4, 5
    TRUE ~ NA_character_                                # Handle missing or invalid values
  ))

# Print the first few rows to verify
print(head(demographics %>% select(DMDEDUC2, Education_Binary)))

# Assuming SMQ040 (Smoking Status): 1 = Every day, 2 = Some days, 3 = Not at all
quest_smq040 <- quest_smq040 %>%
  mutate(Smoking_Binary = case_when(
    SMQ040 %in% c(1, 2) ~ "Current Smoker",  # Categories 1, 2
    SMQ040 == 3 ~ "Non-Smoker",              # Category 3
    TRUE ~ NA_character_                     # Handle missing or invalid values
  ))

# Print the first few rows to verify
print(head(quest_smq040 %>% select(SMQ040, Smoking_Binary)))

--------------------
# Q4: Calculate descriptive statistics for the variables, including the recoded ones. Pay attention to the
#accuracy and the consistency of the variable types and the descriptives, i.e. frequencies for categorical and binary variables, and descriptives such as mean or median for quantitative variables.

  # Load necessary libraries
  library(dplyr)
library(janitor)  # For tabulating categorical frequencies

# Function to calculate descriptive statistics
calculate_descriptive_stats <- function(data) {
  
  # Identify variable types
  var_types <- sapply(data, class)
  
  # Initialize lists to store results
  descriptive_stats <- list()
  
  # Loop through variables
  for (var in names(data)) {
    cat("\nVariable:", var, "\n")
    
    if (var_types[var] %in% c("factor", "character")) {
      # For categorical/binary variables: calculate frequencies
      freq_table <- tabyl(data[[var]], show_na = TRUE)
      print(freq_table)
      descriptive_stats[[var]] <- freq_table
      
    } else if (var_types[var] %in% c("numeric", "integer")) {
      # For quantitative variables: calculate mean, median, min, max
      stats <- data %>%
        summarise(
          Mean = mean(.data[[var]], na.rm = TRUE),
          Median = median(.data[[var]], na.rm = TRUE),
          Min = min(.data[[var]], na.rm = TRUE),
          Max = max(.data[[var]], na.rm = TRUE),
          SD = sd(.data[[var]], na.rm = TRUE),
          Missing_Percentage = sum(is.na(.data[[var]]) / n() * 100)
        )
      print(stats)
      descriptive_stats[[var]] <- stats
    } else {
      cat("Unsupported variable type.\n")
    }
  }
  
  return(descriptive_stats)
}

# Apply the function to a dataset (e.g., demographics)
demographics_descriptive_stats <- calculate_descriptive_stats(demographics)

exam_bmxmbi_descriptive_stats <- calculate_descriptive_stats(exam_bmxmbi)
laborat_lbxglu_descriptive_stats <- calculate_descriptive_stats(laborat_lbxglu)

----------------
# For 24 variables
  
  # Filter datasets to include only `quest_24_vars`
filter_to_selected_vars <- function(dataset, selected_vars) {
    available_vars <- intersect(selected_vars, names(dataset))
    dataset %>% select(all_of(available_vars))
  }

# Function to calculate descriptive statistics for selected variables
calculate_descriptive_stats_selected <- function(data, selected_vars) {
  # Filter dataset to selected variables
  data <- filter_to_selected_vars(data, selected_vars)
  
  # Identify variable types
  var_types <- sapply(data, class)
  
  # Initialize list to store results
  descriptive_stats <- list()
  
  # Loop through variables
  for (var in names(data)) {
    cat("\nVariable:", var, "\n")
    
    if (var_types[var] %in% c("factor", "character")) {
      # For categorical/binary variables: calculate frequencies
      freq_table <- tabyl(data[[var]], show_na = TRUE)
      print(freq_table)
      descriptive_stats[[var]] <- freq_table
      
    } else if (var_types[var] %in% c("numeric", "integer")) {
      # For quantitative variables: calculate mean, median, min, max
      stats <- data %>%
        summarise(
          Mean = mean(.data[[var]], na.rm = TRUE),
          Median = median(.data[[var]], na.rm = TRUE),
          Min = min(.data[[var]], na.rm = TRUE),
          Max = max(.data[[var]], na.rm = TRUE),
          SD = sd(.data[[var]], na.rm = TRUE),
          Missing_Percentage = sum(is.na(.data[[var]]) / n() * 100)
        )
      print(stats)
      descriptive_stats[[var]] <- stats
    } else {
      cat("Unsupported variable type.\n")
    }
  }
  
  return(descriptive_stats)
}

# Define the `quest_24_vars`
quest_24_vars <- c("RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "BMXBMI", "LBXTC", 
                   "LBXGLU", "HIQ011", "SMQ040", "ALQ121", "PAD680", "SLD012", "BPQ020", 
                   "DIQ010", "MCQ220", "MCQ160C", "MCQ160F", "BPQ150", "DIQ050", "BPQ101D", 
                   "HSQ590", "DPQ020", "DPQ030")

# Apply the function to each dataset and calculate descriptive statistics for `quest_24_vars`
all_descriptive_stats <- list()
for (i in seq_along(quest_new_datasets)) {
  dataset <- quest_new_datasets[[i]]
  cat("\n--- Dataset", i, "---\n")
  
  descriptive_stats <- calculate_descriptive_stats_selected(dataset, quest_24_vars)
  all_descriptive_stats[[paste0("Dataset_", i)]] <- descriptive_stats
}

#Q4. b Generate visualizations for at least three of the original and three of the recoded variables to illustrate the distribution of your cleaned data. 
library(ggplot2)

# Visualizations for Original Variables
#  Age (RIDAGEYR) - Histogram
ggplot(demographics, aes(x = RIDAGEYR)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    title = "Distribution of Age",
    x = "Age (Years)",
    y = "Count"
  ) +
  theme_minimal()

#  BMI (BMXBMI) - Histogram
ggplot(exam_bmxmbi, aes(x = BMXBMI)) +
  geom_histogram(binwidth = 2, fill = "purple", color = "black") +
  labs(
    title = "Distribution of BMI",
    x = "BMI",
    y = "Count"
  ) +
  theme_minimal()

#  Gender (RIAGENDR) - Bar Plot
ggplot(demographics, aes(x = factor(RIAGENDR), fill = factor(RIAGENDR))) +
  geom_bar(color = "black") +
  labs(
    title = "Distribution of Gender",
    x = "Gender (1 = Male, 2 = Female)",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal()




# Visualizations for Recoded Variables
#  Senior_Status - Bar Plot
ggplot(demographics, aes(x = Senior_Indicator, fill = Senior_Indicator)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribution of Senior Status",
    x = "Status",
    y = "Count",
    fill = "Senior Status"
  ) +
  theme_minimal()

#  BMI_Category - Bar Plot
ggplot(exam_bmxmbi, aes(x = BMI_Category, fill = BMI_Category)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribution of BMI Categories",
    x = "BMI Category",
    y = "Count",
    fill = "BMI Category"
  ) +
  theme_minimal()

#  Education_Binary - Bar Plot
ggplot(demographics, aes(x = Education_Binary, fill = Education_Binary)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribution of Education Levels",
    x = "Education Level",
    y = "Count",
    fill = "Education"
  ) +
  theme_minimal()

----------------------
# Q5.Select one quantitative variable and one of the recoded binary or categorical variables and then perform a basic comparison (e.g., t-test, chi-square test) to examine differences between groups. Briefly interpret the result of your comparison test and discuss any insights related to public health. 

# Perform a t-test to compare BMI between Seniors and Non-Seniors
t_test_result <- demographics %>%
  mutate(
    Senior_Indicator = ifelse(RIDAGEYR >= 65, "Senior", "Non-Senior") # Recoding age to create Senior_Status
  ) %>%
  filter(!is.na(BMXBMI), !is.na(Senior_Indicator)) %>%  # Exclude missing data
  t.test(BMXBMI ~ Senior_Indicator, data = .) # Perform t-test

# Print the t-test results
print(t_test_result)

# Interpretation example:
# The t-test compares the mean BMI of Seniors and Non-Seniors.
# A significant p-value (< 0.05) suggests a meaningful difference in BMI between the two groups.

# Chi-Square Test for Association between Categorical Variables
# Use recoded Education_Binary and Senior_Status
chi_square_data <- demographics %>%
  mutate(
    Senior_Status = ifelse(RIDAGEYR >= 65, "Senior", "Non-Senior"),
    Education_Binary = ifelse(DMDEDUC2 <= 3, "High School or Less", "More than High School")
  ) %>%
  filter(!is.na(Senior_Status), !is.na(Education_Binary)) # Exclude missing data

# Create a contingency table
contingency_table <- table(chi_square_data$Senior_Status, chi_square_data$Education_Binary)

# Perform Chi-Square Test
chi_square_result <- chisq.test(contingency_table)

# Print the Chi-Square Test Results
print(chi_square_result)

# Interpretation example:
# The Chi-Square test examines whether the distribution of education levels differs between Seniors and Non-Seniors.
# A significant p-value (< 0.05) suggests that education level is associated with senior status.



  
  
  