library(tidyverse)
library(readxl)
library(haven)
library(janitor)

install.packages(c(
  "skimr",
  "shiny",
  "bslib",
  "tidymodels",
  "ranger"
))

library(skimr)
library(shiny)

getwd()

setwd("D:/Hackthon_2026_Competition")

list.files()

list.files(recursive = TRUE)

# List files inside the first ZIP file
unzip("Microdata (1).zip", list = TRUE)

# List files inside the second ZIP file
unzip("Microdata.zip", list = TRUE)

# Create folders for our project
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("R", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

# Extract the Season A production dataset
unzip(
  "Microdata (1).zip",
  files = "Microdata/Season A/Rwa_raw_SeasonA2025_Production.dta",
  exdir = "data/raw"
)

# Check whether the extracted file exists
file.exists(
  "data/raw/Microdata/Season A/Rwa_raw_SeasonA2025_Production.dta"
)

# List files in the extracted folder
list.files(
  "data/raw",
  recursive = TRUE
)

# Import data 

data <- read_dta(
  "data/raw/Microdata/Season A/Rwa_raw_SeasonA2025_Production.dta"
)

# Number of rows and columns
dim(data)

# Variable names
names(data)

# Structure of the dataset
glimpse(data)

# Understand the data quality and yield components

# Summary statistics for Yield
summary(data$Yield)

# Missing, zero, and negative values
data %>%
  summarise(
    total_rows = n(),
    missing_yield = sum(is.na(Yield)),
    zero_yield = sum(Yield == 0, na.rm = TRUE),
    negative_yield = sum(Yield < 0, na.rm = TRUE),
    minimum_yield = min(Yield, na.rm = TRUE),
    median_yield = median(Yield, na.rm = TRUE),
    mean_yield = mean(Yield, na.rm = TRUE),
    maximum_yield = max(Yield, na.rm = TRUE)
  )

# check crop categories

# Check the variable label
attr(data$CropCategory, "label")

# Check the category codes and their labels
attr(data$CropCategory, "labels")

# Count observations by crop category
data %>%
  mutate(CropCategory = haven::as_factor(CropCategory)) %>%
  count(CropCategory, sort = TRUE)


# Visualize the distribution of yield

library(ggplot2)

ggplot(data, aes(x = Yield)) +
  geom_histogram(bins = 50, na.rm = TRUE) +
  labs(
    title = "Distribution of Agricultural Yield",
    x = "Yield (unit to be verified)",
    y = "Number of observations"
  ) +
  theme_minimal()

# Investigate unusually high yield values

#1. Check the definition of Yield

# Check the variable labels and units documented in the file
attr(data$Yield, "label")
attr(data$Yield, "labels")

attr(data$Plot_area_ha, "label")
attr(data$Crop_Area_ha, "label")
attr(data$Harvested_Crop_Area_ha, "label")
attr(data$Harvest_ratio, "label")
attr(data$plot_weight, "label")

#2. Examine the distribution more closely

# Calculate selected percentiles
quantile(
  data$Yield,
  probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.995, 1),
  na.rm = TRUE
) # This shows how far the largest observations are from the rest of the data

#3. Identify the 20 highest yield observations

extreme_yields <- data %>%
  mutate(CropCategory = haven::as_factor(CropCategory)) %>%
  select(
    Segment_ID,
    CropCategory,
    Plot_area_ha,
    Crop_Area_ha,
    Harvested_Crop_Area_ha,
    Harvest_ratio,
    Yield
  ) %>%
  filter(!is.na(Yield)) %>%
  arrange(desc(Yield)) %>%
  slice_head(n = 20)

print(extreme_yields, n = 20) #This will help us check whether the highest values are linked to specific crops or unusually large plot areas


#4. Compare yield across crop categories

crop_summary <- data %>%
  mutate(CropCategory = haven::as_factor(CropCategory)) %>%
  group_by(CropCategory) %>%
  summarise(
    observations = n(),
    missing_yield = sum(is.na(Yield)),
    median_yield = median(Yield, na.rm = TRUE),
    mean_yield = mean(Yield, na.rm = TRUE),
    maximum_yield = max(Yield, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_yield))

print(crop_summary, n = 21)# We will use the median as an important comparison measure because it is less sensitive to extreme values than the mean.

## Investigate the yield calculation


# Check metadata for the calculated variables
lapply(
  data[c(
    "Yield",
    "Plot_area_ha",
    "Crop_Area_ha",
    "Harvested_Crop_Area_ha",
    "Harvest_ratio"
  )],
  attributes
)

# Inspect labels for the questionnaire variables
questionnaire_vars <- names(data)[
  grepl("^(s2|s3|s4)", names(data))
]

variable_labels <- tibble(
  variable = questionnaire_vars,
  label = sapply(questionnaire_vars, function(v) {
    label <- attr(data[[v]], "label")
    if (is.null(label)) NA_character_ else as.character(label)
  })
)

print(variable_labels, n = 101)# This will show whether the original questionnaire variables have descriptive labels that help us understand the measurements.

## Investigate the extreme observations by crop

# Compare extreme values with crop-specific percentiles
crop_percentiles <- data %>%
  mutate(CropCategory = haven::as_factor(CropCategory)) %>%
  group_by(CropCategory) %>%
  summarise(
    median_yield = median(Yield, na.rm = TRUE),
    p95_yield = quantile(Yield, 0.95, na.rm = TRUE),
    p99_yield = quantile(Yield, 0.99, na.rm = TRUE),
    maximum_yield = max(Yield, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(maximum_yield))

print(crop_percentiles, n = 21) 

## checks on the relationship between yield, crop area, and harvested area.

# Compare the calculated yield with crop areas
data %>%
  filter(
    !is.na(Yield),
    !is.na(Crop_Area_ha),
    !is.na(Harvested_Crop_Area_ha)
  ) %>%
  summarise(
    yield_area_correlation = cor(
      Yield, Crop_Area_ha,
      use = "complete.obs"
    ),
    yield_harvested_area_correlation = cor(
      Yield, Harvested_Crop_Area_ha,
      use = "complete.obs"
    ),
    median_crop_area = median(Crop_Area_ha, na.rm = TRUE),
    median_harvested_area = median(
      Harvested_Crop_Area_ha, na.rm = TRUE
    )
  )


# check highest-yield observations with original questionnaire variables

data %>%
  filter(!is.na(Yield)) %>%
  arrange(desc(Yield)) %>%
  select(
    Segment_ID,
    CropCategory,
    s2q1:s2q6,
    Plot_area_ha,
    Crop_Area_ha,
    Harvested_Crop_Area_ha,
    Harvest_ratio,
    Yield
  ) %>%
  slice_head(n = 10)


# Verification of Yield calculation

## install package related with Look_for

install.packages("labelled")

library(labelled)

labelled::look_for(data)


# 1. Check labels for the important variables
vars_to_check <- c(
  "s2q2", "s2q4",
  "Plot_area_ha",
  "Crop_Area_ha",
  "Harvested_Crop_Area_ha",
  "Harvest_ratio",
  "Yield"
)

for (v in vars_to_check) {
  cat("\n-----------------------------\n")
  cat("Variable:", v, "\n")
  cat("Label:", attr(data[[v]], "label"), "\n")
  cat("Value labels:\n")
  print(attr(data[[v]], "labels"))
}

# 2. Check whether the area variables are consistent
library(dplyr)

data %>%
  select(
    s2q2, Plot_area_ha,
    Crop_Area_ha, Harvested_Crop_Area_ha,
    Harvest_ratio, Yield
  ) %>%
  slice_head(n = 15) %>%
  print()



## still verifying yield calculation

# Examine the formulas and distributions of the area variables
data %>%
  summarise(
    Total_observations = n(),
    
    Missing_yield = sum(is.na(Yield)),
    Missing_crop_area = sum(is.na(Crop_Area_ha)),
    Missing_harvested_area = sum(is.na(Harvested_Crop_Area_ha)),
    
    Min_harvest_ratio = min(Harvest_ratio, na.rm = TRUE),
    Median_harvest_ratio = median(Harvest_ratio, na.rm = TRUE),
    Max_harvest_ratio = max(Harvest_ratio, na.rm = TRUE),
    
    Min_yield = min(Yield, na.rm = TRUE),
    Median_yield = median(Yield, na.rm = TRUE),
    Max_yield = max(Yield, na.rm = TRUE)
  )

# Check the distribution of harvest ratios
data %>%
  count(Harvest_ratio, sort = TRUE) %>%
  print(n = 30)

# Check whether harvested area equals crop area
data %>%
  summarise(
    Equal_areas = sum(
      abs(Crop_Area_ha - Harvested_Crop_Area_ha) < 0.000001,
      na.rm = TRUE
    ),
    Different_areas = sum(
      abs(Crop_Area_ha - Harvested_Crop_Area_ha) >= 0.000001,
      na.rm = TRUE
    )
  )



# Display variable names and labels related to harvest, production, area, or yield
variable_info <- tibble::tibble(
  variable = names(data),
  label = sapply(data, function(x) {
    lab <- attr(x, "label")
    if (is.null(lab)) NA_character_ else as.character(lab)
  })
)

variable_info %>%
  filter(
    grepl(
      "harvest|production|yield|quantity|crop area|plot area|output",
      paste(variable, label),
      ignore.case = TRUE
    )
  ) %>%
  print(n = Inf)


#display the actual variable labels for the original survey questions


library(labelled)
library(dplyr)
library(tibble)

# Extract labels for the original survey variables
question_labels <- tibble(
  variable = names(data),
  label = vapply(names(data), function(v) {
    lab <- labelled::var_label(data[[v]])
    if (is.null(lab) || length(lab) == 0) {
      return(NA_character_)
    }
    as.character(lab[1])
  }, character(1))
)

# Display all original crop/plot questions
question_labels %>%
  filter(grepl("^s2q", variable)) %>%
  print(n = Inf, width = Inf)


# check the actual values of the most relevant candidate variables:


# Inspect the first 10 observations of candidate variables
candidate_vars <- intersect(
  c(
    "s2q2", "s2q4", "s2q6", "s2q8",
    "s2q13_2", "s2q14", "s2q17",
    "s2q19_2", "s2q19_3", "s2q20_2",
    "s2q21", "s2q22", "s2q23",
    "s2q25", "s2q26", "s2q29",
    "s2q30", "s2q31", "s2q32",
    "s2q33", "s2q34", "s2q35",
    "s2q36", "s2q37_public", "s2q38",
    "s2q41", "s2q42", "s2q43",
    "s2q44", "s2q45", "s2q46",
    "s2q47", "s2q48", "s2q49", "s2q50"
  ),
  names(data)
)

data %>%
  select(all_of(candidate_vars)) %>%
  slice_head(n = 10) %>%
  print(width = Inf)

# yield formulation

yield_check <- data %>%
  mutate(
    yield_from_q21_crop_area =
      s2q21 / Crop_Area_ha,
    
    yield_from_q21_harvested_area =
      s2q21 / Harvested_Crop_Area_ha,
    
    yield_from_q22_crop_area =
      s2q22 / Crop_Area_ha,
    
    yield_from_q22_harvested_area =
      s2q22 / Harvested_Crop_Area_ha
  )

# Compare the existing Yield variable with each possible formula
formula_comparison <- yield_check %>%
  summarise(
    total_rows = n(),
    
    yield_available = sum(!is.na(Yield)),
    
    q21_crop_area_match = sum(
      abs(Yield - yield_from_q21_crop_area) < 1,
      na.rm = TRUE
    ),
    
    q21_harvested_area_match = sum(
      abs(Yield - yield_from_q21_harvested_area) < 1,
      na.rm = TRUE
    ),
    
    q22_crop_area_match = sum(
      abs(Yield - yield_from_q22_crop_area) < 1,
      na.rm = TRUE
    ),
    
    q22_harvested_area_match = sum(
      abs(Yield - yield_from_q22_harvested_area) < 1,
      na.rm = TRUE
    )
  )

print(formula_comparison)


#What this code does: It calculates four possible yield values and counts how many are within 1 unit of your existing Yield. We will also need to check that the numerator and denominator are valid and positive, because a formula comparison can be misleading if missing or zero areas are involved.
#3. Check the area variables and missing values

  area_check <- data %>%
  summarise(
    total_rows = n(),
    
    missing_yield = sum(is.na(Yield)),
    missing_q21 = sum(is.na(s2q21)),
    missing_q22 = sum(is.na(s2q22)),
    
    missing_crop_area = sum(is.na(Crop_Area_ha)),
    missing_harvested_area = sum(is.na(Harvested_Crop_Area_ha)),
    
    zero_crop_area = sum(Crop_Area_ha == 0, na.rm = TRUE),
    zero_harvested_area = sum(
      Harvested_Crop_Area_ha == 0, na.rm = TRUE
    ),
    
    negative_yield = sum(Yield < 0, na.rm = TRUE)
  )

print(area_check)


### verification of Yield

library(dplyr)

yield_verification <- data %>%
  mutate(
    recalculated_yield = if_else(
      !is.na(s2q21) &
        !is.na(Harvested_Crop_Area_ha) &
        Harvested_Crop_Area_ha > 0,
      s2q21 / Harvested_Crop_Area_ha,
      NA_real_
    ),
    difference = Yield - recalculated_yield
  )

# Summarize the comparison
yield_verification %>%
  summarise(
    rows_with_both_values = sum(
      !is.na(Yield) & !is.na(recalculated_yield)
    ),
    matches_within_0_01 = sum(
      abs(difference) < 0.01,
      na.rm = TRUE
    ),
    maximum_absolute_difference = max(
      abs(difference),
      na.rm = TRUE
    ),
    mean_absolute_difference = mean(
      abs(difference),
      na.rm = TRUE
    )
  ) %>%
  print(width = Inf)


yield_verification %>%
  filter(
    !is.na(difference),
    abs(difference) >= 0.01
  ) %>%
  select(
    Segment_ID,
    s2q1,
    s2q2,
    s2q4,
    s2q21,
    Crop_Area_ha,
    Harvested_Crop_Area_ha,
    Yield,
    recalculated_yield,
    difference
  ) %>%
  print(n = Inf, width = Inf)

## prepare the analysis dataset

library(dplyr)
library(haven)

analysis_data <- data %>%
  mutate(
    province = haven::as_factor(s1q1),
    district = haven::as_factor(s1q2),
    farmer_type = haven::as_factor(s1q7),
    crop = haven::as_factor(CropCategory),
    gender = haven::as_factor(s1q8),
    log_yield = if_else(Yield > 0, log(Yield), NA_real_)
  ) %>%
  filter(
    !is.na(Yield),
    is.finite(Yield),
    Yield > 0,
    !is.na(CropCategory),
    !is.na(s1q2),
    !is.na(s1q7),
    !is.na(Harvested_Crop_Area_ha),
    Harvested_Crop_Area_ha > 0
  )

# Check the resulting dataset
cat("Original rows:", nrow(data), "\n")
cat("Analysis rows:", nrow(analysis_data), "\n")
cat("Rows excluded:", nrow(data) - nrow(analysis_data), "\n")

# Review the main variables
analysis_data %>%
  select(
    province, district, farmer_type, crop,
    gender, Harvested_Crop_Area_ha, Yield, log_yield
  ) %>%
  glimpse()

# Overall productivity summary by crop
crop_productivity <- analysis_data %>%
  group_by(crop) %>%
  summarise(
    observations = n(),
    median_yield = median(Yield, na.rm = TRUE),
    mean_yield = mean(Yield, na.rm = TRUE),
    q1_yield = quantile(Yield, 0.25, na.rm = TRUE),
    q3_yield = quantile(Yield, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_yield))

print(crop_productivity, n = Inf)

## Save dataset

saveRDS(analysis_data, "analysis_data.rds")

# check that the file exist

file.exists("analysis_data.rds")


