

## Creating the codes for the shiny app 


# ============================================================
# Rwanda Agricultural Productivity Explorer
# NISR Big Data Hackathon 2026
# Kwigira Team
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(bslib)

# Load the prepared production dataset
analysis_data <- readRDS("analysis_data.rds")

# Load the agricultural-practice data
practice_data <- read.delim(
  "Rwa_raw_SeasonA2025_Agricultural_practice.csv",
  header = TRUE,
  sep = "\t",
  colClasses = "character",
  check.names = FALSE
)

# Keep records with valid yield values for productivity analysis
productivity_data <- analysis_data %>%
  filter(!is.na(Yield), is.finite(Yield), Yield > 0) %>%
  mutate(
    crop = as.character(crop),
    district = as.character(district),
    province = as.character(province),
    farmer_type = as.character(farmer_type),
    
    # Group individual crops into broader categories
    crop_group = case_when(
      crop %in% c(
        "Maize", "Sorghum", "Wheat",
        "Other cereals", "Paddy rice"
      ) ~ "Cereals",
      
      crop %in% c(
        "Bush bean", "Climbing bean", "Soybean",
        "Pea", "Groundnut"
      ) ~ "Legumes",
      
      crop %in% c(
        "Cassava", "Irish potato", "Sweet potato",
        "Taro & Yams"
      ) ~ "Roots and Tubers",
      
      crop %in% c(
        "Cooking banana", "Dessert banana",
        "Banana for beer"
      ) ~ "Bananas",
      
      crop == "Vegetables" ~ "Vegetables",
      
      TRUE ~ "Other Crops"
    )
  )
library(dplyr)

# Prepare consistent identifiers for joining
production_for_model <- productivity_data %>%
  mutate(
    Segment_ID = as.character(Segment_ID),
    s2q1 = as.character(s2q1)
  )

practice_for_model <- practice_data %>%
  mutate(
    Segment_ID = as.character(Segment_ID),
    s2q1 = as.character(s2q1)
  )

# Check for duplicate keys in agricultural-practice data
duplicate_practice_keys <- practice_for_model %>%
  count(Segment_ID, s2q1, name = "records") %>%
  filter(records > 1)

if (nrow(duplicate_practice_keys) > 0) {
  stop(
    "Practice data contains duplicate Segment_ID + s2q1 keys. ",
    "Resolve these before merging."
  )
}

# Merge practice variables into a separate modeling dataset
productivity_model_data <- production_for_model %>%
  left_join(
    practice_for_model %>%
      select(-any_of("plot_weight")),
    by = c("Segment_ID", "s2q1"),
    suffix = c("", "_practice")
  )

# Verify that the merge did not multiply production records
stopifnot(nrow(productivity_model_data) == nrow(production_for_model))

cat("Original production records:", nrow(production_for_model), "\n")
cat("Records after practice merge:", nrow(productivity_model_data), "\n")
cat("Practice variables available:",
    ncol(productivity_model_data) - ncol(production_for_model), "\n")


# ============================================================
# PART 3: CROP YIELD PREDICTION MODEL
# ============================================================

library(ranger)

# Select predictors that may be available before harvest
prediction_variables <- c(
  "Yield",
  "crop",
  "province",
  "district",
  "farmer_type",
  "gender",
  "Plot_area_ha"
)

# Check that the required columns exist
missing_variables <- setdiff(
  prediction_variables,
  names(productivity_model_data)
)

if (length(missing_variables) > 0) {
  stop(
    "Missing prediction variables: ",
    paste(missing_variables, collapse = ", ")
  )
}

# Prepare model data
model_data <- productivity_model_data %>%
  select(all_of(prediction_variables)) %>%
  filter(
    !is.na(Yield),
    is.finite(Yield),
    Yield > 0,
    !is.na(crop),
    !is.na(province),
    !is.na(district),
    !is.na(farmer_type),
    !is.na(gender),
    !is.na(Plot_area_ha),
    is.finite(Plot_area_ha),
    Plot_area_ha > 0
  ) %>%
  mutate(
    across(
      c(crop, province, district, farmer_type, gender),
      as.factor
    )
  ) %>%
  droplevels()

# Split observations into training and testing datasets
set.seed(123)

train_rows <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.80 * nrow(model_data))
)

train_data <- model_data[train_rows, ]
test_data <- model_data[-train_rows, ]

# Train the Random Forest model
yield_model <- ranger(
  Yield ~ crop + province + district +
    farmer_type + gender + Plot_area_ha,
  data = train_data,
  num.trees = 500,
  importance = "permutation",
  seed = 123
)

# Evaluate predictions on held-out test data
test_predictions <- predict(
  yield_model,
  data = test_data
)$predictions

actual_yield <- test_data$Yield

model_rmse <- sqrt(mean((actual_yield - test_predictions)^2))
model_mae <- mean(abs(actual_yield - test_predictions))

model_r_squared <- 1 -
  sum((actual_yield - test_predictions)^2) /
  sum((actual_yield - mean(actual_yield))^2)

cat("\n--- Crop Yield Model Evaluation ---\n")
cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")
cat("RMSE:", round(model_rmse, 2), "\n")
cat("MAE:", round(model_mae, 2), "\n")
cat("R-squared:", round(model_r_squared, 4), "\n")

# Save predictor importance for later display
model_importance <- data.frame(
  Variable = names(yield_model$variable.importance),
  Importance = as.numeric(yield_model$variable.importance)
) %>%
  arrange(desc(Importance))

  # Dashboard interface with agricultural colours
  ui <- fluidPage(
    theme = bslib::bs_theme(
      version = 5,
      bg = "ghostwhite",
      fg = "darkslategray",
      primary = "seagreen",
      secondary = "orange",
      success = "seagreen",
      base_font = bslib::font_google("Roboto")
    ),
    
    tags$head(
      tags$style(HTML("
      body {
        background-color: honeydew;
      }

      .container-fluid {
        padding: 24px;
      }

      h1, h2, h3, h4 {
        color: darkgreen;
        font-weight: bold;
      }

      .well {
        background-color: honeydew;
        border: 1px solid gainsboro;
        border-radius: 12px;
        box-shadow: none;
      }

      .form-control, .selectize-input {
        border-radius: 8px;
      }

      .stat-card {
        background: white;
        border: 1px solid gainsboro;
        border-left: 5px solid gainsboro;
        border-radius: 10px;
        padding: 18px;
        margin-bottom: 16px;
      }

      .stat-label {
        color: seagreen;
        font-size: 14px;
      }

      .stat-value {
        color: black;
        font-size: 25px;
        font-weight: bold;
      }

      .table {
        background-color: white;
      }

      .table thead th {
        background-color: honeydew;
        color: white;
      }

      .table-striped > tbody > tr:nth-of-type(odd) > * {
        background-color: lightgray;
      }
    "))
    ),
    
    
    # Header with existing leaf icon and KWIGIRA logo
    fluidRow(
      column(
        width = 9,
        titlePanel(
          div(
            icon("leaf"),
            " Rwanda Agricultural Productivity Explorer and Yield predictor"
          )
        ),
        p("Explore observed crop productivity across districts and predict harvest based on current production")
      ),
      column(
        width = 3,
        tags$img(
          src = "kwigira_logo.png",
          alt = "KWIGIRA Team Logo",
          style = paste(
            "width: 100%;",
            "max-width: 170px;",
            "height: auto;",
            "display: block;",
            "margin-left: auto;",
            "margin-right: auto;"
          )
        )
      )
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput(
          "crop",
          "Select crop:",
          choices = c("All crops", sort(unique(productivity_data$crop))),
          selected = "All crops"
        ),
        
        
        selectInput(
          "province",
          "Select province:",
          choices = c("All provinces", sort(unique(productivity_data$province))),
          selected = "All provinces"
        ),
        
        selectInput(
          "district",
          "Select district:",
          choices = c("All districts", sort(unique(productivity_data$district))),
          selected = "All districts"
        ),
        
        helpText(
          "Results describe observed survey data. ",
          "They do not necessarily explain the causes of yield differences."
        )
      ),
      
      mainPanel(
        h3("Agricultural Productivity Dashboard"),
        
        fluidRow(
          column(
            4,
            div(
              class = "stat-card",
              div(class = "stat-label", "Records"),
              div(class = "stat-value", textOutput("n_records"))
            )
          ),
          column(
            4,
            div(
              class = "stat-card",
              div(class = "stat-label", "Median yield"),
              div(class = "stat-value", textOutput("median_yield"))
            )
          ),
          column(
            4,
            div(
              class = "stat-card",
              div(class = "stat-label", "Crop categories"),
              div(class = "stat-value", textOutput("n_crops"))
            )
          )
        ),
        
        plotOutput("yield_plot", height = "450px"),
        
        # District comparison chart
        h4("District Median Yield Comparison"),
        
        p(
          "Compare median observed yields across districts for the selected crop and filters."
        ),
        
        plotOutput("district_median_plot", height = "450px"),
        
        h4("Explore Top-Performing Districts"),
        
        numericInput(
          inputId = "top_n_districts",
          label = "Number of districts to display:",
          value = 5,
          min = 1,
          max = 30,
          step = 1
        ),
        
        tableOutput("top_districts"),
    
        
        # Automatic Statistical Insights
        h4("Automatic Agricultural Insights"),
        uiOutput("auto_insights"),
        
        
        # ============================================================
        # PART 3: PRE-HARVEST CROP YIELD PREDICTION INTERFACE
        # ============================================================
        
        h3("Pre-Harvest Crop Yield Prediction"),
        
        p(
          "Enter the crop and farm characteristics to estimate crop yield ",
          "using the Random Forest model."
        ),
        
        fluidRow(
          column(
            6,
            selectInput(
              "pred_crop",
              "Crop category:",
              choices = sort(unique(as.character(train_data$crop)))
            ),
            
            selectInput(
              "pred_province",
              "Province:",
              choices = sort(unique(as.character(train_data$province)))
            ),
            
            selectInput(
              "pred_district",
              "District:",
              choices = sort(unique(as.character(train_data$district)))
            )
          ),
          
          column(
            6,
            selectInput(
              "pred_farmer_type",
              "Farmer type:",
              choices = sort(unique(as.character(train_data$farmer_type)))
            ),
            
            selectInput(
              "pred_gender",
              "Farmer gender:",
              choices = sort(unique(as.character(train_data$gender)))
            ),
            
            numericInput(
              "pred_plot_area",
              "Plot area (hectares):",
              value = 1,
              min = 0.001,
              step = 0.1
            )
          )
        ),
        
        actionButton(
          "predict_yield",
          "Predict Crop Yield",
          class = "btn-success"
        ),
        
        br(),
        br(),
        
        div(
          class = "stat-card",
          div(class = "stat-label", "Predicted crop yield"),
          div(class = "stat-value", textOutput("predicted_yield"))
        ),
        
        h4("Prediction Model Performance"),
        
        tableOutput("model_metrics"),
        
        h4("Factors Contributing to Prediction"),
        
        plotOutput("prediction_importance_plot", height = "350px"),
        
        # Part 2: Productivity Assessment
        h3("Productivity Assessment"),
        
        p(
          "Compare observed crop yields within each crop category. ",
          "Classifications are based on the distribution of the currently ",
          "filtered data, not official national benchmarks."
        ),
  
        
        fluidRow(
          column(
            4,
            div(
              class = "stat-card",
              div(class = "stat-label", "Relatively low productivity"),
              div(class = "stat-value", textOutput("low_yield_count"))
            )
          ),
          column(
            4,
            div(
              class = "stat-card",
              div(class = "stat-label", "Typical productivity"),
              div(class = "stat-value", textOutput("typical_yield_count"))
            )
          ),
          column(
            4,
            div(
              class = "stat-card",
              div(class = "stat-label", "Relatively high productivity"),
              div(class = "stat-value", textOutput("high_yield_count"))
            )
          )
        ),
        
        plotOutput("assessment_plot", height = "350px"),
        
        h4("Explore Productivity Levels by Crop"),
        
        numericInput(
          inputId = "top_n_assessments",
          label = "Number of assessment rows to display:",
          value = 5,
          min = 1,
          max = 100,
          step = 1
        ),
        
        tableOutput("assessment_table"),
        
        # Agricultural Question-and-Answer Assistant
        h3("Agricultural Assistant"),
        p("Ask questions about crop yields, districts, provinces, and productivity."),
        
        wellPanel(
          h4("Ask a question"),
          
          textInput(
            inputId = "assistant_question",
            label = NULL,
            placeholder = "e.g. Which district has the highest median yield for maize?"
          ),
          
          
          fluidRow(
            column(
              width = 6,
              actionButton(
                inputId = "ask_assistant",
                label = "Ask question",
                icon = icon("comments"),
                width = "100%"
              )
            ),
            column(
              width = 6,
              actionButton(
                inputId = "new_conversation",
                label = "New Conversation",
                icon = icon("refresh"),
                width = "100%"
              )
            )
          ),
          
          br(),
          br(),
          
          uiOutput("assistant_conversation")
        )
        
      )
    )
  )
  
  # Application logic
  server <- function(input, output, session) { 
    
    # Update districts according to the selected province
    observeEvent(input$province, {
      
      available_data <- productivity_data
      
      if (input$province != "All provinces") {
        available_data <- available_data %>%
          filter(province == input$province)
      }
      
      district_choices <- sort(unique(as.character(available_data$district)))
      district_choices <- district_choices[!is.na(district_choices)]
      
      updateSelectInput(
        session,
        "district",
        choices = c("All districts", district_choices),
        selected = "All districts"
      )
      
    })
    
    filtered_data <- reactive({
      result <- productivity_data
      
      if (input$crop != "All crops") {
        result <- result %>% filter(crop == input$crop)
      }
      
      if (input$district != "All districts") {
        result <- result %>% filter(district == input$district)
      }
      
      if (input$province != "All provinces") {
        result <- result %>% filter(province == input$province)
      }
      
      result
    })
    
    
    # Automatic Statistical Insights
    output$auto_insights <- renderUI({
      
      data <- filtered_data()
      
      
      # Data Quality Checks
      missing_yield <- sum(is.na(data$Yield))
      
      invalid_yield <- sum(
        !is.na(data$Yield) &
          (!is.finite(data$Yield) | data$Yield < 0)
      )
      
      # Valid, non-negative yield values
      valid_yields <- data$Yield[
        is.finite(data$Yield) & data$Yield >= 0
      ]
      
      # Identify potential outliers using the IQR rule
      if (length(valid_yields) >= 4) {
        q1 <- quantile(valid_yields, 0.25, na.rm = TRUE)
        q3 <- quantile(valid_yields, 0.75, na.rm = TRUE)
        iqr_value <- q3 - q1
        
        lower_limit <- q1 - 1.5 * iqr_value
        upper_limit <- q3 + 1.5 * iqr_value
        
        outlier_count <- sum(
          valid_yields < lower_limit |
            valid_yields > upper_limit
        )
      } else {
        outlier_count <- NA_integer_
      }
      
      # Check whether any district has a small sample
      district_counts <- data %>%
        filter(is.finite(Yield), Yield >= 0) %>%
        group_by(province, district) %>%
        summarise(Records = n(), .groups = "drop")
      
      small_sample_districts <- sum(
        district_counts$Records < 30
      )
      
      # Check whether the selected filters return data
      if (nrow(data) == 0) {
        return(tags$p("No observations match the selected filters."))
      }
      
      # Remove missing or invalid yield values
      valid_data <- data %>%
        filter(is.finite(Yield))
      
      if (nrow(valid_data) == 0) {
        return(tags$p("No valid yield observations are available for these filters."))
      }
      
      # Overall statistics for the selected data
      overall_median <- median(valid_data$Yield, na.rm = TRUE)
      overall_mean <- mean(valid_data$Yield, na.rm = TRUE)
      total_records <- nrow(valid_data)
      
      # Summarize productivity by district
      district_summary <- valid_data %>%
        group_by(province, district) %>%
        summarise(
          Records = n(),
          Median_Yield = median(Yield, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(is.finite(Median_Yield))
      
      # Create the insights
      insight_items <- list(
        tags$p(
          tags$strong("1. Data coverage: "),
          paste(
            format(total_records, big.mark = ","),
            "valid yield observations are included in the current analysis."
          )
        ),
        tags$p(
          tags$strong("2. Overall productivity: "),
          paste(
            "The median yield is",
            round(overall_median, 2),
            "and the mean yield is",
            round(overall_mean, 2),
            "in the dataset's recorded yield"
          )
        )
      )
      
      # Compare districts only when at least one district is available
      if (nrow(district_summary) > 0) {
        
        highest <- district_summary %>%
          arrange(desc(Median_Yield), desc(Records)) %>%
          slice(1)
        
        lowest <- district_summary %>%
          arrange(Median_Yield, desc(Records)) %>%
          slice(1)
        
        insight_items <- append(
          insight_items,
          list(
            tags$p(
              tags$strong("3. Highest observed district median: "),
              paste0(
                highest$district, ", ", highest$province,
                " (median yield: ",
                round(highest$Median_Yield, 2),
                "; observations: ",
                highest$Records, ")."
              )
            ),
            tags$p(
              tags$strong("4. Lowest observed district median: "),
              paste0(
                lowest$district, ", ", lowest$province,
                " (median yield: ",
                round(lowest$Median_Yield, 2),
                "; observations: ",
                lowest$Records, ")."
              )
            )
          )
        )
      }
      
      # Add interpretation guidance
      if (input$crop == "All crops") {
        insight_items <- append(
          insight_items,
          list(
            tags$p(
              tags$strong("Interpretation note: "),
              "Different crops can have different yield scales. ",
              "District comparisons across all crops may therefore be misleading. ",
              "Select one crop for a more meaningful comparison."
            )
          )
        )
      }
      
      
      # Data quality warning messages
      warning_items <- list()
      
      if (missing_yield > 0) {
        warning_items <- append(
          warning_items,
          list(tags$li(
            paste(missing_yield, "observations have missing yield values.")
          ))
        )
      }
      
      if (invalid_yield > 0) {
        warning_items <- append(
          warning_items,
          list(tags$li(
            paste(invalid_yield, "observations have negative or non-finite yields.")
          ))
        )
      }
      
      if (!is.na(outlier_count) && outlier_count > 0) {
        warning_items <- append(
          warning_items,
          list(tags$li(
            paste(
              outlier_count,
              "yield values fall outside the IQR screening limits.",
              "Investigate these values before treating them as errors."
            )
          ))
        )
      }
      
      if (small_sample_districts > 0) {
        warning_items <- append(
          warning_items,
          list(tags$li(
            paste(
              small_sample_districts,
              "districts have fewer than 30 valid yield observations.",
              "Their median yields may be less stable."
            )
          ))
        )
      }
      
      warning_section <- if (length(warning_items) == 0) {
        tags$p("No issues were detected by these checks.")
      } else {
        tags$ul(warning_items)
      }
      
      tags$div(
        style = paste(
          "padding: 15px;",
          "border: 1px solid plum",
          "border-radius: 8px;",
          "margin-bottom: 15px;"
        ),
        insight_items,
        tags$hr(),
        tags$h4("Data Quality Warnings"),
        warning_section
      )
      
    })
    
    # Top-performing districts based on median observed yield
    output$top_districts <- renderTable({
      
      filtered_data() %>%
        group_by(province, district) %>%
        summarise(
          Records = n(),
          Median_Yield = median(Yield, na.rm = TRUE),
          Mean_Yield = mean(Yield, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(Records > 0) %>%
        slice_max(
          order_by = Median_Yield,
          n = input$top_n_districts,
          with_ties = FALSE
        ) %>%
        arrange(desc(Median_Yield))
      
    })
  # PART 2: Prediction code
  
  predicted_yield_value <- eventReactive(input$predict_yield, {
    
    req(
      input$pred_crop,
      input$pred_province,
      input$pred_district,
      input$pred_farmer_type,
      input$pred_gender,
      input$pred_plot_area
    )
    
    validate(
      need(
        is.finite(input$pred_plot_area) && input$pred_plot_area > 0,
        "Plot area must be greater than zero."
      )
    )
    
    # Prepare new data using the same variables as the model
    new_data <- data.frame(
      crop = factor(
        input$pred_crop,
        levels = levels(train_data$crop)
      ),
      province = factor(
        input$pred_province,
        levels = levels(train_data$province)
      ),
      district = factor(
        input$pred_district,
        levels = levels(train_data$district)
      ),
      farmer_type = factor(
        input$pred_farmer_type,
        levels = levels(train_data$farmer_type)
      ),
      gender = factor(
        input$pred_gender,
        levels = levels(train_data$gender)
      ),
      Plot_area_ha = as.numeric(input$pred_plot_area)
    )
    
    # Check whether selected categories are known to the model
    validate(
      need(!is.na(new_data$crop), "Selected crop was not available in the training data."),
      need(!is.na(new_data$province), "Selected province was not available in the training data."),
      need(!is.na(new_data$district), "Selected district was not available in the training data."),
      need(!is.na(new_data$farmer_type), "Selected farmer type was not available in the training data."),
      need(!is.na(new_data$gender), "Selected gender was not available in the training data.")
    )
    
    # Generate prediction
    prediction <- predict(
      yield_model,
      data = new_data
    )$predictions
    
    predicted_yield <- as.numeric(prediction[1])
    
    if (predicted_yield < 1000) {
      formatted_yield <- paste0(
        format(round(predicted_yield, 2), big.mark = ","),
        " kg/ha"
      )
    } else {
      formatted_yield <- paste0(
        format(round(predicted_yield / 1000, 2), big.mark = ","),
        " tonnes/ha"
      )
    }
    
    predicted_yield
  })
  
  # Display predicted crop yield
  output$predicted_yield <- renderText({
    
    req(input$predict_yield)
    
    value <- predicted_yield_value()
    
    if (value < 1000) {
      paste(
        "Predicted crop yield:",
        scales::comma(round(value, 2)),
        "kg/ha"
      )
    } else {
      paste(
        "Predicted crop yield:",
        scales::comma(round(value / 1000, 2)),
        "tonnes/ha"
      )
    }
  })
  
  # Display model performance
  output$model_metrics <- renderTable({
    
    data.frame(
      Metric = c(
        "Training records",
        "Testing records",
        "RMSE",
        "MAE",
        "R-squared"
      ),
      
      Value = c(
        nrow(train_data),
        nrow(test_data),
        round(model_rmse, 2),
        round(model_mae, 2),
        round(model_r_squared, 4)
      )
    )
  })
  
  # Display variable importance
  output$prediction_importance_plot <- renderPlot({
    
    ggplot(
      model_importance,
      aes(
        x = reorder(Variable, Importance),
        y = Importance
      )
    ) +
      geom_col(fill = "seagreen") +
      coord_flip() +
      labs(
        title = "Factors Contributing to Crop Yield Prediction",
        x = "Predictor",
        y = "Importance"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # PART 3: Dashboard output code
  
  # Total number of records
  output$n_records <- renderText({
    
    scales::comma(nrow(filtered_data()))
  })
  
  # Median crop yield
  output$median_yield <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    scales::comma(
      round(median(data$Yield, na.rm = TRUE), 2)
    )
  })
  
  # Number of different crops
  output$n_crops <- renderText({
    
    dplyr::n_distinct(filtered_data()$crop)
  })
  
  # Crop yield distribution graph
  output$yield_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No records match the selected filters."
      )
    )
    
    ggplot(
      data,
      aes(x = crop, y = Yield)
    ) +
      geom_boxplot(
        fill = "seagreen",
        alpha = 0.7
      ) +
      labs(
        title = "Observed Crop Yield Distribution",
        x = "Crop Category",
        y = "Yield (tonnes/ha)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  
  # District median yield comparison
  output$district_median_plot <- renderPlot({
    
    data <- filtered_data()
    
    # Keep valid yield observations
    data <- data %>%
      filter(is.finite(Yield))
    
    # Display a message if no valid observations remain
    validate(
      need(nrow(data) > 0, "No valid yield observations for the selected filters.")
    )
    
    # Calculate median yield for each district
    district_summary <- data %>%
      group_by(district) %>%
      summarise(
        Median_Yield = median(Yield, na.rm = TRUE),
        Records = n(),
        .groups = "drop"
      ) %>%
      filter(is.finite(Median_Yield)) %>%
      arrange(Median_Yield)
    
    validate(
      need(
        nrow(district_summary) > 0,
        "No district-level results are available."
      )
    )
    
    # Plot districts from lowest to highest median yield
    ggplot(
      district_summary,
      aes(
        x = reorder(district, Median_Yield),
        y = Median_Yield
      )
    ) +
      geom_col(
        fill = "seagreen",
        width = 0.75
      ) +
      coord_flip() +
      labs(
        title = "Median Yield by District",
        subtitle = "Based on the currently selected crop and filters",
        x = "District",
        y = "Median observed yield"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(
          face = "bold",
          color = "seagreen"
        ),
        axis.text = element_text(color = "seagreen"),
        panel.grid.major.y = element_blank()
      )
  })
  
  
  # PART 4: Productivity assessment code
  
  # Classify productivity within each crop
  assessment_data <- reactive({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No data available for productivity assessment."
      )
    )
    
    data %>%
      group_by(crop) %>%
      mutate(
        crop_q25 = quantile(
          Yield,
          probs = 0.25,
          na.rm = TRUE
        ),
        
        crop_q75 = quantile(
          Yield,
          probs = 0.75,
          na.rm = TRUE
        ),
        
        productivity_level = case_when(
          Yield < crop_q25 ~ "Relatively low",
          Yield > crop_q75 ~ "Relatively high",
          TRUE ~ "Typical"
        ),
        productivity_level = factor(
          productivity_level,
          levels = c(
            "Relatively low",
            "Typical",
            "Relatively high"
          )
        )
      ) %>%
      ungroup()
  })
  
  # Count records with relatively low productivity
  output$low_yield_count <- renderText({
    
    sum(
      assessment_data()$productivity_level == "Relatively low",
      na.rm = TRUE
    )
  })
  
  # Count records with typical productivity
  output$typical_yield_count <- renderText({
    
    sum(
      assessment_data()$productivity_level == "Typical",
      na.rm = TRUE
    )
  })
  
  # Count records with relatively high productivity
  output$high_yield_count <- renderText({
    
    sum(
      assessment_data()$productivity_level == "Relatively high",
      na.rm = TRUE
    )
  })
  
  # Productivity assessment graph
  
  output$assessment_plot <- renderPlot({
    assessment_data <- assessment_data()
    
    ggplot(
      assessment_data,
      aes(x = crop, fill = productivity_level)
    ) +
      geom_bar(position = "stack") +
      scale_fill_manual(
        values = c(
          "Relatively low" = "red",
          "Typical" = "yellow",
          "Relatively high" = "seagreen"
        ),
        breaks = c(
          "Relatively low",
          "Typical",
          "Relatively high"
        ),
        drop = FALSE
      ) +
      labs(
        title = "Productivity Classification by Crop",
        subtitle = "Number of records within each productivity level",
        x = "Crop category",
        y = "Number of records",
        fill = "Productivity level"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  })
  
  
  
  # Productivity assessment table with adjustable number of rows
  output$assessment_table <- renderTable({
    
    assessment_data <- filtered_data() %>%
      filter(is.finite(Yield)) %>%
      group_by(crop) %>%
      mutate(
        productivity_level = case_when(
          Yield < quantile(Yield, 1/3, na.rm = TRUE) ~ "Relatively low",
          Yield > quantile(Yield, 2/3, na.rm = TRUE) ~ "Relatively high",
          TRUE ~ "Typical"
        )
      ) %>%
      group_by(crop, productivity_level) %>%
      summarise(
        Records = n(),
        Median_Yield = median(Yield, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      slice_max(
        order_by = Median_Yield,
        n = input$top_n_assessments,
        with_ties = FALSE
      ) %>%
      arrange(desc(Median_Yield))
    
    assessment_data
  })
  
  # --------------------------------------------------
  # R-powered Agricultural Question-and-Answer Assistant
  # --------------------------------------------------
  
  assistant_history <- reactiveVal(list())
  
  observeEvent(input$ask_assistant, {
    
    question <- trimws(input$assistant_question)
    
    if (is.null(question) || !nzchar(question)) {
      return()
    }
    
    question_lower <- tolower(question)
    
    # Start with data matching the dashboard filters
    assistant_data <- filtered_data()
    
    # Identify a crop mentioned in the question
    crop_names <- unique(as.character(productivity_data$crop))
    crop_names <- crop_names[!is.na(crop_names) & nzchar(crop_names)]
    
    matched_crops <- crop_names[
      vapply(
        crop_names,
        function(x) grepl(tolower(x), question_lower, fixed = TRUE),
        logical(1)
      )
    ]
    
    if (length(matched_crops) > 0) {
      # Prefer the longest matching crop name
      selected_crop <- matched_crops[
        which.max(nchar(matched_crops))
      ]
      
      assistant_data <- productivity_data %>%
        filter(crop == selected_crop)
    }
    
    # Identify a district mentioned in the question
    district_names <- unique(as.character(productivity_data$district))
    district_names <- district_names[
      !is.na(district_names) & nzchar(district_names)
    ]
    
    matched_districts <- district_names[
      vapply(
        district_names,
        function(x) grepl(tolower(x), question_lower, fixed = TRUE),
        logical(1)
      )
    ]
    
    if (length(matched_districts) > 0) {
      selected_district <- matched_districts[
        which.max(nchar(matched_districts))
      ]
      
      assistant_data <- assistant_data %>%
        filter(district == selected_district)
    }
    
    # Identify a province mentioned in the question
    province_names <- unique(as.character(productivity_data$province))
    province_names <- province_names[
      !is.na(province_names) & nzchar(province_names)
    ]
    
    matched_provinces <- province_names[
      vapply(
        province_names,
        function(x) grepl(tolower(x), question_lower, fixed = TRUE),
        logical(1)
      )
    ]
    
    if (length(matched_provinces) > 0) {
      selected_province <- matched_provinces[
        which.max(nchar(matched_provinces))
      ]
      
      assistant_data <- assistant_data %>%
        filter(province == selected_province)
    }
    
    # Keep valid, non-negative yield values for calculations
    valid_data <- assistant_data %>%
      filter(is.finite(Yield), Yield >= 0)
    
    # Produce a response based on the question
    if (nrow(assistant_data) == 0 || nrow(valid_data) == 0) {
      
      answer <- paste(
        "I could not find valid yield observations for that question.",
        "Try another crop, district, or province."
      )
      
    } else if (
      grepl("highest|top|most productive|best", question_lower)
    ) {
      
      result <- valid_data %>%
        group_by(province, district) %>%
        summarise(
          Records = n(),
          Median_Yield = median(Yield, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Median_Yield), desc(Records)) %>%
        slice(1)
      
      answer <- paste0(
        "The district with the highest observed median yield is ",
        result$district, ", ", result$province,
        ". Its median yield is ",
        round(result$Median_Yield, 2),
        " based on ", result$Records, " valid observations. ",
        "This result applies to the selected data; ",
        "it does not establish the cause of the difference."
      )
      
    } else if (
      grepl("lowest|least productive|worst", question_lower)
    ) {
      
      result <- valid_data %>%
        group_by(province, district) %>%
        summarise(
          Records = n(),
          Median_Yield = median(Yield, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(Median_Yield, desc(Records)) %>%
        slice(1)
      
      answer <- paste0(
        "The district with the lowest observed median yield is ",
        result$district, ", ", result$province,
        ". Its median yield is ",
        round(result$Median_Yield, 2),
        " based on ", result$Records, " valid observations."
      )
      
    } else if (grepl("median", question_lower)) {
      
      answer <- paste0(
        "The median yield in the selected observations is ",
        round(median(valid_data$Yield, na.rm = TRUE), 2),
        ", based on ", nrow(valid_data),
        " valid observations."
      )
      
    } else if (
      grepl("average|mean", question_lower)
    ) {
      
      answer <- paste0(
        "The mean yield in the selected observations is ",
        round(mean(valid_data$Yield, na.rm = TRUE), 2),
        ", based on ", nrow(valid_data),
        " valid observations."
      )
      
    } else if (
      grepl("how many|number of|count|records|observations", question_lower)
    ) {
      
      answer <- paste0(
        "There are ", nrow(valid_data),
        " valid yield observations matching the question."
      )
      
    } else if (
      grepl("data quality|missing|outlier|unusual|invalid", question_lower)
    ) {
      
      missing_count <- sum(is.na(assistant_data$Yield))
      invalid_count <- sum(
        !is.na(assistant_data$Yield) &
          (!is.finite(assistant_data$Yield) |
             assistant_data$Yield < 0)
      )
      
      answer <- paste0(
        "For the selected data, there are ",
        missing_count, " missing yield values and ",
        invalid_count, " negative or non-finite yield values. ",
        "These checks identify potential data quality issues; ",
        "they do not confirm that every other value is correct."
      )
      
    } else {
      
      answer <- paste(
        "I can currently answer questions about the highest or lowest",
        "district median yield, mean yield, median yield, observation counts,",
        "and basic yield data quality.",
        "Try: 'Which district has the highest median yield for maize?'"
      )
    }
    
    # Save the question and answer in the conversation history
    history <- assistant_history()
    
    history[[length(history) + 1]] <- list(
      question = question,
      answer = answer
    )
    
    assistant_history(history)
    
    # Clear the question box after submitting
    updateTextInput(
      session,
      "assistant_question",
      value = ""
    )
  })
  # Start a new conversation
  observeEvent(input$new_conversation, {
    
    # Clear the conversation history
    assistant_history(list())
    
    # Clear the question input 
    updateTextInput(
      session,
      "assistant_question",
      value = ""
    )
    
  })
  # Display the conversation
  output$assistant_conversation <- renderUI({
    
    history <- assistant_history()
    
    if (length(history) == 0) {
      return(
        tags$p(
          "Your questions and answers will appear here.",
          style = "color: gray"
        )
      )
    }
    
    tags$div(
      lapply(history, function(item) {
        tags$div(
          style = paste(
            "padding: 12px;",
            "margin-top: 10px;",
            "border: 1px solid darkgreen;",
            "border-radius: 8px;"
          ),
          tags$p(
            tags$strong("You: "),
            item$question
          ),
          tags$p(
            tags$strong("Agricultural Assistant: "),
            item$answer
          )
        )
      })
    )
  })
  
}
  

  # RUN THE SHINY APPLICATION
  
  shinyApp(ui = ui, server = server)

