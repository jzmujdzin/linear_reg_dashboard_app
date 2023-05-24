library(DT)
library(corrplot)
library(haven)
library(tidyverse)
library(lmtest)
library(moments)
library(stargazer)
library(sandwich)
library(knitr)
library(car)
library(tseries)
library(glue)
library(shiny)

#' Interactive Linear Regression Shiny App
#'
#' This function launches a Shiny app for performing interactive linear regression analysis.
#' The app allows users to upload a CSV file, choose various options for data preprocessing,
#' perform exploratory data analysis, fit linear regression models, conduct model diagnostics,
#' perform stepwise model selection, and compare different models.
#'
#' @author Adam Janczyszyn, Jakub Zmujdzin
#' @return This function launches a Shiny app and does not return any value.
#' @examples
#' \dontrun{
#' linearRegressionApp()
#' }
#' @export
linearRegressionApp <- function() {

  ui <- fluidPage(
    titlePanel("Interactive Linear Regression"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose a CSV file:",
                  accept = c(".csv")),
        br(),
        radioButtons(
          "sep",
          "Separator:",
          choices = c(
            "Comma" = ",",
            "Semicolon" = ";",
            "Tab" = "\t"
          ),
          selected = ","
        ),
        br(),
        radioButtons(
          "dec",
          "Decimal Operator:",
          choices = c("Period" = ".", "Comma" = ","),
          selected = "."
        ),
        br(),
        radioButtons(
          "header",
          "Header?",
          choices = c("Yes" = TRUE, "No" = FALSE),
          selected = TRUE
        ),
        br(),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Summary", DT::dataTableOutput("data_table")),
          tabPanel(
            "Data Types",
            fluidRow(
              h3(
                "Warning: Changing data types to inappropriate ones may cause NaNs to appear.",
                style = "color: red;"
              )
            ),
            fluidRow(uiOutput("select_types")),
            fluidRow(h3("Summary of dtypes:")),
            fluidRow(verbatimTextOutput("dtypes_summary"))
          ),
          tabPanel("Scatter Plot",
                   fluidRow(
                     uiOutput("select_vars"),
                     plotOutput("scatter_plot")
                   )),
          tabPanel(
            "Missing Values",
            fluidRow(
              radioButtons(
                "missing_display",
                label = "Display:",
                choices = list("Statistics" = "stats",
                               "Examples" = "examples"),
                selected = "stats",
                inline = T
              ),
              DT::dataTableOutput("missing_table")
            ),
            br(),
            br(),
            fluidRow(h3(
              "How would you like to handle missing values?"
            )),
            fluidRow(
              radioButtons(
                "nan_imputation",
                label = "Choose imputation technique:",
                choices = list(
                  "Drop NaNs" = "drop",
                  "Median" = "median",
                  "Mean" = "mean"
                ),
                selected = "drop",
                inline = T
              )
            ),
            fluidRow(actionButton("apply_imputation", label = "Apply Imputation"))
          ),

          tabPanel(
            "Variable Normalization",
            fluidRow(uiOutput("select_var")),
            fluidRow(
              radioButtons(
                "var_normalization",
                label = "Choose normalization technique:",
                choices = list(
                  "Z-Score" = "zscore",
                  "Min-Max" = "minmax",
                  "Quantile" = "quantile"
                ),
                selected = "zscore",
                inline = T
              )
            ),
            fluidRow(column(width = 6,
                            plotOutput("before_norm_plot")),
                     column(width = 6,
                            plotOutput("after_norm_plot"))),
            fluidRow(
              actionButton("apply_normalization", label = "Apply Normalization")
            )
          ),
          tabPanel("Correlations",
                   fluidRow(
                     plotOutput("correlations_plot", width = "700", height = "700")
                   )),
          tabPanel(
            "Creating Models",
            fluidRow(h3("Your Model ")),
            fluidRow(p("Available variables: ")),
            fluidRow(verbatimTextOutput("var_names")),
            fluidRow(uiOutput("formula_ui")),
            fluidRow(
              verbatimTextOutput("model_summary"),
              br(),
              br(),
              actionButton("save_model_button1", label = "Save Model to Model 1"),
              actionButton("save_model_button2", label = "Save Model to Model 2"),
              actionButton("save_model_button3", label = "Save Model to Model 3"),
              actionButton("save_model_button4", label = "Save Model to Model 4"),
              actionButton("save_model_button5", label = "Save Model to Model 5")
            )
          ),
          tabPanel(
            "Diagnostic",
            fluidRow(h3("Select a saved model:")),
            fluidRow(
              selectInput(
                "selected_model",
                label = "Saved Model",
                choices = c(1, 2, 3, 4, 5)
              )
            ),

            fluidRow(
              actionButton("plotbutton", label = "Model Diagnostic"),
              actionButton("corr_button", label = "Variable Correlations"),
              actionButton("reset_button", label = "RESET Test"),
              actionButton("homoscedasticity_button", label = "Homoscedasticity Test"),
              actionButton("normality_button", label = "Normality Test"),
              actionButton("summary_button", label = "Summary")
            ),

            fluidRow(column(width = 6,
                            plotOutput("plot_1")),
                     column(width = 6,
                            plotOutput("plot_2"))),
            fluidRow(column(width = 6,
                            plotOutput("plot_3")),
                     column(width = 6,
                            plotOutput("plot_5"))),
            fluidRow(
              verbatimTextOutput("plot_interpretation")),

            fluidRow(
              verbatimTextOutput("corr_output"),
              verbatimTextOutput("reset_output"),
              verbatimTextOutput("homoscedasticity_output"),
              verbatimTextOutput("normality_output"),
              verbatimTextOutput("summary_output")
            )
          ),

          tabPanel(
            "Stepwise Selection",
            fluidRow(
              column(
                width = 6,
                h3("Models formulas"),
                h4("FitAll model"),
                uiOutput("formula_full")
              )
            ),

            fluidRow(
              column(
                width = 6,
                h4("FitStart Model"),
                uiOutput("formula_start")
              )
            ),

            fluidRow(
              column(
                width = 6,
                align = "center",
                actionButton("forward_button", "Forward Selection")
              ),
              column(
                width = 6,
                align = "center",
                actionButton("both_button", "Both Selection")
              )
            ),

            fluidRow(
              column(
                width = 12,
                h4("Output model"),
                verbatimTextOutput("selected_model_after_selection")
              )
            ),

            fluidRow(
              column(
                width = 2,
                actionButton("save_model_button_1", "Save Model 1")
              ),
              column(
                width = 2,
                actionButton("save_model_button_2", "Save Model 2")
              ),
              column(
                width = 2,
                actionButton("save_model_button_3", "Save Model 3")
              ),
              column(
                width = 2,
                actionButton("save_model_button_4", "Save Model 4")
              ),
              column(
                width = 2,
                actionButton("save_model_button_5", "Save Model 5")
              )
            )
          ),

          tabPanel(
            "Model Comparison",
            fluidRow(
              column(
                width = 12,
                h4("Select models to compare"),
                uiOutput("checkboxes")
              )
            ),

            fluidRow(
              column(
                width = 12,
                h4("Comparison Output"),
                verbatimTextOutput("modelComparison")
              )
            )
          )


        )
      )

    )
  )

  server <- function(input, output, session) {
    # Define a reactiveValues variable to hold the loaded dataset
    rv <- reactiveValues(df = NULL, lm = NULL, models = list(NULL, NULL, NULL, NULL, NULL), model_full = NULL, model_start = NULL, selected_model=NULL, stepwise = NULL)

    # Load the dataset when a file is selected
    observeEvent(input$file, {
      df <- read.csv(
        input$file$datapath,
        sep = input$sep,
        dec = input$dec,
        header = input$header == TRUE
      )
      rv$df <- df
    })

    # Render dataset summary table
    output$data_table <- DT::renderDataTable({
      DT::datatable(rv$df,
                    options = list(pageLength = 10, scrollX = T),
                    class = "data-table-style")
    })

    # Update variable selection inputs and render dataset head table when the dataset changes
    observeEvent(rv$df, {
      updateSelectInput(session,
                        "xcol",
                        choices = names(rv$df),
                        selected = NULL)
      updateSelectInput(session,
                        "ycol",
                        choices = names(rv$df),
                        selected = NULL)
      output$dataset_head <- renderDataTable({
        head(rv$df)
      })
      output$scatterplot <- renderPlot(NULL)
    })

    # Render variable selection inputs
    output$select_vars <- renderUI({
      req(rv$df)
      fluidRow(column(
        6,
        selectInput(
          "xcol",
          "X Variable",
          choices = names(rv$df),
          selected = NULL
        )
      ),
      column(
        6,
        selectInput(
          "ycol",
          "Y Variable",
          choices = names(rv$df),
          selected = NULL
        )
      ))
    })

    output$select_var <- renderUI({
      req(rv$df)
      fluidRow(selectInput(
        "norm_var",
        "Variable",
        choices = names(Filter(is.numeric, rv$df)),
        selected = NULL
      ))
    })

    # Render scatter plot when x and y variables are selected
    output$scatter_plot <- renderPlot({
      if (!is.null(input$xcol) & !is.null(input$ycol)) {
        plot(
          rv$df[, input$xcol],
          rv$df[, input$ycol],
          xlab = input$xcol,
          ylab = input$ycol,
          main = paste0("Scatterplot for ", input$xcol, " and ", input$ycol)
        )
      }
    })

    # Render missing data table
    output$missing_table <- DT::renderDataTable({
      req(rv$df)
      if (input$missing_display == "stats") {
        missing_df <-
          data.frame(colnames(rv$df),
                     colSums(is.na(rv$df)),
                     round(100 * colSums(is.na(rv$df)) / nrow(rv$df), 2))
        colnames(missing_df) <-
          c("Variable", "Missing Values", "Missing Perc")
      } else {
        missing_df <- rv$df[rowSums(is.na(rv$df)) > 0,]
      }
      DT::datatable(missing_df,
                    options = list(pageLength = 10, scrollX = T),
                    class = "data-table-style")
    })

    # Render data type selection inputs
    output$select_types <- renderUI({
      types <- sapply(rv$df, class)
      lapply(names(types), function(name) {
        select_input <- selectizeInput(
          paste0("select_", name),
          label = name,
          choices = c("character", "numeric", "integer", "factor", "logical"),
          selected = types[name]
        )
        observeEvent(input[[paste0("select_", name)]], {
          selected_type <- input[[paste0("select_", name)]]
          rv$df[[name]] <- switch(
            selected_type,
            "character" = as.character(rv$df[[name]]),
            "numeric" = as.numeric(rv$df[[name]]),
            "integer" = as.integer(rv$df[[name]]),
            "factor" = as.factor(rv$df[[name]]),
            "logical" = as.logical(rv$df[[name]])
          )
        })
        values <- head(rv$df[[name]], 5)
        verbatim_output <- renderPrint(paste(values, collapse = "; "))

        fluidRow(select_input, verbatim_output)
      })
    })

    # Render the test output to verify if datatypes have changed
    output$dtypes_summary <- renderPrint({
      str(rv$df)
    })

    output$before_norm_plot <- renderPlot({
      req(input$norm_var)
      if (!is.null(input$norm_var)) {
        if (is.numeric(rv$df[[input$norm_var]])) {
          hist(rv$df[[input$norm_var]], main = "Before normalization -", xlab = input$norm_var)
        } else {
          plot(rv$df[[input$norm_var]], main = "Before normalization -", xlab = input$norm_var)
        }
      }
    })

    normalize_var <- function(var, method) {
      if (method == "zscore") {
        normalized_var <- scale(var)
      } else if (method == "minmax") {
        normalized_var <-
          (var - min(var, na.rm = T)) / (max(var, na.rm = T) - min(var, na.rm = T))
      } else if (method == "quantile") {
        normalized_var <- ecdf(var)(var)
      }
      return(normalized_var)
    }

    # Plot the variable after normalization
    output$after_norm_plot <- renderPlot({
      req(input$norm_var)
      if (!is.null(input$norm_var)) {
        normalized_var <-
          normalize_var(rv$df[[input$norm_var]], input$var_normalization)
        if (is.numeric(rv$df[[input$norm_var]])) {
          hist(
            normalized_var,
            main = paste0("After ", input$var_normalization, " normalization - "),
            xlab = input$norm_var
          )
        } else {
          plot(
            normalized_var,
            main = paste0("After ", input$var_normalization, " normalization - "),
            xlab = input$norm_var
          )
        }
      }
    })

    # Apply normalization to the selected variable
    observeEvent(input$apply_normalization, {
      if (!is.null(input$norm_var)) {
        normalized_var <-
          normalize_var(rv$df[[input$norm_var]], input$var_normalization)
        rv$df[[input$norm_var]] <- normalized_var
      }
    })

    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    # Handle missing values based on the user's selection
    observeEvent(input$apply_imputation, {
      # Drop NaNs
      if (input$nan_imputation == "drop") {
        rv$df <- rv$df[complete.cases(rv$df),]
      } else {
        for (col in names(rv$df)) {
          if (is.numeric(rv$df[[col]]) && any(is.na(rv$df[[col]]))) {
            if (input$nan_imputation == "median") {
              rv$df[[col]][is.na(rv$df[[col]])] <-
                median(rv$df[[col]], na.rm = TRUE)
            } else {
              rv$df[[col]][is.na(rv$df[[col]])] <-
                mean(rv$df[[col]], na.rm = TRUE)
            }
          } else if (is.factor(rv$df[[col]]) &&
                     any(is.na(rv$df[[col]]))) {
            mode_val <- getmode(rv$df[[col]])
            rv$df[[col]][is.na(rv$df[[col]])] <- mode_val
          }
        }
      }

    })

    output$correlations_plot <- renderPlot({
      req(rv$df)
      corrs <- cor(Filter(is.numeric, rv$df[complete.cases(rv$df),]))
      corrplot(corrs, col = COL1("Greys"), tl.col = "black")
    })

    output$var_names <- renderPrint({
      req(rv$df)
      names(rv$df)
    })


    output$formula_ui <- renderUI({
      req(rv$df)
      fluidRow(
        textInput(
          "model_formula",
          "Model Formula (y ~ x1 + x2 ...)",
          value = paste(names(rv$df)[1], "~", paste(names(rv$df)[-1], collapse = " + ")),
          width = "1000px"
        ),
        actionButton("apply_formula", label = "Try Formula")
      )
    })


    observeEvent(input$apply_formula, {
      req(rv$df)
      rv$lm <- lm(input$model_formula, data = rv$df)
    })

    output$model_summary <- renderPrint({
      req(rv$lm)
      summary(rv$lm)
    })

    # Create and save the model based on the formula and button click
    observeEvent(input$save_model_button1, {
      save_model(1, "save_model_button1")
    })

    observeEvent(input$save_model_button2, {
      save_model(2, "save_model_button2")
    })

    observeEvent(input$save_model_button3, {
      save_model(3, "save_model_button3")
    })

    observeEvent(input$save_model_button4, {
      save_model(4, "save_model_button4")
    })

    observeEvent(input$save_model_button5, {
      save_model(5, "save_model_button5")
    })

    # Function to save the model to a specific model number
    save_model <- function(model_number, button_id) {
      if (!is.null(input$model_formula)) {
        model <- lm(input$model_formula, data = rv$df)
        rv$models[[model_number]] <- model
        updateActionButton(session, button_id, label = paste("Model", model_number, "- Saved"))
      }
    }

    # Operation of the "Model Diagnostic" button
    observeEvent(input$plotbutton, {
      selected_model <- as.integer(input$selected_model)

      model <- rv$models[[selected_model]]

      # Display graphs
      output$plot_1 <- renderPlot({
        if (!is.null(model))
          plot(model, which = 1)
      })

      output$plot_2 <- renderPlot({
        if (!is.null(model))
          plot(model, which = 2)
      })

      output$plot_3 <- renderPlot({
        if (!is.null(model))
          plot(model, which = 3)
      })

      output$plot_5 <- renderPlot({
        if (!is.null(model))
          plot(model, which = 5)
      })

      # Add plot interpretations
      plots_interpretation <- c(
        "Plot 1 (Residuals vs. Fitted): This plot helps us assess the linearity and homoscedasticity assumptions. We want to see a random scatter of residuals around zero, indicating linearity, and an approximately constant spread of residuals, indicating homoscedasticity.",
        "Plot 2 (Normal Q-Q): This plot helps us assess the normality assumption of residuals. If the points roughly follow the diagonal reference line, it suggests that the residuals are normally distributed. Deviations from the line may indicate departures from normality.",
        "Plot 3 (Scale-Location): This plot helps us assess the homoscedasticity assumption and identify influential observations. We want to see a random scatter of residuals with a relatively constant spread across the range of fitted values. Any patterns or trends in the spread could indicate heteroscedasticity or influential observations.",
        "Plot 5 (Cook's Distance): This plot helps us identify influential observations that have a significant impact on the regression model. Points with high Cook's distances indicate potential influential observations that may have a strong influence on the estimated coefficients and overall model fit."
      )

      # Display the plot interpretations
      output$plot_interpretation <- renderText({
        paste(plots_interpretation, collapse = "\n\n")
      })
    })


    # Operation of the "Variable Correlations" button
    observeEvent(input$corr_button, {
      selected_model <- as.integer(input$selected_model)

      # Load the selected model
      model <- rv$models[[selected_model]]

      # Check if the model has been loaded
      if (!is.null(model)) {
        # Calculate variable correlations using vif()
        vif_result <- car::vif(model)

        # Interpretation of Variable Correlations
        corr_interpretation <- "Variable correlations provide information about the multicollinearity between predictor variables. High correlation values indicate potential multicollinearity issues, which can affect the stability and interpretability of the regression model."

        # Clear the corr_output
        corr_output <- NULL

        # Combine test results and interpretation
        corr_output <- c(corr_output, "Variable Correlations:")
        corr_output <- c(corr_output, "")
        corr_output <- c(corr_output, capture.output(vif_result))
        corr_output <- c(corr_output, "")
        corr_output <- c(corr_output, "Interpretation:")
        corr_output <- c(corr_output, corr_interpretation)

        # Display the combined output
        output$corr_output <- renderText({
          paste(corr_output, collapse = "\n")
        })
      } else {
        # Display a message if the model is not loaded
        output$corr_output <- renderText({
          "No model selected."
        })
      }
    })


    observeEvent(input$reset_button, {
      selected_model <- as.integer(input$selected_model)

      # Load the selected model
      model <- rv$models[[selected_model]]

      # Check if the model has been loaded
      if (!is.null(model)) {
        # Perform RESET test
        reset_results <- lmtest::resettest(model, power = 2:3)

        # Interpretation of Reset Test
        reset_interpretation <- if (reset_results$p.value < 0.05) {
          "The p-value is below 0.05, indicating that there is evidence to reject the null hypothesis. This suggests that the current model may be misspecified and requires modification."
        } else {
          "The p-value is greater than 0.05, indicating that there is no significant evidence to reject the null hypothesis. This suggests that the current model does not require modification."
        }

        # Clear the reset_output
        reset_output <- NULL

        # Combine test results and interpretation
        reset_output <- c(reset_output, "RESET Test Results:")
        reset_output <- c(reset_output, "")
        reset_output <- c(reset_output, capture.output(reset_results))
        reset_output <- c(reset_output, "")
        reset_output <- c(reset_output, "Interpretation:")
        reset_output <- c(reset_output, reset_interpretation)

        # Display the combined output
        output$reset_output <- renderText({
          paste(reset_output, collapse = "\n")
        })
      } else {
        # If the model is not loaded, display a message
        output$reset_output <- renderText({
          "No model selected."
        })
      }
    })

    observeEvent(input$homoscedasticity_button, {
      selected_model <- as.integer(input$selected_model)

      # Load the selected model
      model <- rv$models[[selected_model]]

      # Check if the model has been loaded
      if (!is.null(model)) {
        # Perform the homoscedasticity test
        homoscedasticity_test <- bptest(model)

        # Interpretation of Homoscedasticity Test
        homoscedasticity_interpretation <- if (homoscedasticity_test$p.value < 0.05) {
          "The p-value is below 0.05, indicating that there is evidence to reject the null hypothesis. This suggests that there is heteroscedasticity in the model, and the assumption of constant variance may be violated."
        } else {
          "The p-value is greater than 0.05, indicating that there is no significant evidence to reject the null hypothesis. This suggests that the model satisfies the assumption of constant variance."
        }

        # Clear the homoscedasticity_output
        homoscedasticity_output <- NULL

        # Combine test results and interpretation
        homoscedasticity_output <- c(homoscedasticity_output, "Homoscedasticity Test Results:")
        homoscedasticity_output <- c(homoscedasticity_output, "")
        homoscedasticity_output <- c(homoscedasticity_output, capture.output(homoscedasticity_test))
        homoscedasticity_output <- c(homoscedasticity_output, "")
        homoscedasticity_output <- c(homoscedasticity_output, "Interpretation:")
        homoscedasticity_output <- c(homoscedasticity_output, homoscedasticity_interpretation)

        # Display the combined output
        output$homoscedasticity_output <- renderText({
          paste(homoscedasticity_output, collapse = "\n")
        })
      } else {
        # Display a message if no model is selected
        output$homoscedasticity_output <- renderText({
          "No model selected."
        })
      }
    })

    observeEvent(input$normality_button, {
      selected_model <- as.integer(input$selected_model)

      # Load the selected model
      model <- rv$models[[selected_model]]

      # Check if the model has been loaded
      if (!is.null(model)) {
        # Perform Jarque-Bera test for normality
        jb_test <- jarque.bera.test(model$residuals)

        # Calculate descriptive statistics of the residuals
        residuals_mean <- round(mean(model$residuals), 2)
        residuals_sd <- round(sd(model$residuals), 2)
        residuals_skewness <- round(moments::skewness(model$residuals), 2)
        residuals_kurtosis <- round(moments::kurtosis(model$residuals), 2)

        # Interpretation of Jarque-Bera Test for Normality
        normality_interpretation <- if (jb_test$p.value < 0.05) {
          "The p-value is below 0.05, suggesting that there is evidence to reject the null hypothesis. This indicates that the residuals may not follow a normal distribution."
        } else {
          "The p-value is greater than 0.05, indicating that there is no significant evidence to reject the null hypothesis. This suggests that the residuals approximately follow a normal distribution."
        }

        # Clear the normality_output
        normality_output <- NULL

        # Combine test results, descriptive statistics, and interpretation
        normality_output <- c(normality_output, "Jarque-Bera Test for Normality:")
        normality_output <- c(normality_output, glue("Test Statistic: {jb_test$statistic}"))
        normality_output <- c(normality_output, glue("p-value: {jb_test$p.value}"))
        normality_output <- c(normality_output, "")
        normality_output <- c(normality_output, "Descriptive Statistics of Residuals:")
        normality_output <- c(normality_output, glue("Mean: {residuals_mean}"))
        normality_output <- c(normality_output, glue("Standard Deviation: {residuals_sd}"))
        normality_output <- c(normality_output, glue("Skewness: {residuals_skewness}"))
        normality_output <- c(normality_output, glue("Kurtosis: {residuals_kurtosis}"))
        normality_output <- c(normality_output, "")
        normality_output <- c(normality_output, "Interpretation:")
        normality_output <- c(normality_output, normality_interpretation)

        # Display the combined output
        output$normality_output <- renderText({
          paste(normality_output, collapse = "\n")
        })
      } else {
        # Model not found
        output$normality_output <- renderText({
          "No model selected."
        })
      }
    })

    observeEvent(input$summary_button, {
      selected_model <- as.integer(input$selected_model)

      # Load the selected model
      model <- rv$models[[selected_model]]

      # Check if the model has been loaded
      if (!is.null(model)) {
        # Generate summary of the model
        model_summary <- summary(model)

        # Interpretation of the F-test
        f_test_interpretation <- "The F-test in the model summary assesses the overall significance of the regression model. It tests the null hypothesis that all the regression coefficients are zero. If the p-value associated with the F-test is below a certain significance level (e.g., 0.05), it indicates that at least one of the predictor variables is significantly related to the response variable."

        # Interpretation of the R-squared statistic
        r_squared_interpretation <- "The R-squared statistic in the model summary represents the proportion of the variance in the response variable that is explained by the predictor variables. It ranges from 0 to 1, where 0 indicates no linear relationship between the predictors and the response, and 1 indicates a perfect linear relationship. A higher R-squared value suggests that the model provides a better fit to the data."

        # Overall interpretation
        overall_interpretation <- "The model summary provides an overview of the regression model, including information about the significance of the predictors, the overall goodness of fit, and other relevant statistics. It is important to carefully review the model summary to evaluate the adequacy and validity of the model for the given data."

        # Clear the summary_output
        summary_output <- NULL

        # Combine model summary and interpretations
        summary_output <- c(summary_output, capture.output(model_summary))
        summary_output <- c(summary_output, "")
        summary_output <- c(summary_output, "Interpretation:")
        summary_output <- c(summary_output, overall_interpretation)
        summary_output <- c(summary_output, "")
        summary_output <- c(summary_output, "F-Test Interpretation:")
        summary_output <- c(summary_output, f_test_interpretation)
        summary_output <- c(summary_output, "")
        summary_output <- c(summary_output, "R-Squared Interpretation:")
        summary_output <- c(summary_output, r_squared_interpretation)

        # Display the combined output
        output$summary_output <- renderText({
          paste(summary_output, collapse = "\n")
        })
      } else {
        # Model not found
        output$summary_output <- renderText({
          "No model selected."
        })
      }
    })

    observeEvent(input$selected_model, {
      # Clear the existing outputs when a model is selected
      output$plot_1 <- renderPlot(NULL)
      output$plot_2 <- renderPlot(NULL)
      output$plot_3 <- renderPlot(NULL)
      output$plot_5 <- renderPlot(NULL)
      output$plot_interpretation <- renderText(NULL)
      output$corr_output <- renderText(NULL)
      output$reset_output <- renderText(NULL)
      output$homoscedasticity_output <- renderText(NULL)
      output$normality_output <- renderText(NULL)
      output$summary_output <- renderText(NULL)
    })

    output$formula_full <- renderUI({
      req(rv$df)

      fluidRow(
        textInput(
          inputId = "model_formula_full",
          label = "Model Formula (y ~ x1 + x2 ...)",
          value = paste(names(rv$df)[1], "~", paste(names(rv$df)[-1], collapse = " + ")),
          width = "1000px"
        ),
        actionButton("save_formula", label = "Save Model Full")
      )
    })

    observeEvent(input$save_formula, {
      req(rv$df)

      full_formula <- input$model_formula_full

      # Fit the model using the provided formula
      rv$model_full <- lm(as.formula(full_formula), data = rv$df)

      # Update the button label to "Model Saved"
      updateActionButton(session, "save_formula", label = "Model Full Saved")
    })

    output$formula_start <- renderUI({
      req(rv$df)

      fluidRow(
        textInput(
          inputId = "model_formula_start",
          label = "Model Formula (y ~ 1)",
          value = paste(names(rv$df)[1], "~ 1"),
          width = "1000px"
        ),
        actionButton("save_formula_start", label = "Save Model Start")
      )
    })

    observeEvent(input$save_formula_start, {
      req(rv$df)

      start_formula <- input$model_formula_start

      # Fit the model using the provided formula
      rv$model_start <- lm(as.formula(start_formula), data = rv$df)

      # Update the button label to "Model Saved"
      updateActionButton(session, "save_formula_start", label = "Model Start Saved")
    })

    clean_data <- reactive({
      req(rv$df)
      na.omit(rv$df)
    })

    observeEvent(input$forward_button, {
      req(rv$model_start, rv$model_full, clean_data())

      # Fit the start model on the cleaned data
      start_model <- lm(formula(rv$model_start), data = clean_data())

      # Perform stepwise forward selection
      forward_model <- step(start_model, direction = "forward", scope = formula(rv$model_full), data = clean_data())

      # Save argument stepwise
      rv$stepwise = "forward"

      # Update the selected model with the forward-selected model
      rv$selected_model <- forward_model

      # Display the selected model after forward selection
      output$selected_model_after_selection <- renderPrint({
        summary(forward_model)
      })
    })

    observeEvent(input$both_button, {
      req(rv$model_start, rv$model_full, clean_data())

      # Fit the start model on the cleaned data
      start_model <- lm(formula(rv$model_start), data = clean_data())

      # Perform stepwise selection using both methods
      both_model <- step(start_model, direction = "both", scope = formula(rv$model_full), data = clean_data())

      # Save argument stepwise
      rv$stepwise = "both"

      # Update the selected model with the both-selected model
      rv$selected_model <- both_model

      # Display the selected model after both selection
      output$selected_model_after_selection <- renderPrint({
        summary(both_model)
      })
    })

    # Create and save the stepwise model based on the selected method and button click
    observeEvent(input$save_model_button_1, {
      save_stepwise_model(1, "save_model_button_1")
    })

    observeEvent(input$save_model_button_2, {
      save_stepwise_model(2, "save_model_button_2")
    })

    observeEvent(input$save_model_button_3, {
      save_stepwise_model(3, "save_model_button_3")
    })

    observeEvent(input$save_model_button_4, {
      save_stepwise_model(4, "save_model_button_4")
    })

    observeEvent(input$save_model_button_5, {
      save_stepwise_model(5, "save_model_button_5")
    })

    # Function to save the stepwise model to a specific model number
    save_stepwise_model <- function(model_number, button_id) {
      req(rv$model_start, rv$model_full, rv$stepwise, clean_data())

      # Fit the start model on clean data
      fit_start <- lm(formula(rv$model_start), data = clean_data())

      # Fit the full model on clean data
      fit_full <- lm(formula(rv$model_full), data = clean_data())

      # Check the stepwise type
      if (rv$stepwise == "forward") {
        # Perform forward stepwise selection on the start model using the full model as the scope
        stepwise_model <- step(fit_start, direction = "forward", scope = formula(fit_full))
      } else if (rv$stepwise == "both") {
        # Perform both stepwise selection on the start model using the full model as the scope
        stepwise_model <- step(fit_start, direction = "both", scope = formula(fit_full))
      } else {
        # Invalid stepwise type
        return()
      }

      stepwise_formula = formula(stepwise_model)

      model <- lm(stepwise_formula, data = rv$df)
      rv$models[[model_number]] <- model

      updateActionButton(session, button_id, label = paste("Model", model_number, "- Saved"))
    }

    # Generate the checkboxes
    output$checkboxes <- renderUI({
      models_avail <- rv$models[!sapply(rv$models, is.null)]
      checkboxGroupInput(
        "checkbox",
        NULL,
        choices = paste0("Model ", seq_along(models_avail)),
        selected = NULL
      )
    })

    # Model Comparison
    output$modelComparison <- renderText({
      # If there are no models selected, return an informative message
      if(is.null(input$checkbox) || length(input$checkbox) == 0) {
        return("No models selected for comparison.")
      }

      # Get the selected models
      selected_models <- input$checkbox

      # Retrieve the corresponding models
      models <- lapply(selected_models, function(model) {
        model_number <- as.integer(sub("Model ", "", model))

        # Check if the model_number is within bounds
        if(model_number <= length(rv$models) && !is.null(rv$models[[model_number]])) {
          rv$models[[model_number]]
        } else {
          return(NULL)
        }
      })

      # Remove any NULL models from the list
      models <- models[!sapply(models, is.null)]

      # If there are no valid models after filtering, return an informative message
      if(length(models) == 0) {
        return("No valid models selected for comparison.")
      }

      # Compare the models using stargazer
      comparison_results <- capture.output(stargazer::stargazer(models, type = "text", df = FALSE, align = TRUE, star.cutoffs = c(0.05, 0.01, 0.001)))

      # Join the comparison results as a single string
      comparison_results <- paste(comparison_results, collapse = "\n")

      # Return the comparison results
      comparison_results
    })

  }

  shinyApp(ui, server)
}
