library(shiny)
library(bslib)
library(fpp3) # Includes fable, tsibble, feasts, lubridate, dplyr, ggplot2
library(tidyverse)
library(gt)
library(here)

# -------------------------------------------------------------------------
# 1. Global Setup & Data Processing
# -------------------------------------------------------------------------

# Load Data
# We wrap this in tryCatch to handle cases where the file might not be present yet
oil_ts <- tryCatch({
  read_rds(here::here("cleaned_oil_ts.rds"))
}, error = function(e) {
  stop("Could not find 'cleaned_oil_ts.rds'. Please ensure the file is in the project root.")
})

# Pre-process Data: Pivot wide to calculate lags for both commodities
oil_processed <- oil_ts |> 
  pivot_wider(
    id_cols = c(Date, State), 
    values_from = Volume, 
    names_from = Commodity
  ) |> 
  rename(
    Gas = `Gas (Mcf)`,
    Oil = `Oil (bbl)`
  ) |> 
  arrange(State, Date) |> 
  group_by(State) |> 
  mutate(
    Month_Lbl = month.abb[month(Date)],
    # Gas Lags (to predict Oil)
    Gas_Lag_1 = lag(Gas, n = 1),
    Gas_Lag_2 = lag(Gas, n = 2),
    Gas_Lag_6 = lag(Gas, n = 6),
    Gas_Lag_12 = lag(Gas, n = 12),
    # Oil Lags (to predict Gas)
    Oil_Lag_1 = lag(Oil, n = 1),
    Oil_Lag_2 = lag(Oil, n = 2),
    Oil_Lag_6 = lag(Oil, n = 6),
    Oil_Lag_12 = lag(Oil, n = 12)
  ) |> 
  ungroup() |> 
  as_tsibble(key = State, index = Date)

available_states <- unique(oil_processed$State)
commodities <- c("Oil", "Gas")

# -------------------------------------------------------------------------
# 2. UI Definition
# -------------------------------------------------------------------------

ui <- page_navbar(
  title = "US Oil & Gas Production Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # --- Tab 1: Initial Exploration ---
  nav_panel(
    title = "Initial Exploration",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("exp_state", "Select State(s)", choices = available_states, selected = "NM", multiple = TRUE),
        selectInput("exp_commodity", "Select Commodity", choices = commodities, selected = "Oil"),
        
        hr(),
        h5("STL Decomposition Controls"),
        selectInput("stl_scope", "STL Scope", choices = c("Specific State" = "State", "Total US" = "US")),
        conditionalPanel(
          condition = "input.stl_scope == 'State'",
          selectInput("stl_state_select", "Select State for STL", choices = available_states, selected = "NM")
        ),
        
        hr(),
        sliderInput("exp_date_range", "Date Range",
                    min = as.Date("2015-01-01"),
                    max = as.Date("2025-05-01"),
                    value = c(as.Date("2015-01-01"), as.Date("2025-05-01")),
                    timeFormat = "%Y-%m")
      ),
      
      card(
        card_header("Time Series Exploration"),
        height = "600px",
        input_task_button("view_ts", "View Time Series"),
        input_task_button("view_stl", "View STL Decomposition"),
        plotOutput("exp_plot", height = "500px")
      )
    )
  ),
  
  # --- Tab 2: Modeling ---
  nav_panel(
    title = "Modeling",
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        h4("Model Configuration"),
        selectInput("mod_state", "Select State", choices = available_states, selected = "NM"),
        selectInput("mod_commodity", "Select Commodity", choices = commodities, selected = "Oil"),
        
        sliderInput("train_split", "Forecast Horizon Start (Test Split)",
                    min = as.Date("2015-01-01"), max = as.Date("2025-05-01"),
                    value = as.Date("2024-01-01"), timeFormat = "%Y-%m"),
        
        radioButtons("mod_type", "Model Type", choices = c("TSLM", "ETS")),
        
        checkboxInput("customize_mod", "Customize Model Parameters", value = FALSE),
        
        # --- Customization Panels with Tooltips ---
        conditionalPanel(
          condition = "input.customize_mod == true && input.mod_type == 'ETS'",
          card(
            card_header("ETS Parameters", class = "bg-light"),
            tooltip(
              selectInput("ets_error", "Error Type", choices = c("Auto" = "Z", "Additive" = "A", "Multiplicative" = "M")),
              "Error: 'Additive' for constant variance, 'Multiplicative' if error grows with trend."
            ),
            tooltip(
              selectInput("ets_trend", "Trend Type", choices = c("Auto" = "Z", "None" = "N", "Additive" = "A", "Additive Damped" = "Ad")),
              "Trend: 'Additive' for linear trend, 'Damped' to flatten trend over time."
            ),
            tooltip(
              selectInput("ets_season", "Season Type", choices = c("Auto" = "Z", "None" = "N", "Additive" = "A", "Multiplicative" = "M")),
              "Season: 'Additive' for constant seasonality, 'Multiplicative' if seasonality grows with trend."
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.customize_mod == true && input.mod_type == 'TSLM'",
          card(
            card_header("TSLM Regressors", class = "bg-light"),
            tooltip(
              checkboxInput("tslm_trend", "Include Trend", value = TRUE),
              "Adds a linear trend component."
            ),
            tooltip(
              checkboxInput("tslm_season", "Include Seasonality", value = TRUE),
              "Adds monthly dummy variables."
            ),
            h6("Exogenous Regressors (Lags)"),
            tooltip(
              checkboxGroupInput("tslm_lags", NULL, 
                                 choices = c("Lag 1 Month" = "lag1", 
                                             "Lag 2 Months" = "lag2", 
                                             "Lag 6 Months" = "lag6", 
                                             "Lag 12 Months" = "lag12")),
              "Use previous values of the OTHER commodity as predictors."
            )
          )
        ),
        
        actionButton("train_btn", "Train Model", class = "btn-primary w-100 mt-3")
      ),
      
      layout_columns(
        col_widths = 12,
        card(
          card_header("Model Report"),
          verbatimTextOutput("model_report")
        ),
        card(
          card_header("Training Accuracy History"),
          gt_output("train_accuracy_table")
        )
      )
    )
  ),
  
  # --- Tab 3: Forecasting ---
  nav_panel(
    title = "Forecasting",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Forecast Comparison"),
        p("Select models built in Tab 2 to compare."),
        # Default selection is explicitly empty
        checkboxGroupInput("forecast_models_select", "Select Models (Max 10)", choices = NULL, selected = character(0)),
        
        sliderInput("forecast_zoom", "Plot Range Zoom",
                    min = as.Date("2015-01-01"), max = as.Date("2026-05-01"),
                    value = c(as.Date("2022-01-01"), as.Date("2025-05-01")),
                    timeFormat = "%Y-%m")
      ),
      
      layout_columns(
        col_widths = 12,
        card(
          card_header("Forecast Visualization (Top 4 Selected)"),
          plotOutput("forecast_plot", height = "500px")
        ),
        card(
          card_header("Test Set Accuracy Metrics"),
          gt_output("test_accuracy_table")
        )
      )
    )
  ),
  
  # --- Tab 4: About ---
  nav_panel(
    title = "About",
    card(
      markdown("
        ### About the Data
        Data provided by U.S. Department of the Interior, Office of Natural Resources Revenue.
        
        ### About Tidyverts
        Built using the `fpp3` ecosystem (tsibble, fable, feasts).
      ")
    )
  )
)

# -------------------------------------------------------------------------
# 3. Server Logic
# -------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # --- Reactive Values ---
  v <- reactiveValues(
    models = list(),      # Stores model objects and metadata
    model_counter = 0     # To create unique IDs
  )
  
  # --- Tab 1: Exploration ---
  exp_data <- reactive({
    req(input$exp_state)
    oil_processed |> 
      filter(State %in% input$exp_state) |> 
      filter_index(as.character(input$exp_date_range[1]) ~ as.character(input$exp_date_range[2]))
  })
  
  stl_data <- reactive({
    req(input$stl_scope)
    if(input$stl_scope == "State") {
      oil_processed |> 
        filter(State == input$stl_state_select) |> 
        filter_index(as.character(input$exp_date_range[1]) ~ as.character(input$exp_date_range[2]))
    } else {
      oil_processed |> 
        index_by(Date) |> 
        summarise(Gas = sum(Gas, na.rm=TRUE), Oil = sum(Oil, na.rm=TRUE)) |> 
        filter_index(as.character(input$exp_date_range[1]) ~ as.character(input$exp_date_range[2]))
    }
  })
  
  plot_mode <- reactiveVal("TS")
  observeEvent(input$view_ts, { plot_mode("TS") })
  observeEvent(input$view_stl, { plot_mode("STL") })
  
  output$exp_plot <- renderPlot({
    y_var <- input$exp_commodity 
    if(plot_mode() == "TS") {
      exp_data() |> 
        autoplot(.data[[y_var]]) + theme_minimal() + labs(title = paste(y_var, "Production"))
    } else {
      f <- as.formula(paste(y_var, "~ season(window='periodic')"))
      stl_data() |> model(STL(f)) |> components() |> autoplot() + theme_minimal()
    }
  })
  
  # --- Tab 2: Modeling Logic ---
  
  observeEvent(input$train_btn, {
    req(input$mod_state, input$mod_commodity, input$train_split)
    
    # 1. Prepare Data
    train_date <- input$train_split
    target_var <- input$mod_commodity
    
    dat_train <- oil_processed |> 
      filter(State == input$mod_state) |> 
      filter(Date < yearmonth(train_date))
    
    mod_name_base <- ""
    fit <- NULL
    
    # 2. Build Model using Robust String Construction
    if(input$mod_type == "ETS") {
      mod_name_base <- "ETS"
      if(input$customize_mod) {
        # Construct exact formula string to avoid parsing errors
        f_str <- paste0(target_var, " ~ error('", input$ets_error, "') + trend('", input$ets_trend, "') + season('", input$ets_season, "')")
        fit <- dat_train |> model(custom_ets = ETS(as.formula(f_str)))
      } else {
        fit <- dat_train |> model(auto_ets = ETS(!!sym(target_var)))
      }
    } else {
      mod_name_base <- "TSLM"
      rhs <- c()
      if(!input$customize_mod) {
        rhs <- c("trend()", "season()")
      } else {
        if(input$tslm_trend) rhs <- c(rhs, "trend()")
        if(input$tslm_season) rhs <- c(rhs, "season()")
        
        pfx <- if(target_var == "Oil") "Gas_Lag_" else "Oil_Lag_"
        if("lag1" %in% input$tslm_lags) rhs <- c(rhs, paste0(pfx, "1"))
        if("lag2" %in% input$tslm_lags) rhs <- c(rhs, paste0(pfx, "2"))
        if("lag6" %in% input$tslm_lags) rhs <- c(rhs, paste0(pfx, "6"))
        if("lag12" %in% input$tslm_lags) rhs <- c(rhs, paste0(pfx, "12"))
      }
      if(length(rhs)==0) rhs <- c("1")
      form <- as.formula(paste(target_var, "~", paste(rhs, collapse="+")))
      fit <- dat_train |> model(custom_tslm = TSLM(form))
    }
    
    # Check fit success
    if(nrow(fit) == 0) return()
    
    # 3. Save Model
    v$model_counter <- v$model_counter + 1
    model_id <- paste(mod_name_base, input$mod_state, target_var, v$model_counter, sep="_")
    
    fit_renamed <- fit |> rename(!!model_id := 2)
    
    v$models[[model_id]] <- list(
      mable = fit_renamed,
      state = input$mod_state,
      commodity = input$mod_commodity,
      split_date = train_date
    )
    
    # Output Report
    output$model_report <- renderPrint({ fit_renamed |> report() })
    
    # Update Forecast Selector (keep current selection)
    updateCheckboxGroupInput(session, "forecast_models_select",
                             choices = names(v$models),
                             selected = input$forecast_models_select)
  })
  
  # --- Training Accuracy Table ---
  # Calculated dynamically from stored models to prevent crashes in the observer
  output$train_accuracy_table <- render_gt({
    req(length(v$models) > 0)
    
    # Iterate through all stored models and compute accuracy
    acc_list <- lapply(names(v$models), function(id) {
      mod_info <- v$models[[id]]
      if(is.null(mod_info)) return(NULL)
      
      # accuracy(mable) calculates training metrics based on residuals
      # DO NOT pass 'data' here, or it attempts forecast accuracy
      acc <- accuracy(mod_info$mable)
      
      acc |> 
        mutate(State = mod_info$state, Commodity = mod_info$commodity) |> 
        select(State, Commodity, .model, ME, RMSE, MAE, MPE, MAPE)
    })
    
    bind_rows(acc_list) |> 
      gt() |> 
      fmt_number(columns = c(ME:MAPE), decimals = 3) |> 
      cols_label(.model = "Model ID")
  })
  
  # --- Tab 3: Forecasting Logic ---
  
  forecast_results <- reactive({
    req(input$forecast_models_select)
    selected_ids <- input$forecast_models_select
    if(length(selected_ids) > 10) selected_ids <- selected_ids[1:10]
    
    fc_list <- list()
    acc_list <- list()
    
    for(id in selected_ids) {
      mod <- v$models[[id]]
      if(is.null(mod)) next
      
      test_data <- oil_processed |> 
        filter(State == mod$state) |> 
        filter(Date >= yearmonth(mod$split_date))
      
      fc <- mod$mable |> forecast(new_data = test_data)
      # Forecast Accuracy requires 'new_data'
      acc <- accuracy(fc, test_data) |> mutate(State = mod$state, Commodity = mod$commodity)
      
      fc_list[[id]] <- fc
      acc_list[[id]] <- acc
    }
    
    list(forecasts = bind_rows(fc_list), accuracy = bind_rows(acc_list))
  })
  
  output$test_accuracy_table <- render_gt({
    req(forecast_results())
    forecast_results()$accuracy |> 
      select(State, Commodity, .model, ME, RMSE, MAE, MPE, MAPE) |> 
      gt() |> fmt_number(columns = c(ME:MAPE), decimals = 2)
  })
  
  output$forecast_plot <- renderPlot({
    req(forecast_results())
    res <- forecast_results()
    
    selected_ids <- input$forecast_models_select
    if(length(selected_ids) == 0) return(NULL)
    if(length(selected_ids) > 4) selected_ids <- selected_ids[1:4]
    
    plot_fc <- res$forecasts |> filter(.model %in% selected_ids)
    
    # Build History Data Robustly (Avoiding tsibble binding errors)
    hist_list <- lapply(selected_ids, function(mid) {
      m_info <- v$models[[mid]]
      
      d <- oil_processed |> 
        filter(State == m_info$state) |> 
        filter_index(as.character(input$forecast_zoom[1]) ~ as.character(input$forecast_zoom[2])) |> 
        select(Date, State, Gas, Oil) |> 
        as_tibble() # Convert to tibble to avoid key clashes during bind_rows
      
      # Pivot to generic 'Value' for plotting
      d |> 
        pivot_longer(c(Gas, Oil), names_to = "Commodity", values_to = "Value") |> 
        filter(Commodity == m_info$commodity) |> 
        mutate(.model = mid)
    })
    
    # Bind as tibble first
    hist_df <- bind_rows(hist_list) |> 
      as_tsibble(key = .model, index = Date) # Convert back to tsibble for plotting
    
    plot_fc |> 
      autoplot() +
      geom_line(data = hist_df, aes(x = Date, y = Value), color = "black") +
      facet_wrap(~ .model, ncol = 1, scales = "free_y") +
      labs(title = "Forecast vs Actuals", y = "Volume") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)