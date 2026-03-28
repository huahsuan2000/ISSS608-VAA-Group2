pacman::p_load(
  shiny, shinydashboard, shinythemes, shinyWidgets,
  plotly, tidyverse, ggstatsplot, readr, rpart, rpart.plot, sparkline,
  factoextra, scales, cluster, DT, corrplot,
  tools, dplyr, tidyr, ggplot2, lubridate, caret, visNetwork, partykit, vcd,
  ranger
)


customer <- read_csv("data/customer_data.csv")
transaction <- read_csv("data/transactions_data.csv")
customer_data_clean <- readr::read_rds("data/customer_data_clean.rds")
customer_cluster <- read_csv("data/customer_data.csv")

# recode variable >> for CDA
customer <- customer |>
  mutate(
    
    active_products = factor(
      active_products,
      levels = c(0,1,2,3,4,5),
      labels = c(
        "0 product","1 product","2 products",
        "3 products","4 products","5 products"
      )
    ),
    
    satisfaction_score = factor(
      satisfaction_score,
      levels = c(2,3,4,5,6),
      labels = c(
        "Very low","Low","Medium","High","Very high"
      )
    ),
    
    app_logins_group = cut(
      app_logins_frequency,
      breaks = c(0,25,50,100),
      labels = c("Rarely","Often","Always"),
      include.lowest = TRUE
    ),
    
    churn_group = cut(
      churn_probability,
      breaks = c(0,0.2,0.3,0.4),
      labels = c("Low","Medium","High"),
      include.lowest = TRUE
    )
    
  )
# Join table
transaction <- transaction %>%
  left_join(
    customer %>% select(customer_id, clv_segment),
    by = "customer_id"
  )

transaction$date <- as.Date(transaction$date)

transaction <- transaction %>%
  mutate(
    year_month = floor_date(date, "month"),
    year = year(date),
    month = month(date, label = TRUE)
  )

monthly_type <- transaction %>%
  group_by(clv_segment, type, year_month) %>%
  summarise(
    transaction_count = n(),
    transaction_amount = sum(amount, na.rm = TRUE),
    .groups = "drop"
  )

# Decision tree
target_var <- "churn_probability"
#predictor = every variable - target variable
predictor_choices <- setdiff(names(customer_data_clean), target_var)

# cluster prepare data

cluster_data <- customer_cluster %>%
  mutate(
    average_transaction_value = log1p(average_transaction_value),
    total_transaction_volume = log1p(total_transaction_volume),
    customer_lifetime_value = log1p(customer_lifetime_value),
    support_tickets_count = log1p(support_tickets_count)
  ) %>%
  drop_na()

cluster_var_choices <- c(
  "Active Products" = "active_products",
  "App Logins Frequency" = "app_logins_frequency",
  "Feature Usage Diversity" = "feature_usage_diversity",
  "Monthly Transaction Count" = "monthly_transaction_count",
  "Average Transaction Value" = "average_transaction_value",
  "Total Transaction Volume" = "total_transaction_volume",
  "Transaction Frequency" = "transaction_frequency",
  "Weekend Transaction Ratio" = "weekend_transaction_ratio",
  "Avg Daily Transactions" = "avg_daily_transactions",
  "Support Tickets Count" = "support_tickets_count",
  "Resolved Tickets Ratio" = "resolved_tickets_ratio",
  "Satisfaction Score" = "satisfaction_score",
  "NPS Score" = "nps_score",
  "Churn Probability" = "churn_probability",
  "Customer Lifetime Value" = "customer_lifetime_value"
)

ui <- dashboardPage(
  
  dashboardHeader(
    title = tagList(
      icon("chart-line"),
      "Churn Analysis"
    )
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Summary", tabName = "summary", icon = icon("chart-line")),
      
      menuItem("Exploration", icon = icon("search"),
               menuSubItem("Customer Overview", tabName = "customer_overview"),
               menuSubItem("Transaction Behavior", tabName = "transaction_behavior"),
               menuSubItem("Churn Analysis", tabName = "churn_analysis")
      ),
      
      menuItem("Predictive model", tabName = "predictive_model", icon = icon("sitemap"),
               menuSubItem("Decision Tree", tabName = "decision_tree"),
               menuSubItem("Random Forest", tabName = "random_forest")
      ),
      
      menuItem("Cluster", tabName = "cluster", icon = icon("project-diagram"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # Summary Page
      tabItem(
        tabName = "summary",
        
        # 1. KPI card
        fluidRow(
          valueBoxOutput("total_customers", width = 3),
          valueBoxOutput("total_transaction", width = 3),
          valueBoxOutput("avg_churn", width = 3),
          valueBoxOutput("high_risk_count", width = 3)
        ),
        
        # 2. Main insight row
        fluidRow(
          box(
            title = "Churn Risk Heatmap by CLV Segment and App Engagement",
            width = 8,
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("summary_churn_heatmap", height = 245)
          ),
          box(
            title = "Overall Churn Risk Distribution",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("summary_churn_dist", height = 245)
          )
        ),
        
        # 3. Supporting insight row
        fluidRow(
          box(
            title = "Average Transaction Value",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("bulletPlot", height = 178)
          ),
          box(
            title = "Top High-Risk Customer Segments",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("summary_top_risk_segments", height = 178)
          ),
          box(
            title = "Engagement vs. Churn Risk (App Logins)",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("summary_engagement_churn", height = 178)
          )
        )
      ),
      
      # Customer Overview
      tabItem(
        tabName = "customer_overview",
        
        # KPI Row
        fluidRow(
          
          valueBoxOutput("avg_age", width = 4),
          valueBoxOutput("avg_household", width = 4),
          valueBoxOutput("avg_login", width = 4)
          
        ),
        
        fluidRow(
          box(
            title = "Customer Insight Explorer",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              
              column(
                3,
                selectInput(
                  "x_type",
                  "X Axis Type",
                  choices = c("Categorical", "Numerical")
                )
              ),
              
              column(
                3,
                selectInput(
                  "x_var",
                  "Select X Variable",
                  choices = NULL
                )
              ),
              
              column(
                3,
                selectInput(
                  "y_type",
                  "Y Axis Type",
                  choices = c("Categorical", "Numerical")
                )
              ),
              
              column(
                3,
                selectInput(
                  "y_var",
                  "Select Y Variable",
                  choices = NULL
                )
              )
              
            ),
            
            
            p("Press button below to update graph"),
            
            actionButton(
              "update_plot",
              "Update Plot",
              icon = icon("chart-line")
            ),
            
            br(), br(),
            
            
            plotlyOutput("customer_dynamic_plot", width = 700, height = 300)
            
          )    
          
        )
        
      ),
      
      # Transaction Behavior
      tabItem(
        tabName = "transaction_behavior",
        
        fluidRow(
          
          box(
            title = "Filters",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              
              column(
                4,
                selectInput(
                  "clv_filter",
                  "Customer Lifetime Value",
                  choices = unique(transaction$clv_segment)
                )
              ),
              
              column(
                4,
                selectInput(
                  "type_filter",
                  "Transaction Type",
                  choices = unique(transaction$type)
                )
              ),
              
              column(
                4,
                dateRangeInput(
                  "date_filter",
                  "Transaction Period",
                  start = min(transaction$date),
                  end = max(transaction$date)
                )
              )
              
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Transaction Count and Amount Trend",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            plotOutput("transaction_plot", height = 500)
            
          )
          
        )
      ),
      
      # Churn Analysis
      tabItem(
        tabName = "churn_analysis",
        
        fluidRow(
          
          # LEFT CONTROL PANEL
          box(
            width = 3,
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              "x_var",
              "Select X Variable",
              choices = c(
                "Gender" = "gender",
                "Income Bracket" = "income_bracket",
                "Education Level" = "education_level",
                "Marital Status" = "marital_status",
                "Customer Segment" = "customer_segment",
                "Savings Account" = "savings_account",
                "Acquisition Channel" = "acquisition_channel",
                "Active Products" = "active_products",
                "Satisfaction Score" = "satisfaction_score",
                "Feedback Sentiment" = "feedback_sentiment",
                "CLV Segment" = "clv_segment",
                "App Logins Group" = "app_logins_group"
              )
            ),
            
            selectInput(
              "test_type",
              "Statistical Test",
              choices = c(
                "Parametric" = "parametric",
                "Nonparametric" = "nonparametric",
                "Robust" = "robust",
                "Bayes" = "bayes"
              )
            ),
            
            selectInput(
              "conf_level",
              "Confidence Level",
              choices = c(
                "95%" = 0.95,
                "99%" = 0.99
              ),
              selected = 0.95
            ),
            
            selectInput(
              "display_type",
              "Display",
              choices = c("Count","Percentage")
            ),
            
            actionButton("update_churn_plot", "Update Plot")
            
          ),
          
          # RIGHT PLOT AREA
          box(
            width = 9,
            title = "Churn Distribution Analysis",
            status = "info",
            solidHeader = TRUE,
            
            tabsetPanel(
              tabPanel(
                "Statistical Test",
                plotOutput("churn_plot", height = 500)
              ),
              tabPanel(
                "VCD Mosaic",
                plotOutput("churn_vcd_plot", height = 500, width = "100%")
              )
            )
          )
          
        )
      ),
      

      # Cluster analysis
      tabItem(
        tabName = "cluster",
        
        box(
          width = 3,
          title = "Cluster Setup",
          status = "primary",
          solidHeader = TRUE,
          
          pickerInput(
            inputId = "cluster_vars",
            label = "Variable Selection",
            choices = cluster_var_choices,
            selected = c(
              "app_logins_frequency",
              "monthly_transaction_count",
              "customer_lifetime_value",
              "churn_probability"
            ),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `selected-text-format` = "count > 3",
              `none-selected-text` = "Please select variables"
            )
          ),
          
          sliderInput(
            "k_clusters",
            "Number of Clusters (K)",
            min = 2,
            max = 5,
            value = 4
          ),
          
          sliderInput(
            "sample_size",
            "Sample Size",
            min = 500,
            max = 2000,
            value = 1500,
            step = 500
          ),
          
          br(),
          
          actionButton(
            "run_cluster",
            "Run Clustering",
            icon = icon("play"),
            class = "btn-primary"
          ),
          
          br(), br(),
          
          helpText("Select at least 2 variables for clustering.")
          
        ),
        
        # 🔹 RIGHT SIDE (OUTPUT TABS)
        box(
          width = 9,
          title = "Cluster Visualisation",
          status = "info",
          solidHeader = TRUE,
          
          tabsetPanel(
            
            tabPanel(
              "Elbow Method",
              plotOutput("wss_plot", height = 400)
            ),
            
            tabPanel(
              "Cluster Map",
              plotOutput("cluster_map", height = 500)
            ),
            
            tabPanel(
              "Summary Table",
              DT::dataTableOutput("cluster_table")
            ),
            
            tabPanel(
              "Heatmap",
              plotlyOutput("cluster_heatmap", height = 500)
            )
            
          )
        )
        
      ),
      

      # Decision Tree
      tabItem(
        tabName = "decision_tree",
        
        fluidRow(
          
          # LEFT CONTROL PANEL
          box(
            width = 3,
            title = "Model Setup",
            status = "primary",
            solidHeader = TRUE,
            
            pickerInput(
              inputId = "predictors",
              label = "Variable Selection",
              choices = predictor_choices,
              selected = predictor_choices,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 10",
                `none-selected-text` = "Please select variables"
              )
            ),
            
            sliderInput(
              "split_ratio",
              "Train/Test Split Ratio",
              min = 0.5,
              max = 0.9,
              value = 0.8,
              step = 0.05
            ),
            
            sliderInput(
              "minsplit",
              "Minimum Split",
              min = 5,
              max = 50,
              value = 10
            ),
            
            sliderInput(
              "maxdepth",
              "Maximum Depth",
              min = 1,
              max = 15,
              value = 10
            ),
            
            sliderInput(
              "cp",
              "Complexity Parameter (CP)",
              min = 0.0001,
              max = 0.05,
              value = 0.001,
              step = 0.0005
            ),
            
            actionButton("build_model", "Build Model"),
            
            br(), br(),
            
            h4("Parameter Explanation"),
            
            tags$p(
              tags$b("Train/Test Split Ratio: "),
              "Controls how the dataset is divided into training and testing sets. 
    For example, 0.8 means 80% of the data is used for training and 20% for testing."
            ),
            
            tags$p(
              tags$b("Minimum Split: "),
              "Defines the minimum number of observations required in a node before the tree can split."
            ),
            
            tags$p(
              tags$b("Maximum Depth: "),
              "Sets the maximum number of levels the tree can grow. Deeper trees are more flexible but may overfit."
            ),
            
            tags$p(
              tags$b("Complexity Parameter (CP): "),
              "Controls whether a split is worthwhile. Higher CP makes the tree simpler, lower CP allows more splits."
            )
            
          ),
          
          
          # RIGHT CONTENT AREA
          box(
            width = 9,
            title = "Model Performance",
            status = "info",
            solidHeader = TRUE,
            
            # KPI Row
            fluidRow(
              
              valueBoxOutput("rmse_box", width = 4),
              valueBoxOutput("mae_box", width = 4),
              valueBoxOutput("r2_box", width = 4)
              
            )
          ),
          
          box(
            width = 9,
            title = "Model Visualisation",
            status = "info",
            solidHeader = TRUE,
            
            tabsetPanel(
              
              tabPanel(
                "Decision Tree",
                
                fluidRow(
                  column(
                    12,
                    box(
                      title = "Decision Tree",
                      width = NULL,
                      visNetworkOutput("tree_plot", height = 300)
                    ),
                  )
                )
              ),
              
              tabPanel(
                "Predicted vs Actual",
                
                fluidRow(
                  
                  column(
                    12,
                    box(
                      title = "Predicted vs Actual",
                      width = NULL,
                      plotOutput("pred_actual_plot", height = 300)
                    )
                  )
                )
              ),
              
              tabPanel(
                "Feature Importance",
                
                fluidRow(
                  
                  column(
                    12,
                    box(
                      title = "Feature Importance",
                      width = NULL,
                      plotOutput("importance_plot", height = 300)
                    )
                  )
                )
              ),
              
              
              tabPanel(
                "Scenario Prediction",
                
                fluidRow(
                  
                  column(
                    4,
                    box(
                      title = "Input Scenario",
                      width = NULL,
                      uiOutput("scenario_inputs"),
                      br(),
                      actionButton("predict_case", "Predict Scenario")
                    )
                  ),
                  
                  column(
                    8,
                    
                    box(
                      title = "Prediction Result",
                      width = NULL,
                      uiOutput("scenario_pred_box")
                    ),
                    
                    box(
                      title = "Decision Path",
                      width = NULL,
                      uiOutput("decision_path")
                    )
                    
                  )
                  
                )
              )
              
            )
            
          )
          
        )
      ),
      

      # Random Forest
      tabItem(
        tabName = "random_forest",
        
        fluidRow(
          
          # LEFT CONTROL PANEL
          box(
            width = 3,
            title = "Model Setup",
            status = "primary",
            solidHeader = TRUE,
            
            pickerInput(
              inputId = "rf_predictors",
              label = "Variable Selection",
              choices = predictor_choices,
              selected = predictor_choices,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `selected-text-format` = "count > 10",
                `none-selected-text` = "Please select variables"
              )
            ),
            
            sliderInput(
              "rf_split_ratio",
              "Train/Test Split Ratio",
              min = 0.5,
              max = 0.9,
              value = 0.8,
              step = 0.05
            ),
            
            sliderInput(
              "rf_num_trees",
              "Number of Trees",
              min = 50,
              max = 300,
              value = 100,
              step = 25
            ),
            
            actionButton("rf_build_model", "Build Model"),
            
            br(), br(),
            
            h4("Parameter Explanation"),
            
            tags$p(
              tags$b("Train/Test Split Ratio: "),
              "Controls how the dataset is divided into training and testing sets. 
    For example, 0.8 means 80% of the data is used for training and 20% for testing."
            ),
            
            tags$p(
              tags$b("Number of Trees: "),
              "Defines how many trees are built in the random forest model."
            )
            
          ),
          
          # RIGHT CONTENT AREA
          box(
            width = 9,
            title = "Model Performance",
            status = "info",
            solidHeader = TRUE,
            
            # KPI Row
            fluidRow(
              
              valueBoxOutput("rf_rmse_box", width = 4),
              valueBoxOutput("rf_mae_box", width = 4),
              valueBoxOutput("rf_r2_box", width = 4)
              
            )
          ),
          
          box(
            width = 9,
            title = "Model Visualisation",
            status = "info",
            solidHeader = TRUE,
            
            tabsetPanel(
              
              tabPanel(
                "Predicted vs Actual",
                
                fluidRow(
                  
                  column(
                    12,
                    box(
                      title = "Predicted vs Actual",
                      width = NULL,
                      plotOutput("rf_pred_actual_plot", height = 300)
                    )
                  )
                )
              ),
              
              tabPanel(
                "Feature Importance",
                
                fluidRow(
                  
                  column(
                    12,
                    box(
                      title = "Feature Importance",
                      width = NULL,
                      plotOutput("rf_importance_plot", height = 300)
                    )
                  )
                )
              ),
              
              tabPanel(
                "Scenario Prediction",
                br(),
                fluidRow(
                  column(
                    width = 4,
                    div(
                      class = "chart-card",
                      h4("Input Scenario"),
                      p("Enter values for the selected predictors to generate a random forest prediction."),
                      uiOutput("rf_scenario_inputs"),
                      br(),
                      actionButton("rf_predict_case", "Predict Scenario")
                    )
                  ),
                  
                  column(
                    width = 8,
                    div(
                      class = "chart-card",
                      h4("Prediction Result"),
                      uiOutput("rf_scenario_pred_box")
                    ),
                    
                    div(
                      class = "chart-card",
                      h4("Model Note"),
                      div(
                        class = "path-box",
                        "Random Forest is an ensemble of many trees, so there is no single decision path to display."
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  

  # Summary Page
  
  # KPI 1
  output$total_customers <- renderValueBox({
    valueBox(
      format(nrow(customer), big.mark = ","),
      "Total Customers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # KPI 2
  output$total_transaction <- renderValueBox({
    valueBox(
      paste0(round(sum(customer$total_transaction_volume, na.rm = TRUE) / 1e9, 2), "B"),
      "Total Transactions",
      icon = icon("credit-card"),
      color = "purple"
    )
  })
  
  # KPI 3
  output$avg_churn <- renderValueBox({
    valueBox(
      scales::percent(mean(customer$churn_probability, na.rm = TRUE)),
      "Avg Churn Probability",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  # KPI 4
  output$high_risk_count <- renderValueBox({
    high_risk_n <- sum(customer$churn_group == "High", na.rm = TRUE)
    valueBox(
      format(high_risk_n, big.mark = ","),
      "High Risk Customers",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Key Insight
  output$summary_key_insight <- renderUI({
    
    heatmap_data <- customer %>%
      drop_na(clv_segment, app_logins_group, churn_probability) %>%
      group_by(clv_segment, app_logins_group) %>%
      summarise(
        avg_churn = mean(churn_probability, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_churn))
    
    top_group <- heatmap_data %>% slice(1)
    
    top_segment_data <- customer %>%
      drop_na(customer_segment, churn_probability) %>%
      group_by(customer_segment) %>%
      summarise(
        avg_churn = mean(churn_probability, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_churn)) %>%
      slice(1)
    
    tagList(
      tags$p(
        style = "font-size: 14px; margin-bottom: 8px;",
        tags$b("Highest-risk combination: "),
        paste0(
          top_group$app_logins_group, " app engagement + ",
          top_group$clv_segment, " CLV segment"
        ),
        paste0(
          " shows an average churn risk of ",
          scales::percent(top_group$avg_churn, accuracy = 0.1),
          "."
        )
      ),
      tags$p(
        style = "font-size: 14px; margin-bottom: 0;",
        tags$b("Top customer segment by risk: "),
        paste0(
          top_segment_data$customer_segment,
          " (",
          scales::percent(top_segment_data$avg_churn, accuracy = 0.1),
          ")"
        )
      )
    )
  })
  
  # Heatmap
  output$summary_churn_heatmap <- renderPlotly({
    
    heatmap_data <- customer %>%
      drop_na(clv_segment, app_logins_group, churn_probability) %>%
      group_by(clv_segment, app_logins_group) %>%
      summarise(
        avg_churn = mean(churn_probability, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        app_logins_group = factor(
          app_logins_group,
          levels = c("Rarely", "Often", "Always")
        ),
        label_text = paste0(
          scales::percent(avg_churn, accuracy = 0.1),
          "<br>n = ", scales::comma(n)
        ),
        tooltip_text = paste0(
          "CLV Segment: ", clv_segment,
          "<br>App Engagement: ", app_logins_group,
          "<br>Average Churn Risk: ", scales::percent(avg_churn, accuracy = 0.1),
          "<br>Customers: ", scales::comma(n)
        )
      )
    
    p <- ggplot(
      heatmap_data,
      aes(
        x = clv_segment,
        y = app_logins_group,
        fill = avg_churn,
        text = tooltip_text
      )
    ) +
      geom_tile(color = "white", linewidth = 0.7) +
      geom_text(aes(label = label_text), size = 3.2, fontface = "bold") +
      scale_fill_gradient(
        low = "#fde2e2",
        high = "#c0392b",
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(
        x = "Customer Lifetime Value Segment",
        y = "App Logins Frequency Group",
        fill = "Avg Churn Risk"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.position = "right"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 60, r = 20, b = 40, t = 10),
        font = list(size = 10)
      )
  })
  
  # Overall churn distribution
  output$summary_churn_dist <- renderPlotly({
    
    p <- ggplot(customer, aes(x = churn_probability)) +
      geom_histogram(
        fill = "#e74c3c",
        color = "white",
        bins = 30,
        alpha = 0.85
      ) +
      geom_vline(
        xintercept = mean(customer$churn_probability, na.rm = TRUE),
        linetype = "dashed",
        linewidth = 1,
        color = "#2c3e50"
      ) +
      theme_minimal(base_size = 11) +
      labs(
        x = "Churn Probability",
        y = "Number of Customers"
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 60, r = 20, b = 45, t = 10),
        font = list(size = 10)
      )
  })
  
  # bullet
  output$bulletPlot <- renderPlotly({
    
    plot_data <- customer %>%
      group_by(preferred_transaction_type) %>%
      summarise(
        avg_value = mean(average_transaction_value, na.rm = TRUE)
      )
    
    target <- mean(plot_data$avg_value)
    
    p <- ggplot(plot_data, aes(
      y = reorder(preferred_transaction_type, avg_value),
      x = avg_value
    )) +
      geom_col(fill = "steelblue", width = 0.6) +
      geom_vline(xintercept = target, linetype = "dashed", color = "red") +
      
      scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
      
      labs(
        x = "Average Transaction Value",
        y = "Preferred Transaction"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })  
  # Top high-risk customer segments
  output$summary_top_risk_segments <- renderPlotly({
    
    risk_segment_data <- customer %>%
      drop_na(customer_segment, churn_probability) %>%
      group_by(customer_segment) %>%
      summarise(
        avg_churn = mean(churn_probability, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_churn)) %>%
      slice_head(n = 8) %>%
      mutate(
        customer_segment = reorder(customer_segment, avg_churn),
        tooltip_text = paste0(
          "Customer Segment: ", customer_segment,
          "<br>Average Churn Risk: ", scales::percent(avg_churn, accuracy = 0.1),
          "<br>Customers: ", scales::comma(n)
        )
      )
    
    p <- ggplot(
      risk_segment_data,
      aes(
        x = customer_segment,
        y = avg_churn,
        fill = avg_churn,
        text = tooltip_text
      )
    ) +
      geom_col(width = 0.68) +
      coord_flip() +
      scale_fill_gradient(
        low = "#fde2e2",
        high = "#c0392b",
        guide = "none"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        x = NULL,
        y = "Average Churn Risk"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 70, r = 20, b = 40, t = 10),
        font = list(size = 10)
      )
  })
  
  # Engagement vs churn
  output$summary_engagement_churn <- renderPlotly({
    
    engagement_summary <- customer %>%
      drop_na(app_logins_group, churn_probability) %>%
      group_by(app_logins_group) %>%
      summarise(
        avg_churn = mean(churn_probability, na.rm = TRUE),
        median_churn = median(churn_probability, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        app_logins_group = factor(
          app_logins_group,
          levels = c("Rarely", "Often", "Always")
        ),
        tooltip_text = paste0(
          "App Engagement: ", app_logins_group,
          "<br>Average Churn Risk: ", scales::percent(avg_churn, accuracy = 0.1),
          "<br>Median Churn Risk: ", scales::percent(median_churn, accuracy = 0.1),
          "<br>Customers: ", scales::comma(n)
        )
      )
    
    p <- ggplot(
      engagement_summary,
      aes(
        x = app_logins_group,
        y = avg_churn,
        group = 1,
        text = tooltip_text
      )
    ) +
      geom_line(linewidth = 1.1, color = "#2c7fb8") +
      geom_point(size = 3.2, color = "#2c7fb8") +
      geom_text(
        aes(label = scales::percent(avg_churn, accuracy = 0.1)),
        vjust = -0.7,
        size = 3.2,
        fontface = "bold"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        x = "App Logins Frequency Group",
        y = "Average Churn Risk"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 60, r = 20, b = 45, t = 10),
        font = list(size = 10)
      )
  }) 
  
  #customer overview plot
  # define variables
  categorical_vars <- c(
    "gender",
    "income_bracket",
    "household_size",
    "occupation",
    "education_level",
    "marital_status",
    "acquisition_channel",
    "customer_segment",
    "feedback_sentiment",
    "clv_segment",
    "preferred_transaction_type",
    "satisfaction_score",
    "active_products"
  )
  
  numeric_vars <- c(
    "age",
    "app_logins_frequency",
    "feature_usage_diversity",
    "monthly_transaction_count",
    "average_transaction_value",
    "churn_probability"
  )
  
  #Dynamic variable selection
  observe({
    
    if(input$x_type == "Categorical"){
      updateSelectInput(session, "x_var", choices = categorical_vars)
    } else {
      updateSelectInput(session, "x_var", choices = numeric_vars)
    }
    
  })
  
  observe({
    
    if(input$y_type == "Categorical"){
      updateSelectInput(session, "y_var", choices = categorical_vars)
    } else {
      updateSelectInput(session, "y_var", choices = numeric_vars)
    }
    
  })
  
  plot_data <- eventReactive(input$update_plot, {
    
    req(input$x_var, input$y_var)
    
    list(x = input$x_var, y = input$y_var)
    
  })
  
  output$customer_dynamic_plot <- renderPlotly({
    
    vars <- plot_data()
    
    x <- vars$x
    y <- vars$y
    
    p <- NULL
    
    # numeric vs numeric
    if(input$x_type == "Numerical" & input$y_type == "Numerical"){
      
      p <- ggplot(customer,
                  aes_string(x = x, y = y)) +
        geom_point(alpha = 0.6, color = "#2C7FB8") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        theme_minimal() +
        labs(title = paste("Relationship between", x, "and", y))
      
    }
    
    # categorical vs numeric
    if(input$x_type == "Categorical" & input$y_type == "Numerical"){
      
      p <- ggplot(customer,
                  aes_string(x = x, y = y, fill = x)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = paste("Distribution of", y, "by", x))
      
    }
    
    # numeric vs categorical
    
    if(input$x_type == "Numerical" & input$y_type == "Categorical"){
      
      p <- ggplot(customer,
                  aes_string(x = y, y = x, fill = y)) +
        geom_boxplot() +
        coord_flip() +
        theme_minimal() +
        labs(title = paste("Distribution of", x, "across", y))+
        aes_string(
          x = y,
          y = paste0("reorder(", x, ",", x, ")"),
          fill = y
        )
      
    }
    
    # categorical vs categorical
    if(input$x_type == "Categorical" & input$y_type == "Categorical"){
      
      df <- customer %>%
        count(.data[[x]], .data[[y]])
      
      p <- ggplot(df,
                  aes_string(x = x, y = y, fill = "n")) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        theme_minimal() +
        labs(title = paste("Relationship between", x, "and", y))
      
    }
    
    ggplotly(p)
    
  })
  
  #Transaction analysis graph
  output$transaction_plot <- renderPlot({
    
    filtered_data <- monthly_type %>%
      filter(
        clv_segment == input$clv_filter,
        type == input$type_filter,
        year_month >= input$date_filter[1],
        year_month <= input$date_filter[2]
      )
    
    scale_factor <- max(filtered_data$transaction_count) /
      max(filtered_data$transaction_amount)
    
    ggplot(filtered_data, aes(x = year_month)) +
      
      geom_line(aes(y = transaction_count), linewidth = 1) +
      geom_point(aes(y = transaction_count)) +
      
      geom_line(
        aes(y = transaction_amount * scale_factor),
        linetype = "dashed",
        linewidth = 1
      ) +
      
      scale_y_continuous(
        name = "Transaction Count",
        sec.axis = sec_axis(
          ~ . / scale_factor,
          name = "Transaction Amount"
        )
      ) +
      
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y"
      ) +
      
      labs(
        x = "Month",
        title = paste(
          "Transaction Trend:",
          input$type_filter,
          "-", input$clv_filter
        )
      ) +
      
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  #Churn analysis plot
  # Trigger updates when the button is pressed
  churn_inputs <- eventReactive(input$update_churn_plot, {
    list(
      x_var = input$x_var,
      test_type = input$test_type,
      conf_level = as.numeric(input$conf_level),
      display_type = input$display_type
    )
  })
  # Statistical test plot (ggstatsplot)
  output$churn_plot <- renderPlot({
    vars <- churn_inputs()
    x_var <- rlang::sym(vars$x_var)
    
    plot_title <- paste(
      "Churn Distribution by",
      tools::toTitleCase(gsub("_", " ", vars$x_var)),
      "| Confidence Level:", vars$conf_level * 100, "%"
    )
    
    if(vars$display_type == "Percentage"){
      ggstatsplot::ggbarstats(
        data = customer,
        x = churn_group,
        y = !!x_var,
        type = vars$test_type,
        conf.level = vars$conf_level,
        results.subtitle = TRUE,
        label = "percentage",
        perc.k = TRUE,
        title = plot_title
      )
    } else {
      ggstatsplot::ggbarstats(
        data = customer,
        x = churn_group,
        y = !!x_var,
        type = vars$test_type,
        conf.level = vars$conf_level,
        results.subtitle = TRUE,
        label = "count",
        title = plot_title
      )
    }
  })
  
  # mosaic plot
  vcd_plot_data <- eventReactive(input$update_plot, {
    req(input$x_var)
    
    x <- customer[[input$x_var]]
    y <- customer$churn_group
    
    # Convert logicals to factor if needed
    if (is.logical(x)) x <- factor(x, levels = c(FALSE, TRUE))
    
    # Wrap long factor labels to avoid overlap
    if (is.factor(x)) {
      levels(x) <- stringr::str_wrap(levels(x), width = 12)
    }
    
    table(x, y)
  })
  
  # VCD plot for Churn Analysis with shading
  output$churn_vcd_plot <- renderPlot({
    
    req(input$x_var)
    
    # Extract the selected variable and churn group
    x <- customer[[input$x_var]]
    churn <- customer$churn_group
    
    # Convert logicals to factor if needed
    if (is.logical(x)) x <- factor(x, levels = c(FALSE, TRUE))
    
    # Wrap long factor labels
    if (is.factor(x)) {
      levels(x) <- stringr::str_wrap(levels(x), width = 15)  # adjust width as needed
    }
    
    # Create contingency table
    tbl <- table(x, churn)
    
    # Adjust margins
    par(mar = c(8, 4, 4, 2))
    
    # Draw shaded mosaic plot
    mosaic(
      tbl,
      gp = shading_max,             # color shading like Arthritis example
      split_vertical = TRUE,        # splits vertical first
      legend = TRUE,
      main = paste(
        "Mosaic Plot: ",
        tools::toTitleCase(gsub("_"," ", input$x_var)),
        "vs Churn Group"
      ),
      labeling_args = list(
        rot_labels = c(left = 90, top = 0, bottom = 45)  # rotate bottom labels
      )
    )
    
  })
  
  # ==================================================
  # Cluster analysis
  # ==================================================
  
  cluster_results <- eventReactive(input$run_cluster, {
    
    req(input$cluster_vars)
    
    validate(
      need(length(input$cluster_vars) >= 2,
           "Please select at least 2 variables")
    )
    
    set.seed(1234)
    
    sample_df <- cluster_data %>%
      select(all_of(input$cluster_vars)) %>%
      slice_sample(n = min(input$sample_size, 2000))   
    
    scaled <- scale(sample_df)
    
    km <- kmeans(
      scaled,
      centers = min(input$k_clusters, 5), 
      nstart = 10
    )
    
    clustered <- sample_df %>%
      mutate(cluster = factor(km$cluster))
    
    list(
      scaled = scaled,
      km = km,
      clustered = clustered,
      vars = input$cluster_vars
    )
  })
  
  # 🔹 Update WSS plot
  output$wss_plot <- renderPlot({
    req(cluster_results())
    
    fviz_nbclust(
      cluster_results()$scaled,
      kmeans,
      method = "wss",
      k.max = 8
    ) +
      ggtitle("Elbow Method (WSS)")
  })
  
  
  # 🔹 Update Cluster Map (PCA)
  output$cluster_map <- renderPlot({ 
    req(cluster_results()) 
    fviz_cluster( 
      cluster_results()$km, data = cluster_results()$scaled 
    ) + 
      ggtitle("Cluster Visualization (PCA)") 
    
  })
  
  # 🔹 Update Summary Table
  output$cluster_table <- DT::renderDataTable({
    req(cluster_results())
    
    df <- cluster_results()$clustered
    vars <- cluster_results()$vars
    
    cluster_summary <- df %>%
      group_by(cluster) %>%
      summarise(
        n = n(),
        across(all_of(vars), mean, na.rm = TRUE),
        .groups = "drop"
      )
    
    DT::datatable(cluster_summary, options = list(pageLength = 5))
  })
  
  # 🔹 Update Cluster Heatmap
  output$cluster_heatmap <- renderPlotly({
    req(cluster_results())
    
    df <- cluster_results()$clustered
    
    profile <- df %>%
      group_by(cluster) %>%   
      summarise(
        across(all_of(cluster_results()$vars), mean, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(-cluster, names_to = "metric", values_to = "value")
    
    p <- ggplot(profile,
                aes(x = cluster, y = metric, fill = value)) + 
      geom_tile() +
      theme_minimal() +
      labs(
        title = "Cluster Profile Heatmap",
        x = "Cluster",
        y = "Metric"
      )
    
    ggplotly(p)
  })
  
  # ==================================================
  # Decision Tree
  # ==================================================
  
  tree_model <- function(data, predictors, min_split, complexity_parameter, max_depth) {
    model_formula <- as.formula(
      paste(target_var, "~", paste(predictors, collapse = " + "))
    )
    
    rpart(
      formula = model_formula,
      data = data,
      method = "anova",
      control = rpart.control(
        minsplit = min_split,
        cp = complexity_parameter,
        maxdepth = max_depth
      )
    )
  }
  
  # most frequent level for factor(default for scenario input)
  get_factor_default <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) == 0) {
      return(levels(x)[1])
    }
    names(sort(table(x_non_na), decreasing = TRUE))[1]
  }
  
  # most frequent value for logical(default for scenario input)
  get_logical_default <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) == 0) {
      return(FALSE)
    }
    as.logical(names(sort(table(x_non_na), decreasing = TRUE))[1])
  }
  
  # median for numeric
  get_numeric_default <- function(x) {
    val <- median(x, na.rm = TRUE)
    if (!is.finite(val)) val <- 0
    as.numeric(val)
  }
  
  make_input_ui <- function(var, data) {
    x <- data[[var]]
    input_id <- paste0("case_", var)
    
    if (is.factor(x)) {
      default_val <- get_factor_default(x)
      
      selectInput(
        inputId = input_id,
        label = var,
        choices = levels(x),
        selected = default_val
      )
      
    } else if (is.logical(x)) {
      default_val <- get_logical_default(x)
      
      selectInput(
        inputId = input_id,
        label = var,
        choices = c(TRUE, FALSE),
        selected = default_val
      )
      
    } else {
      default_val <- get_numeric_default(x)
      
      numericInput(
        inputId = input_id,
        label = var,
        value = round(default_val, 2)
      )
    }
  }
  
  build_new_case <- function(all_vars, input_vars, data, input) {
    out <- vector("list", length(all_vars))
    names(out) <- all_vars
    
    # first fill all selected predictors with defaults
    for (var in all_vars) {
      template <- data[[var]]
      
      if (is.factor(template)) {
        default_val <- get_factor_default(template)
        out[[var]] <- factor(default_val, levels = levels(template))
        
      } else if (is.logical(template)) {
        default_val <- get_logical_default(template)
        out[[var]] <- as.logical(default_val)
        
      } else {
        default_val <- get_numeric_default(template)
        out[[var]] <- as.numeric(default_val)
      }
    }
    
    # then overwrite only tree-used variables with user inputs
    for (var in input_vars) {
      val <- input[[paste0("case_", var)]]
      template <- data[[var]]
      
      if (is.factor(template)) {
        out[[var]] <- factor(as.character(val), levels = levels(template))
        
      } else if (is.logical(template)) {
        out[[var]] <- as.logical(val)
        
      } else {
        out[[var]] <- as.numeric(val)
      }
    }
    
    as.data.frame(out, stringsAsFactors = FALSE)
  }
  
  model_results <- eventReactive(input$build_model, {
    req(input$predictors)
    req(length(input$predictors) > 0)
  
    df_model <- customer_data_clean %>%
      select(all_of(c(target_var, input$predictors)))
    
    set.seed(1234)
    n_sample <- min(2000, nrow(df_model))   #if the record is less than 2000, then take all

    train_index <- createDataPartition(
      df_model[[target_var]],
      p = input$split_ratio,
      list = FALSE
    )
    
    df_train <- df_model[train_index, , drop = FALSE]
    df_test  <- df_model[-train_index, , drop = FALSE]
    
    fit_tree <- tree_model(
      data = df_train,
      predictors = input$predictors,
      min_split = input$minsplit,
      complexity_parameter = input$cp,
      max_depth = input$maxdepth
    )
    
    df_test$pred_tree <- predict(fit_tree, newdata = df_test)
    
    if (is.null(fit_tree$variable.importance)) {
      tree_importance <- tibble(
        Variable = character(),
        Importance = numeric()
      )
    } else {
      tree_importance <- tibble(
        Variable = names(fit_tree$variable.importance),
        Importance = as.numeric(fit_tree$variable.importance)
      ) %>%
        arrange(desc(Importance))
    }
    
    rmse_value <- sqrt(mean((df_test[[target_var]] - df_test$pred_tree)^2))
    mae_value  <- mean(abs(df_test[[target_var]] - df_test$pred_tree))
    
    r2_value <- if (sd(df_test[[target_var]]) == 0 || sd(df_test$pred_tree) == 0) {
      NA_real_
    } else {
      cor(df_test[[target_var]], df_test$pred_tree)^2
    }
    
    tree_vars <- unique(fit_tree$frame$var[fit_tree$frame$var != "<leaf>"])
    
    list(
      fit_tree = fit_tree,
      df_model = df_model,
      df_test = df_test,
      tree_importance = tree_importance,
      rmse_value = rmse_value,
      mae_value = mae_value,
      r2_value = r2_value,
      tree_vars = tree_vars,
      selected_predictors = input$predictors
    )
  })
  
  scenario_result <- eventReactive(input$predict_case, {
    req(model_results())
    
    input_vars <- model_results()$tree_vars
    all_vars   <- model_results()$selected_predictors
    
    req(length(input_vars) > 0)
    
    new_case <- build_new_case(
      all_vars = all_vars,
      input_vars = input_vars,
      data = model_results()$df_model,
      input = input
    )
    
    pred_value <- as.numeric(
      predict(model_results()$fit_tree, newdata = new_case)
    )
    
    party_model <- as.party(model_results()$fit_tree)
    leaf_node <- as.integer(
      predict(party_model, newdata = new_case, type = "node")
    )
    
    path_steps <- path.rpart(
      model_results()$fit_tree,
      nodes = leaf_node,
      print.it = FALSE
    )[[1]]
    
    list(
      new_case = new_case,
      pred_value = pred_value,
      leaf_node = leaf_node,
      path_steps = path_steps
    )
  })
  
  output$rmse_box <- renderValueBox({
    
    req(model_results())
    
    valueBox(
      round(model_results()$rmse_value, 4),
      "RMSE",
      icon = icon("chart-line"),
      color = "purple"
    )
    
  })
  
  output$mae_box <- renderValueBox({
    
    req(model_results())
    
    valueBox(
      round(model_results()$mae_value, 4),
      "MAE",
      icon = icon("chart-bar"),
      color = "yellow"
    )
    
  })
  
  output$r2_box <- renderValueBox({
    
    req(model_results())
    
    valueBox(
      round(model_results()$r2_value, 4),
      "R Squared",
      icon = icon("percentage"),
      color = "green"
    )
    
  })
  
  output$tree_plot <- renderVisNetwork({
    req(model_results())
    
    visTree(
      model_results()$fit_tree,
      edgesFontSize = 14,
      nodesFontSize = 16,
      width = "100%",
      height = "650px"
    )
  })
  
  output$pred_actual_plot <- renderPlot({
    req(model_results())
    
    ggplot(model_results()$df_test, aes(x = churn_probability, y = pred_tree)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
      labs(
        x = "Actual Churn Probability",
        y = "Predicted Churn Probability"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$importance_plot <- renderPlot({
    req(model_results())
    
    validate(
      need(
        nrow(model_results()$tree_importance) > 0,
        "No variable importance available for this tree."
      )
    )
    
    model_results()$tree_importance %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Variable",
        y = "Importance"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$scenario_inputs <- renderUI({
    req(model_results())
    
    input_vars <- model_results()$tree_vars
    
    if (length(input_vars) == 0) {
      return(tags$p("This tree has no split variables."))
    }
    
    tagList(
      lapply(input_vars, make_input_ui, data = model_results()$df_model)
    )
  })
  
  # ADD
  
  output$scenario_pred_box <- renderUI({
    req(scenario_result())
    
    div(
      class = "metric-box",
      style = "width: 220px;",
      h3(sprintf("%.2f%%", scenario_result()$pred_value * 100)),
      p("Predicted Churn Probability")
    )
  })
  
  output$decision_path <- renderUI({
    req(scenario_result())
    
    div(
      class = "path-box",
      tags$p(
        tags$b("Terminal node: "),
        scenario_result()$leaf_node
      ),
      tags$ol(
        lapply(scenario_result()$path_steps, function(step) {
          tags$li(step)
        })
      )
    )
  })
  
  # =========================
  # Random Forest
  # =========================
  
  
  # ----- Helper functions -----
  
  # Most frequent level for factor
  get_factor_default <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) == 0) return(levels(x)[1])
    names(sort(table(x_non_na), decreasing = TRUE))[1]
  }
  
  # Numeric default (median) for numeric predictors
  get_numeric_default <- function(x) {
    val <- median(x, na.rm = TRUE)
    if (!is.finite(val)) val <- 0
    as.numeric(val)
  }
  
  # Safe numeric for value boxes
  safe_numeric <- function(x, fallback = 0) {
    if (is.null(x) || !is.finite(x)) fallback else as.numeric(x)
  }
  
  # Generate scenario input UI for RF
  make_input_ui_rf <- function(var, data) {
    x <- data[[var]]
    input_id <- paste0("rf_case_", var)
    
    if (is.factor(x)) {
      default_val <- get_factor_default(x)
      selectInput(
        inputId = input_id,
        label = var,
        choices = levels(x),
        selected = default_val
      )
    } else {
      default_val <- get_numeric_default(x)
      numericInput(
        inputId = input_id,
        label = var,
        value = round(default_val, 2)
      )
    }
  }
  
  # Build new scenario case for RF prediction
  build_new_case_rf <- function(all_vars, data, input) {
    out <- vector("list", length(all_vars))
    names(out) <- all_vars
    
    for (var in all_vars) {
      template <- data[[var]]
      val <- input[[paste0("rf_case_", var)]]
      
      if (is.factor(template)) {
        out[[var]] <- factor(as.character(val), levels = levels(template))
      } else {
        out[[var]] <- as.numeric(val)
      }
    }
    
    as.data.frame(out, stringsAsFactors = FALSE)
  }
  
  #Random Forest model reactive
  rf_model_results <- eventReactive(input$rf_build_model, {
    tryCatch({
      req(input$rf_predictors)
      req(length(input$rf_predictors) > 0)
      
      df_model <- customer_data_clean %>%
        select(all_of(c(target_var, input$rf_predictors))) %>%
        mutate(across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE)))) %>%
        drop_na() %>%
        slice_sample(n = 2000) %>%
        as.data.frame()
      
      set.seed(1234)
      train_index <- createDataPartition(df_model[[target_var]],
                                         p = input$rf_split_ratio,
                                         list = FALSE)
      df_train <- df_model[train_index, , drop = FALSE]
      df_test  <- df_model[-train_index, , drop = FALSE]
      
      p <- length(input$rf_predictors)
      
      rf_grid <- expand.grid(
        mtry = max(1, floor(sqrt(p))),
        splitrule = "variance",
        min.node.size = 5
      )
      
      rf_control <- trainControl(method = "none")
      
      fit_rf <- caret::train(
        x = df_train[, input$rf_predictors, drop = FALSE],
        y = df_train[[target_var]],
        method = "ranger",
        trControl = rf_control,
        tuneGrid = rf_grid,
        num.trees = input$rf_num_trees,
        importance = "impurity"
      )
      
      df_test$pred_rf <- predict(fit_rf, newdata = df_test[, input$rf_predictors, drop = FALSE])
      
      rf_importance <- varImp(fit_rf)$importance %>%
        tibble::rownames_to_column("Variable") %>%
        rename(Importance = Overall) %>%
        arrange(desc(Importance))
      
      rmse_value <- sqrt(mean((df_test[[target_var]] - df_test$pred_rf)^2))
      mae_value  <- mean(abs(df_test[[target_var]] - df_test$pred_rf))
      r2_value   <- if (sd(df_test[[target_var]]) == 0 || sd(df_test$pred_rf) == 0) NA_real_ else cor(df_test[[target_var]], df_test$pred_rf)^2
      
      list(
        ok = TRUE,
        error_message = NULL,
        fit_rf = fit_rf,
        df_model = df_model,
        df_test = df_test,
        rf_importance = rf_importance,
        rmse_value = rmse_value,
        mae_value = mae_value,
        r2_value = r2_value,
        selected_predictors = input$rf_predictors
      )
      
    }, error = function(e) {
      list(
        ok = FALSE,
        error_message = e$message
      )
    })
  })
  
  #Random Forest scenario prediction
  rf_scenario_result <- eventReactive(input$rf_predict_case, {
    tryCatch({
      req(rf_model_results())
      req(isTRUE(rf_model_results()$ok))
      
      all_vars <- rf_model_results()$selected_predictors
      
      new_case <- build_new_case_rf(
        all_vars = all_vars,
        data = rf_model_results()$df_model,
        input = input
      )
      
      pred_value <- as.numeric(predict(rf_model_results()$fit_rf, newdata = new_case))
      
      list(
        ok = TRUE,
        error_message = NULL,
        new_case = new_case,
        pred_value = pred_value
      )
      
    }, error = function(e) {
      list(
        ok = FALSE,
        error_message = e$message
      )
    })
  })
  
  #Value boxes
  output$rf_rmse_box <- renderValueBox({
    req(rf_model_results())
    valueBox(
      round(safe_numeric(rf_model_results()$rmse_value), 4),
      "RMSE",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$rf_mae_box <- renderValueBox({
    req(rf_model_results())
    req(isTRUE(rf_model_results()$ok))
    valueBox(
      round(safe_numeric(rf_model_results()$mae_value), 4),
      "MAE",
      icon = icon("chart-bar"),
      color = "yellow"
    )
  })
  
  output$rf_r2_box <- renderValueBox({
    req(rf_model_results())
    req(isTRUE(rf_model_results()$ok))
    valueBox(
      round(safe_numeric(rf_model_results()$r2_value), 4),
      "R Squared",
      icon = icon("percentage"),
      color = "green"
    )
  })
  
  output$rf_error_box <- renderUI({
    req(rf_model_results())
    if (!isTRUE(rf_model_results()$ok)) {
      div(class = "error-box",
          strong("Random Forest error: "),
          rf_model_results()$error_message)
    }
  })
  
  #Plots
  output$rf_pred_actual_plot <- renderPlot({
    req(rf_model_results())
    req(isTRUE(rf_model_results()$ok))
    ggplot(rf_model_results()$df_test, aes(x = churn_probability, y = pred_rf)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
      labs(x = "Actual Churn Probability", y = "Predicted Churn Probability") +
      theme_minimal(base_size = 12)
  })
  
  output$rf_importance_plot <- renderPlot({
    req(rf_model_results())
    req(isTRUE(rf_model_results()$ok))
    
    validate(need(nrow(rf_model_results()$rf_importance) > 0, "No variable importance available for this forest."))
    
    rf_model_results()$rf_importance %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col() +
      coord_flip() +
      labs(x = "Variable", y = "Importance") +
      theme_minimal(base_size = 12)
  })
  
  #Scenario inputs and prediction UI
  output$rf_scenario_inputs <- renderUI({
    req(rf_model_results())
    req(isTRUE(rf_model_results()$ok))
    
    input_vars <- rf_model_results()$selected_predictors
    tagList(lapply(input_vars, make_input_ui_rf, data = rf_model_results()$df_model))
  })
  
  output$rf_scenario_pred_box <- renderUI({
    req(rf_scenario_result())
    
    if (!isTRUE(rf_scenario_result()$ok)) {
      div(class = "error-box",
          strong("Prediction error: "),
          rf_scenario_result()$error_message)
    } else {
      div(class = "metric-box", style = "width: 220px;",
          h3(sprintf("%.2f%%", rf_scenario_result()$pred_value * 100)),
          p("Predicted Churn Probability"))
    }
  })
  
  
  
}


shinyApp(ui, server)