# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(fpp3)) install.packages("fpp3", repos = "http://cran.us.r-project.org")
if(!require(tseries)) install.packages("tseries", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(tsibble)) install.packages("tsibble", repos = "http://cran.us.r-project.org")
if(!require(tsibbledata)) install.packages("tsibbledata", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT")
source("variable_map.R")

raw_data <- readxl::read_excel(path = "input_data/TS_RawData.xlsx", col_names = TRUE, skip = 2, sheet = "Sheet1")

raw_data[is.na(raw_data)] <- 0

sales <- pivot_longer(raw_data, cols = starts_with("4"),
                      names_to = "Date",
                      values_to = "Sales",
                      values_drop_na = FALSE)

sales_ts <- sales %>%
  mutate(Month = yearmonth(as.Date(as.numeric(Date), origin = "1899-12-30"))) %>%
  select(-Date) %>%
  as_tsibble(key = c(Part, Area, Country),
             index = Month)


analysis_parts = function(sales_ts) {
  sales_parts <- sales_ts %>%
    group_by(Part) %>%
    summarize(Total_Sales = sum(Sales))
  
  sales_parts %>% 
    autoplot(Total_Sales) +
    labs(y = "Sales (Count)",
         title = "Total Sales Grouped by Parts") 
}

analysis_seasonal = function(sales_ts) {
  sales_parts <- sales_ts %>%
    group_by(Part) %>%
    summarize(Total_Sales = sum(Sales))
  
  sales_parts %>%
    gg_subseries(Total_Sales)  +
    labs(y = "Sales (Count)",
         title = "Seasonal Sales by Parts")
}

analysis_aggre = function(sales_ts) {
  sales_hts <- sales_ts %>%
    aggregate_key(Area/Country, Total_Sales = sum(Sales))
  
  sales_hts %>%
    filter(is_aggregated(Country)) %>%
    autoplot(Total_Sales) +
    labs(y = "Sales (Count)",
         title = "Total Sales Aggregated by Area/Country") +
    facet_wrap(vars(Area), scales = "free_y", ncol = 2) +
    theme(legend.position = "right")
}

analysis_area = function(sales_ts) {
  sales_hts <- sales_ts %>%
    aggregate_key((Area / Country) * Part, Sales = sum(Sales))
  
  sales_hts %>%
    filter(!is_aggregated(Part), !is_aggregated(Area),
           !is_aggregated(Country)) %>%
    mutate(Part = as.character(Part)) %>%
    ggplot(aes(x = Month, y = Sales,
               group = Part, colour=Part)) +
    stat_summary(fun = sum, geom = "line") +
    labs(title = "Sales by Part and Area",
         y = "Unit Sales") +
    facet_wrap(~ as.character(Area),
               nrow = 1, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
}

decomp_drift = function(sales_ts) {
  sales_hts <- sales_ts %>%
    aggregate_key((Area / Country) * Part, Sales = sum(Sales))
  
  sales_total <- sales_hts %>% filter(is_aggregated(Country), is_aggregated(Part), is_aggregated(Area))
  aug <- sales_total %>%
    model(NAIVE(Sales)) %>%
    augment()
  
  sales_total %>%
    model(NAIVE(Sales)) %>%
    gg_tsresiduals()
}

decomposition = function(sales_ts, decomp_type) {
  sales_hts <- sales_ts %>%
    aggregate_key((Area / Country) * Part, Sales = sum(Sales))
  
  aggregated_sales_ts <- sales_hts %>% filter(is_aggregated(Country), is_aggregated(Part), is_aggregated(Area))
  
  if(decomp_type == "Simple")
    aggregated_sales_ts %>%
      model(classical_decomposition(Sales, type="additive")) %>%
      components() %>% 
      autoplot()    
  else
    aggregated_sales_ts %>%
    model(STL(Sales)) %>%
    components() %>% 
    autoplot()
}

predict_arima = function(sales_ts) {
  sales_hts <- sales_ts %>%
    aggregate_key((Area / Country) * Part, Sales = sum(Sales))
  
  sales_total <- sales_hts %>% filter(is_aggregated(Country), is_aggregated(Part), is_aggregated(Area))
  
  arima_fit <- sales_total %>%
    model(arima110 = ARIMA(Sales ~ pdq(1,1,0)),
          arima011 = ARIMA(Sales ~ pdq(0,1,1)),
          stepwise = ARIMA(Sales),
          search = ARIMA(Sales, stepwise=FALSE))
  
  arima_fit %>%
    forecast(h=12) %>%
    filter(.model=='search') %>%
    autoplot(sales_total)
}

arima_resid = function(sales_hts) {
  sales_total <- sales_hts %>% 
    filter(is_aggregated(Country), is_aggregated(Part), is_aggregated(Area))
  
  sales_total %>%
    gg_tsdisplay(difference(Sales), plot_type='partial')
}

predict_tslm = function(sales_hts) {
  # sales_hts <- sales_ts %>%
  #   aggregate_key((Area / Country) * Part, Sales = sum(Sales))
  # 
  fit <- sales_hts %>%
    filter(year(Month) <= 2021) %>%
    model(base = TSLM(Sales~ trend() + season())) %>%
    reconcile(
      bu = bottom_up(base),
      ols = min_trace(base, method = "ols"),
      mint = min_trace(base, method = "mint_shrink"),
    )
  
  fc <- fit %>% forecast(h = "2 years")
  
  fc %>%
    filter(is_aggregated(Area), is_aggregated(Part)) %>%
    autoplot(
      sales_hts
    ) +
    labs(y = "Sales Units") +
    facet_wrap(vars(Country), scales = "free_y")
}

predict_tslm_parts = function(sales_hts) {
  # sales_hts <- sales_ts %>%
  #   aggregate_key((Area / Country) * Part, Sales = sum(Sales))
  # 
  fit <- sales_hts %>%
    filter(year(Month) <= 2021) %>%
    model(base = TSLM(Sales~ trend() + season())) %>%
    reconcile(
      bu = bottom_up(base),
      ols = min_trace(base, method = "ols"),
      mint = min_trace(base, method = "mint_shrink"),
    )
  
  fc <- fit %>% forecast(h = "2 years")
  
  fc %>%
    filter(is_aggregated(Area), !is_aggregated(Part)) %>%
    autoplot(    
      sales_hts %>% filter(year(Month) >= 2017),
      level = NULL
    ) +
    labs(y = "Sales Unit") +
    facet_wrap(vars(Part), scales = "free_y")
}

predict_ets = function(fc, sales_ts) {
  sales_gts <- sales_ts %>%
    aggregate_key((Area / Country) * Part, Total_Sales = sum(Sales))

  # fit <- sales_gts %>%
  #   filter(year(Month) <=2021) %>%
  #   model(base = ETS(Total_Sales)) %>%
  #   reconcile(
  #     bu = bottom_up(base),
  #     ols = min_trace(base, method = "ols"),
  #     mint = min_trace(base, method = "mint_shrink"),
  #   )
  # fc <- fit %>% forecast(h = "2 years")
  
  fc %>%
    filter(is_aggregated(Area), is_aggregated(Part)) %>%
    autoplot(
      sales_gts
    ) +
    labs(y = "Sales Units") +
    facet_wrap(vars(Country), scales = "free_y")
}

predict_ets_parts = function(fc, sales_ts) {
  sales_gts <- sales_ts %>%
    aggregate_key((Area / Country) * Part, Total_Sales = sum(Sales))

    fc %>%
    filter(is_aggregated(Area), !is_aggregated(Part)) %>%
    autoplot(
      sales_gts %>% filter(year(Month) >= 2017),
      level = NULL
    ) +
    labs(y = "Sales Units") +
    facet_wrap(vars(Part), scales = "free_y")
}

### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Demand Forecast Improvement of Medical Device Products</a>'), id="nav",
             windowTitle = "DS590 Project",
             
             tabPanel("Analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h5("Select values from filters below to select a subset:")), style="color:#045a8d"),
                          sliderInput("km_yearmonth",
                                      "Date Range:",
                                      min = as.Date(min(sales_ts$Month)),
                                      max = as.Date(max(sales_ts$Month)),
                                      #max = max(as.numeric(polis_survival[polis_survival$Age.at.Dignosis<999,"Age.at.Dignosis"])),
                                      value=(c(as.Date(min(sales_ts$Month)), as.Date(max(sales_ts$Month)))),
                                      timeFormat = "%b %Y"
                                      ),
                          pickerInput("km_part", "Part:",
                                      choices = map_part,
                                      options = list(`actions-box` = TRUE),
                                      selected = map_part,
                                      multiple = TRUE),
                          
                          pickerInput("km_area", "Area:",
                                      choices = map_area,
                                      options = list(`actions-box` = TRUE),
                                      selected = map_area,
                                      multiple = TRUE),
                          
                          pickerInput("km_country", "Country:",
                                      choices = map_country,
                                      options = list(`actions-box` = TRUE),
                                      selected = map_country,
                                      multiple = TRUE),
                          
                          ),
                        
                        mainPanel(
                              fluidRow(
                                tabsetPanel(type = "tabs",
                                    tabPanel("Data Analysis", value = 1,
                                             titlePanel("Parts Sales"),
                                             fluidRow(plotOutput("analysis_parts", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             titlePanel("Area-wise Parts Sales"),
                                             fluidRow(plotOutput("analysis_area", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             titlePanel("Aggregated by Area"),
                                             fluidRow(plotOutput("analysis_aggre", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             titlePanel("Aggregated Seasonal Sales"),
                                             fluidRow(plotOutput("analysis_seasonal", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             br()
                                    ),
                                    
                                    tabPanel("Decomposition", value = 2,
                                             titlePanel("Decomposition"),
                                             fluidRow(align = "center", 
                                               radioButtons("km_decomp", "Choose Decomposition: ",
                                                            choiceNames = c("Simple", "STL"),
                                                            choiceValues = c("Simple", "STL"),
                                                            selected = "Simple", width = "90%", inline = TRUE),
                                             ),
                                             fluidRow(plotOutput("decomposition", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             titlePanel("Innovation Residuals"),
                                             fluidRow(plotOutput("decomp_drift", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             # titlePanel("Ljung-Box Test"),
                                             # fluidRow(plotOutput("analysis_ljung", width = "90%") %>% withSpinner(color="#2c3e50")),
                                             br()
                                    ),
                                    
                                    tabPanel("Prediction Models", value = 3,
                                    tabsetPanel(type = "pills",
                                                tabPanel("ARIMA",
                                                         titlePanel("Residuals"),
                                                         fluidRow(plotOutput("arima_resid", width = "90%") %>% withSpinner(color="#2c3e50")),
                                                         titlePanel("Prediction Model"),
                                                         fluidRow(plotOutput("predict_arima", width = "90%") %>% withSpinner(color="#2c3e50"))
                                                         ),
                                                tabPanel("TSLM",
                                                         fluidRow(plotOutput("predict_tslm", width = "90%") %>% withSpinner(color="#2c3e50")),
                                                         br(),
                                                         fluidRow(plotOutput("predict_tslm_parts", width = "90%") %>% withSpinner(color="#2c3e50")),
                                                ),
                                                tabPanel("ETS", 
                                                         fluidRow(plotOutput("predict_ets", width = "90%") %>% withSpinner(color="#2c3e50")),
                                                         br(),
                                                         fluidRow(plotOutput("predict_ets_parts", width = "90%") %>% withSpinner(color="#2c3e50")),
                                                         br()
                                                         ),
                                                ),
                                    ),
                                    )
                                ),
                              )
                        )
                      ),
  )          
)

### SHINY SERVER ###

server = function(input, output, session) {
  sales_reactive = reactive({
    sales_ts_reactive = sales_ts
    
    filt_area <- input$km_area
    if(!is.null(filt_area)) {
      sales_ts_reactive <- sales_ts_reactive %>% dplyr::filter(Area %in% filt_area)
    }
    
    filt_country <- input$km_country
    if(!is.null(filt_country)) {
      sales_ts_reactive <- sales_ts_reactive %>% dplyr::filter(Country %in% filt_country)
    }

    filt_part <- input$km_part
    if(!is.null(filt_part)) {
      sales_ts_reactive <- sales_ts_reactive %>% dplyr::filter(Part %in% filt_part)
    }

    filt_timerange <- input$km_yearmonth
    if(!is.null(filt_timerange)) {
      sales_ts_reactive <- sales_ts_reactive %>% filter_index(as.character(filt_timerange[1]) ~ as.character(filt_timerange[2]))
    }
    
    sales_ts_reactive
  })
  
  sales_hts_reactive = reactive({
    sales_hts_reactive <- sales_reactive() %>%
      aggregate_key((Area / Country) * Part, Sales = sum(Sales))
    sales_hts_reactive
  })
  
  ets_fc_reactive = reactive({
    sales_gts <- sales_reactive() %>%
      aggregate_key((Area / Country) * Part, Total_Sales = sum(Sales))
    
    fit <- sales_gts %>%
      filter(year(Month) <= input$km_yearmonth[2]) %>%
      model(base = ETS(Total_Sales)) %>%
      reconcile(
        bu = bottom_up(base),
        ols = min_trace(base, method = "ols"),
        mint = min_trace(base, method = "mint_shrink"),
      )
    fc <- fit %>% forecast(h = "2 years")
    fc
  })
  
  output$analysis_parts <- renderPlot({
    analysis_parts(sales_reactive())
  })

  output$analysis_seasonal <- renderPlot({
    analysis_seasonal(sales_reactive())
  })
  
  output$analysis_aggre <- renderPlot({
    analysis_aggre(sales_reactive())
  })
  
  output$analysis_area <- renderPlot({
    analysis_area(sales_reactive())
  })
  
  output$decomposition <- renderPlot({
    decomposition(sales_reactive(), input$km_decomp)
  })
  
  output$decomp_drift <- renderPlot({
    decomp_drift(sales_reactive())
  })
  
    output$predict_arima <- renderPlot({
    predict_arima(sales_reactive())
  })
  
  output$predict_tslm <- renderPlot({
    predict_tslm(sales_hts_reactive())
  })
  
  output$predict_tslm_parts <- renderPlot({
    predict_tslm_parts(sales_hts_reactive())
  })
  
  output$predict_ets <- renderPlot({
    predict_ets(ets_fc_reactive(), sales_reactive())
  })
  
  output$predict_ets_parts <- renderPlot({
    predict_ets_parts(ets_fc_reactive(), sales_reactive())
  })
  
  output$arima_resid <- renderPlot({
    arima_resid(sales_hts_reactive())
  })
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
