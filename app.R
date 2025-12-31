############################################################
# Shiny App: Health Dashboard (Category & Region Filters)
# Dataset: COVID-19 Data Hub (R package: COVID19)
#
# Requirements satisfied:
# - Filters: Category (metric), Country, Region, Date range
# - Visualizations: Bar chart, Heatmap, Time-series
# - Auto-install required packages
# - Runs with one click (Run App)
############################################################

# ---------------------------
# 1) Auto-install & load packages
# ---------------------------
required_pkgs <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "COVID19",
  "lubridate"
)

missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---------------------------
# 2) Load data safely (from internet)
# ---------------------------
# level = 2  --> country + region/state/province
load_covid_data <- function() {
  tryCatch(
    {
      COVID19::covid19(level = 2)
    },
    error = function(e) {
      message("Data download failed: ", e$message)
      NULL
    }
  )
}

cov_raw <- load_covid_data()

# ---------------------------
# 3) If data fails to load, show message-only app
# ---------------------------
if (is.null(cov_raw) || nrow(cov_raw) == 0) {
  
  ui <- fluidPage(
    titlePanel("Health Dashboard"),
    tags$div(
      style = "padding:15px; font-size:16px;",
      tags$b("Data could not be loaded."),
      tags$p("Possible reasons:"),
      tags$ul(
        tags$li("No internet connection"),
        tags$li("Firewall / proxy restrictions"),
        tags$li("Temporary data source issue")
      )
    )
  )
  
  server <- function(input, output, session) {}
  shinyApp(ui, server)
  
} else {
  
  # ---------------------------
  # 4) Prepare & clean dataset
  # ---------------------------
  df <- cov_raw %>%
    transmute(
      date    = as.Date(date),
      
      # Administrative levels:
      # level_1 = country, level_2 = region/state
      country = as.factor(administrative_area_level_1),
      region  = as.factor(administrative_area_level_2),
      
      cases  = as.numeric(confirmed),
      deaths = as.numeric(deaths),
      hosp   = as.numeric(hosp),
      tests  = as.numeric(tests)
    ) %>%
    drop_na(country, date)
  
  # Category selector (what the assignment calls "category")
  metrics <- c(
    "Cases"            = "cases",
    "Deaths"           = "deaths",
    "Hospitalizations" = "hosp",
    "Tests"            = "tests"
  )
  
  # Default date range
  max_date <- max(df$date, na.rm = TRUE)
  min_date <- min(df$date, na.rm = TRUE)
  default_start <- max(min_date, max_date - 365)
  
  # ---------------------------
  # 5) UI
  # ---------------------------
  ui <- fluidPage(
    titlePanel("Health Dashboard — Category & Region Filters"),
    
    sidebarLayout(
      sidebarPanel(
        # Category filter
        selectInput(
          "metric",
          "Category (metric)",
          choices = names(metrics),
          selected = "Cases"
        ),
        
        # Country filter
        selectInput(
          "country",
          "Country",
          choices = c("All", levels(df$country)),
          selected = "All"
        ),
        
        # Region filter (dynamic)
        uiOutput("region_ui"),
        
        # Date filter
        dateRangeInput(
          "dr",
          "Date range",
          start = default_start,
          end   = max_date,
          min   = min_date,
          max   = max_date
        ),
        
        tags$hr(),
        tags$small(
          "Note: Some countries do not provide region-level data ",
          "or specific metrics (tests/hospitalizations)."
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Time-series", plotOutput("tsPlot", height = 320)),
          tabPanel("Bar chart", plotOutput("barPlot", height = 320)),
          tabPanel("Heatmap", plotOutput("heatPlot", height = 380))
        )
      )
    )
  )
  
  # ---------------------------
  # 6) Server
  # ---------------------------
  server <- function(input, output, session) {
    
    # ---- Helper: empty plot with message ----
    empty_plot <- function(msg = "No data for the selected filters") {
      plot.new()
      text(0.5, 0.5, msg, cex = 1.1)
    }
    
    # ---- Detect available regions for selected country ----
    available_regions <- reactive({
      d0 <- df
      if (input$country != "All") {
        d0 <- d0 %>% filter(country == input$country)
      }
      
      regs <- sort(unique(as.character(d0$region)))
      regs <- regs[!is.na(regs) & regs != ""]
      regs
    })
    
    # ---- Region UI (smart handling when region doesn't exist) ----
    output$region_ui <- renderUI({
      regs <- available_regions()
      
      if (length(regs) == 0) {
        tags$div(
          tags$label("Region"),
          tags$div(
            style = "padding:8px; background:#f2f2f2; border-radius:5px;",
            "No region-level data available for this country."
          )
        )
      } else {
        selectInput(
          "region",
          "Region",
          choices = c("All", regs),
          selected = "All"
        )
      }
    })
    
    # ---- Apply filters + choose metric ----
    filtered_data <- reactive({
      met <- metrics[[input$metric]]
      
      d <- df %>%
        filter(date >= input$dr[1], date <= input$dr[2])
      
      if (input$country != "All") {
        d <- d %>% filter(country == input$country)
      }
      
      if (!is.null(input$region) &&
          input$region != "All" &&
          length(available_regions()) > 0) {
        d <- d %>% filter(as.character(region) == input$region)
      }
      
      d %>%
        mutate(value = .data[[met]]) %>%
        drop_na(value)
    })
    
    # ---- Plot 1: Time-series ----
    output$tsPlot <- renderPlot({
      d <- filtered_data()
      if (nrow(d) == 0)
        return(empty_plot("No data for this metric / region / date range"))
      
      d2 <- d %>%
        group_by(date) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      
      ggplot(d2, aes(date, total)) +
        geom_line() +
        theme_minimal() +
        labs(
          x = "Date",
          y = paste("Total", input$metric),
          title = paste("Time-series:", input$metric)
        )
    })
    
    # ---- Plot 2: Bar chart (Top regions) ----
    output$barPlot <- renderPlot({
      d <- filtered_data()
      if (nrow(d) == 0)
        return(empty_plot())
      
      d2 <- d %>%
        group_by(region) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = 12)
      
      if (nrow(d2) == 0 || all(is.na(d2$region))) {
        return(empty_plot("Region-level data not available"))
      }
      
      ggplot(d2, aes(reorder(region, total), total)) +
        geom_col() +
        coord_flip() +
        theme_minimal() +
        labs(
          x = "Region",
          y = paste("Total", input$metric),
          title = "Top Regions"
        )
    })
    
    # ---- Plot 3: Heatmap (Region × Month) ----
    output$heatPlot <- renderPlot({
      d <- filtered_data()
      if (nrow(d) == 0)
        return(empty_plot())
      
      d2 <- d %>%
        mutate(month = format(date, "%Y-%m")) %>%
        group_by(region, month) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      
      if (nrow(d2) == 0)
        return(empty_plot("Not enough data for heatmap"))
      
      ggplot(d2, aes(month, region, fill = total)) +
        geom_tile() +
        theme_minimal() +
        labs(
          x = "Month",
          y = "Region",
          title = paste("Heatmap:", input$metric)
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  }
  
  shinyApp(ui, server)
}
