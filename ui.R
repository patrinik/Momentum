
# https://rstudio.github.io/shinydashboard/appearance.html
# https://r-posts.com/using-shiny-dashboards-for-financial-analysis/
# https://github.com/pmaji/financial-asset-comparison-tool

require(shiny)
require(shinydashboard)
require(plotly)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Momentum & Allocation",
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(
        dateRangeInput("date", label = "Date", start = start.date, end = end.date, weekstart = 1)
      ),
      menuItem(
        selectInput("model_choices", label = "Models", choices = model_choices, 
                    selected = lapply(c(1:5), function(x){model_choices[[x]]}), multiple = TRUE)
      ),
      menuItem(
        sliderInput("nTop", label = "# Assets", min = 1, max = max_assets, value = 4, step = 1)
      ),
      menuItem(
        sliderInput("nMom", label = "Lookback momentum", min = 60, max = 140, value = 120, step = 5)
      ),
      menuItem(
        sliderInput("nVol", label = "Lookback vola", min = 20, max = 120, value = 60, step = 5)
      ),
      menuItem(
        sliderInput("trgtVol", label = "Target vola", min = 4, max = 20, value = 10, step = 1)
      ),
      menuItem(
        actionButton("run_models", label = "Submit")
      ),
      menuItem(
        "Source Code",
        href="https://github.com/patrinik/Momentum",
        icon=icon("github")
      )
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "ETF Data",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("etf_data"),
        height = 600,
        width = 6
      ),
      box(title = "Model Equity",
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("model_data"),
          height = 600,
          width = 6
      )
    ),
    fluidRow(
      box(
        title = "Momentum",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("hist_mom")
      ),
      tabBox(
        tabPanel("Table",
                 tableOutput("summary_tab")
        ),
        tabPanel("Allocation",
                 uiOutput("alloc_plch"),
                 plotlyOutput("allocation")
        )
      )
    )
  )
)

###----------------------------------------------------
# bye bye 




