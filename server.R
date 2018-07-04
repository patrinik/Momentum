
server <- function(input, output, session){
  
  timeSeries <- reactive({
    
    xts.ts <- xts(select(xl.data, -Date), order.by = xl.data$Date)
    
    # Dates for time series
    tmp <- format(input$date, "%Y%m%d") 
    date <- paste(tmp, collapse = "/")
    
    xts.obj <- xts.ts[date]
    
    
    xl.obj <- coredata(xts.obj) %>% as.data.frame()
    xl.obj$Date <- index(xts.obj)
    
    out <- list(xts.ts = xts.obj, xl.data = xl.obj)
  })
  
  parameter <- reactive({
    
    nTop <- input$nTop
    nMom <- input$nMom
    nVol <- input$nVol
    trgtVol <- input$trgtVol / 100
    
    out <- list(nTop = nTop, nMom = nMom, nVol = nVol, trgtVol = trgtVol)
  })
  
  mom_models <- eventReactive(input$run_models, {
    
    prm <- parameter()
    
    ts <- timeSeries()
    
    mdls <- momentum.models(prices = ts$xts.ts, parameter = prm, selected = input$model_choices)
  })
  
  ## plotly version ##
  # suppress warning msg: Warning in origRenderFunc() Shiny doesn't use them
  # https://github.com/ropensci/plotly/issues/985
  
  output$etf_data <- renderPlotly({
    
    ts <- timeSeries()
    xl.data <- ts$xl.data
    
    date <- as.Date(xl.data[, "Date"])
    p <- plot_ly(height = 500, colors = "Dark2", source = "main")
    p$elementId <- NULL
    for( iETF in 2:ncol(xl.data) ){
      
      tmp <- xl.data[,sprintf("%s", names(xl.data)[iETF])]
      
      p <- add_trace(p, x = date, y = tmp, name = sprintf("%s", names(xl.data)[iETF]),
                     mode = "lines", type = "scatter", text = round(tmp, 2), 
                     hoverinfo = "x+text+name"
      )
    }
    
    x <- y <- list(title = "")
    p %>% layout(xaxis = x, yaxis = y)
    p
  })
  
  output$hist_mom <- renderPlotly({
    
    ts <- timeSeries()
    prices <- ts$xts.ts
    
    MOM <- prices / mlag(prices, input$nMom)
    
    eventdata <- event_data("plotly_hover", source = "main")
    
    
    validate(need(!is.null(eventdata), "Hover over price series chart"))
    
    if( !is.null(eventdata) ){
      
      # retrieve date
      datapoint <- eventdata$x 
      
      tmp <- (MOM[datapoint] - 1) * 100
      
      if( all(is.na(tmp)) ){
        tmp[1,] <- 0
      }
      
      x <- names(tmp)
      y <- c(round(coredata(tmp), 2))
      
      disp <- data.frame(x,y)
      
      p <- plot_ly()
      p$elementId <- NULL
      p <- add_trace(p, data = disp, x = ~x, y = ~y, text = y, textposition = 'auto', type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(
          xaxis = list(title = datapoint),
          yaxis = list(title = "Momentum")) 
    }
  })
  
  output$model_data <- renderPlotly({
    
    mdls <- mom_models()
    
    date <- index(mdls[[sprintf("%s", names(mdls)[1]) ]]$weight)
    p <- plot_ly(height = 500, colors = "Dark2")
    p$elementId <- NULL
    
    for( iMOD in 1:length(mdls) ){
      
      tmp <- unlist(as.data.frame(mdls[[sprintf("%s", names(mdls)[iMOD]) ]]$equity)) * 100
      
      p <- add_trace(p, x = date, y = tmp, name = sprintf("%s", names(mdls)[iMOD]),
                     mode = "lines", type = "scatter", text = round(tmp, 2), 
                     hoverinfo = "x+text+name"
      )
    }
    
    x <- y <- list(title = "")
    p %>% layout(xaxis = x, yaxis = y)
  })
  
  output$alloc_plch <- renderUI({
    
    selected_mdls <- input$model_choices
    
    if( !is.null(selected_mdls) ){
      list(
        radioButtons("single_mdl", label = NULL,
                     choices = as.list(selected_mdls), inline = TRUE) 
      )
    }
  })
  
  output$summary_tab <- renderTable({
    
    mdls <- mom_models()
    tbl <- plotbt.strategy.sidebyside(mdls, return.table = TRUE, make.plot = FALSE)
  }, hover = TRUE, rownames = TRUE) # , width = "100%"
  
  output$allocation <- renderPlotly({
    
    mdls <- mom_models()
    
    single_mdl <- req(input$single_mdl)
    
    out <- mdls[[sprintf("%s", single_mdl)]]$weight
    
    df <- out %>% data.frame()
    df$Date <- date <- index(out)
    
    p <- plot_ly(type = 'bar', colors = "Dark2")  
    p$elementId <- NULL
    for( iETF in names(df)[names(df) != "Date"] ){
      
      tmp <- df[,sprintf("%s", iETF)]
      
      p <- add_trace(p, x = date, y = tmp, name = iETF,
                     text = round(tmp, 2),
                     hoverinfo = "x+text+name"
      )
    }
    p %>% 
      layout(xaxis = list(title = ""), yaxis = list(title = "Allocation %"), barmode = 'stack', bargap = 0)
  })
} # END server

###--------------------------------------------------------------
# bye bye


