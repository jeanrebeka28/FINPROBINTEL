library(shiny)
library(ggplot2)
library(quantmod)
library(scales)
library(dplyr)
library(tidyr)
library(tibble)
library(TTR)
library(dygraphs)
library(tidyquant)
library(shinythemes)

# Define UI for application 
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tabsetPanel(
    id = 'main-tabs',
    tabPanel(
      id = 'landing-tab',
      title = 'Home',
      fluidRow(
      div(
        column(width=2),
        column(
          h1('Welcome!',
             style = "text-align:center;"),
          div(
          h4("Are you interested in exploring investing opportunities in the stock market?",
          style="text-align:center;line-height:1.6"),
          h4(strong("Then, this is the right place for you!"),
             style="text-align:center;line-height:1.6"),
          style="color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
          br(),
          h2("What you will find here"),
          column(
            h4("Analysis", style="text-align:center;line-height:1.6"),
            p("- In this tab, you will be able to explore 4 of the construction subsector companies in Indonesia."),
            p("- You can look at the value of each of their shares (which refer to a small fraction of the company) and how it was evolved through time."),
            p("- You will also be able to compute stock price averages, minimums, maximums, variances, and standard deviations from closing prices."),
            p("- Furthermore, you can download the data for further analysis."),
            style="color:black;background-color:lavender;padding:15px;border-radius:10px",
            width = 12
            ),
          width = 12
          )
        )
        )
      ),
      tabPanel(
        id = 'analysis-tab',
        title = 'Analysis',
        headerPanel('Data Analysis'),
        sidebarPanel(
          selectInput("ticker", "Select Stock Symbol:",
                      choices = c("WIKA.JK", "WSKT.JK", "ADHI.JK", "PTPP.JK"), selected = "WIKA.JK"),
          dateRangeInput("date", "Select Date Range:",
                         start = Sys.Date() - 365, end = Sys.Date()),
          selectInput("dateBreaks", "Date breaks: ",
                      choices = c("1 day", "1 week",
                                  "1 month", "1 year")),
          actionButton("button", "Compute", class = "btn-block"),
          br(),
          br(),
          selectInput("summaryType", "Select Summary Type:",
                      choices = c("Mean", "Minimum", "Maximum", "Variance", "Standard Deviation")),
          actionButton("calculateButton", "Calculate"),
          downloadButton("downloadData", "Download Stock Data")
          ),
    
    # Show plot and data
    mainPanel(
      fluidRow(
        column(12,
               plotOutput("candlePlot", brush = "plot_brush"))
      ),
      fluidRow(
        
      ),
      fluidRow(
        column(12,
               h4("Stock Data Table"),
               tableOutput("data"))
      )
    )
  )
)
)


# Define server logic 
server <- function(input, output, session) {
  
  
  stock <- eventReactive(input$button, {
    req(input$ticker, input$date)
    
    getSymbols(input$ticker, 
               from = input$date[1],
               to = input$date[2],
               auto.assign = FALSE) %>%
      na.locf() %>%
      as.data.frame() %>%
      `colnames<-`(c("open", "high", "low", "close", "volume", "adjusted")) %>%
      rownames_to_column(var = "date") %>%
      mutate(date = as.Date(date)) %>% 
      mutate(GreenRed=ifelse(open-close>0,
                             "Red",
                             "Green"))
  }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("stock_data_", input$ticker, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(stock(), file, row.names = FALSE)
    }
  )
  
  
  cp <- eventReactive(input$button, {
    req(input$ticker, input$date, input$dateBreaks)
    
    ggplot(stock()) +
      geom_segment(aes(x = date,
                       xend = date,
                       y = open,
                       yend = close,
                       colour = GreenRed),
                   size = 3) +
      geom_segment(aes(x = date,
                       xend = date,
                       y = high,
                       yend = low,
                       colour = GreenRed)) +
      geom_col(aes(x = date,
                   y = (((max(close) - min(close)) * ((volume - min(volume)) / (max(volume) - min(volume)))) + min(close))),
                       fill = "red", alpha = 0.1) +
      theme_bw() +
      scale_color_manual(values = c("Forest Green","Red")) +
      labs(title = paste0(input$ticker, ": from ", stock()$date[1], 
                          " to ", stock()$date[nrow(stock())]),
           subtitle  =  paste0("Close price: $", round(stock()$close[nrow(stock())], 2),
                               "\n",
                               "Close date: ", stock()$date[nrow(stock())])) +
      theme(legend.position ="none",
            axis.title.y = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title= element_text(hjust=0.5)) +
      scale_y_continuous(labels = scales::label_dollar(),
                         position = "right") +
      scale_x_date(date_breaks = input$dateBreaks, date_labels = "%Y-%m-%d") +
      coord_cartesian(ylim = c(min(stock()$close)*0.9, max(stock()$close))*1.05)
  })
  
  
  output$candlePlot <- renderPlot({
    cp()
  }, res = 96)
  
  
  output$data <- renderTable({
    stock() %>%
      select(-date)
  })
  
  # Calculate and display summary statistic
  observeEvent(input$calculateButton, {
    summary_stat <- switch(input$summaryType,
                           "Mean" = mean(stock()$close),
                           "Minimum" = min(stock()$close),
                           "Maximum" = max(stock()$close),
                           "Variance" = var(stock()$close),
                           "Standard Deviation" = sqrt(var(stock()$close)))
    
    showModal(modalDialog(
      title = paste("Summary -", input$summaryType),
      paste("The", input$summaryType, "closing price is:", round(summary_stat, 2))
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)