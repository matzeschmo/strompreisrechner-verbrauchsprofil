#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(stringr)
library(tsibble)
library(lubridate)
library(zoo)
library(ggplot2)
library(httr2)
library(glue)
library(jsonlite)
library(dplyr)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Strompreisrechner mit Verbrauchsprofil"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput(
            "csv_path",
            "Verbrauch Upload",
            multiple = FALSE,
            accept = NULL,
            width = NULL,
            buttonLabel = "Browse...",
            placeholder = "Keine Datei ausgewählt",
            capture = NULL
          ),
          dateRangeInput(
            "date_range",
            "Zeitraum filtern",
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 0,
            language = "de",
            separator = " bis ",
            width = NULL,
            autoclose = TRUE
          ),
          textOutput("overall_consumption"),
          textOutput("overall_payed"),
          textOutput("average_price"),
          selectInput( 
            "select_aggregate", 
            "Daten zusammenfassen:", 
            list("Stündlich" = "hourly", "Täglich" = "daily", "Wöchentlich" = "weekly") 
          ),
          textOutput("helptext"),
          a(href="sample.csv", "Beispieldatei herunterladen", download=NA, target="_blank")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          navset_pill(
            nav_panel(
              "Übersicht",
              plotOutput("marketDataPlot"),
              plotOutput("consumptionPricePlot")
            ),
            nav_panel(
              "Analyse",
              "Hier kommt ein fancy ding"
            ),
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$helptext <- renderText({
      "Hinweis: Um das Tool auszuprobieren kann einmal unten die Beispieldatei heruntergeladen und danach unter 'Verbrauch Upload' hochgeladen werden."
    }) 
  
    time_series_loaded <- reactive({
      req(input$csv_path)
      consumption_data <- read.csv(input$csv_path$datapath, sep=';', skip=1)
      consumption_data$Verbrauch.Gesamt...1.8.0 <- str_replace(consumption_data$Verbrauch.Gesamt...1.8.0, ",", ".")
      consumption_data$Verbrauch.Gesamt...1.8.0 <- as.numeric(consumption_data$Verbrauch.Gesamt...1.8.0)
      consumption_data[is.na(consumption_data)] <- 0
      
      consumption_data$Ablesezeitpunkt <- ymd_hms(consumption_data$Ablesezeitpunkt)
      
      df_consumption <- data.frame(timestamp = consumption_data$Ablesezeitpunkt, consumption = consumption_data$Verbrauch.Gesamt...1.8.0)
      df_hourly_data <- df_consumption %>%
        mutate(date_index = floor_date(timestamp, unit = "hour")) %>%
        group_by(date_index) %>%
        summarize(consumption = sum(consumption, na.rm = TRUE))  # Use any summary function here
      
      
      time_series <- as_tsibble(df_hourly_data)
      
      start_timestamp <- format(as.numeric(min(time_series$date_index)) * 1000, scientific = FALSE)
      end_timestamp <- format((as.numeric(max(time_series$date_index)) + 60 * 60) * 1000, scientific = FALSE)
      
      req_string <- glue("https://api.awattar.at/v1/marketdata?start={start_timestamp}&end={end_timestamp}")
      
      req <- request(req_string)
      resp <- req_perform(req)
      
      market_data_json <- resp |> resp_body_string()
      
      market_data <- as.data.frame(fromJSON(market_data_json))
      market_data$data.marketprice <- market_data$data.marketprice / 10 * 1.2
      market_data$data.start_timestamp <- as_datetime(market_data$data.start_timestamp / 1000)
      market_data_time_series <- as_tsibble(market_data[c("data.start_timestamp", "data.marketprice")])
      
      time_series$marketprice <- market_data$data.marketprice
      time_series$amount_payed <- time_series$marketprice * time_series$consumption
      time_series$aggregated_index <- time_series$date_index
      
      updateDateRangeInput(session, "date_range",
                           start = format_ISO8601(min(time_series$date_index)),
                           end = format_ISO8601(max(time_series$date_index)),
                           min = format_ISO8601(min(time_series$date_index)),
                           max = format_ISO8601(max(time_series$date_index)),
        )

      return(time_series)
      })

  
    time_series_filtered <- reactive({
      time_series_loaded() %>% filter_index(format_ISO8601(input$date_range[1]) ~ format_ISO8601(input$date_range[2]))
    })
    
    
    time_series_aggregated <- reactive({
      switch(input$select_aggregate, 
             hourly={
               return(time_series_filtered())
             },
             daily={
               time_series_filtered() %>% index_by(aggregated_index = ~as_date(.)) %>% summarise(
                 consumption = sum(consumption),
                 amount_payed = sum(amount_payed),
                 marketprice = mean(marketprice)
               )  
             },
             weekly={
               time_series_filtered() %>% index_by(aggregated_index = ~yearweek(.)) %>% summarise(
                 consumption = sum(consumption),
                 amount_payed = sum(amount_payed),
                 marketprice = mean(marketprice)
               )  
             },
             {
               return(time_series_filtered())
             }
        )
    })
    
    scaling <- reactive({
      ylim.prim <- c(min(time_series_aggregated()$marketprice), max(time_series_aggregated()$marketprice))   
      ylim.sec <- c(min(time_series_aggregated()$consumption), max(time_series_aggregated()$consumption))   
      b <- diff(ylim.prim)/diff(ylim.sec)
      a <- ylim.prim[1] - b*ylim.sec[1]
      return(c(a, b))
    })
    
    market_data_filtered <- reactive({
      filter_index(market_data_time_series, format_ISO8601(input$date_range[1]) ~ format_ISO8601(input$date_range[2]))
    })
    
    output$overall_consumption <- renderText({
      paste("Gesamtverbrauch", format(round(sum(time_series_filtered()$consumption), 2), nsmall = 2), "kWh")
    })
    
    output$overall_payed <- renderText({
      paste("Energiekosten", format(round(sum(time_series_filtered()$amount_payed) / 100, 2), nsmall = 2), "€")
    })
    
    output$average_price <- renderText({
      paste("Durchnittspreis", format(round(sum(time_series_filtered()$amount_payed) / sum(time_series_filtered()$consumption), 2), nsmall = 2), "Cent / kWh (Brutto ohne Tarifaufschlag)")
    })
    
    output$marketDataPlot <- renderPlot({
      ggplot(time_series_aggregated()) + 
        aes(x = aggregated_index) +
        geom_point(aes(y=marketprice,colour="Marktpreis (Cent / kWh)"), shape=1) +
        geom_line(aes(y=marketprice,colour="Marktpreis (Cent / kWh)")) +
        geom_point(aes(y = scaling()[1] + consumption*scaling()[2], colour="Verbrauch (kWh)"), shape=1) +
        geom_line(aes(y = scaling()[1] + consumption*scaling()[2], colour="Verbrauch (kWh)", width=1.1)) +
        scale_y_continuous("Marktpreis (Cent / kWh)", sec.axis = sec_axis(~ (. - scaling()[1])/scaling()[2], name = "Verbrauch (kWh)")) +
        theme_linedraw() + 
        theme(legend.position = "top") +
        labs(title = "Marktpreis und Verbrauchsprofil", x = "Datum / Uhrzeit")
    })
    
    output$consumptionPricePlot <- renderPlot({
      ggplot(time_series_aggregated()) + 
        aes(x = aggregated_index) +
        geom_point(aes(y=amount_payed)) +
        geom_line(aes(y=amount_payed)) +
        theme_linedraw() + 
        theme(legend.position = "top", plot.margin = margin(1,34,1,1, "pt")) +
        labs(title = "Effektiv bezahlter Betrag", y= "Bezahlter Betrag (Cent, Verbrauch * Marktpreis)", x = "Datum / Uhrzeit")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
