# Load the `shiny` package (install it in the R terminal if you haven't already)
pacman::p_load(shiny,
               shinythemes,
               bslib,
               lubridate)

source("Wykresy.R")

# wektor z którego wybierane są waluty do wizualizacji
currencies_from_db <- code_to_currency %>% filter( Code != "PLN")
currency_choices <- paste(currencies_from_db$Code, "-", currencies_from_db$Currency)
currency_choices <- setNames(currencies_from_db$Code, currency_choices)

# zakres dat dostępnych do wyboru przez użytkownika 
gold_and_exchange_rates <- import("data/gold_and_exchange_rates.csv")
min_date <- min(gold_and_exchange_rates$date)
max_date <- max(gold_and_exchange_rates$date)
dates_disabled <- setdiff(seq.Date(from = min_date, to = max_date ,by = "day"), gold_and_exchange_rates$date )

# Elementy UI
page_front <- tabPanel(
  "Start",
  titlePanel("Projekt NBP"),
  h1("Analiza kursów walutowych i ceny złota publikowanych przez NBP."),
  
  p("Projekt obejmuje:",
    tags$ul(
      tags$li("prezentację postaci czasowej szeregów,"),
      tags$li("analizę korelacji wybranych szeregów,"),
      tags$li("wizualzację rozkładów w zadanym okresie w postaci histogramów"),
    ),
    "Dla szeregów czasowych mozliwe jest określenie opóźnienia bądź przyspieszenia",
    
    # img(src = "https://bi.im-g.pl/im/a5/c6/1d/z31220645IH,Waluty---zdjecie-ilustracyjne.jpg",
    #     width = 500
    #     )
  )
  
)
page_time_plot <- tabPanel(
  "Wykresy",
  page_sidebar(
    title = "Wykresy",
    theme = shinytheme("cosmo"),
    sidebar = sidebar(
      title = "Parametry wykresów",
      dateInput(inputId = "d1",
                label = "Data początkowa",
                min = min_date,
                max = max_date,
                datesdisabled = dates_disabled,
                value = max_date %m-% months(1)), #miesiąc przed ostatnią dostępną datą
      
      dateInput(inputId = "d2",
                label = "Data końcowa",
                min = min_date,
                max = max_date,
                datesdisabled = dates_disabled,
                value = max_date),
      
      checkboxGroupInput(inputId = "waluty",
                         label = "Waluta",
                         choices = currency_choices,
                         selected = "EUR")
    ),
    tabsetPanel(type = "tabs",
                  tabPanel("Wykres czasowy", plotOutput("time_plot")),
                  tabPanel("Histogram", plotOutput("hist_plot")),
                  tabPanel("Korelacja", plotOutput("heatmap"),
                           plotOutput("graph_corel"))
                           
      )
      
    
  )
)

page_analysis <- tabPanel(
  "Analiza",
  theme = shinytheme("cosmo")
)
page_forecast <- tabPanel(
  "Prognoza",
  theme = shinytheme("cosmo")
)

ui <- navbarPage(
  
  "Projekt NBP",
  theme = shinytheme("cosmo"),
  page_front,
  page_time_plot,
  page_analysis,
  page_forecast
    )



# This defines a server that doesn't do anything yet, but is needed to run the app.

server <- function(input, output) {
  output$hist_plot <- renderPlot({
    multi_hist(input$d1, input$d2, input$waluty)
  })
  
  output$time_plot <- renderPlot({

    multi_plot(input$d1, input$d2, input$waluty)
  })
  
  output$heatmap <- renderPlot({
    graph_correlation(input$d1, input$d2, input$waluty)[["heatmap"]] 
  })
  
  output$graph_corel <- renderPlot({
    graph_correlation(input$d1, input$d2, input$waluty)[["graph"]]
  })

}

# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)



