# Load the `shiny` package (install it in the R terminal if you haven't already)
pacman::p_load(shiny,
               lubridate)

source("Wykresy.R")

#
currencies_from_db <- code_to_currency %>% filter( Code != "PLN")
currency_choices <- paste(currencies_from_db$Code, "-", currencies_from_db$Currency)
currency_choices <- setNames(currencies_from_db$Code, currency_choices)

gold_and_exchange_rates <- import("data/gold_and_exchange_rates.csv")
min_date <- min(gold_and_exchange_rates$date)
max_date <- Sys.Date()
# Define a new `ui` variable. This variable should be assigned a `fluidPage()`
# layout. The `fluidPage()` layout should be passed the following:
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
  "wykresy czasowe",
  titlePanel("Wykresy czasowe"),
  sidebarLayout(
    sidebarPanel(

      dateInput(inputId = "d1",
                label = "Data początkowa",
                min = min_date,
                max = max_date,
                value = Sys.Date() %m-% months(1)), #miesiąc przed dzisiejszą datą
      dateInput(inputId = "d2",
                label = "Data końcowa",
                min = min_date,
                max = max_date,
                value = Sys.Date()),
      
      checkboxGroupInput(inputId = "waluty",
                         label = "Waluta",
                         choices = currency_choices,
                         selected = "EUR")
    ),
    mainPanel(
      plotOutput("static_plot")
    )
  )
)

page_histogram <- tabPanel(
  "Histogramy"
)
page_correlation <- tabPanel(
  "Korelacje"
)

ui <- navbarPage(
  "Projekt NBP",
  page_front,
  page_time_plot,
  page_histogram,
  page_correlation
)


# This defines a server that doesn't do anything yet, but is needed to run the app.
server <- function(input, output) {
  output$static_plot <- renderPlot({

    multi_plot(input$d1, input$d2, input$waluty)
  })
}

# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)



