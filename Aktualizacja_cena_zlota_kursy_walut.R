#Aktualizacja plików gold_price.csv oraz exchange_rates.csv dla zadanego przedziału dat

# Załadowanie pakietów
pacman::p_load(httr, rio)
source("Pobieranie_cena_zlota.R")
source("Pobieranie_kursy_walut.R")

is_valid_date <- function(date_string) {
  # Próba konwersji na datę
  date <- try(as.Date(date_string), silent = TRUE)
  
  # Sprawdzamy, czy data jest prawidłowa
  if (inherits(date, "try-error")) {
    return(FALSE)  # Jeśli wystąpił błąd, zwróć FALSE
  } else {
    return(TRUE)   # Jeśli data jest poprawna, zwróć TRUE
  }
}



while(TRUE){
  start_date <- readline(prompt = "Podaj datę poczatkową aktualizacji w formacie YYYY-MM-DD: ")
  last_date <- readline(prompt = "Podaj datę końcową aktualizacji w formacie YYYY-MM-DD: ")
  if (is_valid_date(start_date) & (is_valid_date(last_date))){
    break
  } else{
    print("Daty są nieprawidłowe.")
  }
}
#start_date = "2024-11-28"
#last_date = "2024-12-01"

start_date <- as.Date(start_date)
last_date <- as.Date(last_date)

gold_price <- get_gold_data_from_NBP(start_date, last_date)
exchange_rates <- get_exchange_data_from_NBP(start_date, last_date)


gold_price_old <- import("data/gold_price.csv")
gold_price <- rbind(gold_price_old, gold_price)
gold_price <- unique(gold_price)
export(gold_price, "data/gold_price.csv")


exchange_rates_old <- import("data/exchange_rates.csv")
exchange_rates <- rbind(exchange_rates_old, exchange_rates)
exchange_rates <- unique(exchange_rates)
export(exchange_rates, "data/exchange_rates.csv")