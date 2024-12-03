#Aktualizacja plików gold_price.csv oraz exchange_rates.csv dla zadanego przedziału dat

# Załadowanie pakietów
pacman::p_load(httr, rio, lubridate, dplyr)
source("Funkcje.R")

while(TRUE){
  start_date <- readline(prompt = "Podaj datę poczatkową aktualizacji w formacie YYYY-MM-DD: ") %>% 
                ymd()
  last_date <- readline(prompt = "Podaj datę końcową aktualizacji w formacie YYYY-MM-DD: ") %>% 
               ymd()

  if (is.na(start_date) | is.na(last_date)){
    print("Wprowadzono nieprawidłowy format daty.\n")
  } else{
      break
  }
}


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