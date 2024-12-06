#Aktualizacja plików gold_price.csv oraz exchange_rates.csv dla zadanego przedziału dat

# Załadowanie pakietów
pacman::p_load(httr, rio, lubridate, dplyr)
source("Pobieranie_cena_zlota.R")
source("Pobieranie_kursy_walut.R")

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


gold_price_old <- import("data/gold_price.csv") %>%  mutate(date = as.character(date))
gold_price_new <- bind_rows(gold_price_old, gold_price) %>%
                  distinct(date, .keep_all = TRUE)
export(gold_price_new, "data/gold_price.csv")


exchange_rates_old <- import("data/exchange_rates.csv") %>%  mutate(date = as.character(date))
exchange_rates_new <- bind_rows(exchange_rates_old, exchange_rates) %>%
                      distinct(date, .keep_all = TRUE)
export(exchange_rates_new, "data/exchange_rates.csv")


gold_and_exchange_rates <- full_join(gold_price_new, exchange_rates_new, by = "date") %>%
                           arrange(date)

export(gold_and_exchange_rates, "data/gold_and_exchange_rates.csv")
