pacman::p_load(dplyr, rio)
#Funkcja Dane zwraca ciąg wartości i dat szeregu id pomiędzy datami d1, d2 
# id może przyjmować jedną z wartości:  
# "gold" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
# "USD" "CHF" "SEK" "HUF" "GBP" "ITL" "XDR" "CYP" "HKD" "LTL" "LVL" "MTL" "ZAR" "RUB" "SKK" "SIT" "UAH" "BGN"
# "RON" "THB" "NZD" "SGD" "ISK" "HRK" "TRY" "PHP" "MXN" "BRL" "MYR" "IDR" "KRW" "CNY" "ILS" "INR" "CLP"
#
# Przykład użycia
# dane_EUR <- Dane("2005-01-01", "2010-01-01", "EUR")

Dane <- function(d1, d2, id){
  if (id == "gold"){
    gold_price_df <- import("data/gold_price.csv")
    gold_price <- gold_price_df %>% filter(date >= d1 & date <= d2)
    return(gold_price)
  } else {
    exchange_rates_df <- import("data/exchange_rates.csv")
    
      if (any(exchange_rates_df$currency_code == id)) {
        print("OK")
        exchange_rates <- exchange_rates_df %>% filter(date >= d1 & date <= d2 & currency_code == id) %>% select(date, rate)
        return(exchange_rates)}
  }
}

dane_EUR <- Dane("2005-01-01", "2010-01-01", "EUR")
plot(dane_EUR)

dane_zloto <- Dane("2015-01-01", "2024-01-01", "gold")
plot(dane_zloto)

exchange_rates_df <- import("data/exchange_rates.csv")
codes <- exchange_rates_df %>% distinct(currency_code)


