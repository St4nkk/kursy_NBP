pacman::p_load(dplyr, rio)
#' Zwraca ramkę danych zawierającą daty i wartości szeregu id pomiędzy datami d1 i d2
#'
#' id może przyjmować jedną z wartości:  
#' "gold" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
#' "USD" "CHF" "SEK" "HUF" "GBP" "ITL" "XDR" "CYP" "HKD" "LTL" "LVL" "MTL" "ZAR" "RUB" "SKK" "SIT" "UAH" "BGN"
#' "RON" "THB" "NZD" "SGD" "ISK" "HRK" "TRY" "PHP" "MXN" "BRL" "MYR" "IDR" "KRW" "CNY" "ILS" "INR" "CLP"
#' 
#' @param d1 początkowa data YYYY-MM-DD
#' @param d2 końcowa data YYYY-MM-DD
#' @param id kod wlauty lub złota
#' @return ramka danych 
#' @examples
#' Dane("2005-01-01", "2010-01-01", "EUR")
#' Dane("2015-01-01", "2024-01-01", "gold")
 
#Funkcja Dane zwraca ciąg wartości i dat szeregu id pomiędzy datami d1, d2 
Dane <- function(d1, d2, id){
  if (id == "gold"){
    gold_price_df <- import("data/gold_price.csv")
    gold_price <- gold_price_df %>% filter(date >= d1 & date <= d2)
    return(gold_price)
  } else {
    exchange_rates_df <- import("data/exchange_rates.csv")
    
      if (any(exchange_rates_df$currency_code == id)) {
        exchange_rates <- exchange_rates_df %>% filter(date >= d1 & date <= d2 & currency_code == id) %>% select(date, rate)
        return(exchange_rates)}
  }
}


#dane_EUR <- Dane("2005-01-01", "2010-01-01", "EUR")

#exchange_rates_df <- import("data/exchange_rates.csv")
#codes <- exchange_rates_df %>% distinct(currency_code)

#' Zwraca wartość korelacji dwóch szeregów id1 oraz id2 pomiędzy datami d1 i d2 oóżnionymi odpowiednio o lag1 i lag2
#'
#' id1 i id2 może przyjmować jedną z wartości:  
#' "gold" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
#' "USD" "CHF" "SEK" "HUF" "GBP" "ITL" "XDR" "CYP" "HKD" "LTL" "LVL" "MTL" "ZAR" "RUB" "SKK" "SIT" "UAH" "BGN"
#' "RON" "THB" "NZD" "SGD" "ISK" "HRK" "TRY" "PHP" "MXN" "BRL" "MYR" "IDR" "KRW" "CNY" "ILS" "INR" "CLP"
#' 
#' @param d1 początkowa data YYYY-MM-DD
#' @param d2 końcowa data YYYY-MM-DD
#' @param id1 kod wlauty lub złota 1
#' @param id2 kod wlauty lub złota 2
#' @param lag1 opóźnienie w dniach dla szeregu 1 
#' @param lag2 opóźnienie w dniach dla szeregu 2 
#' @return wartośc korelacji 
#' @examples
#' Korel("2005-01-01", "2008-01-01", "EUR", "EUR", 0, 0)
#' Korel("2015-03-01", "2016-03-01", "gold", "EUR", 30, 0)

Korel <- function(d1, d2, id1, id2, lag1, lag2) {
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  x <- Dane(d1-lag1, d2-lag1, id1)[, 2]
  y <- Dane(d1-lag2, d2-lag2, id2)[, 2]
  min_length <- min(length(x), length(y))
  x <- x[1:min_length]
  y <- y[1:min_length]
  korelacja <- cor(x, y, method = "pearson")
  return(korelacja)
}

test_Korel <- function(d1, d2, id1, id2, lag1, lag2){
  korel <- Korel(d1, d2, id1, id2, lag1, lag2)
  return(korel)
}

#test_Korel("2005-01-01", "2008-01-01", "EUR", "EUR", 0, 0)


