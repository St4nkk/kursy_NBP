pacman::p_load(dplyr, rio)
#' Zwraca ramkę danych zawierającą daty i wartości szeregu id pomiędzy datami d1 i d2 z opóżnieniem lub przyspieszeniem
#'
#' id może przyjmować jedną z wartości:  
#' "gold" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
#' "USD" "CHF" "SEK" "HUF" "GBP" "ITL" "XDR" "CYP" "HKD" "LTL" "LVL" "MTL" "ZAR" "RUB" "SKK" "SIT" "UAH" "BGN"
#' "RON" "THB" "NZD" "SGD" "ISK" "HRK" "TRY" "PHP" "MXN" "BRL" "MYR" "IDR" "KRW" "CNY" "ILS" "INR" "CLP"
#' 
#' @param d1 początkowa data YYYY-MM-DD
#' @param d2 końcowa data YYYY-MM-DD
#' @param id kod wlauty lub złota
#' @param mode opóżnienie - "lag", przyspieszenie - "lead" (domyślnie NULL)
#' @param n liczba dni roboczych przesunięcia (domyślnie 0)
#' @return ramka danych (2 kolumny: date i rate)
#' @examples
#' Dane("2020-01-01", "2020-01-10", "gold")
#' Dane("2020-01-01", "2020-01-10", "gold", "lead", 2)
#' Dane("2018-01-01", "2020-01-10", "EUR","lag", 30)
 
#Funkcja Dane zwraca ciąg wartości i dat szeregu id pomiędzy datami d1, d2 
Dane <- function(d1, d2, id, mode = NULL, n = 0){
  if (id == "gold"){
    df <- import("data/gold_price.csv") %>% 
      arrange(date)
    
    if (is.null(mode)) {
      NULL
    } else if (mode == "lag"){
      df <- mutate(df, rate = lag(rate, n = n))
    } else if (mode == "lead"){
      df <- mutate(df, rate = lead(rate, n = n))
    } 

    gold_price <- df %>% 
      filter(date >= d1 & date <= d2) 
      
    
    return(gold_price)
  } else {
    df <- import("data/exchange_rates.csv")
    
      if (any(df$currency_code == id)) {
        df <- df %>% 
          filter(currency_code == id) %>%
          arrange(date) 
        
        if (is.null(mode)) {
          NULL
        } else if (mode == "lag"){
          df <- mutate(df, rate = lag(rate, n = n))
        } else if (mode == "lead"){
          df <- mutate(df, rate = lead(rate, n = n))
        }
        
        exchange_rates <- df %>% 
          filter(date >= d1 & date <= d2) 

        return(exchange_rates)
      }
    return(NULL)
  }
}

dane_gold <- Dane("2020-01-01", "2020-01-10", "gold")
dane_gold_lag2 <- Dane("2020-01-01", "2020-01-10", "gold", "lag", 2)
dane_gold_lead2 <- Dane("2020-01-01", "2020-01-10", "gold", "lead", 2)

dane_eur <- Dane("2020-01-01", "2020-01-10", "EUR")
dane_eur_2 <- Dane("2020-01-01", "2020-01-10", "EUR","lag", 2)

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

  x <- Dane(d1, d2, id1, lag1) %>% select(rate)
  y <- Dane(d1, d2, id2, lag2) %>% select(rate)
  
  if (length(x) != length(y)){
    min_length <- min(length(x), length(y))
    x <- x[1:min_length]
    y <- y[1:min_length]
  }
  
  korelacja <- cor(x, y, method = "pearson")
  return(korelacja)
}

test_Korel <- function(d1, d2, id1, id2, lag1, lag2){
  korel <- Korel(d1, d2, id1, id2, lag1, lag2)
  return(korel)
}

#test_Korel("2005-01-01", "2008-01-01", "EUR", "EUR", 0, 0)


