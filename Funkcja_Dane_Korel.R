pacman::p_load(dplyr, rio, rlang)
#' Zwraca ramkę danych zawierającą daty i wartości szeregu id pomiędzy datami d1 i d2 z opóżnieniem lub przyspieszeniem
#'
#' id może przyjmować jedną z wartości:  
#' "gold_" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
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
#' Dane("2020-01-01", "2020-01-10", "gold_")
#' Dane("2020-01-01", "2020-01-10", "gold_", "lead", 2)
#' Dane("2018-01-01", "2020-01-10", "EUR","lag", 30)
 
#Funkcja Dane zwraca ciąg wartości i dat szeregu id pomiędzy datami d1, d2 
Dane <- function(d1, d2, id, mode = NULL, n = 0){
  
  df <- import("data/gold_and_exchange_rates.csv") %>%
        select(date, id) %>%
        arrange(date)
    
  if (is.null(mode)) {
    NULL
  } else if (mode == "lag"){
    df <- mutate(df, !!paste0(id, "_lag_", n) := lag(!!sym(id), n = n))
  } else if (mode == "lead"){
    df <- mutate(df, !!paste0(id, "_lead_", n) := lead(!!sym(id), n = n))
  } 

  df <- df %>% 
    filter(date >= d1 & date <= d2) %>%
    select(1, ncol(df))
      
  return(df)
}

dane_gold <- Dane("2020-01-01", "2020-01-10", "gold_")
dane_gold_lag2 <- Dane("2020-01-01", "2020-01-10", "gold_", "lag", 2)
dane_gold_lead2 <- Dane("2020-01-01", "2020-01-10", "gold_", "lead", 2)

dane_eur <- Dane("2020-01-01", "2020-01-10", "EUR")
dane_eur_2 <- Dane("2020-01-01", "2020-01-10", "EUR","lag", 2)

#exchange_rates_df <- import("data/exchange_rates.csv")
#codes <- exchange_rates_df %>% distinct(currency_code)

#' Zwraca wartość korelacji dwóch szeregów id1 oraz id2 pomiędzy datami d1 i d2 oóżnionymi odpowiednio o lag1 i lag2
#'
#' id1 i id2 może przyjmować jedną z wartości:  
#' "gold_" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
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
#' correl("2005-01-01", "2008-01-01", "EUR", "EUR", 0, 0)
#' correl("2015-03-01", "2016-03-01", "gold_", "EUR", 30, 0)

correl <- function(d1, d2, id1, id2, lag1 = 0, lag2 = 0) {

  x <- Dane(d1, d2, id1, mode="lag", lag1) %>% select(2) %>% pull()
  y <- Dane(d1, d2, id2, mode="lag", lag2) %>% select(2) %>% pull()

  if (length(x) != length(y)){
    min_length <- min(length(x), length(y))
    x <- x[1:min_length]
    y <- y[1:min_length]
  }
  
  korelacja <- cor(x, y, use = "all.obs", method = "pearson")
  return(korelacja)
}

test_correl <- function(){
  korel <- correl("2005-01-01", "2008-01-01", "EUR", "EUR")
  print(korel)
  korel <- correl("2005-01-01", "2008-01-01", "EUR", "USD")
  print(korel)  
  korel <- correl("2002-06-01", "2008-08-01", "USD", "gold_")
  print(korel)
}

test_correl()


