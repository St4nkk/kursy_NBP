pacman::p_load(dplyr, rio, rlang)
#' Zwraca ramkę danych zawierającą daty i wartości szeregu id pomiędzy datami d1 i d2.
#' Możliwe jest dodanie opóżnienienia lub przyspieszenia o zadaną wartość.
#'
#' 
#' @param d1 początkowa data YYYY-MM-DD
#' @param d2 końcowa data YYYY-MM-DD
#' @param id kod wlauty lub złota, może przyjmować jedną z wartości:
#' "XAU" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
#' "USD" "CHF" "SEK" "HUF" "GBP" "ITL" "XDR" "CYP" "HKD" "LTL" "LVL" "MTL" "ZAR" "RUB" "SKK" "SIT" "UAH" "BGN"
#' "RON" "THB" "NZD" "SGD" "ISK" "HRK" "TRY" "PHP" "MXN" "BRL" "MYR" "IDR" "KRW" "CNY" "ILS" "INR" "CLP"
#' @param mode "normal" - bez przesunięcia (domyślnie)
#'             "lag" - opóżnienie, 
#'             "lead" - przyspieszenie 
#' @param n liczba dni roboczych przesunięcia (domyślnie 0)
#' @return ramka danych (2 kolumny okeślające datę i wartości szeregu)
#' @examples
#' Dane("2020-01-01", "2020-01-10", "XAU")
#' Dane("2020-01-01", "2020-01-10", "XAU", "lead", 2)
#' Dane("2018-01-01", "2020-01-10", "EUR","lag", 30)
 
#Funkcja Dane zwraca ciąg wartości i dat szeregu id pomiędzy datami d1, d2 
Dane <- function(d1, d2, id, mode = "normal", n = 0){
  
  if (!(mode %in% c("normal", "lag", "lead"))) {
    print("Nieprawidłowa wartość parametru mode. Dostępne: lag, lead lub normal (domyślny)")
    return()
  }
  
  if (n < 0) {
    print("Wartość parametru n musi być nieujemna.")
    return()
  }
  
  df <- import("data/gold_and_exchange_rates.csv") 

  if (!(id %in% colnames(df)[-1])){
    print(paste("Nie udało się odczytać danych z bazy. Niepoprawne id:", id))
    return()
  }
  
  df <- df %>% select(date, !!sym(id)) %>%
        arrange(date)
    
  if (mode == "normal") {
    NULL
  } else if (mode == "lag"){
    df <- mutate(df, !!paste0(id, "_lag_", n) := lag(!!sym(id), n = n))
  } else if (mode == "lead"){
    df <- mutate(df, !!paste0(id, "_lead_", n) := lead(!!sym(id), n = n))
  } 

  df <- df %>% 
    filter(date >= d1 & date <= d2 & !is.na(!!sym(id))) %>%
    select(1, ncol(df))  #wybierz kolumnę pierwszą (data) i ostatnią (zwykły, opóźniony lub przyspieszony wektor) 
  if (nrow(df) == 0 ){
    df = NULL
  }
  return(df)
}

test_Dane <- function() {
  dane_gold <- Dane("2020-01-01", "2019-01-10", "data")
  dane_gold <- Dane("2000-01-01", "2002-01-10", "XAU")
  dane_gold_lag2 <- Dane("2020-01-01", "2020-01-10", "XAU", "lag", -2)
  dane_gold_lead2 <- Dane("2020-01-01", "2020-01-10", "XAU", "lead", 2)
  
  dane_eur <- Dane("2020-01-01", "2020-01-10", "EUR")
  dane_eur_2 <- Dane("2020-01-01", "2020-01-10", "EUR","lag", 2)
}

test_Dane()

#exchange_rates_df <- import("data/exchange_rates.csv")
#codes <- exchange_rates_df %>% distinct(currency_code)

#' Zwraca wartość korelacji dwóch szeregów id1 oraz id2 pomiędzy datami d1 i d2 oóżnionymi odpowiednio o lag1 i lag2
#'
#' id1 i id2 może przyjmować jedną z wartości:  
#' "XAU" "AUD" "ATS" "BEF" "CZK" "DKK" "EEK" "FIM" "FRF" "GRD" "ESP" "NLG" "IEP" "JPY" "CAD" "LUF" "NOK" "PTE" "EUR"
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
#' correl("2015-03-01", "2016-03-01", "XAU", "EUR", 30, 0)

correl <- function(d1, d2, id1, id2, lag1 = 0, lag2 = 0) {
  
  x <- Dane(d1, d2, id1, mode="lag", lag1) 
  y <- Dane(d1, d2, id2, mode="lag", lag2) 

  if (is.null(x)){
    print(paste("Wektor", id1, "ma wartość NULL"))
    return()
  } 
  if (is.null(y)) {
    print(paste("Wektor", id2, "ma wartość NULL"))
    return()
  }
  x <- x %>% pull(2)
  y <- y %>% pull(2)
  
  if (length(x) != length(y)){
    min_length <- min(length(x), length(y))
    x <- x[1:min_length]
    y <- y[1:min_length]
    print(paste0("Wektory są różnej długości! Dokonano zrównania do krótszego wektora (len = ", min_length, ")"))
  }
  
  korelacja <- cor(x, y, method = "pearson")
  return(korelacja)
}

test_correl <- function(){
  korel <- correl("2005-01-01", "2008-01-01", "EUR", "EURO")
  print(korel)
  print("---------------------------------------")
  korel <- correl("2005-01-01", "2008-01-01", "EUR", "USD")
  print(korel)
  print("---------------------------------------")
  korel <- correl("2002-06-01", "2008-08-01", "USD", "XAU")
  print(korel)
  print("---------------------------------------")
  korel <- correl("2002-06-01", "2008-06-01", "USD", "XAU1")
  print(korel)
}

#test_correl()

