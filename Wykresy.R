pacman::p_load(ggplot2,dplyr,e1071, zoo)
source("Funkcja_Dane_Korel.R")


new_plot <- function(df) {

  df <- mutate(df, label = colnames(df)[2])
  colnames(df)[2] <- "rate"
  p <- ggplot(df, aes(x=date, y=rate, color = label)) + 
    geom_line() + 
    labs(title="Kurs średni", x="data", y="kurs", color = "Waluta") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 

  return(p) 
}

add_to_plot <- function(p, df) {

  df <- mutate(df, label = colnames(df)[2])
  colnames(df)[2] <- "rate"
  
  p <- p + geom_line(data=df)

  return(p)
}

d1 <- "2010-01-01"
d2 <- "2010-03-01"
data1 = Dane(d1, d2, "EUR")
np <- new_plot(data1)
plot(np)
data2 = Dane(d1, d2, "USD", "lag", 30)
p <- add_to_plot(np, data2)
plot(p)
data3 = Dane(d1, d2, "AUD", "lag", 30)
p <- add_to_plot(p, data3)
plot(p)

#' Tworzy obiekt ggplot wyznaczający histogram danych
#' 
#' @param df ramka danych zawierająca kolumnę rate
#' 
#' @return obiekt ggplot
#' @examples 
#' data1 = Dane("2010-01-01", "2010-03-01", "EUR")
#' h <- histogram(data1)
#' plot(h)
#' 

histogram <- function(df) {
  
  h <- ggplot(df, aes(x = !!sym(colnames(df)[2]))) + 
    geom_histogram(binwidth = 0.02, fill = "blue", color = "black", alpha = 0.7) +
    theme_bw() +
    labs(title = paste("Histogram", colnames(df)[2]), x = "Kurs", y = "Częstotliwość")

  return(h)
}

data1 = Dane(d1, d2, "EUR")
h <- histogram(data1)
plot(h)

#' Wyznacza statystyki opisowe wektora 
#' Statystyki: minimum (min)
#'            pierwszy kwartyl (Qu_1)
#'            mediana (median)
#'            średnia (mean)
#'            trzeci kwartyl (Qu_3)
#'            maksimum (max)
#'            odchylenie standardowe (std)
#'            skośność (skew)
#'            kurtoza (kurt)
#'            
#' @return lista statystyk 
#' @examples 
#' data <- Dane("2010-01-01", "2010-03-01", "gold")
#' stats(data)
#' 
stats <- function(df) {
  data <- df %>% pull(2)
  s <- list("min" = min(data), 
            "Qu_1" = quantile(data, 0.25, names = FALSE),
            "median" = median(data),
            "mean" = mean(data),
            "Qu_3" = quantile(data, 0,75, names = FALSE),
            "max" = max(data),
            "std" = sd(data),
            "skew" = skewness(data),
            "kurt" = kurtosis(data))

  return (s)
}

(s <- stats(data1))

#' Rysuje wykres korelacji kroczącej 2 szeregów. Dla każdego dnia roboczego z 
#' zadanego zakresu wyliczana jest korelacja szeregów na podstawie 'window_size' 
#' poprzednich dni. 
#' 
#' @param d1 początkowa data YYYY-MM-DD
#' @param d2 końcowa data YYYY-MM-DD
#' @param id1 kod 1 szeregu
#' @param id2 kod 2 szeregu
#' @param window_size rozmiar okna analizy
#' @return ramka danych składająca się z 2 kolumn (date, cor_val) 
#' 
rolling_correl <- function(d1, d2, id1, id2, window_size){
  
  df <- import("data/gold_and_exchange_rates.csv") %>%
    select(date, id1, id2) %>%
    arrange(date)
  
  idx_d1 <- which(df$date >= d1) %>% min() # rzeczywista data poczatkawa wykresu
  idx_d2 <- which(df$date <= d2) %>% max() 
  idx_d0 <- idx_d1 - window_size + 1 # pierwsza data potrzebna do wyznaczenia korelacji dla d1
  print(idx_d0)
  df_cor <- df %>% 
    slice(idx_d0:idx_d2) %>% 
    select(date, id1, id2) %>%
    mutate(correlation = rollapply(
      data = df_cor[, c(id1, id2)], 
      width = window_size, 
      FUN = function(z) cor(z[, 1], z[, 2]), 
      by.column = FALSE, 
      align = "right", 
      fill = NA)) %>%
    filter(date > d1 & date < d2) %>%
    select(date, correlation)
  
  return(df_cor)
}

c <- rolling_correl("2010-01-01", "2010-03-01", "EUR", "USD", 30)

plot(c)

  
