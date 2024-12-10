pacman::p_load(ggplot2,dplyr,e1071, zoo, igraph, reshape2, viridis)
source("Funkcja_Dane_Korel.R")

#' Tworzy i zwraca nowy obiekt gg zawierający liniowy wykres ramki danych o 2 kolumnach
#' 
#' @param df ramka danych 2 kolumny (w pierwszaj daty, w drugiej wartości)
#' @param title tytuł wykresu (domyśnie: "Kurs średni")
#' @param X_label etykieta osi x (domyślnie: "data")
#' @param y_label etykieta osi y (domyślnie: "cena w PLN"
#' 
#' @return obiekt gg z wykresem
#' @example 
#' data1 = Dane("2010-01-01", "2010-03-01", "EUR")
#' np <- new_plot(data1)
#' plot(np)

#
new_plot <- function(df, title="Kurs średni", x_label="data", y_label="cena w PLN") {

  df <- mutate(df, label = colnames(df)[2])
  colnames(df)[2] <- "rate"
  p <- ggplot(df, aes(x=date, y=rate, color = label)) + 
    geom_line(linewidth = 1) + 
    labs(title=title, x=x_label, y=y_label, color = "Szereg") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(
      breaks = scales::pretty_breaks(n = 10),
      labels = scales::label_date("%Y-%m-%d")
    )
    

  return(p) 
}

#' Dodaje nowy wykres do istniejącego 
#' 
#' @param p obiekt gg, istniejący wykres
#' @param df ramka danych 2 kolumny (w pierwszaj daty, w drugiej wartości)
#' 
#' @return obiekt gg z dodanym wykresem
#' @example 
#' np <- new_plot(data1)
#' data2 = Dane("2010-01-01", "2010-03-01", "USD", "lag", 30)
#' p <- add_to_plot(np, data2)
#' plot(p)
#' 

add_to_plot <- function(p, df) {

  df <- mutate(df, label = colnames(df)[2])
  colnames(df)[2] <- "rate"
  
  p <- p + geom_line(data=df, linewidth=1)

  return(p)
}

d1 <- "2010-01-01"
d2 <- "2015-03-01"
data1 = Dane(d1, d2, "EUR")
np <- new_plot(data1)
plot(np)
data2 = Dane(d1, d2, "USD", "lag", 30)
p <- add_to_plot(np, data2)
data3 = Dane(d1, d2, "AUD", "lag", 30)
p <- add_to_plot(p, data3)
plot(p)

multi_plot <- function(d1, d2, ids) {
  data1 <- Dane(d1, d2, ids[1])
  p <- new_plot(data1)
  
  for (id in ids[2:length(ids)]) {
    data <- Dane(d1, d2, id)
    p <- add_to_plot(p, data)
  }
  return(p)
}

mp <- multi_plot("2002-02-01", 
                 "2024-08-01", 
                c("RON", "THB", "NZD", "SGD", "ISK", "TRY", "PHP", "MXN", 
                "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"))
plot(mp)

#' Tworzy obiekt gg wyznaczający histogram danych
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
  bin_num = 50
  h <- ggplot(df, aes(x = !!sym(colnames(df)[2]))) + 
    geom_histogram(bins = bin_num, fill = "blue", color = "black", alpha = 0.7) +
    theme_bw() +
    labs(title = paste("Histogram", colnames(df)[2],"od", df$date[1], "do", df$date[length(df$date)]), x = "Kurs [PLN]", y = "Liczba wystąpień") + 
    scale_x_continuous(
      breaks = round(seq(floor(min(df[, 2])), ceiling(max(df[, 2])), length.out = bin_num), 2),  # Ustalamy ticksy
      labels = round(seq(floor(min(df[, 2])), ceiling(max(df[, 2])), length.out = bin_num), 2))   # Etykiety dla ticków
  
  return(h)
}

data1 = Dane(d1, d2, "TRY")
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

  df <- df %>% 
    slice(idx_d0:idx_d2) 
  
  df_cor <- mutate(df, correlation := rollapply(
      data = df[, c(id1, id2)], 
      width = window_size, 
      FUN = function(z) cor(z[, 1], z[, 2]), 
      by.column = FALSE, 
      align = "right", 
      fill = NA)) %>%
    filter(date > d1 & date < d2) %>%
    select(date, correlation)
  colnames(df_cor)[2] <- paste0(id1,"-", id2)
  return(df_cor)
}

window_size = 30
d1 <- "2010-01-01"
d2 <- "2010-03-01"
c <- rolling_correl(d1, d2, "EUR", "USD", window_size)
c2 <- rolling_correl(d1, d2, "XAU", "EUR", window_size)

p <- new_plot(c, title=paste("Korelacja krocząca z ostatnich", window_size, "dni\nOd", d1, "do", d2), y_label="wsp. korelacji" )
plot(p)
add_to_plot(p, c2)
  
#' Tworzy wykres grafu korelacji między wybranymi szeregami. Wierzchołki 
#' odpowiadają wybranym szeregom zaś krawędzie określają korelację miedzy nimi.
#' Kolor krawędzi zależy od wartości korelacji (od niebieskiego (korelacja -1) 
#' przez zielony (korelacja 0) do czerwonego (korelacja 1). 
#' Szerokość krawedzi zależy od wartości absolutnej korelacji.
#'
#' @param d1 data początkowa w formacie YYYY-MM-DD
#' @param d2 data końcowa w formacie YYYY-MM-DD
#' @param ids wektor z identyfikatorami szeregów
#' @example 
#' graph_correlation("2010-01-01", "2010-03-01", c("EUR", "USD", "XAU", "AUD"))
#' graph_correlation("2024-02-01", "2024-08-01", c("RON", "THB", "NZD", "SGD", "ISK", "TRY", "PHP", "MXN", "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"))

graph_correlation <- function(d1, d2, ids){
  df <- import("data/gold_and_exchange_rates.csv")
  print(head(df))
  df <- df[ ,c("date", ids)] %>%
    filter(date > d1 & date < d2)
  
  cor_matrix <- cor(df[, ids], use = "complete.obs") #wyznaczenie maczierzy korelacji dla wszystkich zmiennych

  diag(cor_matrix) <- NA #Wartości na diagonali są równe 1 - nie są przydatne do wykresu
  cor_matrix[lower.tri(cor_matrix)] <- NA #wartości poniżej diagonali są takie same jak powyżej diagonali - są nadmiarowe
  cor_long <- melt(cor_matrix) # przekształcenie do formatu długiego
  cor_long <- cor_long[!is.na(cor_long[, 3]), ] #usunięcie wierszy o wartości NA

  g <- graph_from_data_frame(cor_long, directed = FALSE)
  
  E(g)$weight <- abs(E(g)$value)  # Grubość krawędzi będzie odpowiadała wartości korelacji

  # Stworzenie palety kolorów turbo
  colors <- turbo(100)
  # Mapowanie wartości krawędzi na kolory
  edge_colors <- colors[as.numeric(cut(c(E(g)$value, 1,-1), breaks = 100))]
  
  # ustawienie rozłożenia grafu i słupka kolorów
  par(mfcol = c(1, 2), mar = c(1, 1, 1, 2))
  layout(matrix(c(1, 2), 1, 2, byrow = TRUE),
         widths=c(15,2), heights=c(15, 15))
  layout_fruchterman <- layout_with_fr(g)
  
  #rysowanie grafu
  plot(g, 
       layout = layout_fruchterman,
       edge.width = E(g)$weight * 10,  # Skalowanie grubości krawędzi
       edge.color = edge_colors, #kolor krawędzi
       vertex.size = 20,  # Rozmiar wierzchołków
       vertex.label.cex = 1,  # Rozmiar etykiet wierzchołków
       vertex.color = "grey",  # Kolor wierzchołków
       vertex.label.color = "black",
       vertex.label.family = "TT Arial",
       vertex.label.font = 2, #bold font 
       main = "Graf korelacji między szeregami czasowymi"
  )  

  # Rysowanie słupka kolorów po prawej stronie
  image(1, seq(-1, 1, length.out = 100), matrix(1:100,nrow=1), col = colors, axes = FALSE, ylab = "", xlab = "")
  axis(2)
  #resetowanie ustawień
  par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))
}

graph_correlation("2010-01-01", "2010-03-01", c("EUR", "USD", "XAU", "AUD"))
graph_correlation("2024-02-01", "2024-08-01", c("EUR", "AUD","XAU","CAD", "USD", "NOK", "SEK", "RON", "THB", "NZD", "SGD", "ISK", "TRY", "PHP", "MXN", "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"))


graph_correlation("2010-01-01", "2010-03-01", c("USD", "NOK", "SEK"))
d <- Dane("2010-01-01", "2010-03-01", "NOK")

(colnames(df))

