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
    geom_line() + 
    labs(title=title, x=x_label, y=y_label, color = "Szereg") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 

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
  print(head(df))
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

c <- rolling_correl("2010-01-01", "2010-03-01", "EUR", "USD", 30)
c2 <- rolling_correl("2010-01-01", "2010-03-01", "gold_", "EUR", 30)

p <- new_plot(c, title="Korelacja krocząca z 30 dni", y_label="wsp. korelacji" )
add_to_plot(p, c2)
  
#'Tworzy wykres gragu
#'
#'

graph_correlation <- function(d1, d2, ids){
  df <- import("data/gold_and_exchange_rates.csv")

  df <- df[ ,c("date", ids)] %>%
    filter(date > d1 & date < d2)
  
  cor_matrix <- cor(df[, ids], use = "complete.obs") #wyznaczenie maczierzy korelacji dla wszystkich zmiennych

  diag(cor_matrix) <- NA #Wartości na diagonali są równe 1 - nie są przydatne do wykresu
  cor_matrix[lower.tri(cor_matrix)] <- NA #wartości poniżej diagonali są takie same jak powyżej diagonali - są nadmiarowe
  cor_long <- melt(cor_matrix) # przekształcenie do formatu długiego
  cor_long <- cor_long[!is.na(cor_long[, 3]), ] #usunięcie wierszy o wartości NA

  g <- graph_from_data_frame(cor_long, directed = FALSE)
  
  E(g)$weight <- abs(E(g)$value)  # Grubość krawędzi będzie odpowiadała wartości korelacji

  # Stworzenie palety viridis
  colors <- turbo(100)
  # Mapowanie wag na kolory
  edge_colors <- colors[as.numeric(cut(c(E(g)$value, 1,-1), breaks = 100))]
  
  
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
  
  par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))
}

graph_correlation("2010-01-01", "2010-03-01", c("EUR", "USD", "gold_", "AUD"))
graph_correlation("2024-02-01", "2024-08-01", c("RON", "THB", "NZD", "SGD", "ISK", "TRY", "PHP", "MXN", "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"))

#----------------------------------------------

