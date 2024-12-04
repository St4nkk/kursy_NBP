pacman::p_load(ggplot2,dplyr,e1071)
source("Funkcja_Dane_Korel.R")


new_plot <- function(d1, d2, id, lag = 0) {

  df <- Dane(d1, d2, id, lag)
  df <- mutate(df, code = paste(id, "lag =", lag))
  
  p <- ggplot(df, aes(x=date, y=rate, color = code)) + 
    geom_line() + 
    labs(title="Kurs średni", x="data", y="kurs", color = "Waluta") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 

  return(p) 
}

add_to_plot <- function(p, d1, d2, id, lag = 0) {

  df <- Dane(d1, d2, id, lag)
  df <- mutate(df, code = paste(id, "lag =", lag))
  
  p <- p + geom_line(data=df)
  #plot(p)
  return(p)
}

d1 <- "2010-01-01"
d2 <- "2010-03-01"
  
np <- new_plot(d1, d2, "EUR")
plot(np)
p <- add_to_plot(np, d1, d2, "USD", 30)
plot

histogram <- function(d1, d2, id, lag = 0) {
  df <- Dane(d1, d2, id, lag)
  
  h <- ggplot(df, aes(x = rate)) + 
    geom_histogram(binwidth = 0.02, fill = "blue", color = "black", alpha = 0.7) +
    theme_bw() +
    labs(title = paste("Histogram", id,"od", first(df$date),"do",last(df$date)), x = "Kurs", y = "Częstotliwość")

  return(h)
}

h <- histogram(d1, d2, "EUR")
plot(h)

stats <- function(d1, d2, id, lag = 0) {
  data <- Dane(d1, d2, id, lag) %>% pull(rate)
  s <- list("min" = min(data), 
            "1st_Qu" = quantile(data, 0.25, names = FALSE),
            "median" = median(data),
            "mean" = mean(data),
            "3rd_Qu" = quantile(data, 0,75, names = FALSE),
            "max" = max(data),
            "std" = sd(data),
            "skew" = skewness(data),
            "kurt" = kurtosis(data))
  
  return (s)
}

(s <- stats(d1, d2, "EUR"))

data <- Dane(d1, d2, "EUR")  %>% pull(rate)

