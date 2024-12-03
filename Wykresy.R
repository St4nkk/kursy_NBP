pacman::p_load(ggplot2)
source("Funkcja_Dane_Korel.R")


new_plot <- function(d1, d2, id, lag = 0) {

  df <- Dane(d1, d2, id, lag)
  df <- mutate(df, code = id)
  
  p <- ggplot(df, aes(x=date, y=rate, color = code)) + 
    geom_line() + 
    labs(title="Kurs średni", x="data", y="kurs", color = "Waluta") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 

  return(p) 
}

add_to_plot <- function(p, d1, d2, id, lag = 0) {

  df <- Dane(d1, d2, id, lag)
  df <- mutate(df, code = id)
  
  p <- p + geom_line(data=df)
  #plot(p)
  return(p)
}

d1 <- "2010-01-01"
d2 <- "2010-03-01"
  
np <- new_plot(d1, d2, "EUR")
plot(np)
p <- add_to_plot(np, d1, d2, "USD", 30)
plot(p)
