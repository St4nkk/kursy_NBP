pacman::p_load(ggplot2)
source("Funkcja_Dane_Korel.R")


new_plot <- function(d1, d2, id, lag) {
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  df <- Dane(d1-lag,d2-lag,id)
  df <- mutate(df, code = id)
  
  p <- ggplot(df, aes(x=date, y=rate, color = code)) + 
    geom_line() + 
    labs(title="Kurs średni", x="data", y="kurs", color = "Waluta") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 

  return(p) 
}

add_to_plot <- function(p, d1, d2, id, lag) {
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  df <- Dane(d1-lag,d2-lag,id)
  df <- mutate(df, code = id)
  
  p <- p + geom_line(data=df)
  #plot(p)
  return(p)
}

np <- new_plot("2010-01-01", "2010-03-01", "EUR", 0)
plot(np)
p <- add_to_plot(np, "2010-01-01", "2010-03-01", "USD", 30)
plot(p)
