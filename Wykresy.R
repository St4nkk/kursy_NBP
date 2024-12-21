pacman::p_load(ggplot2,
               dplyr,
               e1071,           #pakiet statysyczny 
               zoo,
               igraph,
               reshape2,
               viridis,         #palety kolorów    
               purrr,
               directlabels,    #etykiety na wykresach
               gridExtra,       #rysowanie układu wilu wykresów
               plotly,
               ggraph)        

source("Funkcja_Dane_Korel.R")  #Funkcja Dane

scaling_rates <- import("data/scaling_rates.csv")  #pobieranie wsp. skalujacych do wykresów
code_to_currency <- import("data/code_to_currency.csv")  #ramka do zamiany kodu nazwę waluty

#' Zwraca pełna nazwę waluty dla danego kodu
#' 
#' @param id kod waluty np."EUR"
#' @return nazwa waluty
#' @example 
#' full_currency_name("EUR")
#' 
full_currency_name <- function(ids){
  
  currency_names <- code_to_currency$Currency[match(ids, code_to_currency$Code)]
  return(currency_names)
}

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


new_plot <- function(df, 
                     scaled=TRUE, 
                     title="Kurs średni", 
                     x_label="data", 
                     y_label="cena w PLN",
                     line_labs=TRUE) {
  
  id_with_desc <- colnames(df)[2] 
  
  id <- strsplit(id_with_desc, "_")[[1]][[1]]
  scaler <- ifelse(scaled, scaling_rates[,id], 1)
  label <- ifelse(scaler == 1, id_with_desc, paste(scaler, id_with_desc))
  
  currency_name <- full_currency_name(id)
  if(length(currency_name) != 0){
    label <- paste(label, "-", full_currency_name(id))
  }
   
  df <- df %>%
    mutate(df, label = label, rate = !!sym(id_with_desc) * scaler) %>%
    select(date, label, rate)

  #colnames(df)[2] <- "rate"
  p <- ggplot(df, aes(x=date, y=rate, color = label)) + 
    geom_line(linewidth = 1) + 
    labs(title=paste(title,"\nod",df$date[1], "do", df$date[length(df$date)]) , x=x_label, y=y_label, color = "Szereg") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(
      breaks = scales::pretty_breaks(n = 10),
      labels = scales::label_date("%Y-%m-%d")) +
    scale_y_continuous(
      breaks = scales::breaks_extended(10))
  
  if(line_labs) {
    p <- p +
      geom_dl(aes(label = id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
      geom_dl(aes(label = id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8))
  }
  
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


add_to_plot <- function(p, 
                        df, 
                        scaled=TRUE,
                        line_labs=TRUE) {
  
  id_with_desc <- colnames(df)[2] 
  id <- strsplit(id_with_desc, "_")[[1]][[1]]
  scaler <- ifelse(scaled, scaling_rates[,id], 1)
  label <- ifelse(scaler == 1, id_with_desc, paste(scaler, id_with_desc))
  
  currency_name <- full_currency_name(id)
  if(length(currency_name) != 0){
    label <- paste(label, "-", full_currency_name(id))
  }
  
  df <- df %>%
    mutate(df, label = label, rate = !!sym(id_with_desc) * scaler) %>%
    select(date, label, rate)

  p <- p + geom_line(data=df, linewidth=1) 
  
  if (line_labs) {
    p <- p +
      geom_dl(data = df, aes(label = id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
      geom_dl(data = df, aes(label = id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8))
  }
  
  return(p)
}

#------------------testowanie-------------------
# d1 <- "2010-01-01"
# d2 <- "2015-03-01"
# data1 = Dane(d1, d2, "EUR")
# np <- new_plot(data1)
# plot(np)
# data2 = Dane(d1, d2, "USD", "lag", 30)
# p <- add_to_plot(np, data2)
# data3 = Dane(d1, d2, "AUD", "lag", 30)
# p <- add_to_plot(p, data3)
# plot(p)
#--------------------------------------
interactive_plot <- function(gg_plot) {
  ggplotly(gg_plot)
}

#interactive_plot(p)

multi_plot <- function(d1,
                       d2,
                       ids,
                       scaled=TRUE) {
  
  data1 <- Dane(d1, d2, ids[1])
  p <- new_plot(data1, scaled)
  if(length(ids) > 1) {
    for (id in ids[2:length(ids)]) {
      data <- Dane(d1, d2, id)
      p <- add_to_plot(p, data, scaled)
    }
  }
  return(p)
}
#----------------------------test----------------------
# mp <- multi_plot("2008-02-01", 
#                  "2024-08-01", 
#                 c("RON", "THB", "NZD", "SGD", "ISK", "TRY", "PHP", "MXN", 
#                 "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"))
# plot(mp)
# 
# interactive_plot(mp)
#--------------------------------------------------------------


#' zwraca interaktywny wykres z wieloma osiami y po jednej dla jażdej waluty
#' Maksymalna liczba walut to 6.
#'  
#' @param data_list lista ramek danych, każda o 2 kolumnach (w pierwszej data w drugiej dane)
#' @return obiekt plotly zawierający wykres danych wejściowych
#'
#' @example 
#' d1 <- "2018-01-01"
#' d2 <- "2023-01-01"
#' test_data <- list(Dane(d1, d2, "EUR"), Dane(d1, d2, "XAU"),Dane(d1, d2, "SEK"), Dane(d1, d2, "CHF"), Dane(d1, d2, "ISK"))
#' plot_multiple_axes(test_data)
#'       
plot_multiple_axes <- function(data_list) {
  if (length(data_list) > 6) {
    stop("Maksymalna liczba walut wynosi 6.")
  }
  
  left_edge_plot <- 0.08
  right_edge_plot <- 0.88
  colors <- c(
    "#1f77b4",  # Blue
    "#ff7f0e",  # Orange
    "#2ca02c",  # Green
    "#d62728",  # Red
    #"#9467bd",  # Purple
    "#8c564b",  # Brown
    "#e377c2"  # Pink
    #"#7f7f7f"   # Gray
  )
  fig <- plot_ly()
  id <- colnames(data_list[[1]])[2] #kod waluty
  fig <- fig %>% add_trace(
                           x = data_list[[1]][,1], # data
                           y = data_list[[1]][,2], # wartości
                           name = paste(id, "-", full_currency_name(id)), 
                           mode = "lines",
                           type = "scatter",
                           line = list(color = colors[1]))
  y_axes <- list()
  
  for (i in 2:length(data_list)) {
   id <- colnames(data_list[[i]])[2] #kod waluty
   fig <- fig %>% add_trace(
     x = data_list[[i]][,1],
     y = data_list[[i]][,2],
     name = paste(id, "-", full_currency_name(id)),
     yaxis = paste0("y", i),
     mode = "lines",
     type = "scatter",
     line = list(color = colors[i]))
   
   y_axes[[paste0("yaxis", i)]] <- list(
     tickfont = list(color = colors[[i]]),
     titlefont = list(color = colors[[i]]),
     tickformat = ".3",
     overlaying = "y",
     side = ifelse(i %% 2 == 0, "right", "left"),
     title = "")
   
   if (i != 2) { #tylko druga oś umiejscowiona przy prawej krawędzi wykresu
     y_axes[[paste0("yaxis", i)]][["anchor"]] <- "free"
     y_axes[[paste0("yaxis", i)]][["position"]] <- ifelse(i %% 2 == 0, right_edge_plot+(i/2-1)*0.04, left_edge_plot-(i-1)/2*0.04)
   }


  }
  args <- c(list(p = fig, 
               title = "Wykres kursów",
               xaxis = list(title = '', domain = c(left_edge_plot, right_edge_plot)),
               yaxis = list(title="",#"yaxis title",
                            tickfont = list(color = "#1f77b4"),
                            titlefont = list(color = "#1f77b4"))),
               y_axes)


  fig <- do.call(layout, args)%>%
    layout(plot_bgcolor='#e5ecf6',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff')
    )
  
  fig
  
}

d1 <- "2018-01-01"
d2 <- "2023-01-01"
test_data <- list(Dane(d1, d2, "EUR"), Dane(d1, d2, "XAU"),Dane(d1, d2, "IDR"),Dane(d1, d2, "SEK"), Dane(d1, d2, "CHF"), Dane(d1, d2, "ISK"))
fplot_multiple_axes(test_data)

f




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
  id <- strsplit(colnames(df)[2], "_")[[1]][[1]]

  h <- ggplot(df, aes(x = !!sym(colnames(df)[2]))) + 
    geom_histogram(bins = bin_num, fill = "blue", color = "black", alpha = 0.7) +
    theme_bw() +
    labs(title = paste("Histogram", colnames(df)[2], "-" , full_currency_name(id), 
                       "\nod", df$date[1], "do", df$date[length(df$date)]), 
         x = "Kurs [PLN]", y = "Liczba wystąpień") + 
    scale_x_continuous(
      breaks = scales::breaks_extended(bin_num/4))
      
  return(h)
}

#---------------------------------------------
# d1 <- "2002-01-01"
# d2 <- "2024-01-01"
# data1 = Dane(d1, d2, "TRY")
# h <- histogram(data1)
# plot(h)
# 
# ids <- c("EUR", "USD", "XAU", "RUB", "CNY", "ILS")
multi_hist <- function(d1, d2, ids){
  plot_list <- lapply(ids, function(id) {
    histogram(Dane(d1,d2,id))})
  grid.arrange(grobs = plot_list)
}
# plot_list <- lapply(ids, function(id) {
#   histogram(Dane(d1,d2,id))})
# 
# grid.arrange(grobs = plot_list, nrow = 2, ncol = 3)
#--------------------------------------------

#' Wyznacza statystyki opisowe wektora, wartości NA są pomijane 
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
  data <- df %>% pull(2) %>% na.omit()

  s <- list("min" = min(data), 
            "Qu_1" = quantile(data, 0.25, names = FALSE),
            "median" = median(data),
            "mean" = mean(data),
            "Qu_3" = quantile(data, 0.75, names = FALSE),
            "max" = max(data),
            "std" = sd(data),
            "skew" = skewness(data),
            "kurt" = kurtosis(data))

  return (s)
}
#------------------------------------------------------
# (s <- stats(Dane("2000-01-01", "2020-01-01", "XAU")))
# 
# 
# code_to_currency <- import("data/code_to_currency.csv")
# 
# # wyznaczanie statystyk dla każdej waluty
# all_stats <- map(code_to_currency$Code, ~ {
#   data <- Dane("2000-01-01", "2024-11-30", .x)
#   stats(data)
# })
# 
# names(all_stats) <- code_to_currency$Code
# all_stats_df <- as.data.frame(do.call(cbind, all_stats))
# View(all_stats_df)
# 
# # Ustalenie skalera do wykresów na podstawie wartości
# conversion <- function(x) {
#   if(x<0.6 & x>=0.08) {
#     scaler = 10
#   } else if(x<0.08 & x>=0.008){
#     scaler = 100
#   } else if(x<0.008 & x>=0.0008){
#     scaler = 1000
#   } else if(x<0.0008 & x>=0.00008){
#     scaler = 10000
#   } else{
#     scaler = 1
#   } 
#   return(scaler)
# }
# 
# median_val <- as.numeric(unlist(all_stats_df["median",]))
# #Wyznaczenie wsp. skalowania na podstawie mediany 
# scale_rates <- map(colnames(all_stats_df), ~ {
#   data <- all_stats_df["median", .x]
#   conversion(data)
# })
# names(scale_rates) <- colnames(all_stats_df)
# scale_rates_df <- as.data.frame(do.call(cbind, scale_rates))
# export(scale_rates_df, "data/scaling_rates.csv")
# 
# con_rate <- as.numeric(unlist(scale_rates))
# scaled <- median_val * con_rate
# 
# barplot(scaled[-52], 
#         names.arg = colnames(all_stats_df)[-52], 
#         main = "Bar Plot of median", 
#         xlab = "Currency Codes", 
#         ylab = "Median", 
#         col = "lightblue", 
#         las = 2)
#------------------------------------------------------------


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
rolling_correl <- function(d1,
                           d2,
                           id1,
                           id2,
                           window_size,
                           mode1="normal",
                           mode2="normal",
                           n1=0,
                           n2=0){
  
  dates <- import("data/gold_and_exchange_rates.csv") %>%
    select(date) 
  
  idx_d1 <- which(dates >= d1) %>% min() # rzeczywista data poczatkawa wykresu
  idx_d0 <- idx_d1 - window_size + 1 # pierwsza data potrzebna do wyznaczenia korelacji dla d1
  
  if (idx_d0 <= 0) {
    stop("Nie można wyznaczyć korelacji dla wybranych parametrów. Wybierz późniejszą datę początkową lub zmniejsz rozmiar okna.")
  }
  
  d0 <- dates[idx_d0,]
  df1 <- Dane(d0, d2, id1, mode1, n1)
  df2 <- Dane(d0, d2, id2, mode2, n2)
  
  df <- inner_join(df1, df2, by = "date")
  
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

#------------------------------------------------------------
# window_size = 30
# d1 <- "2002-04-01"
# d2 <- "2015-03-01"
# c <- rolling_correl(d1, d2, "EUR", "USD", window_size)
# c2 <- rolling_correl(d1, d2, "XAU", "EUR", window_size)
# 
# p <- new_plot(c, scaled = FALSE, 
#               title = paste("Korelacja krocząca z ostatnich", window_size, "dni"),
#               y_label = "wsp. korelacji",
#               line_labs = FALSE)
# plot(p)
# add_to_plot(p, c2, scaled = FALSE, line_labs = FALSE)


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

graph_correlation <- function(d1,
                              d2,
                              ids,
                              base = "PLN"){
  if (!(base %in% code_to_currency$Code)) {  #waluta odniesienia base musi być jedną w walut z bazy danych lub PLN
    stop("Nieprawidłowa waluta odniesienia base.")
  }
  #cody walut do pobrania z bazy danych/pliku
  ids_db <- unique(c(ids, base))
  ids_db <- setdiff(ids_db,"PLN") # wartości dla id "PLN" nie ma w bazie - trzeba je wyznaczyć

  df <- lapply(ids_db, function(id) {Dane(d1, d2, id)}) #lista ramek z poszczególnymi walutami 
  df <- Reduce(function(x, y) merge(x, y, by = 'date', all = TRUE), df) #łączenie w jedną ramkę  
  
  if ("PLN" %in% ids) {
    df <- df %>% 
      mutate(PLN = 1)
  }

  
  if (base != "PLN") {

    df <- df %>% 
      mutate(across(-c(date, !!sym(base)), ~ . / !!sym(base))) %>% 
      select(-!!sym(base))
    
  }  
  
  #wyznaczenie korelacji
  cor_matrix <- cor(df[,-1], use = "complete.obs") #wyznaczenie maczierzy korelacji dla wszystkich zmiennych bez 1 kolumny zawierającej daty
  
  #tytuły do grafiki
  title_ending <- paste0("między walutami (waluta odniesienia: ", base, " - ", full_currency_name(base), ")")
  subtitle <- paste("od", df$date[1], "do", df$date[length(df$date)])
  #------------------
  #heatmapa ggplot
  
  cor_long <- melt(cor_matrix, value.name = "korelacja")
  
  heatmap <- ggplot(data = cor_long, 
                    aes(x = Var1, 
                        y = Var2, 
                        fill = korelacja,
                        alpha = abs(korelacja)*0.5+0.5)) +
    guides(alpha = "none") + 
    geom_tile() +
    scale_fill_viridis(option = "turbo", limits = c(-1, 1)) +
    scale_alpha_continuous(range = c(0.3, 1), limits = c(0,1)) +
    theme_minimal() +
    labs(title = paste("Macierz korelacji", title_ending),
         subtitle = subtitle,
         x = "Waluta", y = "Waluta") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_blank(),    # Usunięcie głównej siatki
          panel.grid.minor = element_blank()) +  # Usunięcie drobnej siatki
    geom_text(aes(Var1, Var2, label = round(korelacja,2)), color = "black", size = 3)+
    scale_y_discrete(
      labels = paste(full_currency_name(colnames(cor_matrix)), "-", colnames(cor_matrix))  # na osi y opisy kodów walut
    )
  
  #plot(heatmap)
  #--------------------------
  diag(cor_matrix) <- NA #Wartości na diagonali są równe 1 - nie są przydatne do wykresu
  cor_matrix[lower.tri(cor_matrix)] <- NA #wartości poniżej diagonali są takie same jak powyżej diagonali - są nadmiarowe
  cor_long <- melt(cor_matrix) # przekształcenie do formatu długiego
  cor_long <- cor_long[!is.na(cor_long[, 3]), ] #usunięcie wierszy o wartości NA
  

  #utworzenie grafu
  g <- graph_from_data_frame(cor_long, directed = FALSE)
  
  #atrybut weights jest wykorzystywany w wyznaczaniu rozkładu wierzchołków Fruchterman-Reingold
  E(g)$weight <- E(g)$value + 1  
  
  Korelacja <- E(g)$value

  #pełne nazwy walut
  full_currency_names <- full_currency_name(ids)
  full_currency_names <- paste(ids, "-", full_currency_names)

  #-----------------rysowanie grafu----------------
  title <- paste("Graf korelacji między walutami od",
                 df$date[1], "do", df$date[length(df$date)],
                 "Waluta odniesienia:", base)

  graph_plot <- ggraph(g, layout = "fr") +  # "fr" rozkład wierzchołków Fruchterman-Reingold 
    geom_edge_link(aes(edge_color = Korelacja, #rysowanie krawędzi
                       edge_width = abs(Korelacja),# Grubość krawędzi odpowiada wartości abs korelacji
                       #edge_alpha = abs(Korelacja)), #przezroczystośc zależy od korelacji 
                       )) + 
    scale_edge_color_viridis(option = "turbo", #skala kolorów dla krawędzi
                        limits = c(-1, 1),
                        guide =  "edge_colourbar") +
    scale_edge_width_continuous(range= c(0.5, 3), limits = c(0, 1)) + 
    #scale_alpha_continuous(range = c(0.5, 1), limits = c(0,1)) + #skalowanie zakresu 0.5-1
    geom_node_point(aes(color = full_currency_names, #rysowanie wierzchołków
                        stroke = 10)) +  
    scale_color_manual(values = rep("lightgrey", length(full_currency_names)),
                       name = "Waluty") + 
    geom_node_text(aes(label = V(g)$name)) +  #etykiety wierzchołków
    guides(edge_width = "none",
           edge_alpha = "none",
           color = guide_legend(position = "left")) + #ustawienia legendy
    theme_void() +  # usuwa osie i tło
    labs(
      title = paste("Graf korelacji", title_ending),
      subtitle = subtitle)

  return(list("graph" = graph_plot, "heatmap" = heatmap))

  #----------------------------------
  #wersja z kołem
  # ggraph(g, layout = 'linear', circular = TRUE) + 
  #   geom_edge_arc(aes(color = Korelacja, edge_width = E(g)$weight/8)) +
  #   scale_edge_color_viridis(option = "turbo", 
  #                            limits = c(-1, 1),
  #                            guide =  "edge_colourbar") +
  #   geom_node_text(aes(label = V(g)$name)) +
  #   coord_fixed()
  #----------------------------------
}

#-------------------------------------------------------
# graph_correlation("2010-01-01", "2010-03-01", c("EUR", "USD", "XAU", "AUD"))
# graph_correlation("2012-02-01", "2024-08-01", c("EUR", "AUD","XAU","CAD", "USD", "NOK", "SEK", "RON", "THB", "NZD", "SGD", "ISK", "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"))
# graph_correlation("2012-02-01", "2024-08-01", c("PLN", "AUD","XAU","CAD", "USD", "NOK", "SEK", "RON", "THB", "NZD", "SGD", "ISK", "BRL", "MYR", "IDR", "KRW", "CNY", "ILS", "INR", "CLP"), "EUR")
# 
# 
# graph_correlation("2010-01-01", "2010-03-01", c("USD", "NOK", "SEK"), "XAU")

#---------------------------------------------------------
