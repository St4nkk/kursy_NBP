#Skrypt pobiera dane dot. ceny złota (1g w próbie 1000) ze strony NBP od daty początkowej "2013-01-02" do dnia obecnego
#Następnie dane w formacie: (data, cena) są zapisywane do pliku csv

# Załadowanie pakietów
pacman::p_load(httr, rio)

get_gold_data_from_NBP <- function(start_date, last_date){
  
  first_available_date = as.Date("2013-01-02")
  last_available_date = Sys.Date()
  
  #zapytanie nie może obejmować dat przed 2013-01-02 
  if (start_date < first_available_date){
    start_date <- first_available_date
  }
  
  #zapytanie nie może obejmować dat następujących po dniu obecnym 
  if (last_date < last_available_date){
    last_date <- last_available_date
  }
  
  query_limit <- 93           #pojedyncze zapytanie nie może obejmować przedziału dłuższego niż 93 dni 
  
  # Utworzenie pustej ramki danych dla cen złota
  gold_price <- data.frame(
    date = character(0),   # Kolumna 'date' 
    price = numeric(0)      # Kolumna 'price'  
  )
  
  while (start_date <= last_date){
    
    end_date <- start_date + query_limit - 1  #data końcowa pojedynczego zapytania
    if (end_date > last_date) {
      end_date <- last_date
    }
    # URL API 
    url <- sprintf("https://api.nbp.pl/api/cenyzlota/%s/%s", start_date, end_date)
    # Wysłanie zapytania GET do API
    response <- GET(url)
    
    # Sprawdzenie, czy odpowiedź była poprawna (status 200)
    if (status_code(response) == 200) {
      # Pobranie zawartości odpowiedzi w formacie JSON
      data <- content(response, "parsed", type = "application/json")
    } else {
      cat("Błąd: ", status_code(response), "\n")
    }
    
    #utworzenie obiektu typu data frame z zagnieżdżonej listy data
    new_data  <- do.call(rbind, lapply(data, function(x) data.frame(date = x$data, price = x$cena)))
    
    # Dodanie nowych danych z zapytania 
    gold_price <- rbind(gold_price, new_data)
    
    start_date <- end_date + 1
  }
  return(gold_price)
}


start_date <- as.Date("2013-01-02")       #data początkowa zapytania wartość ze strony https://api.nbp.pl/
last_date <- Sys.Date()                  #ostatnia data całego zapytania

gold_price <- get_gold_data_from_NBP(start_date, last_date)

#zapis do pliku csv 
export(gold_price, "data/gold_price.csv")



