library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

list = readRDS("C:/Users/César/Desktop/Boletines/lista_boletines_2016.rds")

bag_utf_ch <- c("Ã¡" = "á", "Ã©" = "é", "Ã³" = "ó", "Ãº" = "ú", "Ã‰" = "É", 
                "Ã“" = "Ó", "Ãš" = "Ú", "Ã±" = "ñ", "Ã‘" = "Ñ", "Âº" = "º", "Âª" = "ª",
                "Â¿" = "¿", "â" = "", "Ã¼" = "ü", "Ã" = "í", "Ã" = "Á")

Boletines.Data <- as_tibble()
CounterBulletin = 0

while (nrow(Boletines.Data)<nrow(list)) {

for (k in list$Lista.Boletines[nrow(Boletines.Data):length(list$Lista.Boletines)]) {
    # Print Status
    CounterBulletin = nrow(Boletines.Data) + 1
    print(paste0("Scraping bulletin ", CounterBulletin, " of ", length(list$Lista.Boletines)))
    page = read_html(k)
    
    # Bulletin Date
    #Fecha = k %>% str_extract("[0-9]{4}(/)(.+)(/)") %>% str_replace("/$", "") %>% parse_date("%Y/%B/%D", locale = locale("es"))
    #Fecha = paste0(year(ydm(Fecha)), "-", month(ydm(Fecha)), "-", day(ydm(Fecha)))
    Fecha = list$Fecha[nrow(Boletines.Data) + 1]
    
    # Bulletin Number
    Numero = html_nodes(page, css = ".TitulosVerde") %>% html_text()
    Numero = Numero[[5]] %>% str_replace("(.+)(\\n)", "")
    
    # Bulletin Title
    Titulo = html_nodes(page, css = ".textoVerde") %>% html_text() %>% 
      str_trim() %>% str_replace_all(bag_utf_ch)
    
    # Corpus
    Texto  = html_nodes(page, css = ".smallNegra") %>% html_text() %>%
      str_replace_all("\\u0080|\\u009c|\\u009d", " ") %>% 
      str_replace_all("(.+)(\\.- )", " ") %>% str_trim() %>% str_replace_all(bag_utf_ch)
    
    # Combine
    page__df <- as_tibble(cbind(Fecha, Numero, Titulo, Texto))
    Boletines.Data <- bind_rows(Boletines.Data, page__df)
    Sys.sleep(sample(seq(1, 4, by = 0.001), 1))
  } # End Bulletins Data Loop

  Sys.sleep(50)
  
} #End of while


boletines_2016 = Boletines.Data
write_rds(boletines_2016, "C:/Users/César/Desktop/Boletines/boletines_2016.rds")
