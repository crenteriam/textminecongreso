#This function obtains a list of URLS based on a set period of days.
#Follow this syntaxis: dmY"25-09-2017"
boletin_getlist <- function(start_date = NA, end_date = NA, sleep = 3) {
#THE PROGRESS BAR ##############################################

#Dependencies
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

#=========================================================================================================
# 1 GET THE LIST OF DAYS
#=========================================================================================================
if (is.na(end_date)) {end_date = start_date}

  # Set correct date format
i        = as.Date(start_date, format = "%d-%m-%Y" )
end_date = as.Date(end_date, format = "%d-%m-%Y" )
base.url = "http://www5.diputados.gob.mx/index.php/esl/Comunicacion/Boletines/"

# Create the list of Days selected and generate the data
URL.Day.List = list()
Date.List    = list()
while (i <= end_date) {
  #1 Untangle Date Elements
  ch.year  = as.character(year(i))
  ch.month = month(i) %>% 
    recode("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre", 
           .default = "other", .missing = "missing") %>% 
    as.character()
  ch.day   = formatC(day(i),width = 2,format="d",flag="0") 
  
  #2 Store output in the list
  URL.Day = paste0(base.url, ch.year, "/", ch.month, "/", ch.day )
  URL.Day.List[[length(URL.Day.List)+1]] = URL.Day
  
  #3 Store the Date in a List
  Date.List[[length(Date.List)+1]] = paste0(day(i), "-", month(i), "-", year(i))
  
  #4 Go to the next Day
  i <- i + 1
  } # End of URL Loop

#=========================================================================================================
# 2 GET THE LIST OF BULLETINS PER DAY
#=========================================================================================================

# Keep using values from above: start_date, end_date, i, ch.(year|month|day) and URL.Day.List
# Extract all the links found in each page

Bulletins.List = as_tibble()
counter = 0
for (j in URL.Day.List[nrow(Bulletins.List):length(URL.Day.List)]) {
# Print Status
  counter = counter + 1
  print(paste0("Getting bulletin list for day ", Date.List[[counter]], " up to ", Date.List[[length(Date.List)]]))
# Process
paged           = read_html(j)
Lista.Boletines = html_nodes(paged, ".linkNegro") %>%  html_attr("href")
Lista.Boletines = paste0("http://www5.diputados.gob.mx", Lista.Boletines)
Fecha           = Date.List[[counter]] %>% as.character()
URL_Dia         = URL.Day.List[[counter]]
page_df         = as_tibble(cbind(Lista.Boletines, Fecha, URL_Dia))
Bulletins.List  = bind_rows(Bulletins.List, page_df)
Sys.sleep(sample(seq(1, sleep, by = 0.001), 1))
} # End of Bulletins List

return(Bulletins.List)
} # End of FUNCTION bulletin_getlist
#=========================================================================================================
#=========================================================================================================
#=========================================================================================================
# 3 GET THE BULLETINS DATASET
#=========================================================================================================

boletin_scraping <- function(Bulletins.List = NULL, sleep = 4) {
bag_utf_ch <- c("Ã¡" = "á", "Ã©" = "é", "Ã³" = "ó", "Ãº" = "ú", "Ã‰" = "É", 
                "Ã“" = "Ó", "Ãš" = "Ú", "Ã±" = "ñ", "Ã‘" = "Ñ", "Âº" = "º", "Âª" = "ª",
                "Â¿" = "¿", "â" = "", "Ã¼" = "ü", "Ã" = "í", "Ã" = "Á")

library(tidyverse)
library(rvest)
library(stringr)
# Loop to extract the Number, Title and Contents
Boletines.Data <- as_tibble()
func_list = Bulletins.List

CounterBulletin = 0
for (k in func_list$Lista.Boletines[nrow(Boletines.Data):length(func_list$Lista.Boletines)]) {
  # Print Status
  CounterBulletin = CounterBulletin + 1
  print(paste0("Scraping bulletin ", CounterBulletin, " of ", length(func_list$Lista.Boletines)))
  page = read_html(k)
  
  # Bulletin Date
  Fecha = func_list$Fecha[CounterBulletin]
  
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
  Sys.sleep(sample(seq(1, sleep, by = 0.001), 1))
} # End Bulletins Data Loop

return(Boletines.Data)
#=========================================================================================================
#=========================================================================================================
} #End of FUNCTION boletines_scrapping