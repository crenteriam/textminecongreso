library(tidyverse)
library(rvest)
library(stringr)

#Define date
start_date = "25-09-2017"
end_date   = "27-09-2017"
b.start = as.Date(start_date, format = "%d-%m-%Y" )
b.end   = as.Date(end_date, format = "%d-%m-%Y" )
i = b.start
ch.year  = as.character(year(i))
ch.month = month(i) %>% 
  recode("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
         "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre", 
         .default = "other", .missing = "missing") %>% 
  as.character()
ch.day   = as.character(day(i))

#Extract all the links found
page.raw   = a[[1]]
page.clean = read_html(page.raw) %>% 
            html_nodes("a") %>% html_attr('href') %>% as_tibble()
    
#The best way to extract is to compare the base link with the vector of extracted links

#Base link
base_link = str_extract(a[[1]], "/index.php/esl/Comunicacion/Boletines/")
c <- filter(page.clean, grepl(paste0(base_link, ch.year, "/", ch.month, "/", ch.day, "/(.+)"), value))
c$value <- str_replace(c$value, "^/index.php/", "http://www5.diputados.gob.mx/index.php/")
d = unique(c)
e = as.list(d$value)
#Set unique vaobservations
