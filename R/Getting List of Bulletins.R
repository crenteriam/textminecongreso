#LISTS -------------------------------------------------------------------------
month = as.list(c("02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
library(tidyverse)
year  = "2016"
year2 = "2017"
Data_2013 = as_tibble()

for (i in month) {
Data_i <- boletin_getlist(start_date = paste0("01-", i, "-", year), 
                          end_date = paste0("01-", month[[length(i)+1]], "-", if (month == "12") {year2} else {year}), sleep = 8) %>% 
                boletin_scraping(sleep = 8)
Sys.sleep(30)
Data   = bind_rows(Data_i)
}


# Alone
Data_x <- boletin_getlist(start_date = "01-01-2017", end_date = "01-02-2017", sleep = 8) %>% 
  boletin_scraping(sleep = 8)

write_rds(data, "C:/Users/César/Desktop/Boletines/lista_boletines_2016.rds")