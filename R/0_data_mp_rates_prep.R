library(readr)
library(zoo)

deu_mp_rate <- read_delim("data/central_banks/deu_leitzins.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(TIME = col_date(format = "%d.%m.%Y")), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

usa_mp_rate <- read_delim("data/central_banks/usa_ffunds_rate.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(TIME = col_date(format = "%Y-%m-%d")), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

aus_mp_rate <- read_delim("data/central_banks/aus_cash_rate.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(TIME = col_date(format = "%d.%m.%Y")), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

chl_mp_rate <- read_delim("data/central_banks/chl_cbank_rate.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(TIME = col_date(format = "%d.%m.%Y")), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

nor_mp_rate <- read_delim("data/central_banks/nor_key_rate.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(TIME = col_date(format = "%d.%m.%Y")), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

zaf_mp_rate <- read_delim("data/central_banks/zaf_cbank_rate.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(TIME = col_date(format = "%d.%m.%Y")), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

# We don't transform these
mp_rates <- list(aus_mp_rate, chl_mp_rate, deu_mp_rate, nor_mp_rate, usa_mp_rate, zaf_mp_rate)
names(mp_rates) <- c("aus_mp_rate", "chl_mp_rate", "deu_mp_rate", "nor_mp_rate", "usa_mp_rate", "zaf_mp_rate")

mp_rates <- lapply(mp_rates, function(x) {
  x$TIME <- as.yearqtr(x$TIME, format = "%Y-Q%q")
  # only keep the latest observation of a quarter
  x <- x[which(!duplicated(x$TIME, fromLast = TRUE)), ]
  return(x)
})

saveRDS(mp_rates, "data/mp_rates.rds")
