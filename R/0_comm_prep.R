library(readr)

data_commodities <- read_delim(
  "data/commodities/data_commodities.csv",
  "\t",
  escape_double = FALSE,
  col_types = cols(
    `CC21AMMN Steel Index` = col_double(),
    `CC21BMMN Iron Index` = col_number(),
    Date = col_number(),
    Date_1 = col_number(),
    Date_17 = col_number(),
    Date_18 = col_number(),
    Date_19 = col_number(),
    Date_2 = col_number(),
    Date_20 = col_number(),
    Date_21 = col_number(),
    Date_22 = col_number(),
    Date_23 = col_number(),
    Date_3 = col_number(),
    Date_4 = col_number(),
    Date_5 = col_number(),
    Date_6 = col_number(),
    Date_7 = col_number(),
    `HRC1 Steel Comdty` = col_double(),
    `LMAHDS03 Alu Comdty` = col_double(),
    `LMNIDS03 Alu Comdty` = col_double(),
    `LMSNDS03 Lead Comdty` = col_double(),
    `NLSAH Alu Index` = col_double(),
    `NLSCA Copper Index` = col_double(),
    `NLSNI Nickel Index` = col_double(),
    `NLSSN Tin Index` = col_double(),
    `NLSZS Zinc Index` = col_double()
  ),
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE,
  skip = 1
)

data_indices <- read_delim(
  "data/commodities/data_indices.csv",
  "\t",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE,
  skip = 1
)

data_oilfutures <- read_delim(
  "data/commodities/data_oilfutures.csv",
  ";",
  escape_double = FALSE,
  col_types = cols(
    `Brent + 2Y` = col_number(),
    `Brent + 3Y` = col_number(),
    Date = col_number(),
    Date_1 = col_number(),
    Date_2 = col_number(),
    Date_3 = col_number(),
    Date_4 = col_number(),
    Date_5 = col_number(),
    Date_6 = col_number(),
    Date_7 = col_number(),
    X2 = col_double()
  ),
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE,
  skip = 1
)
