library(readr)

gdp = read_delim("data/gdp.csv", 
                 ";", 
                 escape_double = FALSE, 
                 locale = locale(decimal_mark = ",", grouping_mark = "."), 
                 trim_ws = TRUE)

total_wealth = read_delim("data/total_wealth.csv", 
                          ";", 
                          escape_double = FALSE, 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), 
                          trim_ws = TRUE)

subsoil_wealth = read_delim("data/subsoil_wealth.csv", 
                            ";", 
                            escape_double = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

natural_wealth = read_delim("data/natural_wealth.csv", 
                            ";", 
                            escape_double = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

resource_rents = read_delim("data/resource_rents.csv", 
                            ";", 
                            escape_double = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

metal_exports = read_delim("data/metal_exports.csv", 
                            ";", 
                            escape_double = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

missing = gdp[which(!gdp$`Country Code` %in% subsoil_wealth$`Country Code`), c(1, 61)]
# Misses aggregates, Iran, Israel, Czech Republic, Hong Kong and other small states.
rm(missing)

subsoil = merge(subsoil_wealth[, c("Country Name", "2014")], gdp[, c("Country Name", "2014")], by = "Country Name")
names(subsoil) = c("Country Name", "subsoil_assets", "gdp")
subsoil$subsoil_to_gdp = subsoil$subsoil_assets / subsoil$gdp

wealth = merge(natural_wealth[, c("Country Name", "2014")], total_wealth[, c("Country Name", "2014")], by = "Country Name")
names(wealth) = c("Country Name", "natural_wealth", "total_wealth")
wealth$natural_to_total_wealth = wealth$natural_wealth / wealth$total_wealth

rents = resource_rents[, c("Country Name", "2014")]
names(rents) = c("Country Name", "resource_rents_to_gdp")

metal = metal_exports[, c("Country Name", "2014")]
names(metal) = c("Country Name", "metal_exports_to_total")

dep = merge(subsoil, wealth, by = "Country Name")
dep = merge(dep, rents, by = "Country Name")
dep = merge(dep, metal, by = "Country Name")

dep = na.omit(dep)

summary(dep)
dep$ind1 = ifelse(dep$subsoil_to_gdp > quantile(dep$subsoil_to_gdp, probs = 0.6), 1, 0)
dep$ind2 = ifelse(dep$natural_to_total_wealth > quantile(dep$natural_to_total_wealth, probs = 0.6), 1, 0)
dep$ind3 = ifelse(dep$resource_rents_to_gdp > quantile(dep$resource_rents_to_gdp, probs = 0.6), 1, 0)
dep$ind4 = ifelse(dep$metal_exports_to_total > quantile(dep$metal_exports_to_total, probs = 0.6), 1, 0)
dep$result = dep$ind1 + dep$ind2 + dep$ind3 + dep$ind4
results = dep[which(dep$result >= 3), ]
results = results[which(results$gdp > 2e+11), ]
results = results[which(!results$`Country Name` %in% c("Latin America & Caribbean", "Lower middle income", "Middle East & North Africa", "Sub-Saharan Africa")), ]

gdp[which(gdp$`Country Name` %in% results$`Country Name`), 2]
