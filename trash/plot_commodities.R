library(readr)
library(reshape2)
library(ggplot2)
library(cowplot)

index <- read_delim("data/commodity_indices.csv", 
                    ";", 
                    escape_double = FALSE, 
                    col_types = cols(DATE = col_date(format = "%d.%m.%Y"), GCSI_OI = col_skip()), 
                    locale = locale(decimal_mark = ",", grouping_mark = "."), 
                    trim_ws = TRUE)

index_re = data.frame(mapply(`/`, index[, 2:5], as.numeric(index[481, 2:5]))) * 100
index_re = data.frame(index[, 1], index_re, index[, 6:9])

index_diff = sapply(index[, 2:ncol(index)], diff)
index_diff = data.frame(index[2:nrow(index), 1], index_diff)
  
bcoms = melt(index[, c(1, 6:9)], id = "DATE")
gcsis = melt(index[, c(1, 2:5)], id = "DATE")
gcsis_re = melt(index_re[, c(1, 2:5)], id = "DATE")

main = melt(index_re[, c(1, 2, 6)], id = "DATE")
prec = melt(index_re[, c(1, 3, 7)], id = "DATE")
indu = melt(index_re[, c(1, 4, 8)], id = "DATE")
ener = melt(index_re[, c(1, 5, 9)], id = "DATE")

bcom_plot = ggplot(bcoms, aes(x = DATE, y = value)) +
  geom_line(aes(colour = variable)) +
  geom_area(data = index, aes(x = DATE, y = BCOM), alpha = 0.3, fill = "gray") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_x_date(limits = c(as.Date("1991-01-31"), NA), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0))
bcom_plot

gcsi_plot = ggplot(gcsis, aes(x = DATE, y = value)) +
  geom_line(aes(colour = variable)) +
  geom_area(data = index, aes(x = DATE, y = GCSI), alpha = 0.3, fill = "gray") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_x_date(limits = c(as.Date("1970-01-30"), NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
gcsi_plot

main_plot = ggplot(main, aes(x = DATE, y = value)) +
  geom_line(aes(colour = variable)) +
  scale_colour_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  scale_x_date(limits = c(as.Date("1970-01-30"), NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_line(colour = "gray"))
main_plot

prec_plot = ggplot(prec, aes(x = DATE, y = value)) +
  geom_line(aes(colour = variable)) +
  scale_colour_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  scale_x_date(limits = c(as.Date("1970-01-30"), NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_line(colour = "gray"))
prec_plot

indu_plot = ggplot(indu, aes(x = DATE, y = value)) +
  geom_line(aes(colour = variable)) +
  scale_colour_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  scale_x_date(limits = c(as.Date("1970-01-30"), NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_line(colour = "gray"))
indu_plot

ener_plot = ggplot(ener, aes(x = DATE, y = value)) +
  geom_line(aes(colour = variable)) +
  scale_colour_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  scale_x_date(limits = c(as.Date("1970-01-30"), NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_line(colour = "gray"))
ener_plot

plot_grid(main_plot, prec_plot, indu_plot, ener_plot, ncol = 2,
          labels = c("Main Index", "Precious Metals", "Industrial Metals", "Energy"), label_x = 0.2, label_y = 0.95)