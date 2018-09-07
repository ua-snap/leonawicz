#install.packages("hexSticker")
library(hexSticker)
library(ggplot2)
pkg <- basename(getwd())
img <- "data-raw/snap_symbol_prewarp.png"
out <- paste0("data-raw/snap", c("", "-small"), ".png")

hex_plot <- function(out, mult = 1){
  sticker(img, 1, 1.15, 0.5, 0.3, "snap", p_color="#6A7072", p_y = 0.45, p_size = mult*20, h_size = mult * 1.4, h_fill = "white",
          h_color = "#96A73A", url = "leonawicz.github.io/snap", u_color = "#6A7072", u_size = mult * 3,
          filename = out)
  # overwrite file for larger size
  ggsave(out, width = mult*43.9, height = mult*50.8, bg = "transparent", units = "mm")
}

hex_plot(out[1], 4) # multiplier for larger sticker size
hex_plot(out[2])
