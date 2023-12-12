library(tidyverse)
library(magick)

# Change Year
setwd(here::here("2022/"))

file.remove(here::here("2022/finalImage.png"))

img_list <- list.files(pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
img_list <- img_list[!img_list %in% c(img_list[41])]

img_montage <- image_read(img_list) %>% 
  image_montage(tile = "5x0",
                geometry = "1000x1000+0+0",
                shadow = FALSE, 
                bg = "grey95")

image_write(img_montage, format = "png", 
            density = 300,
            path = here::here("2022/finalImage.png"), 
            quality = 100)
