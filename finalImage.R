library(tidyverse)
library(magick)

# Change Year
setwd(here::here("2023/"))

# Change Year
file.remove(here::here("2023/finalImage.png"))

img_list <- list.files(pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
img_list <- img_list[!img_list %in% c(img_list[41])]

img_montage <- image_read(img_list) %>% 
  image_montage(tile = "8x0",
                geometry = "1000x1000+0+0",
                shadow = FALSE, 
                bg = "grey95")

# Change Year
image_write(img_montage, format = "png", 
            density = 300,
            path = here::here("2023/finalImage.png"), 
            quality = 100)
