library(tidyverse)
library(magick)

# Change Year
setwd(here::here("2025/"))

# Change Year
file.remove(here::here("2025/finalImage.png"))

img_list <- list.files(pattern = "\\.png$", recursive = TRUE, full.names = TRUE)

# Remove non-plot images
img_list <- img_list[!img_list %in% c(img_list[10], img_list[12], img_list[13])]

img_montage <- image_read(img_list) %>% 
  image_montage(tile = "4x0",
                geometry = "1000x1000+0+0",
                shadow = FALSE, 
                bg = "grey99")

# Change Year
image_write(img_montage, format = "png", 
            density = 300,
            path = here::here("2025/finalImage.png"), 
            quality = 100)
