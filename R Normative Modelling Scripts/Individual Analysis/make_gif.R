
library(magick)

## list file names and read in
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 24 frames per second
img_animated <- image_animate(img_joined, fps = 24)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "DOWNLOAD PATH")