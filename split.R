library("EBImage")
rm(list = ls())

step = 3



################################################################################################
# Step 1: prepare images for crack marking
################################################################################################

if (step == 1) {
  input_dir <- "/home/key/code/by_topic/crack_detection/original"
  output_dir <- "/home/key/code/by_topic/crack_detection/input"
  filepaths <- list.files(input_dir)
  filepaths
  files <- paste(input_dir, filepaths, sep = '/')
  files
  
  images <- Map(function(f) readImage(f), files)
  images
  
  img_eq = Map(function(i) equalize(i, range= c(0., 1.), levels = 20), images)
  for(i in 1:length(img_eq)) {
    writeImage(img_eq[[i]], paste0(output_dir, '/img_equalized_', i, '.tif'))
  }
}


################################################################################################
# Step 2: mark cracks
################################################################################################

################################################################################################
# Step 3: create training/test samples with diff. offsets
################################################################################################

# TEST CODE
# img <- matrix(LETTERS[1:20], nrow = 4, byrow = TRUE)
# img
# height <- 4
# width <- 5
# newsize <-3
# 
# for (i in 1:(height - newsize + 1)) {
#   for (j in 1:(width - newsize + 1)) {
#     img_part <- img[i:(i + (newsize - 1)), j:(j + (newsize - 1))]
#     print(img_part)
#   }
# }

if (step == 3) {
  
  input_dir <- "/home/key/code/by_topic/crack_detection/input"
  output_dir_crack <- "/home/key/code/by_topic/crack_detection/crack100"
  output_dir_nocrack <- "/home/key/code/by_topic/crack_detection/nocrack100"
  
  # valid_images <- list.files()[grepl('marked.png',list.files())]
  # valid_images <- c("img_equalized_2_marked.tif", "img_equalized_5_marked.tif")
  valid_images <- c("img_equalized_1_marked.tif", "img_equalized_2_marked.tif", "img_equalized_5_marked.tif",
                    "img_equalized_6_marked.tif", "img_equalized_8_marked.tif", "img_equalized_12_marked.tif")
  #valid_images_clean <- c("SampleA2_Detail_B_dog.tif", "SampleA2_Detail_E_dog.tif")
  valid_images_clean <- c("SampleA2_Detail_A_dog.tif", "SampleA2_Detail_B_dog.tif", "SampleA2_Detail_E_dog.tif",
                          "SampleA1_Detail_A_dog.tif", "SampleB1_Detail_B_dog.tif", "SampleB2_Detail_C_dog.tif")

  
  # cp SampleA2_Detail_B_dog_* ../data/train/nocrack/  
  # cp SampleA2_Detail_A_dog_* ../data/train/nocrack/
  # cp SampleB1_Detail_B_dog_* ../data/train/nocrack/
  # cp SampleA2_Detail_E_dog_* ../data/test/nocrack/
  # cp SampleA1_Detail_A_dog_* ../data/test/nocrack/
  # cp SampleB2_Detail_C_dog_* ../data/test/nocrack/

# cp SampleA2_Detail_B_dog_* ../data/train/crack/
# cp SampleA2_Detail_A_dog_* ../data/train/crack/
# cp SampleB1_Detail_B_dog_* ../data/train/crack/
# cp SampleA2_Detail_E_dog_* ../data/test/crack/
# cp SampleA1_Detail_A_dog_* ../data/test/crack/
# cp SampleB2_Detail_C_dog_* ../data/test/crack/
  
  #newsize <- 50
  newsize <- 100
  stepsize <- 5
  threshold = 10
  
  count_marked_pixels <- function(img) {
    img_r <- channel(img, 'red')
    img_b <- channel(img, 'blue')
    img_g <- channel(img, 'green')
    if (sum(img_b == img_r) != length(img_b)) stop("blue/red channels not ok!")
    greenpix <- sum(img_g != img_r)
    greenpix
  }
  
  
  for (img_index in seq_along(valid_images)) {
    
    img <- readImage(file.path(input_dir, valid_images[img_index]))
    img_clean <- readImage(file.path(input_dir,valid_images_clean[img_index]))
    
    img_name <- sub('.tif', '_', valid_images_clean[img_index])
    img_name
    display(img_clean, method = 'raster')
    
    height <- length(img[ , 1, 1])
    width <- length(img[ 1, , 1])
    
    is <- c()
    js <- c()
    greencounts <- c()
    
    p_other <- 0.03
    
    for (i in seq(1, height - newsize + 1, stepsize)) {
      for (j in seq(1, width - newsize + 1, stepsize)) {
        img_part <- img[i:(i + newsize - 1), j:(j + newsize - 1), ]
        img_part_clean <- img_clean[i:(i + newsize - 1), j:(j + newsize - 1), ]
        img_part_center <- img_part[40:60, 40:60, ]
        greenpix_overall <- count_marked_pixels(img_part)
        greenpix_center <- count_marked_pixels(img_part_center)
        if(greenpix_overall > 0) {
          #display(img_part, method = 'raster')
          #print(paste(i, j, greenpix, sep = ':'))
          is <- c(is, i)
          js <- c(js,j)
          greencounts <- c(greencounts,greenpix_overall)
          if(greenpix_center >= threshold) {
            writeImage(img_part_clean, paste0(output_dir_crack, '/', img_name, i, '_', j, '.png'))
          }
        } else {
          if(rbinom(1,1, prob = p_other) > 0) {
            writeImage(img_part_clean, paste0(output_dir_nocrack, '/', img_name, i, '_', j, '.png')) 
          }
        }
      }
    }
    
    df <- data.frame(is, js, greencounts)
    save(df, file = paste0(img_name, '.RData'))
    
  }
  
}

