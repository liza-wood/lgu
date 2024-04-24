# run the keras model on... 
# pp_db (need to save from 02)
# pvp_db (need to save from 02)

# save as an output to read in for who breeds what (replace pvp_db and pp_db)
# Load in the keras package
library(keras)

# Install TensorFlow
tensorflow::install_tensorflow()
imdb <- dataset_imdb()
x_train <- imdb$train$x
y_train <- imdb$train$y
x_test <- imdb$test$x
y_test <- imdb$test$y

codes <- readRDS('data/codes.RDS')
