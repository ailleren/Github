
# Import Test dataset
predict_set <- read.csv('input/house_price_test.csv', stringsAsFactor=F)

# Function for outlier treatment
outlier_treat <- function(train,test) {
  x <- test
  qnt <- quantile(train, probs=c(.25, .75), na.rm = T)
  caps <- quantile(train, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

#"price","grade_f","sqft_living_tf","sqft_lot_tf","renovated",
#"basement","houseAge_f","bathrooms_tf","bedrooms_tf","floors_f",
#"waterfront","fview","lat", "long","condition_f")

# Outlier treatment
predict_set$bedrooms_t <- outlier_treat(house_price$bedrooms, predict_set$bedrooms)
predict_set$bathrooms_t <- outlier_treat(house_price$bathrooms, predict_set$bathrooms)
predict_set$sqft_living_t <- outlier_treat(house_price$sqft_living, predict_set$sqft_living)
predict_set$sqft_lot_t <- outlier_treat(house_price$sqft_lot, predict_set$sqft_lot)
predict_set$grade_t <- outlier_treat(house_price$grade, predict_set$grade)

# New variables creation
predict_set$basement <- ifelse(predict_set$sqft_basement == 0, 0, 1)
predict_set$fview <- ifelse(predict_set$view == 0, 0, 1)
predict_set$renovated <- ifelse(predict_set$yr_renovated == 0, 0, 1)
predict_set$houseAge <- as.integer(format(Sys.Date(), "%Y")) - as.integer(as.character(predict_set$yr_built))

# Converting to Factor
fact <- c("fview","renovated", "basement", "waterfront")
predict_set[,fact] <- data.frame(apply(predict_set[fact], 2, as.factor))

# Numeric variable scalling
predict_set$grade_tf <-scale(predict_set$grade_t, center = mean(house_price1$grade_t),
      scale = sd(house_price1$grade_t))

predict_set$sqft_living_tf <-scale(predict_set$sqft_living_t, center = mean(house_price1$sqft_living_t),
                           scale = sd(house_price1$sqft_living_t))

predict_set$sqft_lot_tf <-scale(predict_set$sqft_lot_t, center = mean(house_price1$sqft_lot_t),
                                   scale = sd(house_price1$sqft_lot_t))

predict_set$houseAge_f <-scale(predict_set$houseAge, center = mean(house_price1$houseAge),
                                scale = sd(house_price1$houseAge))

predict_set$bedrooms_tf <-scale(predict_set$bedrooms_t, center = mean(house_price1$bedrooms_t),
                               scale = sd(house_price1$bedrooms_t))

predict_set$bathrooms_tf <-scale(predict_set$bathrooms_t, center = mean(house_price1$bathrooms_t),
                               scale = sd(house_price1$bathrooms_t))

predict_set$floors_f <-scale(predict_set$floors, center = mean(house_price1$floors),
                               scale = sd(house_price1$floors))

predict_set$condition_f <-scale(predict_set$condition, center = mean(house_price1$condition),
                             scale = sd(house_price1$condition))

keep <- c("id","grade_tf","sqft_living_tf","sqft_lot_tf","renovated",
              "basement","houseAge_f","bathrooms_tf","bedrooms_tf","floors_f",
              "waterfront","fview","lat", "long","condition_f")
predict_set2 <- predict_set[, (colnames(predict_set) %in% keep)]

# Predicting House prices on the new dataset
temp_pred <- predict_set2$id
predict_set2$id <- NULL
predict_set2$logpred <- predict(model_xgb_logp, newdata = predict_set2)

# Transform predictions
x <- data.frame(id = temp_pred)
x$pred_price <- exp(predict_set2$logpred)

summary(x)

# Export CSV
fwrite(x, "output/Test_Prediction.csv")
