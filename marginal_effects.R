library(marginaleffects)
library(pdp)
library(caret)
library(purrr)


### marginal effects
fitt_xgb_max <- readRDS("fitt_xgb_max.RDS")

# Assuming you have:
# - Your trained model: 'xgb_model' 
# - Your training data: 'training_data'

# # Extract the final model from the caret object
 xgb_model <- fitt_xgb_max$finalModel
# 
# # Load the older version of XGBoost
 library(xgboost) # Use the older version if necessary
# 
# # Save the model using the older version
# xgb_model <- xgboost::xgb.Booster.complete(xgb_model)
# xgb.save(xgb_model, 'xgboost_model.json')
# 
# rev_xgbmodel <- xgboost::xgb.load('xgboost_model.json')

# Define the variables for which you want to plot marginal effects
var1 <- "AirTemp_ave_month" 
var2 <- "Precip_cumsumhy"

train_uden7 <- df_gen |> filter(!Winter_nles5  == "7")

levels(train_uden7$Winter_nles5)

# Create the partial dependence object
pdp_obj <- purrr::partial(fitt_xgb_max, 
                   pred.var = c(var1, var2), 
                   grid.resolution = 90, 
                   train = sample_frac(train_uden7, 0.1)) 

# Plot the partial dependence
plotPartial(pdp_obj) 
  
  pdp_obj |> 
  ggplot(aes(x = AirTemp_ave_month, y = Precip_cumsumhy, z = exp(yhat))) +
  geom_contour(aes(color = ..level..), bins = 10)+
    theme_hc()+
    scale_color_hc()
