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
# xgb_model <- fitt_xgb_max$finalModel
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


lookup_frac <- expand.grid(month=seq(1,12,1),
                             Main_nles5=levels(train_uden7$Main_nles5),
                             Winter_nles5=c("1", "2", "3", "4", "5", "6", "8", "9"),
                             clay_cat=levels(train_uden7$clay_cat),
                             AirTemp_ave_month=seq(-1,25,1),
                             Precip_cumsumhy=c(seq(10,550,20), seq(600,1400,50)),
                             N_mineral_spring=seq(0,320,20)
) |> sample_n(5000)

# contour plot
# Create the partial dependence object
pdp_obj <- partial(fitt_xgb_max, 
                   pred.var = c("Precip_cumsumhy", "AirTemp_ave_month"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdp_obj |> mutate(pred_conc = exp(yhat)) |> 
  ggplot(aes(x = AirTemp_ave_month, y = Precip_cumsumhy, z = pred_conc)) +
 stat_contour_filled(aes(colour = pred_conc))+
    theme_hc()+
    scale_color_hc()


# N mineral spring
# Create the partial dependence object
pdp_obj <- partial(fitt_xgb_max, 
                   pred.var = c("N_mineral_spring", "Main_nles5"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdp_obj |> mutate(pred_conc = exp(yhat)) |> 
  ggplot(aes(x = N_mineral_spring, y = pred_conc, col= Main_nles5)) +
  geom_smooth()+
  theme_hc()+
  scale_color_hc()


# monthly and clay
# Create the partial dependence object
pdp_obj <- partial(fitt_xgb_max, 
                   pred.var = c("month", "clay_cat"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdp_obj |> mutate(pred_conc = exp(yhat)) |> 
  mutate(
    month_hy = ifelse(month < 4, month + 12, month))|> 
  ggplot(aes(x = month_hy, y = pred_conc, col= clay_cat)) +
  geom_smooth()+
  scale_x_continuous(name = "month", breaks = 4:15,
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                "Jan", "Feb", "Mar"))+
  theme_hc()+
  scale_color_hc()


# monthly and main_crop and wintercrop
# Create the partial dependence object
pdp_obj <- partial(fitt_xgb_max, 
                   pred.var = c("month","Main_nles5", "Winter_nles5"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdp_obj |> mutate(pred_conc = exp(yhat)) |> 
  mutate(
    month_hy = ifelse(month < 4, month + 12, month))|> 
  ggplot(aes(x = month_hy, y = pred_conc, col= Winter_nles5)) +
  geom_smooth()+
  facet_wrap("Main_nles5")+
  scale_x_continuous(name = "month", breaks = 4:15,
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                "Jan", "Feb", "Mar"))+
  theme_hc()+
  scale_color_hc()

