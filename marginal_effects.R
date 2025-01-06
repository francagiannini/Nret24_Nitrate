library(pdp)
library(caret)
library(purrr)
library(tidyverse)
library(ggthemes)

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

train_uden7 <- df_gen |> dplyr::filter(!Winter_nles5 == "7")

levels(train_uden7$Winter_nles5)

lookup_frac <- expand.grid(month=seq(1,12,1),
                             Main_nles5=levels(train_uden7$Main_nles5),
                             Winter_nles5=c("1", "2", "3", "4", "5", "6", "8", "9"),
                             clay_cat=levels(train_uden7$clay_cat),
                             AirTemp_ave_month=seq(-1,25,1),
                             Precip_cumsumhy=c(seq(10,550,20), seq(600,1400,50)),
                             N_mineral_spring=seq(0,320,20)
) |> sample_n(5000)

# contour plot ---- not included
# # Create the partial dependence object
# pdp_obj <- partial(fitt_xgb_max,
#                    pred.var = c("Precip_cumsumhy", "AirTemp_ave_month"),
#                    grid.resolution = 90,
#                    train = lookup_frac)
# 
# # Plot the partial dependence
# pdp_obj |> mutate(pred_conc = exp(yhat)) |>
#   ggplot(aes(x = AirTemp_ave_month, y = Precip_cumsumhy, z = pred_conc)) +
#  stat_contour_filled(aes(colour = pred_conc))+
#     theme_hc()+
#     scale_color_hc()


# N mineral spring ----
# Create the partial dependence object
pdp_obj_nspr <- pdp::partial(fitt_xgb_max, 
                   pred.var = c("N_mineral_spring","clay_cat"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdpN <- pdp_obj_nspr |> #mutate(pred_conc = exp(yhat)) |> 
  ggplot(aes(x = N_mineral_spring, y = yhat, col=clay_cat)) +
  geom_point(size=.1)+
  geom_smooth()+
  ylab(expression(log(NO[3]^"-")))+
  xlab("Mineral Nitrogen applied in spring")+
  theme_hc()+
  scale_color_hc(name="Soil Clay", labels = c("low", 
                            "middle","high"))


# monthly and clay ----
# Create the partial dependence object
pdp_obj_mvsclay <- pdp::partial(fitt_xgb_max, 
                   pred.var = c("month", "clay_cat"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdp_obj_mvsclay |> #mutate(pred_conc = exp(yhat)) |> 
  mutate(
    month_hy = ifelse(month < 4, month + 12, month))|> 
  ggplot(aes(x = month_hy, y = yhat, col= clay_cat)) +
  geom_smooth()+
  geom_point(size=.1)+
  scale_x_continuous(name = "month", breaks = 4:15,
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                "Jan", "Feb", "Mar"))+
  ylab(expression(log(NO[3]^"-")))+
  xlab("Month")+
  theme_hc()+
  scale_color_hc(name="Soil Clay", labels = c("low", 
                                              "middle","high"))


# monthly and main_crop and wintercrop
# Create the partial dependence object
pdp_obj_crop_rot <- pdp::partial(fitt_xgb_max, 
                   pred.var = c("month","Main_nles5", "Winter_nles5"), 
                   grid.resolution = 90, 
                   train = lookup_frac) 

# Plot the partial dependence
pdpcrop_rotsup <- 
  pdp_obj_crop_rot |> mutate(pred_conc = exp(yhat)) |> 
  mutate(
    month_hy = ifelse(month < 4, month + 12, month), 
    MainCrop = recode_factor(Main_nles5,
                           '1'="Winter cereal",
                           '2'="Spring cereal",
                           '3'=	'Grain-legume mixtures',
                           '4'= 'Grass or grass-clover',
                           '5'= 'Grass for seed',
                           '7'= 'Set-aside',
                           '6'= 'Set-aside',
                           '8'= 'Sugar beet, fodder beet',
                           '9'= 'Silage maize and potato',
                           '10'= 'Winter oilseed rape',
                           '11'= 'Winter cereal after grass',
                           '12' = 'Maize after grass',
                           '13'= 'Spring cereal after grass',
                           '14'= 'Grain legumes and spring oilseed rape'),
    WinterCrop = recode_factor(Winter_nles5,
                           '1'= 'Winter cereal (excluding winter rape)',
                           '2' = 'Bare soil',
                           '3'= 'Autumn cultivation',
                           '4'= 'Cover crops, undersown grass and set-aside',
                           '5' = 'Weeds and volunteers',
                           '6'= 'Grass and grass-clover, winter rape',
                           '8'='Winter cereal after grass',
                           '9' = 'Spring cereal after grass')
    ) |> 
  ggplot(aes(x = month_hy, y = yhat, col= WinterCrop)) +
  geom_smooth()+
  facet_wrap("MainCrop", nrow = 4,)+
  scale_color_ptol(name= 'Winter Crop')+
  scale_x_continuous(name = "month", breaks = 4:15,
   labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
            "Jan", "Feb", "Mar"))+
  theme(legend.position = "bottom")+
  theme_hc()+
  ylab(expression(log(NO[3]^"-")))+
  xlab("")

# monthly and monthly temp
# Create the partial dependence object
pdp_obj_mvstemp <- pdp::partial(fitt_xgb_max, 
                                pred.var = c("month", "AirTemp_ave_month"), 
                                grid.resolution = 90, 
                                train = lookup_frac) 

# Plot the partial dependence
pdp_obj_mvstemp |> #mutate(pred_conc = exp(yhat)) |> 
  mutate(
    month_hy = ifelse(month < 4, month + 12, month))|> 
  ggplot(aes(x = month, y = AirTemp_ave_month, z = yhat)) +
  stat_contour_filled(aes(colour = yhat))+
  geom_point(size=.1)+
  # scale_x_continuous(name = "month", breaks = 4:15,
  #                    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
  #                               "Jan", "Feb", "Mar"))+
  #zlab(expression(log(NO[3]^"-")))+
  xlab("Montth")+
  ylab("Monthly average daily temperature")
  theme_hc()

# Temp ----
pdp_obj_temp <- pdp::partial(fitt_xgb_max, 
                               pred.var = c("AirTemp_ave_month","clay_cat"), 
                               grid.resolution = 90, 
                               train = lookup_frac) 
  
  pdp_temp <- pdp_obj_temp |> #mutate(pred_conc = exp(yhat)) |> 
    ggplot(aes(x = AirTemp_ave_month, y = yhat, col=clay_cat)) +
    geom_point(size=.1)+
    geom_smooth()+
    ylab(expression(log(NO[3]^"-")))+
    xlab("Monthly average daily temperature")+
    theme_hc()+
    scale_color_hc(name="Soil Clay", labels = c("low", 
                                                "middle","high"))
  
  
  # pp ----
  pdp_obj_pp <- pdp::partial(fitt_xgb_max, 
                               pred.var = c("Precip_cumsumhy","clay_cat"), 
                               grid.resolution = 90, 
                               train = lookup_frac) 

  pdp_pp <- pdp_obj_pp |> #mutate(pred_conc = exp(yhat)) |> 
    ggplot(aes(x = Precip_cumsumhy, y = yhat, col=clay_cat)) +
    geom_point(size=.1)+
    geom_smooth()+
    ylab(expression(log(NO[3]^"-")))+
    xlab("Cumulative pp within the hydrological year")+
    theme_hc()+
    scale_color_hc(name="Soil Clay", labels = c("low", 
                                                "middle","high"))  
  