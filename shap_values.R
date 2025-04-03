#remotes::install_github("NorskRegnesentral/shapr", dependencies = TRUE)

#devtools::install_github("ModelOriented/shapviz")

# tools 
library(shapr)
library(shapviz)
library(xgboost)
library(tidyverse)
library(tidymodels)
library(caret)

#library(remotes)
#remotes::install_version("xgboost", "0.90.0.1", force=TRUE)  # Install version 0.90.0.1

# we read the fitting
fitt_xgb_max <- readRDS("fitt_xgb_max.RDS")

#saveRDS(fitt_xgb_max$finalModel, "fitt_xgb_max_final.RDS")

# Load the model
loaded_model <- readRDS("fitt_xgb_max_final.RDS")

# Save the model using xgb.save
xgb.save(loaded_model, "xgb_model_updated_booster")

# Load the updated model
loaded_model_updated <- xgb.load("xgb_model_updated_booster")

# Get the original training data.
original_training_data <- fitt_xgb_max$trainingData

# Get the predictor names from the model's coefficients.
predictor_names <- fitt_xgb_max$coefnames

# Identify categorical predictors.
categorical_predictors <- names(original_training_data)[sapply(original_training_data, is.factor)]

# Create dummy variables only for categorical predictors.
if (length(categorical_predictors) > 0) {
  dummy_vars <- dummyVars(paste("~", paste(categorical_predictors, collapse = "+")),
                          data = original_training_data)
  dummy_data <- data.frame(predict(dummy_vars, newdata = original_training_data))
  
  # Combine dummy variables with numeric predictors.
  numeric_predictors <- names(original_training_data)[sapply(original_training_data, is.numeric)]
  if(length(numeric_predictors) > 0){
    preprocessed_training_data <- cbind(original_training_data[, numeric_predictors, drop = FALSE], dummy_data)
  } else {
    preprocessed_training_data <- dummy_data
  }
  
} else{
  preprocessed_training_data <- original_training_data
}

head(preprocessed_training_data)
names(preprocessed_training_data)
predictor_names

cleaned_names <- gsub("\\.", "", names(preprocessed_training_data))

# Assign the cleaned names back to the data frame
names(preprocessed_training_data) <- cleaned_names

# Select only the predictors used by the model.
preprocessed_training_data <- preprocessed_training_data[, predictor_names, drop = FALSE]

# Calculate SHAP values
shap_values <- shapviz(loaded_model, X_pred = data.matrix(preprocessed_training_data))


sv_importance(shap_values, show_numbers = TRUE)
sv_importance(shap_values, kind = "beeswarm")
sv_dependence(shap_values, v = c("month", "N_mineral_spring",
                          "AirTemp_ave_month",
                          "Precip_cumsumhy"))
# individial prediction
sv_waterfall(shp, row_id = 1) +
  theme(axis.text = element_text(size = 11))

sv_force(shp, row_id = 1)

# monthly prediction 

library(ggplot2)

plots <- list(seq(1,12,1))  # Replace with your actual plots

for (i in 1:12) {
  ggsave(filename = paste0("plot",i, ".png"), sv_waterfall(shp, shp$X$month == i), width = 8, height = 6)
}

for(i in 1:12) {
  sv_waterfall(shp, shp$X$month == i)
  }

shp_i <- shapviz(
  fit, 
  X_pred = data.matrix(X_explain), 
  X = X_explain, interactions = TRUE
)

sv_dependence(shp_i, v = "month", 
              color_var = c( "clay_cat",
                             "AirTemp_ave_month"), 
              interactions = TRUE)


lookup_01_xbt <- read.table(
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/lookup_02_xbt.txt", 
  sep="\t", header=TRUE, stringsAsFactors=FALSE)

lookup_01_xbt <- lookup_01_xbt |> relocate("N_mineral_spring", .after = "month") |> 
  relocate( "clay_cat" , .after = "Precip_cumsumhy")

fit_bst <- xgboost(
  params = fitt_xgb_max$finalModel$params, 
  data = xgb.DMatrix(data.matrix(df_gen_simp_ind_max |> select(!".outcome")),
                     label = log(df_gen_simp_ind_max$.outcome), nthread = 1),
  nrounds = 1000
)

pred_fit <- predict(fit_bst, lookup_01_xbt[1:10,-8]|> data.matrix())

plot(lookup_01_xbt[1:10,]$pred, exp(pred_fit), 
     xlab="Lookup", ylab="Predicted")

abline(1,1)


