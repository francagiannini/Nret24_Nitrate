library(tidyverse)
library(ggthemes)
# Starting BIS whith alogarithms #####

df_gen_pred <- readRDS("df_gen_predjun24.RDS")

df_gen_pred |>
  mutate(clay_plot = fct_recode(
    clay_cat,
    low = "low",
    'midle-high' = "middle",
    'midle-high' = "high"
  )) |>
  filter(Winter_nles5 == c("1", "4")) |>
  mutate(Winter_nles5=dplyr::recode_factor(Winter_nles5,
                                           `1`="Winter cereal", 
                                           `4`="Cover crop" ) )|> 
  #filter(Main_nles5==1 & Winter_nles5 == 4 & harvest_year== c(1998,1999,2008))|>
  #select(month, rf_pred, meancon,jbnr,harvest_year,clay_cat) |>
  pivot_longer(
    cols = c(
      leach_a_obs,
      leach_xbt_sim_ind,
      leach_rf_full_ind,
      leach_lin_ind
    ),
    #starts_with("leach_"),
    values_to = "Nleaching",
    names_to = "Measurement"
  ) |>
  ggplot(aes(
    x = month,
    y = Nleaching,
    fill = Measurement,
    col = Measurement
  )) +
  geom_smooth(alpha = 0.4, se=FALSE #aes(linetype = Measurement)
  ) +
  geom_boxplot(alpha = .1, aes(group = interaction(Measurement, month), 
                               fill = Measurement)) +
  geom_point(alpha = 0.2, size = 0.1) +
  facet_grid(Winter_nles5 ~ clay_plot) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(0, 30)) +
  
  guides(linetype = FALSE,
         fill = FALSE,
         col = guide_legend("Estimation:")) +
  ylab("N leaching (Kg N/ha)")+
  ggthemes::theme_hc()+
  scale_color_gdocs()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom", panel.background = NULL) 
#theme_bw()



log_den <- df_gen_pred |>
  pivot_longer(
    cols = c(
      leach_a_obs,
      leach_xbt_sim_ind,
      leach_rf_full_ind,
      leach_lin_ind
    ),
    values_to = "Nleaching",
    names_to = "Measurement"
  ) |>
  ggplot(aes(
    x = log(Nleaching) ,
    fill = Measurement,
    col = Measurement
  )) +
  geom_density(stat="density", position="identity", alpha=0.1)+
  #geom_step(aes(y = ..y..), stat = "ecdf") +
  #scale_x_continuous(breaks = c(-2.5, 5.5)) +
  #scale_x_continuous(limits = c(0,75))+
  theme_hc() +
  theme(legend.position = "bottom", 
        panel.background = NULL#,
        #panel.border =NULL
  ) +
  guides(linetype = FALSE,
         fill = FALSE,
         col = guide_legend("Estimation:")) +
  #ylab("Percentile")+
  ylab("Frequency")+
  scale_color_gdocs()+
  scale_fill_gdocs()

ggarrange(log_ecdf, ecdf, log_den, den, ncol=2, nrow=2, #, 
          common.legend = T,  legend="bottom")


# Lookup table ----

#https://climateknowledgeportal.worldbank.org/country/denmark/climate-data-historical


cont <- 
  df_gen_simp_ind_max |> 
  select(!c(month, meancon)) |> 
  select_if(is.numeric) |>
  scale(center = FALSE) |> as.data.frame() 

cont |>  
  pivot_longer(
    cols =colnames(cont),
    values_to = "value",
    names_to = "variable"
  ) |>
  ggplot(aes(
    x = value ,
    fill = variable,
    col = variable
  )) +
  geom_step(aes(y = ..y..), stat = "ecdf") +
  #geom_density(stat="density", position="identity", alpha=0.1)+
  theme_hc() +
  theme(legend.position = "bottom", 
        panel.background = NULL#,
        #panel.border =NULL
  ) +
  guides(linetype = FALSE,
         fill = FALSE,
         col = guide_legend("variable")) +
  ylab("Percentile")+
  #ylab("Frequency")+
  scale_color_gdocs()+
  scale_fill_gdocs()

df_gen_simp_ind_max |>  
  ggplot(aes(x = N_mineral_spring )) +
  geom_density(stat="density", position="identity", alpha=0.1)+
  geom_step(aes(y = ..y..), stat = "ecdf")+
  theme_hc()

quantile(df_gen_simp_ind_max$N_mineral_spring, seq(0.1,1,0.1))

quantile(df_gen_simp_ind_max$AirTemp_ave_month, seq(0.1,1,0.1))

quantile(df_gen_simp_ind_max$Precip_cumsumhy, seq(0.1,1,0.1))

fitt_xgb_max <- readRDS("fitt_xgb_max.RDS")

lookup_01_xbt <- expand.grid(month=seq(1,12,1),
                             Main_nles5=levels(df_gen_simp_ind_max$Main_nles5),
                             Winter_nles5=levels(df_gen_simp_ind_max$Winter_nles5),
                             clay_cat=levels(df_gen_simp_ind_max$clay_cat),
                             AirTemp_ave_month=seq(-1,25,1),
                             Precip_cumsumhy=c(seq(10,550,20), seq(600,1400,50)),
                             N_mineral_spring=seq(0,320,20)
)

lookup_01_xbt$pred <- predict(fitt_xgb_max,lookup_01_xbt)

lookup_01_xbt$pred <- exp(lookup_01_xbt$pred)

write.table(lookup_01_xbt,"lookup_01_xbt.txt", sep="\t")
