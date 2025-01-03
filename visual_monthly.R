# packages ----
library(tidyverse)
library(ggpubr)
library(GGally)
library(RColorBrewer)
library(leaps)
library(ggthemes)


theme_set(theme_bw())
theme_update(panel.grid = element_blank())

df_gen <- readRDS(
  "O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/Nret24files/df_m_imp.RDS")

# sites and years ----

site_yr <- table(df_gen$site_eng, df_gen$harvest_year) |>
  as.data.frame()

site_yr_plot <-   merge(site_yr, df_gen |> select(site_eng,Y) |> unique(),
        by.y="site_eng", 
        by.x="Var1") |> 
  mutate(
    Freq = ifelse(Freq == 0, NA, Freq),
    Site = gsub(
      x = Var1,
      pattern = "[\\]" ,
      replacement = ""
    ),
    #Var1,
    harvest_year = Var2
  ) |> 
  ggplot(aes(x = harvest_year, y = reorder(Site,Y), fill = Freq)) +
  geom_tile(color = 'gray90') +
  scale_fill_gradientn(
    name = "n",
    na.value = "white",
    colors = brewer.pal(5, "Blues")
  ) +
  geom_text(aes(label = ifelse(Freq == 0, paste(""), paste(Freq))), 
            color = "black", size = 2) +
  scale_x_discrete(name = "Harvest year") +
  scale_y_discrete(name = "Experimental Sites") +
  theme_minimal()+
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ),legend.position = "none")

# 4.1 Data representation  and distribution ----
## histogram monthly conc ----

hist_conc <- df_gen |>
  #mutate(log =log(meancon)) |>
  ggplot() +
  geom_histogram(
    aes(x = meancon),
    fill = "#01a2d9",
    #aes(x = log(meancon)),
    #fill = "#01a2a9",
    color = "white",
    bins = 30
  ) +
  #geom_histogram(aes(log))+
  #geom_density(aes(y = rel_freq), color = "red", linetype = "dashed") +
  scale_y_continuous(name = "Absolute frequency") +
  scale_x_continuous(name = "Monthly nitrate concentration (mg/L)") +
  #labs(title = "Histogram with Relative Frequency and Density Curve") +
  theme_minimal()


## Crops cross ----
crops_plot <- 
table(df_gen$Main_nles5,df_gen$Winter_nles5) |> 
  as.data.frame() |>
  mutate(Freq = ifelse(Freq==0,NA, Freq),
         M = recode_factor(Var1,
                         '1'="Winter cereal",
                         '2'="Spring cereal",
                         '3'=	'Grain-legume mixtures',
                         '4'= 'Grass or grass-clover',
                         '5'= 'Grass for seed',
                         '7'= 'Set-aside',
                         #'6'= 'Set-aside',
                         '8'= 'Sugar beet, fodder beet',
                         '9'= 'Silage maize and potato',
                         '10'= 'Winter oilseed rape',
                         '11'= 'Winter cereal after grass',
                         '12' = 'Maize after grass',
                         '13'= 'Spring cereal after grass',
                         '14'= 'Grain legumes and spring oilseed rape'),
         W = recode_factor(Var2,
                           '1'= 'Winter cereal (excluding winter rape)',
                           '2' = 'Bare soil',
                           '3'= 'Autumn cultivation',
                           '4'= 'Cover crops, undersown grass and set-aside',
                           '5' = 'Weeds and volunteers',
                           '6'= 'Grass and grass-clover, winter rape',
                           '8'='Winter cereal after grass',
                           '9' = 'Spring cereal after grass') )|> 
  filter(!W == '7') |> 
  ggplot( aes(x = M, y = W, fill = Freq)) +
  geom_tile(color = "gray90") +
  scale_fill_gradientn(name = "n",
                       na.value = 'white',
                       colors = brewer.pal(5,"Greens")#,
                       #breaks=c(0,40,100,500,1000,2000,5000)
                       ) +
  geom_text(aes(label = ifelse(is.na(Freq), paste(""), paste(Freq))), 
            color = "black", size = 2) +
  scale_x_discrete(name = "Main crop") +
  scale_y_discrete(name = "Winter crop")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text( vjust = 0.2, hjust = 1.02),
        legend.position = "none")

## map ----
library(sf)
#install.packages("plotDK")
library(plotDK)

region_dk <- st_as_sf(plotDK::region, coords = c("long","lat")) |> 
  group_by(id) |> 
  summarise() |> 
  st_cast("POLYGON")

#ggplot(province_dk) +
#  geom_sf()

#plotDK()

# read sf file
dk <- read_rds("gadm36_DNK_2_sf.rds")

df_gen_sf <- df_gen  |> 
  mutate(site_eng=dplyr::recode(site_eng,
                `\\Burrehoj\\`="\\Foulum\\")) |> 
  group_by(site_eng) |>
  summarise(n=n(), X=mean(X), Y=mean(Y))|>
  st_as_sf(coords = c("X", "Y"), crs = 25832, remove=FALSE)  
  

dk <- st_transform(dk,crs=st_crs(df_gen_sf))

df_gen_sf$legend_var <- df_gen_sf$n

point <- ggplot(dk) +
  geom_sf(color = "#b0b0b0") +
  geom_point(
    data = df_gen_sf,
    aes(x = X, y = Y, size = n, color = legend_var),
    alpha = 0.8
  ) +
  scale_size_continuous(range = c(1, 10), 
                        guide = "legend", 
                        breaks = c(50, 500, 5000 ,10000)) +
  scale_color_gradient(
    low = "#000407",
    high = "#01a2d9",
    breaks =  c(50, 500, 5000 ,10000)# Match breaks with size
  ) +
  guides(
    size = guide_legend(title = "N"),
    color = guide_legend(title = "N")  # Same title for color and size
  ) +
  labs(x = "Longitude", y = "Latitude")


colorblindr::cvd_grid(point)

### Figure 1 ----
fig1 <- ggarrange(hist_conc,point, ncol=2, widths = c(2, 3.5), 
                  font.label = list(size = 8))

ggsave(
  plot=fig1,
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/Nret24_Nitrate/Figures/fig1_hst_map.jpeg",
  width = 183,
  height = 60,
  units = "mm"
)

### Supp 1 ----
ggsave(
  plot=site_yr_plot,
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/Nret24_Nitrate/Figures/sup1_stevsyr.jpeg",
  width = 183,
  height = 90,
  units = "mm"
)

### Supp 2 ----

# Add crop periods
timeline_data <- data.frame(
  period = c("Leaching Period", "Main Crop", "Winter Crop"),
  start = as.Date(c("2023-04-01", "2023-04-01", "2023-10-01")),
  end = as.Date(c("2024-03-31", "2023-09-30", "2024-03-31")),
  color = c("#01a2d9", "#00AC00", "#D79300")  # Assign colors to periods
)

# Create the timeline plot
Sys.setlocale("LC_TIME", "English")

diagram <- ggplot(timeline_data) +
  geom_segment(
    aes(x = start, xend = end, y = period, yend = period, color = period),
    size = 5
  ) +
  scale_color_manual(
    values = c("Leaching Period" = "#01a2d9", 
               "Main Crop" = "#00AC00", 
               "Winter Crop" = "#D79300"),
    guide = "none"  # Remove redundant legend
  ) +
  labs(
    title = "Leaching Period and Crop sequence Timeline",
    x = "Date",
    y = "Period",
    caption = "Leaching: 1st April to 31st March\nMain Crop: April to September\nWinter Crop: October to March"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 months"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.title.x = element_text(size = 14),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "gray92", size = 0.5)
  )


ggsave(
  plot=crops_plot,
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/Nret24_Nitrate/Figures/sup2_MW.jpeg",
  width = 183,
  height = 90,
  units = "mm"
)

## Monthly example ----

#Choosing a site and year to plot monthly data
# filter <- df_gen |> 
#   filter(#site_eng=="\\Jyndevad\\" &
#            #harvest_year==2010,
#          Main_nles5 == "1" #winter ceral
#          & Winter_nles5 %in% c("2", "1") #bare soil and Cover crops
#          #& N_mineral_spring>50
#   ) |> group_by(month, site_eng) |> 
#   summarise(n = n())

#table(filter$month, filter$site_eng)

df_gen_plot_month <- 
  df_gen |>
  mutate(
    month_hy = as.numeric(recode(month, 
                      '1' = '10',
                      '2' = '11',
                      '3' = '12',
                      '4' = '1',
                      '5' = '2',
                      '6' = '3',
                      '7' = '4',
                      '8' = '5',
                      '9' = '6',
                      '10' = '7',
                      '11' = '8',
                      '12' = '9')),
      # fct_relevel(
      #   as.character(month),
      #   '4','5','6','7','8','9','10','11','12','1','2','3'),
    month_name = month.abb[month],
    clay_plot = fct_recode(
           clay_cat,
           low = "low",
           'midle-high' = "middle",
           'midle-high' = "high"
         ),
    afstro_sum_month = as.numeric(ifelse(afstro_sum_month <= 0, 0.001, afstro_sum_month)),
  ) |>
  filter(#!between(ident, 2017,2020) &
          site_eng=="\\Foulum\\" &
          #between(ident, 1020, 1158) &
           #harvest_year==2010,
           Main_nles5 == "1" #winter ceral
         & Winter_nles5 %in% c("2", "4") #bare soil and Cover crops
         #& N_mineral_spring>50
  ) |> 
  mutate(Winter_nles5=dplyr::recode_factor(Winter_nles5,
                                           '2' = 'Winter ceral-> Bare soil', 
                                           '4'= 'Winter ceral-> Cover crops'),
         obs_leach=as.numeric(meancon*afstro_sum_month/100))
  #   filter(site_eng=="\\Foulum\\" &
  #   Main_nles5 == "1" #winter ceral
  #   & Winter_nles5 %in% c("2", "4") #bare soil and Cover crops
  #   #& N_mineral_spring>50
  #   ) |> 
  # mutate(Winter_nles5=dplyr::recode_factor(Winter_nles5,
  #                                       '2' = 'Winter ceral-> Bare soil', 
  #                                       '4'= 'Winter ceral-> Cover crops') )



table(df_gen_plot_month$month, df_gen_plot_month$harvest_year)

# library(mgcv)
# library(tidygam)
# 
# gs_conc <- gam(
#   meancon ~ s(month_hy, by = Winter_nles5, k = 3),
#   data = df_gen_plot_month,
#   family = Gamma(link = "log")
# )
# 
# gs_pred_c <- predict_gam(gs_conc, tran_fun = exp, length_out=12*4)
# 
# gs_pred_c |> 
#   plot(series = "month_hy")
# 
# gs_perc <- gam(
#   as.numeric(afstro_sum_month) ~ s(month_hy, by = Winter_nles5, k = 3),
#   data = df_gen_plot_month,
#   family = Gamma(link = "log")
# )
# 
# gs_leach <- gam(
#   as.numeric(obs_leach) ~ s(month_hy, by = Winter_nles5, k = 3),
#   data = df_gen_plot_month,
#   family = Gamma(link = "log")
# )
# 
# predict_gam(gs_leach, tran_fun = exp, length_out=12*4) |> 
#   plot(series = "month_hy")
# 
# 
# gs_preds <- bind_rows(
#   predict_gam(gs_conc, tran_fun = exp, length_out=12*4) |> 
#     mutate(variable="meanconc") |> rename(pred=meancon),
#   
#   predict_gam(gs_perc, tran_fun = exp, length_out=12*4)|> 
#     mutate(variable="perc")|> rename(pred=afstro_sum_month), 
#   
#   predict_gam(gs_leach, tran_fun = exp, length_out=12*4)|> 
#     mutate(variable="leach")|> rename(pred=obs_leach)
# ) 
# 
# gs_preds |> 
#   ggplot(aes(x=month_hy, y=pred, 
#              group=Winter_nles5, 
#              color=Winter_nles5))+
#   geom_line()+
#   facet_grid(variable~., scales = "free_y")+
#   scale_color_manual(values = c("#01a2d9", "#014B55"))+
#   scale_x_continuous(name = "month")+
#   scale_y_continuous(name = "Monthly nitrate conccentration (mg/L)")+
#   theme_minimal()
# 
# 
# 
# gs_preds <- bind_cols(
#   predict_gam(gs_conc, tran_fun = exp, length_out=12*4) |> 
#     rename(se_conc=se,lower_ci_conc=lower_ci,upper_ci_conc=upper_ci),
#   predict_gam(gs_perc, tran_fun = exp, length_out=12*4)|> 
#     rename(se_perc=se,lower_ci_perc=lower_ci,upper_ci_perc=upper_ci), 
#   predict_gam(gs_leach, tran_fun = exp, length_out=12*4)|> 
#     rename(se_leach=se,lower_ci_leach=lower_ci,upper_ci_leach=upper_ci),
#   .name_repair = c("minimal", "unique")
#   ) |> unique()

conc_month <- df_gen_plot_month |> 
  ggplot(aes(x=month_hy,y=meancon, group=Winter_nles5))+
  geom_point(size=0.6, aes(color  = Winter_nles5))+
  geom_smooth(aes(group = Winter_nles5, color = Winter_nles5),
              #color="#01a2d9", 
              alpha=0.1, linewidth=0.8
              )+
  scale_color_manual(values = c("#01a2d9", "#014B55"))+
  #facet_grid(~Winter_nles5)+
  scale_y_continuous(name = "Monthly nitrate conccentration (mg/L)")+
  scale_x_discrete(name = "")

perc_month <- df_gen_plmonth_nameperc_month <- df_gen_plot_month |>
  ggplot(aes(x=month_hy,y=as.numeric(afstro_sum_month), 
             group=Winter_nles5))+
  geom_point(size=0.6, aes(color  = Winter_nles5))+
  geom_smooth(aes(group = Winter_nles5, color=Winter_nles5), 
              alpha=0.1, linewidth=0.8
              )+
  scale_y_continuous(name="Monthly percolation (mm)", limits = c(10,100))+
  scale_color_manual(values = c("#01a2d9", "#014B55"))+#values = c("#0B7100", "#011700"))+
  #facet_grid(~Winter_nles5)+
  scale_x_discrete(name = "")


leached_month <- df_gen_plot_month |>
  ggplot(aes(x=month_hy,y=as.numeric(meancon*afstro_sum_month/100)))+
  geom_point(size=0.6, aes(color  = Winter_nles5))+
  geom_smooth(aes(group = Winter_nles5, color=Winter_nles5), 
              alpha=0.1, linewidth=0.8
  )+
  scale_color_manual(values = c("#01a2d9", "#014B55")) + # "#922CC6","#260339"))+
  scale_y_continuous(name="Monthly nitrate leaching Kg/ha")+
  #facet_grid(~Winter_nles5)+
  scale_x_continuous(name = "month", breaks = 1:12,
                   labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                              "Jan", "Feb", "Mar")) 

### Figure 2 ----
month_exp <- ggarrange(conc_month+rremove("x.text")+
                         labs(color = "Crop sequence"), 
                       perc_month+rremove("x.text")+
                         labs(color = "Crop sequence"), 
                       leached_month+
                         labs(color = "Crop sequence"), 
                       ncol=1, 
                       common.legend = TRUE
                       )+
  scale_x_discrete(name = "month") #|> 

ggsave(
  plot=month_exp,
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/Nret24_Nitrate/Figures/fig2_exp.jpeg",
  width = 180,
  height = 240,
  units = "mm"
)


# SUmmary 


library(plotly)
library(ggthemes)

df_gen |> ggplot(aes(
  x = Precip_sum365
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
  labs(y="Percentile", x="Precipitation in the past year")


df_gen |> select(afstro_cumsumhy, 
                 Precip_sum365, 
                 AirTemp_ave_month,
                 N_mineral_spring,
                 Precip_cumsumhy#,
                 #Winter_nles5,  
                 #Main_nles5, month
                 ) |> 
  summarise(across(everything(), ~ sd(.x)/mean(.x)*100))
  
  summarise_all(list(mean=mean,cv=sd, iqr=IQR, min=min, max=max))
  
  
  #col_var=
  
  
  df_gen |> select(month,
                   #afstro_cumsumhy, 
                   #Precip_sum365, 
                   AirTemp_ave_month,
                   N_mineral_spring,
                   Precip_cumsumhy#,
                   #Winter_nles5,  
                   #Main_nles5, month
  ) |> pivot_longer(cols = c(#afstro_cumsumhy, 
                             #Precip_sum365, 
                             AirTemp_ave_month,
                             N_mineral_spring,
                             Precip_cumsumhy)) |> 
    mutate(name=fct_relevel(name, c(levels = c(#"afstro_cumsumhy", "Precip_sum365", 
                                               "AirTemp_ave_month", 
                                               "N_mineral_spring","Precip_cumsumhy")))) |> 
    ggplot(aes(x=month, y=value, color = name))+
    geom_point(size=0.5)+
    geom_smooth()+
    theme_hc() +
    scale_color_gdocs()+
    scale_x_continuous(breaks = seq(1,12,1))+
    #scale_color_grey(start = 0.7,
    #                 end = 0.2,)+
    facet_grid(name~., scales = "free_y")
  
 # 4.3	Model evaluation and Implementation ----
  
  ## 4.3.1 Model performance density plots----

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

### Figure 3 ----
  
  log_den <- 
    df_gen_pred |>
    pivot_longer(
      cols = c(
        #leach_a_obs,
        #leach_xbt_sim_ind,
        #leach_rf_full_ind,
        #leach_lin_ind
        pred_xbt_sim_ind_max,
        meancon
      ),
      values_to = "est",
      names_to = "Measurement"
    ) |>
    ggplot(aes(
      x = log(est) ,
      fill = Measurement,
      col = Measurement
    )) +
    geom_density(stat="density", position="identity", alpha=0.4)+
    #geom_step(aes(y = ..y..), stat = "ecdf") +
    #scale_x_continuous(breaks = c(-2.5, 5.5)) +
    #scale_x_continuous(limits = c(0,75))+
    theme(legend.position = "bottom", 
          panel.background = NULL,
          #text=element_text(size=11),
          #panel.border =NULL
          plot.margin=margin(t = .5, r = .5, b = .5, l = .5, unit = "cm")
    ) +theme_hc() +
    guides(linetype = FALSE,
           fill = guide_legend(""),
           col = guide_legend("")) +
    xlab(expression(log(NO[3]^"-")))+
    ylab("Frequency")+
    scale_color_hc(labels = c("Observed", "Predicted"))+
    scale_fill_hc(labels = c("Observed", "Predicted"))
  
  
  ident_den <- 
    df_gen_pred |>
    pivot_longer(
      cols = c(
        #leach_a_obs,
        #leach_xbt_sim_ind,
        #leach_rf_full_ind,
        #leach_lin_ind
        pred_xbt_sim_ind_max,
        meancon
      ),
      values_to = "est",
      names_to = "Measurement"
    ) |>
    ggplot(aes(
      x = est,
      fill = Measurement,
      col = Measurement
    )) +
    geom_density(stat="density", position="identity", alpha=0.4)+
    #geom_step(aes(y = ..y..), stat = "ecdf") +
    #scale_x_continuous(breaks = c(-2.5, 5.5)) +
    scale_x_continuous(limits = c(0,120))+
    theme(legend.position = "bottom", 
          panel.background = NULL,
          text=#element_text(size=12),
          #panel.border =NULL
          plot.margin=margin(t = .5, r = .5, b = .5, l = .5, unit = "cm")
    ) + 
    theme_hc() +
    guides(linetype = FALSE,
           fill = guide_legend(""),
           col = guide_legend("")) +
    xlab(expression(NO[3]^"-"))+
    ylab("Frequency")+
    scale_color_hc(labels = c("Observed", "Predicted"))+
    scale_fill_hc(labels = c("Observed", "Predicted"))
  
  ggsave(
    plot= ggarrange(log_den, ident_den, ncol=1, #, 
            common.legend = T,  legend="bottom"),
    "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/Nret24_Nitrate/Figures/fig3_density.jpeg",
    width = 90,
    height = 110,
    units = "mm"
  )           
  
  # 4.3.2 Marginal effects ----

# Relative contribution of predictors ----  
  fitt_xgb_max <- readRDS("fitt_xgb_max.RDS")
  
  summary(fitt_xgb_max)
  
plot(fitt_xgb_max)
  
  plot(caret::varImp(fitt_xgb_max))
  
  library(xgboost)
  
  importance <-
    xgb.importance(model = fitt_xgb_max$finalModel) |> 
    as.data.frame() |> 
    mutate(Predictor=sub("^(Main_nles5).*", "\\1", Feature)) |>
    mutate(Predictor=sub("^(Winter_nles5).*", "\\1", Predictor)) |>
    mutate(Predictor=sub("^(clay_cat).*", "\\1", Predictor)) |> 
    group_by(Predictor) |> 
    summarise(Contribution=sum(Gain)*100) |> 
    arrange(-Contribution) |> mutate(
      predictor_name=c("Monthly daily Temp.",
                       "Cumulative Ppt",
                       "Main Crop",
                       "Mineral N",
                       "Winter Crop",
                       "Month",
                       "Soil clay"), xlab="")
  importance$predictor_name <- reorder(importance$predictor_name, 
                                       importance$Contribution)
  importance$predictor_name <- factor(
    importance$predictor_name, levels = rev(levels(importance$predictor_name)))
  
  
  importance |> ggplot(aes(x=reorder(predictor_name, Contribution), y=Contribution))+
    geom_col(width=0.3)+
    coord_flip()+theme_hc()+
    labs(x='Predictor',
         y="Marginal Contribution (%)")

  contr_plot <- importance |> ggplot(
    aes(y= Contribution, 
        x= xlab,#reorder(predictor_name, Contribution), 
        fill=reorder(predictor_name, Contribution)))+
    geom_bar(stat = "identity", position = "stack")+
    geom_text(aes(label = round(Contribution, 1)), 
              position = position_stack(vjust = 0.5), 
              size = 4,
              col="white") + 
    coord_flip()+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_fill_ptol(name="")+
    labs(
      x='',
      y="Predictor Contribution (%)")+
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, 
                               legend.text.position = "right", 
                               reverse = TRUE))
  
  colorblindr::cvd_grid(contr_plot)
  
# Marginal effects ----
  
# see marginal effects script 
  
  