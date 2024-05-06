# packages ----
library(tidyverse)
library(ggpubr)
library(GGally)
library(RColorBrewer)
library(leaps)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

df_gen <- readRDS(
  "O:/Tech_AGRO/Jornaer/Franca/N_conc/Nudvask/Nret24files/df_m_imp.RDS")

# sites and years ----

site_yr_plot <- table(df_gen$site_eng, df_gen$harvest_year) |>
  as.data.frame() |>
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
  # mutate_all(~ na_if( .,0)) |>
  ggplot(aes(x = harvest_year, y = Site, fill = Freq)) +
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

# hitogram monthly conc ----

hist_conc <- df_gen |>
  #mutate(log =log(meancon)) |>
  ggplot() +
  geom_histogram(
    aes(x = meancon),
    fill = "#01a2d9",
    color = "white",
    bins = 30
  ) +
  #geom_histogram(aes(log))+
  #geom_density(aes(y = rel_freq), color = "red", linetype = "dashed") +
  scale_y_continuous(name = "Absolute Frequency") +
  scale_x_continuous(name = "Monthly nitrate conccentration (mg/L)") +
  #labs(title = "Histogram with Relative Frequency and Density Curve") +
  theme_minimal()


# Crops cross ----
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

# Figure 1 ----
ggarrange(site_yr_plot,ggarrange(crops_plot,hist_conc,  widths = c(2, 1)), 
          nrow = 2)

# monthly example ----
df_gen_plot_month <- df_gen |>
  mutate(
    month_hy =
      fct_relevel(
        as.character(month),
        '4','5','6','7','8','9','10','11','12','1','2','3'),
    month_name = month.abb[month],
    clay_plot = fct_recode(
           clay_cat,
           low = "low",
           'midle-high' = "middle",
           'midle-high' = "high"
         )
  ) |>
    filter(site_eng=="\\Foulum\\" &
    Main_nles5 == "1" #winter ceral
    & Winter_nles5 %in% c("2", "4") #bare soil and Cover crops
    #& N_mineral_spring>50
    ) |> 
  mutate(Winter_nles5=dplyr::recode_factor(Winter_nles5,
                                        '2' = 'Bare soil', 
                                        '4'= 'Cover crops') )

conc_month <- df_gen_plot_month |> 
  ggplot(aes(x=reorder(month_name,month),y=meancon))+
  geom_point(color="#01a2d9", size=0.9)+
  geom_smooth(aes(group = clay_plot),color="#01a2d9", alpha=0.2
              )+
  facet_grid(~Winter_nles5)+
  scale_y_continuous(name = "Monthly nitrate conccentration (mg/L)")+
  scale_x_discrete(name = "")


perc_month <- df_gen_plot_month |>
  ggplot(aes(x=reorder(month_name,month),y=as.numeric(afstro_sum_month)))+
  geom_point(color="#014d64", size=0.9)+
  geom_smooth(aes(group = clay_plot),color="#014d64", alpha=0.2
              )+
  scale_y_continuous(name="Monthly percolation (mm)")+
  facet_grid(~Winter_nles5)+
  scale_x_discrete(name = "")

leached_month <- df_gen_plot_month |>
  ggplot(aes(x=reorder(month_name,month),y=as.numeric(meancon*afstro_sum_month/100)))+
  geom_point(color="#7e0021", size=0.9)+
  geom_smooth(aes(group = Winter_nles5),color="#7e0021", alpha=0.2)+
  scale_y_continuous(name="Monthly nitrate leaching Kg/ha")+
  facet_grid(~Winter_nles5)+
  scale_x_discrete(name = "month")


ggarrange(conc_month+rremove("x.text"), perc_month+rremove("x.text"), leached_month, ncol=1)+
  scale_x_discrete(name = "month") #|> 

library(plotly)
ggplotly(conc_month)


# map ----
library(sf)
#install.packages("plotDK")
library(plotDK)

region_dk <- st_as_sf(plotDK::region, coords = c("long","lat")) |> 
  group_by(id) |> 
  summarise() |> 
  st_cast("POLYGON")

ggplot(province_dk) +
  geom_sf()

plotDK()

# read sf file
dk <- read_rds("gadm36_DNK_2_sf.rds")

df_gen_sf <- df_gen  |> 
  st_as_sf(coords = c("X", "Y"), crs = 25832) |> 
  group_by(site_eng)

dk <- st_transform(dk,crs=st_crs(df_gen_sf))

ggplot(dk)+
  geom_sf( color="#b0b0b0") +
  geom_point(data=df_gen, aes(x=X,y=Y
                              #, col = site_eng
                              ))+
  labs(x="Longitude",y= "Latitude")
  
# plot
ggplot(data = dk) +
  geom_sf(dk, aes(fill = NAME_0))+
  geom_point(data = df_gen, aes(x=X, y=Y) ) 
