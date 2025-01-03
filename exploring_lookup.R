#explore in  lookup table

dont_lookup <- read.table(
  "C:/Users/au710823/OneDrive - Aarhus universitet/NLESS2022Fran/NLESSdata/Nudvask/Nudvask/Nret24_FGK/lookup_02_xbt.txt", 
  header=TRUE, sep="\t")

head(dont_lookup)

dont_lookup|> sample_frac(0.01) |>
  ggplot(aes(x = as.numeric(AirTemp_ave_month), 
             y = as.numeric(Precip_cumsumhy), z = as.numeric(pred))) +
  geom_contour()+
  theme_hc()+
  scale_color_hc()

dont_lookup|> sample_frac(0.01) |>
  ggplot(aes(x = as.numeric(N_mineral_spring), y = as.numeric(pred))) +
  geom_point()+
  geom_smooth(method = "lm")

dont_lookup|> sample_frac(0.01) |>
  ggplot(aes(x = as.numeric(month), y = as.numeric(pred), colour = clay_cat)) +
  geom_point()+
  geom_smooth()

