# script to create plots and outputs for manuscript from script [GIS pH analysis (all crops) v4.R]

# abatement map for UK
Dat_summ1 <- Dat_main %>%
  group_by(x, y) %>%
  summarise(Abatement = sum(Abatement)) %>%
  mutate(Abatement_fac = cut(Abatement,
                             breaks = c(-10000, -1000, -100, -10, 10, 100, 1000, 100000),
                             labels = c("Net emissions, > 1 kilotonne",
                                        "Net emissions, > 100 tonnes",
                                        "Net emissions, > 10 tonnes",
                                        "Net neutral effect",
                                        "Net abatement, > 10 tonnes",
                                        "Net abatement, > 100 tonnes",
                                        "Net abatement, > 1 kilotonne")))

levels(Dat_summ1$Abatement_fac)
qplot(Dat_summ1$Abatement_fac)

Dat_summ1 %>% filter(is.na(Abatement_fac)) %>% nrow()

ggplot() +
  geom_raster(data = Dat_summ1 %>% mutate(Abatement = ifelse(Abatement > 2500, 2500, Abatement)), aes(x = x, y = y, fill = Abatement)) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  xlim(c(-10, 5)) +
  ylim(c(49, 62)) +
  scale_fill_gradient2(low = "darkred", mid = "lightgrey", high = "darkgreen") +
  labs(fill = "Abatement\npotential\n(tonnes)") +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/Abatement map UK.png", width = 8, height = 7)

ggplot() +
  geom_raster(data = Dat_summ1, aes(x = x, y = y, fill = Abatement_fac), alpha = 0.7) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  scale_fill_brewer(type = "div", palette = "RdBu") +
  labs(fill = expression("AP (CO"[2]*"-eq)")) +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/Abatement map UK discrete.png", width = 8, height = 7)

# abatement map per hectare for UK
Dat_main %>%
  group_by(x, y) %>%
  summarise(Abatement = sum(Abatement),
            Area_ha = sum(Area_ha),
            Abatement_ha = Abatement / Area_ha) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = Abatement_ha), alpha = 0.7) +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  scale_fill_gradient2(low = "darkred", mid = "lightgrey", high = "darkgreen") +
  labs(fill = expression("Abatement\npotential\n(tonnes CO"[2]*"-eq ha"^{-1}*")")) +
  coord_quickmap() +
  theme_void()
#ggsave("Output plots/Abatement map UK per ha.png", width = 8, height = 7)

# MAC map for UK
Dat_summ2 <- Dat_main %>%
  filter(GHG_balance <= -0.1) %>% # only measures with reliable mitigation (i.e. < 100 kg CO2-eq / ha / year) included
  group_by(x, y) %>%
  summarise(Tot_cost = sum(Cost_net),
            Abatement = sum(Abatement)) %>%
  ungroup() %>%
  mutate(MAC = Tot_cost / Abatement) %>%
  filter(MAC >= quantile(MAC, 0.05),
         MAC <= quantile(MAC, 0.95)) # filter out divide-by-nearly-zero mad numbers
qplot(Dat_summ2$MAC)

Dat_summ2$MAC %>% summary()

ggplot() +
  geom_raster(data = Dat_summ2, aes(x = x, y = y, fill = MAC)) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  #scale_fill_gradient(high = "red", low = "darkgreen") +
  labs(fill = expression('Marginal\nabatement\ncost\n(£ tonne CO'[2]*'-eq'^{-1}*')')) +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/MAC map UK.png", width = 8, height = 7)

# crop map for UK
Dat_summ3 <- Dat_main %>%
  group_by(x, y) %>%
  mutate(Dom_crop = Area_ha == max(Area_ha)) %>%
  filter(Dom_crop == T) %>%
  dplyr::select(x, y, Crop)

Dat_summ3$Crop[834] <- "Oil crops, other" # cheeky hack to stop this getting dropped altogether - now the colour palette matches the MACCs

qplot(Dat_summ3$Crop)
ggplot() +
  geom_raster(data = Dat_summ3, aes(x = x, y = y, fill = Crop), alpha = 0.7) +
  #geom_polygon(data = UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  coord_quickmap() +
  theme_void()
# ggsave("Output plots/Dominant arable crop map UK.png", width = 8, height = 7)

# how much land area is lost by removing unmatched crops?
Dat_cdf %>%
  filter(Crop %in% Dat_main$Crop == F) %>%
  group_by(Crop) %>%
  summarise(Area_kha = sum(Area_ha) * 10^-3)

# UK full MACC
Dat_main %>%
  filter(GHG_balance <= -0.1) %>% # only measures with reliable mitigation (i.e. < 100 kg CO2-eq / ha / year) included
  dplyr::select(MAC, Abatement, Crop) %>%
  filter(MAC >= quantile(MAC, 0.05),
         MAC <= quantile(MAC, 0.95)) %>%
  arrange(MAC) %>%
  mutate(Abatement = Abatement * 10^-3,
         xmax = cumsum(Abatement),
         xmin = lag(xmax, default = 0),
         ymin = ifelse(MAC < 0, MAC, 0),
         ymax = ifelse(MAC > 0, MAC, 0),
         xav = (xmin + xmax) / 2) %>%
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Crop), colour = NA, alpha = 0.9) +
  scale_fill_brewer(type="qual", palette="Set3", guide=guide_legend(title = NULL)) +
  labs(x = expression('Abatement potential (kt CO'[2]*'eq year'^{-1}*')'),
       y = expression('Marginal abatement cost (GBP tonne CO'[2]*'-eq'^{-1}*')')) +
  theme_classic()
# ggsave("Output plots/UK full MACC.png", width = 8, height = 5)

# devolved administration MACCs
Dat_main %>%
  filter(GHG_balance <= -0.1) %>% # only measures with reliable mitigation (i.e. < 100 kg CO2-eq / ha / year) included
  dplyr::select(MAC, Abatement, Crop, DA) %>%
  filter(MAC >= quantile(MAC, 0.05),
         MAC <= quantile(MAC, 0.95)) %>%
  group_by(DA) %>%
  arrange(MAC) %>%
  mutate(Abatement = Abatement * 10^-3,
         xmax = cumsum(Abatement),
         xmin = lag(xmax, default = 0),
         ymin = ifelse(MAC < 0, MAC, 0),
         ymax = ifelse(MAC > 0, MAC, 0),
         xav = (xmin + xmax) / 2) %>%
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Crop), colour = NA, alpha = 0.9) +
  scale_fill_brewer(type="qual", palette="Set3", guide=guide_legend(title = NULL)) +
  labs(x = expression('Abatement potential (kt CO'[2]*'eq year'^{-1}*')'),
       y = expression('Marginal abatement cost (GBP tonne CO'[2]*'-eq'^{-1}*')')) +
  facet_wrap(~DA, nrow = 2) +
  theme_classic()
# ggsave("Output plots/DA MACCs.png", width = 8, height = 5)

# table 1 (defra liming factors)
DEFRA_LF %>%
  gather(-Soil_type, key = `Land use`, value = `Liming factor`) %>%
  spread(key = Soil_type, value = `Liming factor`) %>%
  mutate(`Land use` = `Land use` %>% str_replace("Liming_factor_", "") %>% first_upper()) %>%
  write_csv("Output plots/Table 1 (Defra_LFs).csv")

# table 1 caption
tibble(abbrev = DEFRA_LF$Soil_type) %>%
  mutate(full1 = abbrev %>%
           str_to_lower() %>%
           str_replace_all("cl", "clay ") %>%
           str_replace_all("sa", "sand ") %>%
           str_replace_all("si", "silt ") %>%
           str_replace_all("lo", "loam ") %>%
           str_replace_all(" $", "") %>%
           str_replace(" ", "y ") %>%
           str_replace_all("clayy", "clayey") %>%
           first_upper(),
         full2 = paste(abbrev, full1, sep = " = ")) %>%
  pull(full2) %>%
  str_c(collapse = "; ")

# yield increase boxplot
Dat_main %>% ggplot(aes(x = Crop, y = Yield_increase, fill = DA)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette = "Set3") +
  ylim(c(1, 1.8)) +
  labs(x = "", y = "Limed yield (fractional)", fill = "") +
  coord_flip() +
  theme_classic()
# ggsave("Output plots/Fractional yield increase.png", width = 8, height = 4)

# yield increase columns
order <- Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  group_by(Crop) %>%
  mutate(Yield_inc_tha = Yield_tha * (Yield_increase - 1)) %>%
  summarise(Yield_inc_t = sum(Yield_inc_tha)) %>%
  arrange(Yield_inc_t) %>%
  pull(Crop)

Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  mutate(Yield_inc_ktha = Yield_tha * (Yield_increase - 1) * 10^-3) %>%
  group_by(Crop, DA) %>%
  summarise(Yield_inc_kt = sum(Yield_inc_ktha)) %>%
  ungroup() %>%
  mutate(Crop = factor(Crop, levels = order)) %>%
  ggplot(aes(x = Crop, y = Yield_inc_kt)) +
  geom_col(aes(fill = DA), position = position_stack()) +
  labs(x = "", y = "Additional crop production (kt)", fill = "") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_classic()
# ggsave("Output plots/Additional crop production.png", width = 8, height = 3)

# table 2
write_csv(Dat_saleval %>% dplyr::select(-Bycrop_ratio), "Output plots/Table 2.csv")

# results descriptives
d1 <- Dat_main %>% pull(Area_ha) %>% sum() * 10^-3 # total area (kha)
print(d1)

d2 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% pull(Area_ha) %>% sum() * 10^-3 # area with abatement
print(d2)

d2 / d1 # area with abatement
Dat_main %>% filter(GHG_balance <= -0.1) %>% pull(Limerate) %>% mean() # average 5-year lime rate
Dat_main %>% filter(GHG_balance <= -0.1) %>% pull(Abatement) %>% sum() * 10^-3 # abatement in kt

# overall yield increase in kt
Dat_main %>%
  mutate(Yield_inc_ktha = Yield_tha * (Yield_increase - 1) * 10^-3,
         Yield_inc_kt = Yield_inc_ktha * Area_ha) %>%
  pull(Yield_inc_kt) %>%
  sum()
# equal to about 362 kg / ha over ENTIRE area


# yield increase in kt only in areas where net abatement is possible
Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  mutate(Yield_inc_ktha = Yield_tha * (Yield_increase - 1) * 10^-3,
         Yield_inc_kt = Yield_inc_ktha * Area_ha) %>%
  pull(Yield_inc_kt) %>%
  sum()

# emissions + abatement
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(GHGmit_SOC = GHGmit_SOC * Area_ha) %>% pull(GHGmit_SOC) %>% sum() * 10^-3 # SCS total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(GHGmit_yield = GHGmit_yield * Area_ha) %>% pull(GHGmit_yield) %>% sum() * 10^-3 # EI reduction total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(Tot_GHG = Tot_GHG * Area_ha) %>% pull(Tot_GHG) %>% sum() * 10^-3 # GHG emissions total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(Limedir_GHG = Limedir_GHG * Area_ha) %>% pull(Limedir_GHG) %>% sum() * 10^-3 # direct lime emissions total
Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(GHGmit_SOC = GHGmit_SOC / Area_ha) %>% pull(GHGmit_SOC) %>% mean()

# area between Hamilton's magic 5—6.7 bracket
d3 <- Dat_main %>% pull(Area_ha) %>% sum() # total area
d4 <- Dat_main %>% filter(pH >= 5, pH <= 6.7) %>% pull(Area_ha) %>% sum()
d4
d4/d3

# costs and benefits
d5 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(x = Area_ha * (Lime_cost + Cont_cost)) %>% pull(x) %>% sum()
d5 / (d2 * 10^3)
d6 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% mutate(x = Area_ha * Crop_revenue_net) %>% pull(x) %>% sum()
d6 / (d2 * 10^3)

d7 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% mutate(x = Area_ha * MAC) %>% pull(x) %>% sum()
d7 / (d2 * 10^3)

Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% pull(MAC) %>% quantile(c(0.025, 0.975))

d8 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% nrow()
d9 <- Dat_main %>% filter(GHG_balance <= -0.1) %>% filter(MAC >= quantile(MAC, 0.05), MAC <= quantile(MAC, 0.95)) %>% filter(MAC <= 66.1) %>% nrow()
d9/d8

Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  filter(MAC <= 66.1) %>%
  mutate(abatement_frac = Abatement / sum(Abatement)) %>%
  group_by(DA) %>%
  summarise(abatement_frac = sum(abatement_frac))

# bar plot showing abatement crops/DAs
order <- Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  group_by(Crop) %>%
  summarise(Area_ha = sum(Area_ha)) %>%
  arrange(Area_ha) %>%
  pull(Crop)

Dat_main %>%
  filter(GHG_balance <= -0.1) %>%
  group_by(DA, Crop) %>%
  summarise(Area_ha = sum(Area_ha) * 10^-3) %>%
  mutate(Crop = factor(Crop, levels = order)) %>%
  ggplot(aes(x = Crop, y = Area_ha, fill = DA)) +
  geom_col(position = position_stack()) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "",
       y = "Area with net abatement available ('000 ha)",
       fill = "") +
  coord_flip() +
  theme_classic()
# ggsave("Output plots/Headline area x crop x DA.png", width = 8, height = 3)