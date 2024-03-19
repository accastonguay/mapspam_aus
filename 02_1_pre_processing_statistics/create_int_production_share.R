library(terra)
library(rnaturalearth)
library(dplyr)
library(stringr)


alldf <- data.frame()
for (i in 1:7){
  
  irri <- read_xls("C:/Temp/mapspamc_db/subnational_statistics/AUS/46180do010_200708.xls", sheet = paste0('Table_',i), skip = 4)

  state <- names(irri)[2]
  print(state)
  names(irri) <- str_replace_all(irri[1,], ' ', "_")
  irri <- irri[-1,1:5]
  
  colnames(irri)[1] <- "Crop"
  
  irri_state <- irri %>% 
    filter(Crop %in% c("Rice", 
                       "Other cereals for grain or seed",
                       "Cotton",
                      "Sugar cane",
                       "Other broadacre crops",
                       "Fruit trees, nut trees, plantation or berry fruits",
                       "Vegetables for human consumption or seed",
                       "Nurseries, cut flowers and cultivated turf",
                       "Grapevines")) %>%
    mutate(Area_irrigated = as.numeric(Area_irrigated),
           Area_under_pasture_or_crop = as.numeric(Area_under_pasture_or_crop),
           Area_irrigated = ifelse(Area_irrigated > Area_under_pasture_or_crop, Area_under_pasture_or_crop, Area_irrigated),
           state = state) %>%
    select(state, Crop, Area_under_pasture_or_crop, Area_irrigated)
  
  alldf <- rbind(alldf, irri_state)
}

alldf


alldf <- alldf %>%
  mutate(Crop = recode(Crop, 'Grapevines' = 'Fruit trees, nut trees, plantation or berry fruits')) %>%
  group_by(state, Crop) %>%
  summarise(Area = sum(Area_under_pasture_or_crop, na.rm = TRUE),
            Area_irrigated = sum(Area_irrigated, na.rm = TRUE),
            I =Area_irrigated/ Area)
     


abs_spam <- read.csv("C:/Temp/mapspamc_aus/mappings/absirri2spam.csv")

area_harv <- read.csv("C:/Temp/mapspamc_db/subnational_statistics/AUS/subnational_harvested_area_2010_AUS.csv")    %>%
  filter(adm_level == 1) %>%
  gather(crop, area, -c(adm_name, adm_code, adm_level))


area_harv2 <- abs_spam %>%
  left_join(area_harv, by = "crop") %>%
  mutate(area = ifelse(area < 0, 0, area)) %>%
  group_by(adm_name, adm_code, adm_level, abs_crops) %>%
  mutate(faction = area / sum(area)) %>%
  left_join(alldf, by = c("adm_name" = "state", 'abs_crops' = 'Crop'))

ps_irri_country <- area_harv2 %>%
  group_by(crop) %>%
  summarise(I = sum(faction * Area_irrigated, na.rm = TRUE) / sum(faction*Area , na.rm = TRUE)) %>%
  mutate(adm_name = 'Australia',
         adm_code = 'AU0',
         adm_level = 0) %>%
  pivot_wider(names_from = crop, values_from = I, id_cols = c(adm_code, adm_name, adm_level))

ps_irri_state <- area_harv2 %>%
  pivot_wider(names_from = crop, values_from =  I, id_cols = c(adm_name, adm_code, adm_level), values_fill = 0) 


totaldf <- bind_rows(ps_irri_country, ps_irri_state) 




prodshare <- totaldf %>%
  select(adm_code, adm_name, adm_level) %>%
  tidyr::expand_grid(system = c("S", 
                                "L", "H", "I")) %>%
  left_join(totaldf %>% mutate(system = 'I'),
            by = c("system",  "adm_code", "adm_name", "adm_level")) %>%
  group_by(adm_code, adm_name,   adm_level) %>%
  mutate(across(bana:yams,  ~case_when(
    system == 'H' ~ 1-sum(.x, na.rm = TRUE),
    system == 'I' & .x > 0 ~ .x,
    TRUE ~ 0) 
    )) %>% as.data.frame()


write.csv(prodshare, "C:/Temp/mapspamc_db/subnational_statistics/prod_share2.csv", row.names = F)
# 
# 
# # 
# #   ungroup() %>%
# #     as.data.frame() %>%
# #   filter(adm_name == 'Victoria',
# #          str_detect(abs_crops    , 'Fruit trees'))
# 
# area_harv %>%
#   group_by(adm_name ) %>%
#   summarise(area = sum(area)*1e-6)
# 
# 
# countrydf <- alldf %>% 
#   group_by(Crop) %>%
#   summarise(area = sum( as.numeric(Area_irrigated), na.rm = TRUE),
#             I = sum( as.numeric(Area_irrigated), na.rm = TRUE)/sum( as.numeric(Area_under_pasture_or_crop), na.rm = TRUE))
# 
# # names(irri) <- ``, `Agricultural businesses`,  `Agricultural businesses irrigating`,  `Area under pasture or crop`,  `Area irrigated`
# 
# 
# # library(R.utils)
# 
# # files <- list.files("C:/Users/CHA780/OneDrive - CSIRO/Documents/mapspamdata",
# #                     full.names = T)
# # 
# # for (f in files){
# #   gunzip(f, remove=FALSE)
# # }
# 
# aus <- ne_states(country = 'Australia')
# 
# 
# 
# # irri <- rast("C:/Users/CHA780/Downloads/G_AEI_2000.asc")
# 
# # c_irri <- crop(irri, aus)
# 
# 
# ############ Irrigated areas #############
# 
# asc_files <- list.files("C:/Users/CHA780/OneDrive - CSIRO/Documents/mapspamdata",
#                     full.names = T, pattern = '.ASC$')
# 
# ittir <- rast(asc_files) %>%
#   crop(aus)
# 
# c('1' = 'Wheat', '2' = 'Maize')
# 
# names(ittir)
# 
# 
# replace_vec <- c(
#   "1" = "Wheat_", 
#   "2" = "Maize_", 
#   "3" = "Rice_", 
#   "4" = "Barley_", 
#   "5" = "Rye_", 
#   "6" = "Millet_", 
#   "7" = "Sorghum_", 
#   "8" = "Soybeans_", 
#   "9" = "Sunflower_", 
#   "10" = "Potatoes_", 
#   "11" = "Cassava_", 
#   "12" = "Sugar cane_", 
#   "13" = "Sugar beets_", 
#   "14" = "Oil palm_", 
#   "15" = "Rape seed / Canola_", 
#   "16" = "Groundnuts / Peanuts_", 
#   "17" = "Pulses_", 
#   "18" = "Citrus_", 
#   "19" = "Date palm_", 
#   "20" = "Grapes / Vine_", 
#   "21" = "Cotton_", 
#   "22" = "Cocoa_", 
#   "23" = "Coffee_", 
#   "24" = "Others perennial_", 
#   "25" = "Fodder grasses_",
#   "26" = "Others annual_")
# 
# names(replace_vec) <- paste0('CROP', names(replace_vec), '_')
# 
# names(ittir) <- str_replace_all(names(ittir), replace_vec) %>%
#   str_remove_all("ANNUAL_AREA_HARVESTED_|_HA")
# 
# states <- rasterize(aus_states, ittir, field = 'diss_me')
# 
# total_irri <- c(ittir, states)
# 
# ittir_df <- as.data.frame(total_irri, cells = TRUE, xy=TRUE) %>%
#   filter(!is.na(diss_me))
# 
# 
# 
#   
# 
# mirca2crop <- read.csv("C:/Temp/mapspamc_aus/mappings/mirca2crop.csv") %>%
#   mutate(mirca = str_trim(mirca))
# spam <- as.character(mirca2crop$spam)
# names(spam) <- mirca2crop$mirca 
# 
# 
# names(ittir_df) <- str_replace_all(names(ittir_df), spam)
# 
# 
# ittir_df2 <- ittir_df %>%
#   # rename(spam) %>%
#   select(matches(paste(names(ha_stat)[-3:0], collapse  = '|'))) %>%
#   head(2)
# 
# irri_long <- ittir_df %>%
#   pivot_longer(cols = -c(cell, x, y, diss_me),
#                names_to = c("Irri", 'crop'),
#                names_sep = "_",
#                values_to = 'area') %>%
#   pivot_wider(names_from = Irri, values_from = area )
# 
# 
# 
# 
# ###############  Fertiliser ####################
# 
# e2s <- read.csv("C:/Temp/mapspamc_aus/mappings/earthstat2spam.csv")
# e2sv <- as.character(e2s$spam)
# names(e2sv) <- e2s$earthstat
# 
# 
# 
# 
# dat.files  <- list.files(path="C:/Users/CHA780/Downloads/FertilizerCropSpecific_Geotiff/FertilizerCropSpecific_Geotiff",
#                          recursive=T,
#                          pattern="*NitrogenApplication_Rate.tif$"
#                          ,full.names=T)
# 
# fert <- rast(dat.files) %>%
#   crop(aus)
# 
# names_fert <- names(fert) %>%
#   str_remove('_NitrogenApplication_Rate') 
# # %>% as.data.frame()
# 
# 
# names(fert) <- str_replace_all(names(fert), e2sv)
# 
# total_fert <- c(fert, states)
# 
# 
# fertdf <- as.data.frame(total_fert, cells = TRUE, xy=TRUE) %>%
#   filter(!is.na(diss_me)) %>%
#   pivot_longer(cols = -c(cell, x, y, diss_me),
#                names_to =  'crop',
#                values_to = 'N_application') %>%
#   mutate(crop = str_remove(crop, "_NitrogenApplication_Rate"))
#   
# 
# ################ Production ####################
# dat.files_prod  <- list.files(path="C:/Users/CHA780/Downloads/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_Geotiff/GeoTiff",
#                               recursive=T,
#                               pattern="*_Production.tif$"
#                               ,full.names=T)%>%
#   str_subset(paste0(names_fert, collapse = '|'))
# 
# prod_r <- rast(dat.files_prod)%>%
#   crop(states) %>%
#   c(states) %>%
#   as.data.frame(cells = TRUE, xy=TRUE) %>%
#   filter(!is.na(diss_me))%>%
#   pivot_longer(cols = -c(cell, x, y, diss_me),
#                names_to =  'crop',
#                values_to = 'production') %>%
#   mutate(crop = str_remove(crop, "_Production") %>%
#            str_replace_all(e2sv))
# 
# ################# Combine all files ####################
# 
# all_files <- irri_long %>%
#   left_join(fertdf, by = c('cell',
#                            # 'x', 'y', 
#                            'diss_me', 'crop')) %>%
#   left_join(prod_r, by = c('cell', 
#                            # 'x', 'y',
#                            'diss_me', 'crop'))
#   
# all_files2 <- all_files %>% 
#   mutate(intensity = case_when(
#     IRC > 0 ~ 'I',
#     IRC  == 0 & N_application > 1 ~ 'H',
#     TRUE ~ 'L'
#   )) %>%
#   group_by(crop, diss_me, intensity) %>%
#   summarise(sum_int = sum(production, na.rm = TRUE)) %>%
#   group_by(crop, diss_me) %>%
#   mutate(total = sum(sum_int, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(share = sum_int/total) %>%
#   left_join(aus_states %>% as.data.frame() %>% select(diss_me, name)) %>%
#   select(-diss_me ) %>%
#   arrange(name, crop   ) %>%
#   group_by(name, crop) %>% 
#   group_modify(~ add_row(.x,.before=0))%>%
#   mutate(intensity = ifelse(is.na(intensity), 'S', intensity),
#          share = ifelse(sum(share, na.rm = TRUE) == 0 & intensity == 'L', 1, share ),
#          share = ifelse(is.na(share),0, share)) %>%
#   # filter(name == 'New South Wales') %>%
#   select(-c(sum_int, total )) %>%
#   pivot_wider(names_from = crop, values_from = share, values_fill = 0)
# 
# 
# country_level <- all_files %>% 
#   mutate(intensity = case_when(
#     IRC > 0 ~ 'I',
#     IRC  == 0 & N_application > 1 ~ 'H',
#     TRUE ~ 'L'
#   )) %>%
#   mutate(diss_me = 'Australia') %>%
#   group_by(crop, diss_me, intensity) %>%
#   summarise(sum_int = sum(production, na.rm = TRUE)) %>%
#   group_by(crop, diss_me) %>%
#   mutate(total = sum(sum_int, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(share = sum_int/total) %>%
#   # left_join(aus_states %>% as.data.frame() %>% select(diss_me, name)) %>%
#   mutate(name = diss_me) %>%
#   select(-diss_me ) %>%
#   arrange(name, crop   ) %>%
#   group_by(name, crop) %>% 
#   group_modify(~ add_row(.x,.before=0))%>%
#   mutate(intensity = ifelse(is.na(intensity), 'S', intensity),
#          share = ifelse(sum(share, na.rm = TRUE) == 0 & intensity == 'L', 1, share ),
#          share = ifelse(is.na(share),0, share)) %>%
#   # filter(name == 'New South Wales') %>%
#   select(-c(sum_int, total )) %>%
#   pivot_wider(names_from = crop, values_from = share, values_fill = 0)
# 
# 
# write.csv(bind_rows(country_level, all_files2), "C:/Temp/mapspamc_db/subnational_statistics/prod_share.csv", row.names = F)
# 
# 
# all_files$N_application %>% summary()
