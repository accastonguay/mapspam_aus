#'========================================================================================
#' Project:  mapspamc
#' Subject:  Script to process raw subnational statistics
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================


# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("C:/Temp/mapspamc_aus","01_model_setup/01_model_setup.r"))

# In this script the raw subnational statistics are processed. In order to prevent errors
# when running the model, it is essential to put the statistics in the right
# format and make sure they are consistent (e.g. total area at national level is equal to
# that of subnational area).

# The user can prepare the data in Excel or use R. mapspamc offers a function to create a
# template for the data. It also providers several support functions to check for consistency
# and modify where needed. See the package documentation for more information.


# LOAD DATA ------------------------------------------------------------------------------
aus_stats <- read.csv('C:/Temp/mapspamc_db/subnational_statistics/AUS/T_yield_2000_ASGS_SA211.csv') %>%
  filter(!SPREAD_Commodity %in% c('Eggs' , "Beef Cattle", "Dairy Cattle", "Sheep", "Hay"))

iso3c_shp <- "SA2_2011_AUST.shp"

# load shapefile
adm_map_raw <- sf::read_sf(file.path(param$db_path, glue("adm/{param$iso3c}/{iso3c_shp}")))
abs2crop <- read.csv("C:/Temp/mapspamc_aus/mappings/ABS2crop.csv")

stats_df <- abs2crop %>% 
  left_join(aus_stats, by = "Commodity") %>%
  filter(!is.na(crop))%>%
    # distinct(crop ) %>%
  mutate(SA2_MAIN11 = as.character(SA2_ID)) %>%
  left_join(as.data.frame(adm_map_raw), by = "SA2_MAIN11") %>%
  select(SA2_MAIN11, Commodity, crop , ha_ASGS , STE_NAME11, STE_CODE11)

adm_list <- read.csv("C:/Temp/mapspamc_aus/processed_data/lists/adm_list_2010_AUS.csv")

adm_list_wide <- dplyr::bind_rows(adm_list %>% dplyr::select_at(vars(contains("adm0"))) %>% 
                                    setNames(c("adm_name", "adm_code")) %>% dplyr::mutate(adm_level = 0) %>% 
                                    unique(), adm_list %>% dplyr::select_at(vars(contains("adm1"))) %>% 
                                    setNames(c("adm_name", "adm_code")) %>% dplyr::mutate(adm_level = 1) %>% 
                                    unique(), adm_list %>% dplyr::select_at(vars(contains("adm2"))) %>% 
                                    setNames(c("adm_name", "adm_code")) %>% dplyr::mutate(adm_level = 2) %>% 
                                    unique())

# ha_template <- adm_list_wide %>% dplyr::filter(adm_level %in%
#                                                  c(0:param$adm_level))
# ha_template[, crop$crop] <- NA

stats_dfadm2 <- stats_df %>%
  select(SA2_MAIN11, crop, ha_ASGS) %>%
  mutate(adm_level = 2) %>%
  
  group_by(SA2_MAIN11,adm_level, crop) %>%
  summarize(ha_ASGS = sum(ha_ASGS, na.rm = TRUE))%>%
  ungroup()%>%
  pivot_wider(values_fill = -999, names_from = crop , values_from = ha_ASGS, id_cols = c(SA2_MAIN11, adm_level)) %>% 
  rename(adm_code = SA2_MAIN11)

stats_dfadm1 <- stats_df %>%
  select(STE_CODE11, crop, ha_ASGS) %>%
  mutate(adm_level = 1) %>%
  group_by(STE_CODE11,
           adm_level = 1,  crop) %>%
  summarize(ha_ASGS = sum(ha_ASGS, na.rm = TRUE))%>%
  ungroup()%>%
  pivot_wider(values_fill = -999, names_from = crop , values_from = ha_ASGS, id_cols = c(adm_level, STE_CODE11)) %>% 
  rename(adm_code = STE_CODE11)

stats_dfadm0 <- stats_df %>%
  select(STE_CODE11, crop, ha_ASGS) %>%
  mutate(adm_code  = as.character(0),
         adm_level = 0) %>%
  group_by(adm_code , adm_level, crop) %>%
  summarize(ha_ASGS = sum(ha_ASGS, na.rm = TRUE))%>%
  ungroup()%>%
  pivot_wider(values_fill = -999, names_from = crop , values_from = ha_ASGS, id_cols = c(adm_code ,adm_level))

all_ha <- bind_rows(stats_dfadm2, stats_dfadm1, stats_dfadm0) %>%
  mutate(adm_code = paste0('AU', adm_code))

final_ha_stat <- adm_list_wide %>%
  left_join(all_ha, by = c("adm_code", "adm_level"))

### Cropping intensity

crop <- read.csv("C:/Temp/mapspamc_aus/mappings/crop.csv")

ci_template <- adm_list_wide %>% dplyr::filter(adm_level %in% 
                                                 c(0:1))
ci_template <- tidyr::expand_grid(ci_template, system = c("S", 
                                                          "L", "H", "I")) %>% dplyr::select(adm_name, adm_code, 
                                                                                            adm_level, system, everything())
ci_template[, crop$crop] <- 2

### Production share

prodshare <- read.csv("C:/Temp/mapspamc_db/subnational_statistics/prod_share2.csv")

ps_template <- adm_list_wide %>% 
  dplyr::filter(adm_level %in%  c(0:1))

ps_template <- tidyr::expand_grid(ps_template, system = c("S", 
                                                          "L", "H", "I")) %>% dplyr::select(adm_name, adm_code, 
                                                                                            adm_level, system, everything())

ps_stat <- ps_template %>%
  left_join(prodshare, by = c('adm_name' ,'adm_level', 'adm_code', 'system' ))

# PREPARE STAT ---------------------------------------------------------------------------

# To create the templates use the following commands
# final_ha_stat <- create_statistics_template("ha", param)
# ps_stat <- create_statistics_template("ps", param)
# ci_stat <- create_statistics_template("ci", param)


# SAVE -----------------------------------------------------------------------------------
write_csv(final_ha_stat, file.path(param$db_path,
  glue("subnational_statistics/{param$iso3c}/subnational_harvested_area_{param$year}_{param$iso3c}.csv")))
write_csv(ci_template, file.path(param$db_path,
  glue("subnational_statistics/{param$iso3c}/cropping_intensity_{param$year}_{param$iso3c}.csv")))
write_csv(ps_stat, file.path(param$db_path,
  glue("subnational_statistics/{param$iso3c}/production_system_shares_{param$year}_{param$iso3c}.csv")))


# CLEAN UP -------------------------------------------------------------------------------
rm(final_ha_stat, ps_stat, ci_stat)


# NOTE -----------------------------------------------------------------------------------
# As you probably created a lot of objects in he R memory, we recommend to
# restart R at this moment and start fresh. This can be done easily in RStudio by
# pressing CTRL/CMD + SHIFT + F10.
