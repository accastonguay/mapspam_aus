#'========================================================================================
#' Project:  mapspamc
#' Subject:  Code to process urban extent maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================

# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("C:/Temp/mapspamc_aus", "01_model_setup/01_model_setup.r"))


# LOAD DATA ------------------------------------------------------------------------------
load_data(c("adm_map"), param)


# PROCESS --------------------------------------------------------------------------------
# Note that glh_smod presents annual maps. Please select the map that is closest to your
# target year.

# Create country polygon with clumps of urban areas
input <- file.path(param$db_path, glue("ghs_smod/ghs_smod_{param$year}.tif"))
output <- crop(rast(input), vect(adm_map$geometry), mask = TRUE)
output <- patches(output, directions = 8, zeroAsNA = TRUE)
output <- st_as_sf(as.polygons(output))
plot(output$geometry)


# SAVE -----------------------------------------------------------------------------------
temp_path <- file.path(param$model_path, glue("processed_data/maps/population/{param$res}"))
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
saveRDS(output, file.path(temp_path, glue("urb_{param$year}_{param$iso3c}.rds")))


# CLEAN UP -------------------------------------------------------------------------------
rm(input, output, adm_map, temp_path)
