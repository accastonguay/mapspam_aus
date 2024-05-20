#'========================================================================================================================================
#' Project:  mapspamc
#' Subject:  Code to select ESACCI land cover map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("01_model_setup/01_model_setup.r"))


# LOAD DATA ------------------------------------------------------------------------------
load_data(c("adm_map", "grid"), param)


# PROCESS --------------------------------------------------------------------------------
temp_path <- file.path(param$model_path, glue("processed_data/maps/land_cover"))
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

# Warp and mask
# If needed change the year of the land cover map
# we use the ESACCI 2000 map here
input <- file.path(param$db_path, glue("land_cover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))
output <- align_raster(input, grid, adm_map$geometry, method = "near")
names(output) <- "land_cover"
plot(output)
writeRaster(output, file.path(temp_path, glue("land_ cover_{param$res}_{param$year}_{param$iso3c}.tif")),
            overwrite = TRUE)


# CLEAN UP -------------------------------------------------------------------------------
rm(input, output, grid, adm_map, temp_path)
