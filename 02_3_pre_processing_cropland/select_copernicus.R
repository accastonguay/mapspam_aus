#'========================================================================================================================================
#' Project:  mapspamc
#' Subject:  Code to select COPERNICUS cropland map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("01_model_setup/01_model_setup.r"))


# LOAD DATA ------------------------------------------------------------------------------
load_data(c("adm_map", "grid"), param)


# PROCESS --------------------------------------------------------------------------------
temp_path <- file.path(param$model_path, glue("processed_data/maps/cropland/{param$res}"))
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

# Warp and mask
# If needed change the year of the cropland map
# input <- file.path(param$db_path, glue("copernicus/copernicus_{param$year}.tif"))
input <- file.path(param$db_path, glue("copernicus/copernicus_2015.tif"))

output <- align_raster(input, grid, adm_map$geometry, method = "bilinear")
names(output) <- "copernicus"
plot(output)
writeRaster(output, file.path(temp_path, glue("cropland_copernicus_{param$res}_{param$year}_{param$iso3c}.tif")),
            overwrite = TRUE)


# CLEAN UP -------------------------------------------------------------------------------
rm(input, output, grid, adm_map, temp_path)

