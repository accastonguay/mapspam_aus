#'========================================================================================
#' Project:  mapspamc
#' Subject:  Aggregate to GLOBIOM simu
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================


# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("01_model_setup/01_model_setup.r"))

# PROCESS --------------------------------------------------------------------------------
library(mapspam2globiom)

# Read land cover map
# Here we use the ESACCI land cover map (create in select_esacci_land_cover.r) because it is available for 2000 but, if available,
# also a national land cover map can be used. Key is to create a mapping from the land cover map classes to the GLOBIOM land cover
# classes - see esacci2globiom for example.
lc <- rast(file.path(param$model_path, glue("processed_data/maps/land_cover/land_ cover_{param$res}_{param$year}_{param$iso3c}.tif")))

# Read simu map from mapspamc_db
simu <- st_read(file.path(param$db_path, glue("simu/simu.shp")))

# Read grid map created by mapspamc
grid <- rast(file.path(param$model_path, glue("processed_data/maps/grid/{param$res}/grid_{param$res}_{param$year}_{param$iso3c}.tif")))

# Select country simu polygon
simu <- simu |>
  filter(iso3c == param$iso3c)
plot(simu$geometry)

# Create land cover mapping
# We use the esacci2globion data.frame stored in the package.
lc_map <- esacci2globiom

# Create crop mapping
# We use the standard crop2globiom data.frame in the package.
crop_map <- crop2globiom

# Create GLOBIOM input. Two files will be created.One with land cover information and one with crop distribution information.
# All classes are aggregated to GLOBIOM crop and land cover classes and an iterative procedure is used to harmonize the
# land cover and crop distributions maps. See vignette and function help for the details.
# Not that the area will be expressed in 1000 ha, which is common in GLOBIOM!
create_globiom_input(lc_map, crop_map, lc, simu, grid, param)
