#'========================================================================================
#' Project:  mapspamc
#' Subject:  Script to run the model
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================

# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("C:/Temp/mapspamc_aus","01_model_setup/01_model_setup.r"))

# param <- mapspamc_par(
#   model_path = model_path,
#   db_path = db_path,
#   gams_path = gams_path,
#   iso3c = "AUS",
#   year = 2010,
#   res = "5min",
#   adm_level = 2,
#   solve_level = 0,
#   model = "min_entropy")
# RUN MODEL -----------------------------------------------------------------------------
# Select solver for each model and use tictoc to show processing time.
tic()
if(param$model == "min_entropy"){
  run_mapspamc(param, solver = "IPOPT")
} else {
  run_mapspamc(param, solver = "CPLEX")
}
toc()


# COMBINE ADM1 RESULTS ------------------------------------------------------------------
combine_results(param)


# INSPECT RESULTS ------------------------------------------------------------------------
view_results("rice", var = "ha", param)
view_results("maiz", var = "ha", param)
