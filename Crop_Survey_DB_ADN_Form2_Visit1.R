##############################################################################
# title         : Crop_Survey_DB_Form2_Visit1.R;
# purpose       : Format crop health survey data for insertion and insert into relational database;
# producer      : prepared by A. Sparks;
# editor        : edited by M. Noel;
# last update   : in Towoomba, QLD, AUS, July 2016;
# inputs        : crop health survey form2;
# outputs       : crop health survey data ready for analysis;
##############################################################################

dsn_raw <- list.files("~/Google Drive/Data/RICE-PRE/RICE-PRE_2015DS/ADN_2015DS",
                      full.names = TRUE)

wb <- "/Users/U8004755/Google Drive/Data/RICE-PRE/RICE-PRE_2015DS/ADN_2015DS/Primitive R1.xls"
dsn_cleaned <- "~/Google Drive/Data/RICE-PRE/Cleaned/"
source("Extract_WB.R")

# Libraries --------------------------------------------------------------------

library(foreach)
library(parallel)
library(iterators)
library(readr)

# Set up workspace -------------------------------------------------------------
f <- NULL
v <- NULL

cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cl)

# Loop for all files in dsn_raw ------------------------------------------------
itw <- iter(dsn_raw)
foreach(f = itw, .packages = c("XLConnect", "iterators", "foreach",
                               "readr")) %dopar% {
  wb <- loadWorkbook(paste0(f))
  injuries <- vector(mode = "list")
}

parallel::stopCluster(cl)

write.csv(data.table::rbindlist(hq_injuries_out),
          paste0("hill_quadrat_injuries_", filename, ".csv"))
write.csv(data.table::rbindlist(systemic_out),
          paste0("systemic_injuries_", filename, ".csv"))
write.csv(data.table::rbindlist(weed_species_out),
          paste0("weed_species_", filename, ".csv"))
write.csv(data.table::rbindlist(weed_rank_out),
          paste0("weed_rank_", filename, ".csv"))
write.csv(data.table::rbindlist(weed_area_out),
          paste0("weed_area_", filename, ".csv"))

# eos
