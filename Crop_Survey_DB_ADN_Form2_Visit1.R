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

  # Extract data ---------------------------------------------------------------

  extracted <- extract(wb)

  itx <- iter(1:5)
  injuries <- foreach(v = itx, .packages = "plyr") %do% {
    injuries[[v]] <- join_all(extracted[[v]], type = "full")
  }

  write_csv(injuries[[1]], paste0(dsn_cleaned, "hq_injuries.csv"),
            append = TRUE)
  write_csv(injuries[[2]], paste0(dsn_cleaned, "weed_area.csv"), append = TRUE)
  write_csv(injuries[[3]], paste0(dsn_cleaned, "weed_rank.csv"), append = TRUE)
  write_csv(injuries[[4]], paste0(dsn_cleaned, "weed_spp.csv"), append = TRUE)
  write_csv(injuries[[5]], paste0(dsn_cleaned, "systemic_injuries.csv"),
            append = TRUE)
}

parallel::stopCluster(cl)

# eos
