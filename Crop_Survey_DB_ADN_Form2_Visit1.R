##############################################################################
# title         : Crop_Survey_DB_Form2_Visit1.R;
# purpose       : Format RICE-PRE study data for analysis
# producer      : prepared by A. Sparks;
# last update   : in Towoomba, QLD, AUS, July 2016;
# inputs        : crop health survey form2;
# outputs       : RICE-PRE Experiment data ready for analysis;
##############################################################################

dsn_raw <- list.files("~/Google Drive/Data/RICE-PRE/RICE-PRE_2015DS/Excel Sheets",
                      full.names = TRUE)
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

                                 injuries <- vector(mode = "list")

                                 # Extract data --------------------------------

                                 extracted <- extract(f)


                                 # Join the five types of injuries from each
                                 # location into five respective data frames
                                 # from a list of data frames for each injury
                                 itx <- iter(1:5)

                                 injuries <- foreach(v = itx,
                                                     .packages =
                                                       c("plyr", "iotools"))

                                 %do% {injuries[[v]] <- join_all(extracted[[v]],
                                                                  type = "full")
                                 }

                                 # add names to columns ------------------------

                                 names(injuries[[1]]) <- c("location",
                                                           "visit_date",
                                                           "visit_no",
                                                           "field_no",
                                                           "water_status",
                                                           "crop_stage",
                                                           "rat", "silvershoot",
                                                           "whitehead",
                                                           "deadheart",
                                                           "leaffolder",
                                                           "leaf_miner",
                                                           "rice_hispa",
                                                           "whorl_maggot",
                                                           "other_defoliator",
                                                           "bacterial_blight",
                                                           "bacterial_leaf_streak",
                                                           "brown_spot",
                                                           "leaf_blast",
                                                           "leaf_scald",
                                                           "narrow_brown_spot",
                                                           "red_stripe",
                                                           "dirty_panicle",
                                                           "false_smut",
                                                           "neck_blast",
                                                           "sheath_blight",
                                                           "sheath_rot")

                                 names(injuries[[2]]) <- c("location",
                                                           "visit_date",
                                                           "visit_no",
                                                           "field_no",
                                                           "water_status",
                                                           "crop_stage",
                                                           "tillers",
                                                           "leaves",
                                                           "panicles",
                                                           "hill_quadrat",
                                                           "weed_area",
                                                           "weed_above",
                                                           "weed_below")

                                 names(injuries[[3]]) <- c("location",
                                                           "visit_date",
                                                           "visit_no",
                                                           "field_no",
                                                           "water_status",
                                                           "crop_stage",
                                                           "broad_leaf_rank",
                                                           "grass_rank",
                                                           "sedge_rank",
                                                           "small_weeds_rank")

                                 names(injuries[[4]]) <- c("location",
                                                           "visit_date",
                                                           "visit_no",
                                                           "field_no",
                                                           "water_status",
                                                           "crop_stage",
                                                           "spp_no.",
                                                           "species")

                                 names(injuries[[5]]) <- c("location",
                                                           "visit_date",
                                                           "visit_no",
                                                           "field_no",
                                                           "water_status",
                                                           "crop_stage",
                                                           "grassy_stunt",
                                                           "ragged_stunt",
                                                           "rice_tungro",
                                                           "yellowing_syndrome",
                                                           "hopperburn",
                                                           "bugburn")

                                 # Write CSV files out to disk -----------------
                                 write.csv.raw(
                                   as.data.frame(injuries[1]),
                                   file = paste0(
                                     path.expand(
                                       dsn_cleaned),
                                     "hq_injuries.csv"),
                                   append = TRUE)

                                 write.csv.raw(
                                   as.data.frame(injuries[2]),
                                   file = paste0(
                                     path.expand(
                                       dsn_cleaned),
                                     "weed_area.csv"),
                                   append = TRUE)

                                 write.csv.raw(
                                   as.data.frame(injuries[3]),
                                   file = paste0(
                                     path.expand(
                                       dsn_cleaned),
                                     "weed_rank.csv"),
                                   append = TRUE)

                                 write.csv.raw(
                                   as.data.frame(injuries[4]),
                                   file = paste0(
                                     path.expand(
                                       dsn_cleaned),
                                     "weed_spp.csv"),
                                   append = TRUE)

                                 write.csv.raw(
                                   as.data.frame(injuries[5]),
                                   file = paste0(
                                     path.expand(
                                       dsn_cleaned),
                                     "systemic_injuries.csv"),
                                   append = TRUE)
                               }
parallel::stopCluster(cl)



# eos
