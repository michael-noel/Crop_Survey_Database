##############################################################################
# title         : RicePRE Extract and Format Data;
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
                                 itx <- iter(1:6)
                                 injuries <- foreach(v = itx, .packages = c("plyr", "iotools")) %do% {
                                   injuries[[v]] <- join_all(extracted[[v]], type = "full")
                                 }

                                 if (f == dsn_raw[1]) {
                                   # add names to columns ------------------------

                                   hq_injuries_names <- c("location",
                                                          "season",
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

                                   weed_area_names <- c("location",
                                                        "season",
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

                                   weed_rank_names <- c("location",
                                                        "season",
                                                        "visit_date",
                                                        "visit_no",
                                                        "field_no",
                                                        "water_status",
                                                        "crop_stage",
                                                        "broad_leaf_rank",
                                                        "grass_rank",
                                                        "sedge_rank",
                                                        "small_weeds_rank")

                                   weed_spp_names <- c("location",
                                                       "visit_date",
                                                       "visit_no",
                                                       "field_no",
                                                       "water_status",
                                                       "crop_stage",
                                                       "spp_no.",
                                                       "species")

                                   systemic_injury_names <- c("location",
                                                              "season",
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

                                   yield_names <- c("location",
                                                    "season",
                                                    "field_no",
                                                    "yield",
                                                    "moisture")

                                   cat(noquote(paste0(paste0(hq_injuries_names,
                                                             collapse = ","),
                                                      "\n")),
                                       file = paste0(path.expand(dsn_cleaned),
                                                     "hq_injuries.csv"))
                                   cat(noquote(paste0(paste0(weed_area_names,
                                                             collapse = ","),
                                                      "\n")),
                                       file = paste0(path.expand(dsn_cleaned),
                                                     "weed_area.csv"))
                                   cat(noquote(paste0(paste0(weed_rank_names,
                                                             collapse = ","),
                                                      "\n")),
                                       file = paste0(path.expand(dsn_cleaned),
                                                     "weed_rank.csv"))
                                   cat(noquote(paste0(paste0(weed_spp_names,
                                                             collapse = ","),
                                                      "\n")),
                                       file = paste0(path.expand(dsn_cleaned),
                                                     "weed_spp.csv"))
                                   cat(noquote(paste0(paste0(systemic_injury_names,
                                                             collapse = ","),
                                                      "\n")),
                                       file = paste0(path.expand(dsn_cleaned),
                                                     "systemic_injuries.csv"))
                                   cat(noquote(paste0(paste0(yield_names,
                                                             collapse = ","),
                                                      "\n")),
                                       file = paste0(path.expand(dsn_cleaned),
                                                     "yield.csv"))
                                 }

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

                                 write.csv.raw(
                                   as.data.frame(injuries[6]),
                                   file = paste0(
                                     path.expand(
                                       dsn_cleaned),
                                     "yield.csv"),
                                   append = TRUE)
                               }

parallel::stopCluster(cl)

# eos
