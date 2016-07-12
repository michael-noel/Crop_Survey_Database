##############################################################################
# title         : Crop_Survey_DB_Form2_Visit1.R;
# purpose       : Format crop health survey data for insertion and insert into relational database;
# producer      : prepared by A. Sparks;
# editor        : edited by M. Noel;
# last update   : in Towoomba, QLD, AUS, July 2016;
# inputs        : crop health survey form2;
# outputs       : crop health survey data ready for analysis;
##############################################################################

# Libraries --------------------------------------------------------------------
library(XLConnect)
library(foreach)
library(parallel)

# Load data --------------------------------------------------------------------

wb <- loadWorkbook("/Users/U8004755/Google Drive/Data/RICE-PRE/RICE-PRE_2015DS/ADN_2015DS/Farmers Practice R1.xls")

foreach(v = 1:4) %do% {
  # General information ----------------------------------------------------------
  visit_date <- readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 1,
                              startCol = 10, endRow = 1, endCol = 11,
                              header = FALSE)
  names(visit_date) <- "visit_date"

  visit_no <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 1,
                            startCol = 15, endRow = 1, endCol = 15,
                            header = FALSE)
  names(visit_no) <- "visit_no"

  field_no <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 2,
                            startCol = 2, endRow = 2, endCol = 2,
                            header = FALSE)
  if (length(row(field_no)) == 0) {
    field_no[1, 1] <- NA
  }
  names(field_no) <- "field_no"

  water_status <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 6,
                                startCol = 12, endRow = 6, endCol = 12,
                                header = FALSE)
  names(water_status) <- "water_status"

  crop_stage <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 6,
                              startCol = 4, endRow = 6, endCol = 4,
                              header = FALSE)
  names(crop_stage) <- "crop_stage"

  general_information <- cbind(visit_date,
                               visit_no,
                               field_no,
                               water_status,
                               crop_stage)
  general_information <- general_information[rep(seq_len(nrow(general_information)), each = 10), ]

  # Crop information -------------------------------------------------------------

  # Generic hill/quadrat count for final data frame
  hill_quadrat <- rep(1:10)

  # Crop growth ------------------------------------------------------------------


  # Tillers ----------------------------------------------------------------------

  tillers <- data.table::rbindlist(foreach(i = 6:14) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 9,
                  startCol = as.numeric(paste(i)), endRow = 9,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  # Leaves ---------------------------------------------------------------------
  leaves <- data.table::rbindlist(foreach(i = 6:14) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 11,
                  startCol = as.numeric(paste(i)), endRow = 9,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  # Weeds ----------------------------------------------------------------------
  # weed above -----------------------------------------------------------------
  weed_area <- rep(c("A", "B", "C"), 2)

  weed_above <- data.table::rbindlist(foreach(i = 3:5) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 62,
                  startCol = as.numeric(paste(i)),
                  endRow = 62, endCol = as.numeric(paste(i)),
                  header = FALSE)
  }
  )

  weed_below <- data.table::rbindlist(foreach(i = 3:5) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 63,
                  startCol = as.numeric(paste(i)),
                  endRow = 63, endCol = as.numeric(paste(i)),
                  header = FALSE)
  }
  )

  weed_canopy <- cbind(weed_area, weed_area, weed_above, weed_below)


  # weed rank ------------------------------------------------------------------



  small_rank <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 17,
                           startCol = 11, endRow = 17, endCol = 11,
                           header = FALSE)
  names(S.rankA) <- "S.Rank.A"

  S.rankB <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 17,
                           startCol = 12, endRow = 17, endCol = 12,
                           header = FALSE)
  names(S.rankB) <- "S.Rank.B"

  S.rankC <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 17,
                           startCol = 13, endRow = 17, endCol = 13,
                           header = FALSE)
  names(S.rankC) <- "S.Rank.C"

  BD.rankA <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 18,
                            startCol = 11, endRow = 18, endCol = 11,
                            header = FALSE)
  names(BD.rankA) <- "BD.Rank.A"

  BD.rankB <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 18,
                            startCol = 12, endRow = 18, endCol = 12,
                            header = FALSE)
  names(BD.rankB) <- "BD.Rank.B"

  BD.rankC <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 18,
                            startCol = 13, endRow = 18, endCol = 13,
                            header = FALSE)
  names(BD.rankC) <- "BD.Rank.C"

  G.rankA <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 19,
                           startCol = 11, endRow = 19, endCol = 11,
                           header = FALSE)
  names(G.rankA) <- "G.Rank.A"

  G.rankB <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 19,
                           startCol = 12, endRow = 19, endCol = 12,
                           header = FALSE)
  names(G.rankB) <- "G.Rank.B"

  G.rankC <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 19,
                           startCol = 13, endRow = 19, endCol = 13,
                           header = FALSE)
  names(G.rankC) <- "G.Rank.C"

  SD.rankA <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 20,
                            startCol = 11, endRow = 20, endCol = 11,
                            header = FALSE)
  names(SD.rankA) <- "SD.Rank.A"

  SD.rankB <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 20,
                            startCol = 12, endRow = 20, endCol = 12,
                            header = FALSE)
  names(SD.rankB) <- "SD.Rank.B"

  SD.rankC <- readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 20,
                            startCol = 13, endRow = 20, endCol = 13,
                            header = FALSE)
  names(SD.rankC) <- "SD.Rank.C"

  # weed species ---------------------------------------------------------------

  weed_species <- data.table::rbindlist(foreach(i = 17:20) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 17,
                  startCol = as.numeric(paste(i)),
                  endRow = 17, endCol = as.numeric(paste(i)),
                  header = FALSE)
  }
  )

  weed_species <- cbind(c("Spp_1", "Spp_2", "Spp_3", "Spp_4"), weed_species)

  # Animal pests----------------------------------------------------------------
  deadheart <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 15,
                  startCol = as.numeric(paste(i)), endRow = 15,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  ## Check rat row numbers!
  rat <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 16,
                  startCol = as.numeric(paste(i)), endRow = 16,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  silver_shoot <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 17,
                  startCol = as.numeric(paste(i)), endRow = 17,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  whitehead <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 18,
                  startCol = as.numeric(paste(i)), endRow = 18,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  leaf_folder <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 21,
                  startCol = as.numeric(paste(i)), endRow = 21,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  leaf_miner <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 22,
                  startCol = as.numeric(paste(i)), endRow = 22,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  rice_hispa <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 23,
                  startCol = as.numeric(paste(i)), endRow = 23,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  whorl_maggot <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 24,
                  startCol = as.numeric(paste(i)), endRow = 24,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  other_defoliator <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 25,
                  startCol = as.numeric(paste(i)), endRow = 25,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  hopperburn <- data.table::rbindlist(foreach(i = 6:10) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 30,
                  startCol = as.numeric(paste(i)), endRow = 30,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  bugburn <- data.table::rbindlist(foreach(i = 6:10) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 29,
                  startCol = as.numeric(paste(i)), endRow = 29,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  # Diseases -------------------------------------------------------------------

  bacterial_blight <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 35,
                  startCol = as.numeric(paste(i)), endRow = 35,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  bacterial_leaf_streak <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 36,
                  startCol = as.numeric(paste(i)), endRow = 36,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  brown_spot <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 37,
                  startCol = as.numeric(paste(i)), endRow = 37,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  leaf_blast <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 38,
                  startCol = as.numeric(paste(i)), endRow = 38,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  leaf_scald <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 39,
                  startCol = as.numeric(paste(i)), endRow = 39,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  narrow_brown_spot <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 40,
                  startCol = as.numeric(paste(i)), endRow = 40,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  red_stripe <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 41,
                  startCol = as.numeric(paste(i)), endRow = 41,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  dirty_panicle <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 44,
                  startCol = as.numeric(paste(i)), endRow = 44,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  false_smut <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 45,
                  startCol = as.numeric(paste(i)), endRow = 45,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  dirty_panicle <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 44,
                  startCol = as.numeric(paste(i)), endRow = 44,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  neck_blast <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 46,
                  startCol = as.numeric(paste(i)), endRow = 46,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  sheath_blight <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 47,
                  startCol = as.numeric(paste(i)), endRow = 47,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  sheath_rot <- data.table::rbindlist(foreach(i = 6:15) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 48,
                  startCol = as.numeric(paste(i)), endRow = 48,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  # systemic diseases ----------------------------------------------------------

  grassy_stunt <- data.table::rbindlist(foreach(i = 6:10) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 53,
                  startCol = as.numeric(paste(i)), endRow = 53,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  ragged_stunt <- data.table::rbindlist(foreach(i = 6:10) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 54,
                  startCol = as.numeric(paste(i)), endRow = 54,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  rice_tungro <- data.table::rbindlist(foreach(i = 6:10) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 55,
                  startCol = as.numeric(paste(i)), endRow = 55,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )

  yellowing_syndrome <- data.table::rbindlist(foreach(i = 6:10) %do% {
    readWorksheet(wb,paste0("Form 2 Visit ", v), startRow = 56,
                  startCol = as.numeric(paste(i)), endRow = 56,
                  endCol = as.numeric(paste(i)), header = FALSE)
  }
  )
}
## cbind the above dataframes

form2.visit1a <- cbind(GEN.INFO,
                       CROP.INFO,
                       WEEDS,
                       ANI.PESTS)
print(form2.visit1a)

form2.visit1b <- DISEASES
print(form2.visit1b)

## create worksheet for form2

createSheet(wb, "Form2_V1a_R")
writeWorksheet(wb, form2.visit1, sheet = "Form2_V1a_R")

createSheet(wb, "Form2_V1b_R")
writeWorksheet(wb, form2.visit1, sheet = "Form2_V1b_R")
saveWorkbook(wb)


# eos
