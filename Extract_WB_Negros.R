################################################################################
# title         : Extract_WB.R;
# purpose       : Extract data from Excel for data analysis;
# producer      : prepared by A. Sparks;
# last update   : in Towoomba, QLD, AUS, July 2016;
# inputs        : crop health survey form2;
# outputs       : crop health survey data ready for analysis;
################################################################################

extract <- function(f) {
  wb <- loadWorkbook(file.path(f))

  i <- NULL
  v <- NULL

  hq_injuries <- yield <- weed_area <- weed_rank <- weed_species <-
    systemic_injuries <- vector(mode = "list")

  itx <- iter(1:4)
  foreach(v = itx, .packages = c("XLConnect", "data.table")) %do% {

    # General information ------------------------------------------------------
    location <- XLConnect::readWorksheet(wb, sheet = "Form 1", startRow = 4,
                                         startCol = 9, endRow = 4, endCol = 18,
                                         header = FALSE)

    season <- substr(f, 57, 58)

    year <- substr(f, 53, 56)

    visit_date <- XLConnect::readWorksheet(wb, sheet = paste0("Form 2 Visit ",
                                                              v),
                                           startRow = 1, startCol = 11,
                                           endRow = 1, endCol = 12,
                                           header = FALSE)

    visit_no <- XLConnect::readWorksheet(wb, sheet = paste0("Form 2 Visit ", v),
                                         startRow = 1, startCol = 15,
                                         endRow = 1, endCol = 15,
                                         header = FALSE)

    field_no <- XLConnect::readWorksheet(wb, sheet = paste0("Form 2 Visit ", v),
                                         startRow = 2, startCol = 2,
                                         endRow = 2, endCol = 2,
                                         header = FALSE)
    if (length(row(field_no)) == 0) {
      field_no[1, 1] <- NA
    }

    water_status <- XLConnect::readWorksheet(wb, sheet = paste0("Form 2 Visit ",
                                                                v),
                                             startRow = 6, startCol = 12,
                                             endRow = 6, endCol = 12,
                                             header = FALSE)

    crop_stage <- XLConnect::readWorksheet(wb, sheet = paste0("Form 2 Visit ",
                                                              v),
                                           startRow = 6, startCol = 4,
                                           endRow = 6, endCol = 4,
                                           header = FALSE)

    general_information <- cbind(location, year, season, visit_date, visit_no,
                                 field_no, water_status, crop_stage,
                                 row.names = NULL)

    # Crop information ---------------------------------------------------------

    # Generic hill/quadrat count for final data frame
    hill_quadrat <- rep(1:10)

    # Tillers ------------------------------------------------------------------

    tillers <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 9,
                               startCol = as.numeric(paste(i)), endRow = 9,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    # Leaves -------------------------------------------------------------------
    leaves <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 11,
                               startCol = as.numeric(paste(i)), endRow = 11,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    # Panicles -----------------------------------------------------------------
    panicles <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 10,
                               startCol = as.numeric(paste(i)), endRow = 10,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    # Animal pests -------------------------------------------------------------
    rat <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 16,
                               startCol = as.numeric(paste(i)), endRow = 16,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    silvershoot <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 17,
                               startCol = as.numeric(paste(i)), endRow = 17,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    deadheart <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 15,
                               startCol = as.numeric(paste(i)), endRow = 15,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    whitehead <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 18,
                               startCol = as.numeric(paste(i)), endRow = 18,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    leaffolder <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 21,
                               startCol = as.numeric(paste(i)), endRow = 21,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    leaf_miner <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 22,
                               startCol = as.numeric(paste(i)), endRow = 22,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    rice_hispa <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 23,
                               startCol = as.numeric(paste(i)), endRow = 23,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    whorl_maggot <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 24,
                               startCol = as.numeric(paste(i)), endRow = 24,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    other_defoliator <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 25,
                               startCol = as.numeric(paste(i)), endRow = 25,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    # Diseases -----------------------------------------------------------------

    bacterial_blight <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 35,
                               startCol = as.numeric(paste(i)), endRow = 35,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    bacterial_leaf_streak <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 36,
                               startCol = as.numeric(paste(i)), endRow = 36,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    brown_spot <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 37,
                               startCol = as.numeric(paste(i)), endRow = 37,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    leaf_blast <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 38,
                               startCol = as.numeric(paste(i)), endRow = 38,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    leaf_scald <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 39,
                               startCol = as.numeric(paste(i)), endRow = 39,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    narrow_brown_spot <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 40,
                               startCol = as.numeric(paste(i)), endRow = 40,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    red_stripe <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 41,
                               startCol = as.numeric(paste(i)), endRow = 41,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    dirty_panicle <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 44,
                               startCol = as.numeric(paste(i)), endRow = 44,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    false_smut <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 45,
                               startCol = as.numeric(paste(i)), endRow = 45,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    dirty_panicle <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 44,
                               startCol = as.numeric(paste(i)), endRow = 44,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    neck_blast <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 46,
                               startCol = as.numeric(paste(i)), endRow = 46,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    sheath_blight <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 47,
                               startCol = as.numeric(paste(i)), endRow = 47,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    sheath_rot <- data.table::rbindlist(foreach(i = 6:15) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 48,
                               startCol = as.numeric(paste(i)), endRow = 48,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    gi <- general_information[rep(seq_len(nrow(general_information)),
                                  each = nrow(sheath_rot)), ]
    # LIST hq_injuries ---------------------------------------------------------
    hq_injuries[[v]] <- data.frame(gi, tillers, leaves, panicles, hill_quadrat,
                                   rat, silvershoot, whitehead, deadheart,
                                   leaffolder, leaf_miner, rice_hispa,
                                   whorl_maggot, other_defoliator,
                                   bacterial_blight, bacterial_leaf_streak,
                                   brown_spot, leaf_blast, leaf_scald,
                                   narrow_brown_spot, red_stripe, dirty_panicle,
                                   false_smut, neck_blast, sheath_blight,
                                   sheath_rot, row.names = NULL)

    rm(gi)

    # Weeds --------------------------------------------------------------------
    # weed above ---------------------------------------------------------------
    weed_area_col <- rep(c("A", "B", "C"), 2)
    names(weed_area_col) <- "weed_area"

    weed_above <- data.table::rbindlist(foreach(i = 3:5) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 61,
                               startCol = as.numeric(paste(i)),
                               endRow = 61, endCol = as.numeric(paste(i)),
                               header = FALSE)
    }
    )

    weed_below <- data.table::rbindlist(foreach(i = 3:5) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 62,
                               startCol = as.numeric(paste(i)),
                               endRow = 62, endCol = as.numeric(paste(i)),
                               header = FALSE)
    }
    )

    gi <- general_information[rep(seq_len(nrow(general_information)),
                                  each = nrow(weed_below)), ]

    # LIST weed_area -----------------------------------------------------------
    weed_area[[v]] <- data.frame(gi[, 1:6], weed_area_col, weed_above,
                                 weed_below, row.names = NULL)
    rm(gi)

    # weed rank ----------------------------------------------------------------

    broad_leaf_rank <- data.table::rbindlist(foreach(i = 12:14) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v),
                               startRow = 61, startCol = i,
                               endRow = 61, endCol = i, header = FALSE)
    }
    )

    grass_rank <- data.table::rbindlist(foreach(i = 12:14) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v),
                               startRow = 62, startCol = i,
                               endRow = 62, endCol = i, header = FALSE)
    }
    )

    sedge_rank <- data.table::rbindlist(foreach(i = 12:14) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v),
                               startRow = 63, startCol = i,
                               endRow = 63, endCol = i,
                               header = FALSE)
    }
    )

    small_rank  <- data.table::rbindlist(foreach(i = 12:14) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v),
                               startRow = 64, startCol = i,
                               endRow = 64, endCol = i,
                               header = FALSE)
    }
    )

    gi <- general_information[rep(seq_len(nrow(general_information)),
                                  each = nrow(small_rank)), ]

    # LIST weed_rank -----------------------------------------------------------
    weed_rank[[v]] <- data.frame(gi, broad_leaf_rank, grass_rank, sedge_rank,
                                 small_rank, row.names = NULL)
    rm(gi)

    # weed species -------------------------------------------------------------

    weed_species_cols <- data.table::rbindlist(foreach(i = 61:64) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v),
                               startRow = as.numeric(paste(i)), startCol = 16,
                               endRow = as.numeric(paste(i)), endCol = 16,
                               header = FALSE)
    }
    )

    gi <- general_information[rep(seq_len(nrow(general_information)),
                                  each = nrow(weed_species_cols)), ]

    # LIST weed_species --------------------------------------------------------
    weed_species[[v]] <- data.frame(gi, c("Spp_1", "Spp_2", "Spp_3", "Spp_4"),
                                    weed_species_cols, row.names = NULL)
    rm(gi)

    # Systemic injuries---------------------------------------------------------

    grassy_stunt <- data.table::rbindlist(foreach(i = 6:10) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 53,
                               startCol = as.numeric(paste(i)), endRow = 53,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    ragged_stunt <- data.table::rbindlist(foreach(i = 6:10) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 54,
                               startCol = as.numeric(paste(i)), endRow = 54,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    rice_tungro <- data.table::rbindlist(foreach(i = 6:10) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 55,
                               startCol = as.numeric(paste(i)), endRow = 55,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    yellowing_syndrome <- data.table::rbindlist(foreach(i = 6:10) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 56,
                               startCol = as.numeric(paste(i)), endRow = 56,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    hopperburn <- data.table::rbindlist(foreach(i = 6:10) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 30,
                               startCol = as.numeric(paste(i)), endRow = 30,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    bugburn <- data.table::rbindlist(foreach(i = 6:10) %do% {
      XLConnect::readWorksheet(wb, paste0("Form 2 Visit ", v), startRow = 29,
                               startCol = as.numeric(paste(i)), endRow = 29,
                               endCol = as.numeric(paste(i)), header = FALSE)
    }
    )

    gi <- general_information[rep(seq_len(nrow(general_information)),
                                  each = nrow(bugburn)), ]

    # LIST systemic_injuries ---------------------------------------------------
    systemic_injuries[[v]] <- data.frame(gi, grassy_stunt,
                                         ragged_stunt,
                                         rice_tungro,
                                         yellowing_syndrome,
                                         hopperburn, bugburn,
                                         row.names = NULL)
    rm(gi)

    # Yield ----------------------------------------------------------------------
    if (v == 1) {
      crop_cut <- data.table::rbindlist(foreach(i = c(14:15, 18:19, 22:23)) %do% {
        XLConnect::readWorksheet(wb, paste0("Form 1"), startRow = 99,
                                 startCol = as.numeric(paste(i)), endRow = 99,
                                 endCol = as.numeric(paste(i)), header = FALSE)
      }
      )

      moisture <- data.table::rbindlist(foreach(i = c(14:15, 18:19, 22:23)) %do% {
        XLConnect::readWorksheet(wb, paste0("Form 1"), startRow = 100,
                                 startCol = as.numeric(paste(i)), endRow = 100,
                                 endCol = as.numeric(paste(i)), header = FALSE)
      }
      )

      gi <- general_information[rep(seq_len(nrow(general_information)),
                                    each = nrow(crop_cut)), ][, c(1:3, 6)]

      # LIST yield ------------- -----------------------------------------------
      yield <- data.frame(gi, crop_cut, moisture, row.names = NULL)
    }

  }

  out <- list(hq_injuries, weed_area, weed_rank, weed_species,
              systemic_injuries, yield)

  return(out)
}

#eos
