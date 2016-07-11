##############################################################################
# title         : Crop_Survey_DB_Form2_Visit1.R;
# purpose       : Format crop health survey data for insertion and insert into relational database;
# producer      : prepared by A. Sparks;
# editor        : edited by M. Noel;
# last update   : in Los Banos, IRRI, 31 OCT 2013;
# inputs        : crop health survey form2;
# outputs       : crop health survey data inserted into relational database;
##############################################################################

library(XLConnect)

wb <- loadWorkbook("/Users/U8004755/Google Drive/Data/RICE-PRE/RICE-PRE_2015DS/ADN_2015DS/Farmers Practice R1.xls")

## Synthesis <- readWorksheet(object, sheet, startRow, startCol, endRow, endCol, header  =  TRUE )

#  =  =  =  =  =  =  =  =  =  = GENERAL INFORMATION =  =  =  =  =  =  =  =  =  =

visit_date <- readWorksheet(wb, "Form 2 Visit 1", startRow = 1,
                            startCol = 10, endRow = 1, endCol = 11,
                            header = FALSE)
names(visit_date) <- "visit_date"

visit_no <- readWorksheet(wb, "Form 2 Visit 1", startRow = 1, startCol = 15,
                          endRow = 1, endCol = 15, header = FALSE)
names(visit_no) <- "Visit"

field_no <- readWorksheet(wb, "Form 2 Visit 1", startRow = 2, startCol = 2,
                          endRow = 2, endCol = 2, header = FALSE)
if (length(row(field_no)) == 0) {
  field_no[1, 1] <- NA
}
names(field_no) <- "field_no"

general_information <- cbind(visit_date,
                             visit_no,
                             field_no)
print(general_information)

#  =  =  =  =  =  =  =  =  =  =  = CROP INFORMATION =  =  =  =  =  =  =  =  =  =

crop_devel <- readWorksheet(wb, "Form 2 Visit 1", startRow = 4, startCol = 4,
                            endRow = 4, endCol = 4, header = FALSE)
names(crop_devel) <- "crop_stage"

water_status <- readWorksheet(wb, "Form 2 Visit 1", startRow = 4, startCol = 11,
                              endRow = 4, endCol = 11, header = FALSE)
names(water_status) <- "water_status"

crop_status <- cbind(crop_devel, water_status)

# Generic Hill/Quadrat count for final data frame
HQ <- rep(1:10)

# Crop growth ------------------------------------------------------------------
# Tillers ----------------------------------------------------------------------
tillers_Q1 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 6,
                            endRow = 9, endCol = 6, header = FALSE)
names(tillers_Q1) <- "tillers"

tillers_Q2 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 6,
                            endRow = 9,  endCol = 6, header = FALSE)
names(tillers_Q2) <- "tillers"

tillers_Q3 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 7,
                            endRow = 9, endCol = 7, header = FALSE)
names(tillers_Q3) <- "tillers"

tillers_Q4 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 8,
                            endRow = 9, endCol = 8, header = FALSE)
names(tillers_Q4) <- "tillers"

tillers_Q5 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 9,
                            endRow = 9, endCol = 9, header = FALSE)
names(tillers_Q5) <- "tillers"

tillers_Q6 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 10,
                            endRow = 9, endCol = 10, header = FALSE)
names(tillers_Q6) <- "tillers"

tillers_Q7 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 11,
                            endRow = 9, endCol = 11, header = FALSE)
names(tillers_Q7) <- "tillers"

tillers_Q8 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 12,
                            endRow = 9, endCol = 12, header = FALSE)
names(tillers_Q8) <- "tillers"

tillers_Q9 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 13,
                            endRow = 9, endCol = 13, header = FALSE)
names(tillers_Q9) <- "tillers"

tillers_Q10 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 9, startCol = 14,
                             endRow = 9, endCol = 14, header = FALSE)
names(tillers_Q10) <- "tillers"

tillers <- rbind(tillers_Q1, tillers_Q2, tillers_Q3, tillers_Q4, tillers_Q5,
                 tillers_Q6, tillers_Q7, tillers_Q8, tillers_Q9, tillers_Q10)

# Leaves -----------------------------------------------------------------------

leaves_Q1 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 6,
                           endRow = 11, endCol = 6, header = FALSE)
names(leaves_Q1) <- "leaves"

leaves_Q2 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 6,
                           endRow = 11, endCol = 6, header = FALSE)
names(leaves_Q2) <- "leaves"

leaves_Q3 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 7,
                           endRow = 11, endCol = 7, header = FALSE)
names(leaves_Q3) <- "leaves"

leaves_Q4 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 8,
                           endRow = 11, endCol = 8, header = FALSE)
names(leaves_Q4) <- "leaves"

leaves_Q5 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 9,
                           endRow = 11, endCol = 9, header = FALSE)
names(leaves_Q5) <- "leaves"

leaves_Q6 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 10,
                           endRow = 11, endCol = 10, header = FALSE)
names(leaves_Q6) <- "leaves"

leaves_Q7 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 11,
                           endRow = 11, endCol = 11, header = FALSE)
names(leaves_Q7) <- "leaves"

leaves_Q8 <- readWorksheet(wb,  "Form 2 Visit 1", startRow = 11, startCol = 12,
                           endRow = 11, endCol = 12, header = FALSE)
names(leaves_Q8) <- "leaves"

leaves_Q9 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 13,
                           endRow = 11, endCol = 13, header = FALSE)
names(leaves_Q9) <- "leaves"

leaves_Q10 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 11, startCol = 14,
                            endRow = 11, endCol = 14, header = FALSE)
names(leaves_Q10) <- "leaves"

leaves <- rbind(leaves_Q1, leaves_Q2, leaves_Q3, leaves_Q4, leaves_Q5,
                leaves_Q6, leaves_Q7, leaves_Q8, leaves_Q9, leaves_Q10)

# Weeds ------------------------------------------------------------------------
# weed above -------------------------------------------------------------------
weed_area <- c("A", "B", "C")

weed_above_A <- readWorksheet(wb, "Form 2 Visit 1", startRow = 62, startCol = 3,
                              endRow = 62, endCol = 3, header = FALSE)
names(weed_above_A) <- "Weed_Above"

weed_above_B <- readWorksheet(wb, "Form 2 Visit 1", startRow = 62, startCol = 4,
                              endRow = 62, endCol = 4, header = FALSE)
names(weed_above_B) <- "Weed_Above"

weed_above_C <- readWorksheet(wb, "Form 2 Visit 1", startRow = 62, startCol = 5,
                              endRow = 62, endCol = 5, header = FALSE)
names(weed_above_C) <- "Weed_Above"

weed_above <- rbind(weed_above_A, weed_above_B, weed_above_C)

# weed below -------------------------------------------------------------------
weed_below_A <- readWorksheet(wb, "Form 2 Visit 1", startRow = 63, startCol = 3,
                              endRow = 63, endCol = 3, header = FALSE)
names(weed_below_A) <- "Weed_below"

weed_below_B <- readWorksheet(wb, "Form 2 Visit 1", startRow = 63, startCol = 4,
                              endRow = 63, endCol = 4, header = FALSE)
names(weed_below_B) <- "Weed_below"

weed_below_C <- readWorksheet(wb, "Form 2 Visit 1", startRow = 63, startCol = 5,
                              endRow = 63, endCol = 5, header = FALSE)
names(weed_below_C) <- "Weed_below"

weed_below <- rbind(weed_below_A, weed_below_B, weed_below_C)

weed_canopy <- cbind(weed_area, weed_above, weed_below)


# weed rank --------------------------------------------------------------------

S.rankA <- readWorksheet(wb, "Form 2 Visit 1", startRow = 17, startCol = 11,
                         endRow = 17, endCol = 11, header = FALSE)
names(S.rankA) <- "S.Rank.A"

S.rankB <- readWorksheet(wb, "Form 2 Visit 1", startRow = 17, startCol = 12,
                         endRow = 17, endCol = 12, header = FALSE)
names(S.rankB) <- "S.Rank.B"

S.rankC <- readWorksheet(wb, "Form 2 Visit 1", startRow = 17, startCol = 13,
                         endRow = 17, endCol = 13, header = FALSE)
names(S.rankC) <- "S.Rank.C"

BD.rankA <- readWorksheet(wb, "Form 2 Visit 1", startRow = 18, startCol = 11,
                          endRow = 18, endCol = 11, header = FALSE)
names(BD.rankA) <- "BD.Rank.A"

BD.rankB <- readWorksheet(wb, "Form 2 Visit 1", startRow = 18, startCol = 12,
                          endRow = 18, endCol = 12, header = FALSE)
names(BD.rankB) <- "BD.Rank.B"

BD.rankC <- readWorksheet(wb, "Form 2 Visit 1", startRow = 18, startCol = 13,
                          endRow = 18, endCol = 13, header = FALSE)
names(BD.rankC) <- "BD.Rank.C"

G.rankA <- readWorksheet(wb, "Form 2 Visit 1", startRow = 19, startCol = 11,
                         endRow = 19, endCol = 11, header = FALSE)
names(G.rankA) <- "G.Rank.A"

G.rankB <- readWorksheet(wb, "Form 2 Visit 1", startRow = 19, startCol = 12,
                         endRow = 19, endCol = 12, header = FALSE)
names(G.rankB) <- "G.Rank.B"

G.rankC <- readWorksheet(wb, "Form 2 Visit 1", startRow = 19, startCol = 13,
                         endRow = 19, endCol = 13, header = FALSE)
names(G.rankC) <- "G.Rank.C"

SD.rankA <- readWorksheet(wb, "Form 2 Visit 1", startRow = 20, startCol = 11,
                          endRow = 20, endCol = 11, header = FALSE)
names(SD.rankA) <- "SD.Rank.A"

SD.rankB <- readWorksheet(wb, "Form 2 Visit 1", startRow = 20, startCol = 12,
                          endRow = 20, endCol = 12, header = FALSE)
names(SD.rankB) <- "SD.Rank.B"

SD.rankC <- readWorksheet(wb, "Form 2 Visit 1", startRow = 20, startCol = 13,
                          endRow = 20, endCol = 13, header = FALSE)
names(SD.rankC) <- "SD.Rank.C"

# weed species -----------------------------------------------------------------

weed.sp1 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 17, startCol = 15,
                          endRow = 17, endCol = 17, header = FALSE)
names(weed.sp1) <- "Weed.Species.1"

weed.sp2 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 18, startCol = 15,
                          endRow = 18, endCol = 17, header = FALSE)
names(weed.sp2) <- "Weed.Species.2"

weed.sp3 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 19, startCol = 15,
                          endRow = 19, endCol = 17, header = FALSE)
names(weed.sp3) <- "Weed.Species.3"

weed.sp4 <- readWorksheet(wb, "Form 2 Visit 1", startRow = 20, startCol = 15,
                          endRow = 20, endCol = 17, header = FALSE)
names(weed.sp4) <- "Weed.Species.4"


# Animal pests------------------------------------------------------------------
deadheart <- data.table::rbindlist(foreach::foreach(i = 6:15) %do% {
  deadheart <- readWorksheet(wb, "Form 2 Visit 1", startRow = 15,
                         startCol = as.numeric(paste(i)), endRow = 15,
                         endCol = as.numeric(paste(i)), header = FALSE)
}
)


rat <- data.table::rbindlist(foreach::foreach(i = 6:15) %do% {
  rat <- readWorksheet(wb, "Form 2 Visit 1", startRow = 16,
                             startCol = as.numeric(paste(i)), endRow = 16,
                             endCol = as.numeric(paste(i)), header = FALSE)
}
)

DH.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 5,
                       endRow = 28,
                       endCol = 5,
                       header = FALSE)
names(DH.Q1) <- "DH.Q1"

DH.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 6,
                       endRow = 28,
                       endCol = 6,
                       header = FALSE)
names(DH.Q2) <- "DH.Q2"

DH.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 7,
                       endRow = 28,
                       endCol = 7,
                       header = FALSE)
names(DH.Q3) <- "DH.Q3"

DH.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 8,
                       endRow = 28,
                       endCol = 8,
                       header = FALSE)
names(DH.Q4) <- "DH.Q4"

DH.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 9,
                       endRow = 28,
                       endCol = 9,
                       header = FALSE)
names(DH.Q5) <- "DH.Q5"

DH.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 10,
                       endRow = 28,
                       endCol = 10,
                       header = FALSE)
names(DH.Q6) <- "DH.Q6"

DH.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 11,
                       endRow = 28,
                       endCol = 11,
                       header = FALSE)
names(DH.Q7) <- "DH.Q7"

DH.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 12,
                       endRow = 28,
                       endCol = 12,
                       header = FALSE)
names(DH.Q8) <- "DH.Q8"

DH.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 28,
                       startCol = 13,
                       endRow = 28,
                       endCol = 13,
                       header = FALSE)
names(DH.Q9) <- "DH.Q9"

DH.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 28,
                        startCol = 14,
                        endRow = 28,
                        endCol = 14,
                        header = FALSE)
names(DH.Q10) <- "DH.Q10"

WH.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 5,
                       endRow = 29,
                       endCol = 5,
                       header = FALSE)
names(WH.Q1) <- "WH.Q1"

WH.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 6,
                       endRow = 29,
                       endCol = 6,
                       header = FALSE)
names(WH.Q2) <- "WH.Q2"

WH.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 7,
                       endRow = 29,
                       endCol = 7,
                       header = FALSE)
names(WH.Q3) <- "WH.Q3"

WH.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 8,
                       endRow = 29,
                       endCol = 8,
                       header = FALSE)
names(WH.Q4) <- "WH.Q4"

WH.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 9,
                       endRow = 29,
                       endCol = 9,
                       header = FALSE)
names(WH.Q5) <- "WH.Q5"

WH.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 10,
                       endRow = 29,
                       endCol = 10,
                       header = FALSE)
names(WH.Q6) <- "WH.Q6"

WH.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 11,
                       endRow = 29,
                       endCol = 11,
                       header = FALSE)
names(WH.Q7) <- "WH.Q7"

WH.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 12,
                       endRow = 29,
                       endCol = 12,
                       header = FALSE)
names(WH.Q8) <- "WH.Q8"

WH.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 29,
                       startCol = 13,
                       endRow = 29,
                       endCol = 13,
                       header = FALSE)
names(WH.Q9) <- "WH.Q9"

WH.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 29,
                        startCol = 14,
                        endRow = 29,
                        endCol = 14,
                        header = FALSE)
names(WH.Q10) <- "WH.Q10"

GM.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 5,
                       endRow = 30,
                       endCol = 5,
                       header = FALSE)
names(GM.Q1) <- "GM.Q1"

GM.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 6,
                       endRow = 30,
                       endCol = 6,
                       header = FALSE)
names(GM.Q2) <- "GM.Q2"

GM.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 7,
                       endRow = 30,
                       endCol = 7,
                       header = FALSE)
names(GM.Q3) <- "GM.Q3"

GM.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 8,
                       endRow = 30,
                       endCol = 8,
                       header = FALSE)
names(GM.Q4) <- "GM.Q4"

GM.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 9,
                       endRow = 30,
                       endCol = 9,
                       header = FALSE)
names(GM.Q5) <- "GM.Q5"

GM.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 10,
                       endRow = 30,
                       endCol = 10,
                       header = FALSE)
names(GM.Q6) <- "GM.Q6"

GM.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 11,
                       endRow = 30,
                       endCol = 11,
                       header = FALSE)
names(GM.Q7) <- "GM.Q7"

GM.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 12,
                       endRow = 30,
                       endCol = 12,
                       header = FALSE)
names(GM.Q8) <- "GM.Q8"

GM.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 30,
                       startCol = 13,
                       endRow = 30,
                       endCol = 13,
                       header = FALSE)
names(GM.Q9) <- "GM.Q9"

GM.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 30,
                        startCol = 14,
                        endRow = 30,
                        endCol = 14,
                        header = FALSE)
names(GM.Q10) <- "GM.Q10"

RT.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 5,
                       endRow = 31,
                       endCol = 5,
                       header = FALSE)
names(RT.Q1) <- "RT.Q1"

RT.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 6,
                       endRow = 31,
                       endCol = 6,
                       header = FALSE)
names(RT.Q2) <- "RT.Q2"

RT.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 7,
                       endRow = 31,
                       endCol = 7,
                       header = FALSE)
names(RT.Q3) <- "RT.Q3"

RT.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 8,
                       endRow = 31,
                       endCol = 8,
                       header = FALSE)
names(RT.Q4) <- "RT.Q4"

RT.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 9,
                       endRow = 31,
                       endCol = 9,
                       header = FALSE)
names(RT.Q5) <- "RT.Q5"

RT.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 10,
                       endRow = 31,
                       endCol = 10,
                       header = FALSE)
names(RT.Q6) <- "RT.Q6"

RT.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 11,
                       endRow = 31,
                       endCol = 11,
                       header = FALSE)
names(RT.Q7) <- "RT.Q7"

RT.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 12,
                       endRow = 31,
                       endCol = 12,
                       header = FALSE)
names(RT.Q8) <- "RT.Q8"

RT.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 31,
                       startCol = 13,
                       endRow = 31,
                       endCol = 13,
                       header = FALSE)
names(RT.Q9) <- "RT.Q9"

RT.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 31,
                        startCol = 14,
                        endRow = 31,
                        endCol = 14,
                        header = FALSE)
names(RT.Q10) <- "RT.Q10"

WM.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 5,
                       endRow = 34,
                       endCol = 5,
                       header = FALSE)
names(WM.Q1) <- "WM.Q1"

WM.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 6,
                       endRow = 34,
                       endCol = 6,
                       header = FALSE)
names(WM.Q2) <- "WM.Q2"

WM.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 7,
                       endRow = 34,
                       endCol = 7,
                       header = FALSE)
names(WM.Q3) <- "WM.Q3"

WM.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 8,
                       endRow = 34,
                       endCol = 8,
                       header = FALSE)
names(WM.Q4) <- "WM.Q4"

WM.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 9,
                       endRow = 34,
                       endCol = 9,
                       header = FALSE)
names(WM.Q5) <- "WM.Q5"

WM.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 10,
                       endRow = 34,
                       endCol = 10,
                       header = FALSE)
names(WM.Q6) <- "WM.Q6"

WM.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 11,
                       endRow = 34,
                       endCol = 11,
                       header = FALSE)
names(WM.Q7) <- "WM.Q7"

WM.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 12,
                       endRow = 34,
                       endCol = 12,
                       header = FALSE)
names(WM.Q8) <- "WM.Q8"

WM.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 34,
                       startCol = 13,
                       endRow = 34,
                       endCol = 13,
                       header = FALSE)
names(WM.Q9) <- "WM.Q9"

WM.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 34,
                        startCol = 14,
                        endRow = 34,
                        endCol = 14,
                        header = FALSE)
names(WM.Q10) <- "WM.Q10"

LF.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 5,
                       endRow = 35,
                       endCol = 5,
                       header = FALSE)
names(LF.Q1) <- "LF.Q1"

LF.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 6,
                       endRow = 35,
                       endCol = 6,
                       header = FALSE)
names(LF.Q2) <- "LF.Q2"

LF.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 7,
                       endRow = 35,
                       endCol = 7,
                       header = FALSE)
names(LF.Q3) <- "LF.Q3"

LF.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 8,
                       endRow = 35,
                       endCol = 8,
                       header = FALSE)
names(LF.Q4) <- "LF.Q4"

LF.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 9,
                       endRow = 35,
                       endCol = 9,
                       header = FALSE)
names(LF.Q5) <- "LF.Q5"

LF.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 10,
                       endRow = 35,
                       endCol = 10,
                       header = FALSE)
names(LF.Q6) <- "LF.Q6"

LF.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 11,
                       endRow = 35,
                       endCol = 11,
                       header = FALSE)
names(LF.Q7) <- "LF.Q7"

LF.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 12,
                       endRow = 35,
                       endCol = 12,
                       header = FALSE)
names(LF.Q8) <- "LF.Q8"

LF.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 35,
                       startCol = 13,
                       endRow = 35,
                       endCol = 13,
                       header = FALSE)
names(LF.Q9) <- "LF.Q9"

LF.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 35,
                        startCol = 14,
                        endRow = 35,
                        endCol = 14,
                        header = FALSE)
names(LF.Q10) <- "LF.Q10"

def.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 5,
                        endRow = 36,
                        endCol = 5,
                        header = FALSE)
names(def.Q1) <- "def.Q1"

def.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 6,
                        endRow = 36,
                        endCol = 6,
                        header = FALSE)
names(def.Q2) <- "def.Q2"

def.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 7,
                        endRow = 36,
                        endCol = 7,
                        header = FALSE)
names(def.Q3) <- "def.Q3"

def.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 8,
                        endRow = 36,
                        endCol = 8,
                        header = FALSE)
names(def.Q4) <- "def.Q4"

def.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 9,
                        endRow = 36,
                        endCol = 9,
                        header = FALSE)
names(def.Q5) <- "def.Q5"

def.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 10,
                        endRow = 36,
                        endCol = 10,
                        header = FALSE)
names(def.Q6) <- "def.Q6"

def.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 11,
                        endRow = 36,
                        endCol = 11,
                        header = FALSE)
names(def.Q7) <- "def.Q7"

def.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 12,
                        endRow = 36,
                        endCol = 12,
                        header = FALSE)
names(def.Q8) <- "def.Q8"

def.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 36,
                        startCol = 13,
                        endRow = 36,
                        endCol = 13,
                        header = FALSE)
names(def.Q9) <- "def.Q9"

def.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 36,
                         startCol = 14,
                         endRow = 36,
                         endCol = 14,
                         header = FALSE)
names(def.Q10) <- "def.Q10"

BPH.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 5,
                        endRow = 38,
                        endCol = 5,
                        header = FALSE)
names(BPH.Q1) <- "BPH.Q1"

BPH.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 6,
                        endRow = 38,
                        endCol = 6,
                        header = FALSE)
names(BPH.Q2) <- "BPH.Q2"

BPH.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 7,
                        endRow = 38,
                        endCol = 7,
                        header = FALSE)
names(BPH.Q3) <- "BPH.Q3"

BPH.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 8,
                        endRow = 38,
                        endCol = 8,
                        header = FALSE)
names(BPH.Q4) <- "BPH.Q4"

BPH.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 9,
                        endRow = 38,
                        endCol = 9,
                        header = FALSE)
names(BPH.Q5) <- "BPH.Q5"

BPH.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 10,
                        endRow = 38,
                        endCol = 10,
                        header = FALSE)
names(BPH.Q6) <- "BPH.Q6"

BPH.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 11,
                        endRow = 38,
                        endCol = 11,
                        header = FALSE)
names(BPH.Q7) <- "BPH.Q7"

BPH.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 12,
                        endRow = 38,
                        endCol = 12,
                        header = FALSE)
names(BPH.Q8) <- "BPH.Q8"

BPH.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 38,
                        startCol = 13,
                        endRow = 38,
                        endCol = 13,
                        header = FALSE)
names(BPH.Q9) <- "BPH.Q9"

BPH.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 38,
                         startCol = 14,
                         endRow = 38,
                         endCol = 14,
                         header = FALSE)
names(BPH.Q10) <- "BPH.Q10"

WPH.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 5,
                        endRow = 39,
                        endCol = 5,
                        header = FALSE)
names(WPH.Q1) <- "WPH.Q1"

WPH.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 6,
                        endRow = 39,
                        endCol = 6,
                        header = FALSE)
names(WPH.Q2) <- "WPH.Q2"

WPH.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 7,
                        endRow = 39,
                        endCol = 7,
                        header = FALSE)
names(WPH.Q3) <- "WPH.Q3"

WPH.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 8,
                        endRow = 39,
                        endCol = 8,
                        header = FALSE)
names(WPH.Q4) <- "WPH.Q4"

WPH.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 9,
                        endRow = 39,
                        endCol = 9,
                        header = FALSE)
names(WPH.Q5) <- "WPH.Q5"

WPH.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 10,
                        endRow = 39,
                        endCol = 10,
                        header = FALSE)
names(WPH.Q6) <- "WPH.Q6"

WPH.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 11,
                        endRow = 39,
                        endCol = 11,
                        header = FALSE)
names(WPH.Q7) <- "WPH.Q7"

WPH.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 12,
                        endRow = 39,
                        endCol = 12,
                        header = FALSE)
names(WPH.Q8) <- "WPH.Q8"

WPH.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 39,
                        startCol = 13,
                        endRow = 39,
                        endCol = 13,
                        header = FALSE)
names(WPH.Q9) <- "WPH.Q9"

WPH.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 39,
                         startCol = 14,
                         endRow = 39,
                         endCol = 14,
                         header = FALSE)
names(WPH.Q10) <- "WPH.Q10"

AW.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 5,
                       endRow = 40,
                       endCol = 5,
                       header = FALSE)
names(AW.Q1) <- "AW.Q1"

AW.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 6,
                       endRow = 40,
                       endCol = 6,
                       header = FALSE)
names(AW.Q2) <- "AW.Q2"

AW.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 7,
                       endRow = 40,
                       endCol = 7,
                       header = FALSE)
names(AW.Q3) <- "AW.Q3"

AW.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 8,
                       endRow = 40,
                       endCol = 8,
                       header = FALSE)
names(AW.Q4) <- "AW.Q4"

AW.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 9,
                       endRow = 40,
                       endCol = 9,
                       header = FALSE)
names(AW.Q5) <- "AW.Q5"

AW.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 10,
                       endRow = 40,
                       endCol = 10,
                       header = FALSE)
names(AW.Q6) <- "AW.Q6"

AW.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 11,
                       endRow = 40,
                       endCol = 11,
                       header = FALSE)
names(AW.Q7) <- "AW.Q7"

AW.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 12,
                       endRow = 40,
                       endCol = 12,
                       header = FALSE)
names(AW.Q8) <- "AW.Q8"

AW.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 40,
                       startCol = 13,
                       endRow = 40,
                       endCol = 13,
                       header = FALSE)
names(AW.Q9) <- "AW.Q9"

AW.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 40,
                        startCol = 14,
                        endRow = 40,
                        endCol = 14,
                        header = FALSE)
names(AW.Q10) <- "AW.Q10"

RB.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 5,
                       endRow = 41,
                       endCol = 5,
                       header = FALSE)
names(RB.Q1) <- "RB.Q1"

RB.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 6,
                       endRow = 41,
                       endCol = 6,
                       header = FALSE)
names(RB.Q2) <- "RB.Q2"

RB.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 7,
                       endRow = 41,
                       endCol = 7,
                       header = FALSE)
names(RB.Q3) <- "RB.Q3"

RB.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 8,
                       endRow = 41,
                       endCol = 8,
                       header = FALSE)
names(RB.Q4) <- "RB.Q4"

RB.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 9,
                       endRow = 41,
                       endCol = 9,
                       header = FALSE)
names(RB.Q5) <- "RB.Q5"

RB.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 10,
                       endRow = 41,
                       endCol = 10,
                       header = FALSE)
names(RB.Q6) <- "RB.Q6"

RB.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 11,
                       endRow = 41,
                       endCol = 11,
                       header = FALSE)
names(RB.Q7) <- "RB.Q7"

RB.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 12,
                       endRow = 41,
                       endCol = 12,
                       header = FALSE)
names(RB.Q8) <- "RB.Q8"

RB.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 41,
                       startCol = 13,
                       endRow = 41,
                       endCol = 13,
                       header = FALSE)
names(RB.Q9) <- "RB.Q9"

RB.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 41,
                        startCol = 14,
                        endRow = 41,
                        endCol = 14,
                        header = FALSE)
names(RB.Q10) <- "RB.Q10"

GLH.sweep1 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 45,
                            startCol = 5,
                            endRow = 45,
                            endCol = 5,
                            header = FALSE)
names(GLH.sweep1) <- "GLH.Sweep.1"

GLH.sweep2 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 45,
                            startCol = 6,
                            endRow = 45,
                            endCol = 6,
                            header = FALSE)
names(GLH.sweep2) <- "GLH.Sweep.2"

GLH.sweep3 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 45,
                            startCol = 7,
                            endRow = 45,
                            endCol = 7,
                            header = FALSE)
names(GLH.sweep3) <- "GLH.Sweep.3"

GLH.sweep4 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 45,
                            startCol = 8,
                            endRow = 45,
                            endCol = 8,
                            header = FALSE)
names(GLH.sweep4) <- "GLH.Sweep.4"

GLH.sweep5 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 45,
                            startCol = 9,
                            endRow = 45,
                            endCol = 9,
                            header = FALSE)
names(GLH.sweep5) <- "GLH.Sweep.5"

BPH.sweep1 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 46,
                            startCol = 5,
                            endRow = 46,
                            endCol = 5,
                            header = FALSE)
names(BPH.sweep1) <- "BPH.Sweep.1"

BPH.sweep2 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 46,
                            startCol = 6,
                            endRow = 46,
                            endCol = 6,
                            header = FALSE)
names(BPH.sweep2) <- "BPH.Sweep.2"

BPH.sweep3 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 46,
                            startCol = 7,
                            endRow = 46,
                            endCol = 7,
                            header = FALSE)
names(BPH.sweep3) <- "BPH.Sweep.3"

BPH.sweep4 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 46,
                            startCol = 8,
                            endRow = 46,
                            endCol = 8,
                            header = FALSE)
names(BPH.sweep4) <- "BPH.Sweep.4"

BPH.sweep5 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 46,
                            startCol = 9,
                            endRow = 46,
                            endCol = 9,
                            header = FALSE)
names(BPH.sweep5) <- "BPH.Sweep.5"

WPH.sweep1 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 47,
                            startCol = 5,
                            endRow = 47,
                            endCol = 5,
                            header = FALSE)
names(WPH.sweep1) <- "WPH.Sweep.1"

WPH.sweep2 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 47,
                            startCol = 6,
                            endRow = 47,
                            endCol = 6,
                            header = FALSE)
names(WPH.sweep2) <- "WPH.Sweep.2"

WPH.sweep3 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 47,
                            startCol = 7,
                            endRow = 47,
                            endCol = 7,
                            header = FALSE)
names(WPH.sweep3) <- "WPH.Sweep.3"

WPH.sweep4 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 47,
                            startCol = 8,
                            endRow = 47,
                            endCol = 8,
                            header = FALSE)
names(WPH.sweep4) <- "WPH.Sweep.4"

WPH.sweep5 <- readWorksheet(wb,
                            "Form 2 Visit 1",
                            startRow = 47,
                            startCol = 9,
                            endRow = 47,
                            endCol = 9,
                            header = FALSE)
names(WPH.sweep5) <- "WPH.Sweep.5"

RC.sweep1 <- readWorksheet(wb,
                           "Form 2 Visit 1",
                           startRow = 48,
                           startCol = 5,
                           endRow = 48,
                           endCol = 5,
                           header = FALSE)
names(RC.sweep1) <- "RC.Sweep.1"

RC.sweep2 <- readWorksheet(wb,
                           "Form 2 Visit 1",
                           startRow = 48,
                           startCol = 6,
                           endRow = 48,
                           endCol = 6,
                           header = FALSE)
names(RC.sweep2) <- "RC.Sweep.2"

RC.sweep3 <- readWorksheet(wb,
                           "Form 2 Visit 1",
                           startRow = 48,
                           startCol = 7,
                           endRow = 48,
                           endCol = 7,
                           header = FALSE)
names(RC.sweep3) <- "RC.Sweep.3"

RC.sweep4 <- readWorksheet(wb,
                           "Form 2 Visit 1",
                           startRow = 48,
                           startCol = 8,
                           endRow = 48,
                           endCol = 8,
                           header = FALSE)
names(RC.sweep4) <- "RC.Sweep.4"

RC.sweep5 <- readWorksheet(wb,
                           "Form 2 Visit 1",
                           startRow = 48,
                           startCol = 9,
                           endRow = 48,
                           endCol = 9,
                           header = FALSE)
names(RC.sweep5) <- "RC.Sweep.5"

ben.ins1 <- readWorksheet(wb,
                          "Form 2 Visit 1",
                          startRow = 45,
                          startCol = 15,
                          endRow = 45,
                          endCol = 17,
                          header = FALSE)
names(ben.ins1) <- "Beneficial.Insect.1"

ben.ins2 <- readWorksheet(wb,
                          "Form 2 Visit 1",
                          startRow = 46,
                          startCol = 15,
                          endRow = 46,
                          endCol = 17,
                          header = FALSE)
names(ben.ins2) <- "Beneficial.Insect.2"

ben.ins3 <- readWorksheet(wb,
                          "Form 2 Visit 1",
                          startRow = 47,
                          startCol = 15,
                          endRow = 47,
                          endCol = 17,
                          header = FALSE)
names(ben.ins3) <- "Beneficial.Insect.3"

ben.ins4 <- readWorksheet(wb,
                          "Form 2 Visit 1",
                          startRow = 48,
                          startCol = 15,
                          endRow = 48,
                          endCol = 17,
                          header = FALSE)
names(ben.ins4) <- "Beneficial.Insect.4"

ANI.PEST <- cbind(SNL.Q1, SNL.Q2, SNL.Q3, SNL.Q4, SNL.Q5, SNL.Q6, SNL.Q7, SNL.Q8, SNL.Q9, SNL.Q10,
                  DH.Q1, DH.Q2, DH.Q3, DH.Q4, DH.Q5, DH.Q6, DH.Q7, DH.Q8, DH.Q9, DH.Q10,
                  WH.Q1, WH.Q2, WH.Q3, WH.Q4, WH.Q5, WH.Q6, WH.Q7, WH.Q8, WH.Q9, WH.Q10,
                  GM.Q1, GM.Q2, GM.Q3, GM.Q4, GM.Q5, GM.Q6, GM.Q7, GM.Q8, GM.Q9, GM.Q10,
                  RT.Q1, RT.Q2, RT.Q3, RT.Q4, RT.Q5, RT.Q6, RT.Q7, RT.Q8, RT.Q9, RT.Q10,
                  WM.Q1, WM.Q2, WM.Q3, WM.Q4, WM.Q5, WM.Q6, WM.Q7, WM.Q8, WM.Q9, WM.Q10,
                  LF.Q1, LF.Q2, LF.Q3, LF.Q4, LF.Q5, LF.Q6, LF.Q7, LF.Q8, LF.Q9, LF.Q10,
                  def.Q1, def.Q2, def.Q3, def.Q4, def.Q5, def.Q6, def.Q7, def.Q8, def.Q9, def.Q10,
                  BPH.Q1, BPH.Q2, BPH.Q3, BPH.Q4, BPH.Q5, BPH.Q6, BPH.Q7, BPH.Q8, BPH.Q9, BPH.Q10,
                  WPH.Q1, WPH.Q2, WPH.Q3, WPH.Q4, WPH.Q5, WPH.Q6, WPH.Q7, WPH.Q8, WPH.Q9, WPH.Q10,
                  AW.Q1, AW.Q2, AW.Q3, AW.Q4, AW.Q5, AW.Q6, AW.Q7, AW.Q8, AW.Q9, AW.Q10,
                  RB.Q1, RB.Q2, RB.Q3, RB.Q4, RB.Q5, RB.Q6, RB.Q7, RB.Q8, RB.Q9, RB.Q10,
                  GLH.sweep1, GLH.sweep2, GLH.sweep3, GLH.sweep4, GLH.sweep5,
                  BPH.sweep1, BPH.sweep2, BPH.sweep3, BPH.sweep4, BPH.sweep5,
                  WPH.sweep1, WPH.sweep2, WPH.sweep3, WPH.sweep4, WPH.sweep5,
                  RC.sweep1, RC.sweep2, RC.sweep3, RC.sweep4, RC.sweep5,
                  ben.ins1, ben.ins2, ben.ins3, ben.ins4)
print(ANI.PEST)

#  =  =  =  =  =  =  =  =  =  = DISEASES =  =  =  =  =  =  =  =  =  =

BLB.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 5,
                        endRow = 51,
                        endCol = 5,
                        header = FALSE)
names(BLB.Q1) <- "BLB.Q1"

BLB.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 6,
                        endRow = 51,
                        endCol = 6,
                        header = FALSE)
names(BLB.Q2) <- "BLB.Q2"

BLB.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 7,
                        endRow = 51,
                        endCol = 7,
                        header = FALSE)
names(BLB.Q3) <- "BLB.Q3"

BLB.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 8,
                        endRow = 51,
                        endCol = 8,
                        header = FALSE)
names(BLB.Q4) <- "BLB.Q4"

BLB.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 9,
                        endRow = 51,
                        endCol = 9,
                        header = FALSE)
names(BLB.Q5) <- "BLB.Q5"

BLB.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 10,
                        endRow = 51,
                        endCol = 10,
                        header = FALSE)
names(BLB.Q6) <- "BLB.Q6"

BLB.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 11,
                        endRow = 51,
                        endCol = 11,
                        header = FALSE)
names(BLB.Q7) <- "BLB.Q7"

BLB.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 12,
                        endRow = 51,
                        endCol = 12,
                        header = FALSE)
names(BLB.Q8) <- "BLB.Q8"

BLB.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 51,
                        startCol = 13,
                        endRow = 51,
                        endCol = 13,
                        header = FALSE)
names(BLB.Q9) <- "BLB.Q9"

BLB.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 51,
                         startCol = 14,
                         endRow = 51,
                         endCol = 14,
                         header = FALSE)
names(BLB.Q10) <- "BLB.Q10"

LB.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 5,
                       endRow = 52,
                       endCol = 5,
                       header = FALSE)
names(LB.Q1) <- "LB.Q1"

LB.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 6,
                       endRow = 52,
                       endCol = 6,
                       header = FALSE)
names(LB.Q2) <- "LB.Q2"

LB.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 7,
                       endRow = 52,
                       endCol = 7,
                       header = FALSE)
names(LB.Q3) <- "LB.Q3"

LB.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 8,
                       endRow = 52,
                       endCol = 8,
                       header = FALSE)
names(LB.Q4) <- "LB.Q4"

LB.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 9,
                       endRow = 52,
                       endCol = 9,
                       header = FALSE)
names(LB.Q5) <- "LB.Q5"

LB.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 10,
                       endRow = 52,
                       endCol = 10,
                       header = FALSE)
names(LB.Q6) <- "LB.Q6"

LB.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 11,
                       endRow = 52,
                       endCol = 11,
                       header = FALSE)
names(LB.Q7) <- "LB.Q7"

LB.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 12,
                       endRow = 52,
                       endCol = 12,
                       header = FALSE)
names(LB.Q8) <- "LB.Q8"

LB.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 52,
                       startCol = 13,
                       endRow = 52,
                       endCol = 13,
                       header = FALSE)
names(LB.Q9) <- "LB.Q9"

LB.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 52,
                        startCol = 14,
                        endRow = 52,
                        endCol = 14,
                        header = FALSE)
names(LB.Q10) <- "LB.Q10"

BS.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 5,
                       endRow = 53,
                       endCol = 5,
                       header = FALSE)
names(BS.Q1) <- "BS.Q1"

BS.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 6,
                       endRow = 53,
                       endCol = 6,
                       header = FALSE)
names(BS.Q2) <- "BS.Q2"

BS.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 7,
                       endRow = 53,
                       endCol = 7,
                       header = FALSE)
names(BS.Q3) <- "BS.Q3"

BS.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 8,
                       endRow = 53,
                       endCol = 8,
                       header = FALSE)
names(BS.Q4) <- "BS.Q4"

BS.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 9,
                       endRow = 53,
                       endCol = 9,
                       header = FALSE)
names(BS.Q5) <- "BS.Q5"

BS.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 10,
                       endRow = 53,
                       endCol = 10,
                       header = FALSE)
names(BS.Q6) <- "BS.Q6"

BS.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 11,
                       endRow = 53,
                       endCol = 11,
                       header = FALSE)
names(BS.Q7) <- BS.Q7

BS.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 12,
                       endRow = 53,
                       endCol = 12,
                       header = FALSE)
names(BS.Q8) <- "BS.Q8"

BS.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 53,
                       startCol = 13,
                       endRow = 53,
                       endCol = 13,
                       header = FALSE)
names(BS.Q9) <- "BS.Q9"

BS.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 53,
                        startCol = 14,
                        endRow = 53,
                        endCol = 14,
                        header = FALSE)
names(BS.Q10) <- "BS.Q10"

BLS.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 5,
                        endRow = 54,
                        endCol = 5,
                        header = FALSE)
names(BLS.Q1) <- "BLS.Q1"

BLS.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 6,
                        endRow = 54,
                        endCol = 6,
                        header = FALSE)
names(BLS.Q2) <- "BLS.Q2"

BLS.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 7,
                        endRow = 54,
                        endCol = 7,
                        header = FALSE)
names(BLS.Q3) <- "BLS.Q3"

BLS.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 8,
                        endRow = 54,
                        endCol = 8,
                        header = FALSE)
names(BLS.Q4) <- "BLS.Q4"

BLS.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 9,
                        endRow = 54,
                        endCol = 9,
                        header = FALSE)
names(BLS.Q5) <- "BLS.Q5"

BLS.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 10,
                        endRow = 54,
                        endCol = 10,
                        header = FALSE)
names(BLS.Q6) <- "BLS.Q6"

BLS.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 11,
                        endRow = 54,
                        endCol = 11,
                        header = FALSE)
names(BLS.Q7) <- "BLS.Q7"

BLS.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 12,
                        endRow = 54,
                        endCol = 12,
                        header = FALSE)
names(BLS.Q8) <- "BLS.Q8"

BLS.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 54,
                        startCol = 13,
                        endRow = 54,
                        endCol = 13,
                        header = FALSE)
names(BLS.Q9) <- "BLS.Q9"

BLS.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 54,
                         startCol = 14,
                         endRow = 54,
                         endCol = 14,
                         header = FALSE)
names(BLS.Q10) <- "BLS.Q10"

NBS.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 5,
                        endRow = 55,
                        endCol = 5,
                        header = FALSE)
names(NBS.Q1) <- "NBS.Q1"

NBS.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 6,
                        endRow = 55,
                        endCol = 6,
                        header = FALSE)
names(NBS.Q2) <- "NBS.Q2"

NBS.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 7,
                        endRow = 55,
                        endCol = 7,
                        header = FALSE)
names(NBS.Q3) <- "NBS.Q3"

NBS.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 8,
                        endRow = 55,
                        endCol = 8,
                        header = FALSE)
names(NBS.Q4) <- "NBS.Q4"

NBS.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 9,
                        endRow = 55,
                        endCol = 9,
                        header = FALSE)
names(NBS.Q5) <- "NBS.Q5"

NBS.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 10,
                        endRow = 55,
                        endCol = 10,
                        header = FALSE)
names(NBS.Q6) <- "NBS.Q6"

NBS.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 11,
                        endRow = 55,
                        endCol = 11,
                        header = FALSE)
names(NBS.Q7) <- NBS.Q7

NBS.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 12,
                        endRow = 55,
                        endCol = 12,
                        header = FALSE)
names(NBS.Q8) <- "NBS.Q8"

NBS.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 55,
                        startCol = 13,
                        endRow = 55,
                        endCol = 13,
                        header = FALSE)
names(NBS.Q9) <- "NBS.Q9"

NBS.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 55,
                         startCol = 14,
                         endRow = 55,
                         endCol = 14,
                         header = FALSE)
names(NBS.Q10) <- "NBS.Q10"

RS.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 5,
                       endRow = 56,
                       endCol = 5,
                       header = FALSE)
names(RS.Q1) <- "RS.Q1"

RS.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 6,
                       endRow = 56,
                       endCol = 6,
                       header = FALSE)
names(RS.Q2) <- "RS.Q2"

RS.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 7,
                       endRow = 56,
                       endCol = 7,
                       header = FALSE)
names(RS.Q3) <- "RS.Q3"

RS.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 8,
                       endRow = 56,
                       endCol = 8,
                       header = FALSE)
names(RS.Q4) <- "RS.Q4"

RS.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 9,
                       endRow = 56,
                       endCol = 9,
                       header = FALSE)
names(RS.Q5) <- "RS.Q5"

RS.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 10,
                       endRow = 56,
                       endCol = 10,
                       header = FALSE)
names(RS.Q6) <- "RS.Q6"

RS.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 11,
                       endRow = 56,
                       endCol = 11,
                       header = FALSE)
names(RS.Q7) <- RS.Q7

RS.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 12,
                       endRow = 56,
                       endCol = 12,
                       header = FALSE)
names(RS.Q8) <- "RS.Q8"

RS.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 56,
                       startCol = 13,
                       endRow = 56,
                       endCol = 13,
                       header = FALSE)
names(RS.Q9) <- "RS.Q9"

RS.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 56,
                        startCol = 14,
                        endRow = 56,
                        endCol = 14,
                        header = FALSE)
names(RS.Q10) <- "RS.Q10"

LS.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 5,
                       endRow = 57,
                       endCol = 5,
                       header = FALSE)
names(LS.Q1) <- "LS.Q1"

LS.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 6,
                       endRow = 57,
                       endCol = 6,
                       header = FALSE)
names(LS.Q2) <- "LS.Q2"

LS.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 7,
                       endRow = 57,
                       endCol = 7,
                       header = FALSE)
names(LS.Q3) <- "LS.Q3"

LS.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 8,
                       endRow = 57,
                       endCol = 8,
                       header = FALSE)
names(LS.Q4) <- "LS.Q4"

LS.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 9,
                       endRow = 57,
                       endCol = 9,
                       header = FALSE)
names(LS.Q5) <- "LS.Q5"

LS.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 10,
                       endRow = 57,
                       endCol = 10,
                       header = FALSE)
names(LS.Q6) <- "LS.Q6"

LS.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 11,
                       endRow = 57,
                       endCol = 11,
                       header = FALSE)
names(LS.Q7) <- "LS.Q7"

LS.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 12,
                       endRow = 57,
                       endCol = 12,
                       header = FALSE)
names(LS.Q8) <- "LS.Q8"

LS.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 57,
                       startCol = 13,
                       endRow = 57,
                       endCol = 13,
                       header = FALSE)
names(LS.Q9) <- "LS.Q9"

LS.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 57,
                        startCol = 14,
                        endRow = 57,
                        endCol = 14,
                        header = FALSE)
names(LS.Q10) <- "LS.Q10"

SHB.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 5,
                        endRow = 59,
                        endCol = 5,
                        header = FALSE)
names(SHB.Q1) <- "SHB.Q1"

SHB.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 6,
                        endRow = 59,
                        endCol = 6,
                        header = FALSE)
names(SHB.Q2) <- "SHB.Q2"

SHB.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 7,
                        endRow = 59,
                        endCol = 7,
                        header = FALSE)
names(SHB.Q3) <- "SHB.Q3"

SHB.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 8,
                        endRow = 59,
                        endCol = 8,
                        header = FALSE)
names(SHB.Q4) <- "SHB.Q4"

SHB.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 9,
                        endRow = 59,
                        endCol = 9,
                        header = FALSE)
names(SHB.Q5) <- "SHB.Q5"

SHB.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 10,
                        endRow = 59,
                        endCol = 10,
                        header = FALSE)
names(SHB.Q6) <- "SHB.Q6"

SHB.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 11,
                        endRow = 59,
                        endCol = 11,
                        header = FALSE)
names(SHB.Q7) <- "SHB.Q7"

SHB.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 12,
                        endRow = 59,
                        endCol = 12,
                        header = FALSE)
names(SHB.Q8) <- "SHB.Q8"

SHB.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 59,
                        startCol = 13,
                        endRow = 59,
                        endCol = 13,
                        header = FALSE)
names(SHB.Q9) <- "SHB.Q9"

SHB.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 59,
                         startCol = 14,
                         endRow = 59,
                         endCol = 14,
                         header = FALSE)
names(SHB.Q10) <- "SHB.Q10"

SHR.Q1 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 5,
                        endRow = 60,
                        endCol = 5,
                        header = FALSE)
names(SHR.Q1) <- "SHR.Q1"

SHR.Q2 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 6,
                        endRow = 60,
                        endCol = 6,
                        header = FALSE)
names(SHR.Q2) <- "SHR.Q2"

SHR.Q3 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 7,
                        endRow = 60,
                        endCol = 7,
                        header = FALSE)
names(SHR.Q3) <- "SHR.Q3"

SHR.Q4 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 8,
                        endRow = 60,
                        endCol = 8,
                        header = FALSE)
names(SHR.Q4) <- "SHR.Q4"

SHR.Q5 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 9,
                        endRow = 60,
                        endCol = 9,
                        header = FALSE)
names(SHR.Q5) <- "SHR.Q5"

SHR.Q6 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 10,
                        endRow = 60,
                        endCol = 10,
                        header = FALSE)
names(SHR.Q6) <- "SHR.Q6"

SHR.Q7 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 11,
                        endRow = 60,
                        endCol = 11,
                        header = FALSE)
names(SHR.Q7) <- "SHR.Q7"

SHR.Q8 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 12,
                        endRow = 60,
                        endCol = 12,
                        header = FALSE)
names(SHR.Q8) <- "SHR.Q8"

SHR.Q9 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 60,
                        startCol = 13,
                        endRow = 60,
                        endCol = 13,
                        header = FALSE)
names(SHR.Q9) <- "SHR.Q9"

SHR.Q10 <- readWorksheet(wb,
                         "Form 2 Visit 1",
                         startRow = 60,
                         startCol = 14,
                         endRow = 60,
                         endCol = 14,
                         header = FALSE)
names(SHR.Q10) <- "SHR.Q10"

SR.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 5,
                       endRow = 61,
                       endCol = 5,
                       header = FALSE)
names(SR.Q1) <- "SR.Q1"

SR.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 6,
                       endRow = 61,
                       endCol = 6,
                       header = FALSE)
names(SR.Q2) <- "SR.Q2"

SR.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 7,
                       endRow = 61,
                       endCol = 7,
                       header = FALSE)
names(SR.Q3) <- "SR.Q3"

SR.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 8,
                       endRow = 61,
                       endCol = 8,
                       header = FALSE)
names(SR.Q4) <- "SR.Q4"

SR.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 9,
                       endRow = 61,
                       endCol = 9,
                       header = FALSE)
names(SR.Q5) <- "SR.Q5"

SR.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 10,
                       endRow = 61,
                       endCol = 10,
                       header = FALSE)
names(SR.Q6) <- "SR.Q6"

SR.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 11,
                       endRow = 61,
                       endCol = 11,
                       header = FALSE)
names(SR.Q7) <- SR.Q7

SR.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 12,
                       endRow = 61,
                       endCol = 12,
                       header = FALSE)
names(SR.Q8) <- "SR.Q8"

SR.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 61,
                       startCol = 13,
                       endRow = 61,
                       endCol = 13,
                       header = FALSE)
names(SR.Q9) <- "SR.Q9"

SR.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 61,
                        startCol = 14,
                        endRow = 61,
                        endCol = 14,
                        header = FALSE)
names(SR.Q10) <- "SR.Q10"

FS.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 5,
                       endRow = 62,
                       endCol = 5,
                       header = FALSE)
names(FS.Q1) <- "FS.Q1"

FS.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 6,
                       endRow = 62,
                       endCol = 6,
                       header = FALSE)
names(FS.Q2) <- "FS.Q2"

FS.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 7,
                       endRow = 62,
                       endCol = 7,
                       header = FALSE)
names(FS.Q3) <- "FS.Q3"

FS.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 8,
                       endRow = 62,
                       endCol = 8,
                       header = FALSE)
names(FS.Q4) <- "FS.Q4"

FS.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 9,
                       endRow = 62,
                       endCol = 9,
                       header = FALSE)
names(FS.Q5) <- "FS.Q5"

FS.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 10,
                       endRow = 62,
                       endCol = 10,
                       header = FALSE)
names(FS.Q6) <- "FS.Q6"

FS.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 11,
                       endRow = 62,
                       endCol = 11,
                       header = FALSE)
names(FS.Q7) <- "FS.Q7"

FS.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 12,
                       endRow = 62,
                       endCol = 12,
                       header = FALSE)
names(FS.Q8) <- "FS.Q8"

FS.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 62,
                       startCol = 13,
                       endRow = 62,
                       endCol = 13,
                       header = FALSE)
names(FS.Q9) <- "FS.Q9"

FS.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 62,
                        startCol = 14,
                        endRow = 62,
                        endCol = 14,
                        header = FALSE)
names(FS.Q10) <- "FS.Q10"

NB.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 5,
                       endRow = 63,
                       endCol = 5,
                       header = FALSE)
names(NB.Q1) <- "NB.Q1"

NB.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 6,
                       endRow = 63,
                       endCol = 6,
                       header = FALSE)
names(NB.Q2) <- "NB.Q2"

NB.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 7,
                       endRow = 63,
                       endCol = 7,
                       header = FALSE)
names(NB.Q3) <- "NB.Q3"

NB.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 8,
                       endRow = 63,
                       endCol = 8,
                       header = FALSE)
names(NB.Q4) <- "NB.Q4"

NB.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 9,
                       endRow = 63,
                       endCol = 9,
                       header = FALSE)
names(NB.Q5) <- "NB.Q5"

NB.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 10,
                       endRow = 63,
                       endCol = 10,
                       header = FALSE)
names(NB.Q6) <- "NB.Q6"

NB.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 11,
                       endRow = 63,
                       endCol = 11,
                       header = FALSE)
names(NB.Q7) <- "NB.Q7"

NB.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 12,
                       endRow = 63,
                       endCol = 12,
                       header = FALSE)
names(NB.Q8) <- "NB.Q8"

NB.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 63,
                       startCol = 13,
                       endRow = 63,
                       endCol = 13,
                       header = FALSE)
names(NB.Q9) <- "NB.Q9"

NB.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 63,
                        startCol = 14,
                        endRow = 63,
                        endCol = 14,
                        header = FALSE)
names(NB.Q10) <- "NB.Q10"

DP.Q1 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 5,
                       endRow = 64,
                       endCol = 5,
                       header = FALSE)
names(DP.Q1) <- "DP.Q1"

DP.Q2 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 6,
                       endRow = 64,
                       endCol = 6,
                       header = FALSE)
names(DP.Q2) <- "DP.Q2"

DP.Q3 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 7,
                       endRow = 64,
                       endCol = 7,
                       header = FALSE)
names(DP.Q3) <- "DP.Q3"

DP.Q4 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 8,
                       endRow = 64,
                       endCol = 8,
                       header = FALSE)
names(DP.Q4) <- "DP.Q4"

DP.Q5 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 9,
                       endRow = 64,
                       endCol = 9,
                       header = FALSE)
names(DP.Q5) <- "DP.Q5"

DP.Q6 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 10,
                       endRow = 64,
                       endCol = 10,
                       header = FALSE)
names(DP.Q6) <- "DP.Q6"

DP.Q7 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 11,
                       endRow = 64,
                       endCol = 11,
                       header = FALSE)
names(DP.Q7) <- "DP.Q7"

DP.Q8 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 12,
                       endRow = 64,
                       endCol = 12,
                       header = FALSE)
names(DP.Q8) <- "DP.Q8"

DP.Q9 <- readWorksheet(wb,
                       "Form 2 Visit 1",
                       startRow = 64,
                       startCol = 13,
                       endRow = 64,
                       endCol = 13,
                       header = FALSE)
names(DP.Q9) <- "DP.Q9"

DP.Q10 <- readWorksheet(wb,
                        "Form 2 Visit 1",
                        startRow = 64,
                        startCol = 14,
                        endRow = 64,
                        endCol = 14,
                        header = FALSE)
names(DP.Q10) <- "DP.Q10"

DISEASES <- cbind(BLB.Q1, BLB.Q2, BLB.Q3, BLB.Q4, BLB.Q5, BLB.Q6, BLB.Q7, BLB.Q8, BLB.Q9, BLB.Q10,
                  LB.Q1, LB.Q2, LB.Q3, LB.Q4, LB.Q5, LB.Q6, LB.Q7, LB.Q8, LB.Q9, LB.Q10,
                  BS.Q1, BS.Q2, BS.Q3, BS.Q4, BS.Q5, BS.Q6, BS.Q7, BS.Q8, BS.Q9, BS.Q10,
                  BLS.Q1, BLS.Q2, BLS.Q3, BLS.Q4, BLS.Q5, BLS.Q6, BLS.Q7, BLS.Q8, BLS.Q9, BLS.Q10,
                  NBS.Q1, NBS.Q2, NBS.Q3, NBS.Q4, NBS.Q5, NBS.Q6, NBS.Q7, NBS.Q8, NBS.Q9, NBS.Q10,
                  RS.Q1, RS.Q2, RS.Q3, RS.Q4, RS.Q5, RS.Q6, RS.Q7, RS.Q8, RS.Q9, RS.Q10,
                  LS.Q1, LS.Q2, LS.Q3, LS.Q4, LS.Q5, LS.Q6, LS.Q7, LS.Q8, LS.Q9, LS.Q10,
                  SHB.Q1, SHB.Q2, SHB.Q3, SHB.Q4, SHB.Q5, SHB.Q6, SHB.Q7, SHB.Q8, SHB.Q9, SHB.Q10,
                  SHR.Q1, SHR.Q2, SHR.Q3, SHR.Q4, SHR.Q5, SHR.Q6, SHR.Q7, SHR.Q8, SHR.Q9, SHR.Q10,
                  SR.Q1, SR.Q2, SR.Q3, SR.Q4, SR.Q5, SR.Q6, SR.Q7, SR.Q8, SR.Q9, SR.Q10,
                  FS.Q1, FS.Q2, FS.Q3, FS.Q4, FS.Q5, FS.Q6, FS.Q7, FS.Q8, FS.Q9, FS.Q10,
                  NB.Q1, NB.Q2, NB.Q3, NB.Q4, NB.Q5, NB.Q6, NB.Q7, NB.Q8, NB.Q9, NB.Q10,
                  DP.Q1, DP.Q2, DP.Q3, DP.Q4, DP.Q5, DP.Q6, DP.Q7, DP.Q8, DP.Q9, DP.Q10)
print(DISEASES)

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


#  =  =  =  =  =  =  =  =  =  = EOS =  =  =  =  =  =  =  =  =  =
