##############################################################################
# title         : Aggregate_Injuries.R;
# purpose       : Aggregate rp injury data as pulled from ODK Aggregator;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, PHL, Nov 2015;
# inputs        : Filtered rp data from Filter_Aggregator_Injury_Data.R;
# outputs       : Aggregated data in data frames for graphing, mapping and summarizing;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### load packages ####
library(data.table)
library(tidyr)
library(doBy)
library(dplyr)
#### end load packages ####


#### Create the "visit" dataframe that holds the metadata to link with injury data ####
## Visit number one or two? ##
visit <- data.frame(rp[, c(495:497, 8:9, 12:13, 16:17, 515, 525)])

colnames(visit) <- c("location", "season", "visit_date", "field_no", "water_status",
                     "crop_stage", "tillers", "leaves", "panicles", "hill_quadrat_no")

#### Growth stage ####
gs <- rp[, grep(pattern = "crop_growth_stage", colnames(rp), perl = TRUE)]

#### tillers, panicle and leaf counts ####
## split into groups for weighted averages ##
tillers <- data.frame(rp[, grep(pattern = "tiller_hill1$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill2$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill3$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill4$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill5$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill6$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill7$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill8$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill9$",
                                   colnames(rp), perl = TRUE)],
                      rp[, grep(pattern = "tiller_hill10$",
                                   colnames(rp), perl = TRUE)])
names(tillers) <- c("Tillers_Hill_1",
                    "Tillers_Hill_2",
                    "Tillers_Hill_3",
                    "Tillers_Hill_4",
                    "Tillers_Hill_5",
                    "Tillers_Hill_6",
                    "Tillers_Hill_7",
                    "Tillers_Hill_8",
                    "Tillers_Hill_9",
                    "Tillers_Hill_10")

panicles <- data.frame(rp[, grep(pattern = "panicle_hill1$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill2$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill3$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill4$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill5$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill6$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill7$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill8$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill9$",
                                    colnames(rp), perl = TRUE)],
                       rp[, grep(pattern = "panicle_hill10$",
                                    colnames(rp), perl = TRUE)])
names(panicles) <- c("Panicles_Hill_1",
                     "Panicles_Hill_2",
                     "Panicles_Hill_3",
                     "Panicles_Hill_4",
                     "Panicles_Hill_5",
                     "Panicles_Hill_6",
                     "Panicles_Hill_7",
                     "Panicles_Hill_8",
                     "Panicles_Hill_9",
                     "Panicles_Hill_10")

leaves <- data.frame(rp[, grep(pattern = "leaves_tiller1$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller2$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller3$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller4$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller5$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller6$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller7$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller8$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller9$",
                                  colnames(rp), perl = TRUE)],
                     rp[, grep(pattern = "leaves_tiller10$",
                                  colnames(rp), perl = TRUE)])
names(leaves) <- c("Leaves_Hill_1",
                   "Leaves_Hill_2",
                   "Leaves_Hill_3",
                   "Leaves_Hill_4",
                   "Leaves_Hill_5",
                   "Leaves_Hill_6",
                   "Leaves_Hill_7",
                   "Leaves_Hill_8",
                   "Leaves_Hill_9",
                   "Leaves_Hill_10")

#### generate weighted average vectors of non-systemic diseases, from 10 samples for each observation, for graphing ####
bak <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 1]),
                              100*(rp[, grep(pattern = "group_2[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 2]),
                              100*(rp[, grep(pattern = "group_3[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 3]),
                              100*(rp[, grep(pattern = "group_4[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 4]),
                              100*(rp[, grep(pattern = "group_5[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 5]),
                              100*(rp[, grep(pattern = "group_6[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 6]),
                              100*(rp[, grep(pattern = "group_7[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 7]),
                              100*(rp[, grep(pattern = "group_8[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 8]),
                              100*(rp[, grep(pattern = "group_9[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 9]),
                              100*(rp[, grep(pattern = "group_10[[:graph:]]*bakanae", colnames(rp), perl = TRUE)]/tillers[, 10])), 1, mean), 1)

bird <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 1]),
                               100*(rp[, grep(pattern = "group_2[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 2]),
                               100*(rp[, grep(pattern = "group_3[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 3]),
                               100*(rp[, grep(pattern = "group_4[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 4]),
                               100*(rp[, grep(pattern = "group_5[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 5]),
                               100*(rp[, grep(pattern = "group_6[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 6]),
                               100*(rp[, grep(pattern = "group_7[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 7]),
                               100*(rp[, grep(pattern = "group_8[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 8]),
                               100*(rp[, grep(pattern = "group_9[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 9]),
                               100*(rp[, grep(pattern = "group_10[[:graph:]]*bird", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

blb <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*bacterialleafblight", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)


bls <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*bacterialleafstreak", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

bst <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*(?<!narrow)(?i)brownspot", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

fsm <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*falsesmut", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

dip <- apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*dirtypanicle", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean)

lba <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*leafblast", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

lsc <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*leafscald", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

nba <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*neckblast", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

nbs <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*narrowbrownspot", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

rsp <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*redstripe", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

shr <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*sheathrot", colnames(rp), perl = TRUE)]/tillers[, 10])), 1, mean), 1)

shb <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*sheathblight", colnames(rp), perl = TRUE)]/tillers[, 10])), 1, mean), 1)

str <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*stemrot$", colnames(rp), perl = TRUE)]/tillers[, 10])), 1, mean), 1)

#### generate data frames of systemic diseases and bug/hopperburn ####
bbn <- rp[, grep(pattern = "bugburn", colnames(rp), perl = TRUE)]
bbn[bbn == 15] <- 20
bbn <- round(apply(bbn, 1, mean), 1)
hbn <- rp[, grep(pattern = "hopperburn", colnames(rp), perl = TRUE)]
hbn[hbn == 15] <- 20
hbn <- round(apply(hbn, 1, mean), 1)
tun <- rp[, grep(pattern = "tungro", colnames(rp), perl = TRUE)]
tun[tun == 15] <- 20
tun <- round(apply(tun, 1, mean), 1)
grs <- rp[, grep(pattern = "grassy", colnames(rp), perl = TRUE)]
grs[grs == 15] <- 20
grs <- round(apply(grs, 1, mean), 1)
rgd <- rp[, grep(pattern = "ragged", colnames(rp), perl = TRUE)]
rgd[rgd == 15] <- 20
rgd <- round(apply(rgd, 1, mean), 1)
olf <- rp[, grep(pattern = "orangeleaf|oramgeleaf", colnames(rp), perl = TRUE)]
olf[olf == 15] <- 20
olf <- round(apply(olf, 1, mean), 1)
ylo <- rp[, grep(pattern = "yellowdwarf", colnames(rp), perl = TRUE)]
ylo[ylo == 15] <- 20
ylo <- round(apply(ylo, 1, mean), 1)

weedabove <- subset(rp[, grep(pattern = "weedabove_area", colnames(rp), perl = TRUE)])
weedabove[weedabove == 15] <- 20
weedabove <- round(apply(weedabove, 1, mean), 1)

weedbelow <- subset(rp[, grep(pattern = "weedbelow_area", colnames(rp), perl = TRUE)])
weedbelow[weedbelow == 15] <- 20
weedbelow <- round(apply(weedbelow, 1, mean), 1)

broadleaf <- subset(rp[, grep(pattern = "weed_broadleaved", colnames(rp), perl = TRUE)])
broadleaf[is.na(broadleaf)] <- 0
broadleaf <- round(apply(broadleaf, 1, mean), 1)

grass <- subset(rp[, grep(pattern = "weed_grass", colnames(rp), perl = TRUE)])
grass[is.na(grass)] <- 0
grass <- round(apply(grass, 1, mean), 1)

sedge <- subset(rp[, grep(pattern = "weed_sedge", colnames(rp), perl = TRUE)])
sedge[is.na(sedge)] <- 0
sedge <- round(apply(sedge, 1, mean), 1)

small <- subset(rp[, grep(pattern = "weed_small", colnames(rp), perl = TRUE)])
small[is.na(small)] <- 0
small <- round(apply(small, 1, mean), 1)

#### Pest injuries ####
def <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*defoliator", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

dht <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*deadheart", colnames(rp), perl = TRUE)]/tillers[, 10])), 1, mean), 1)

lfd <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*leaffolder", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

lfm <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*leafminer", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

rgb <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*ricegrainbug", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

rbg <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*ricebug", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

thp <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*thrip", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

whm <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 1]*tillers[, 1])),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 2]*tillers[, 2])),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 3]*tillers[, 3])),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 4]*tillers[, 4])),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 5]*tillers[, 5])),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 6]*tillers[, 6])),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 7]*tillers[, 7])),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 8]*tillers[, 8])),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 9]*tillers[, 9])),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*whorlmaggot", colnames(rp), perl = TRUE)]/(leaves[, 10]*tillers[, 10]))), 1, mean), 1)

whtbb <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 1]),
                        100*(rp[, grep(pattern = "group_2[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 2]),
                        100*(rp[, grep(pattern = "group_3[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 3]),
                        100*(rp[, grep(pattern = "group_4[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 4]),
                        100*(rp[, grep(pattern = "group_5[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 5]),
                        100*(rp[, grep(pattern = "group_6[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 6]),
                        100*(rp[, grep(pattern = "group_7[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 7]),
                        100*(rp[, grep(pattern = "group_8[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 8]),
                        100*(rp[, grep(pattern = "group_9[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 9]),
                        100*(rp[, grep(pattern = "group_10[[:graph:]]*whiteheadblackbug", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

whtsb <- round(apply(data.frame(100*(rp[, grep(pattern = "group_1.hill1[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 1]),
                              100*(rp[, grep(pattern = "group_2[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 2]),
                              100*(rp[, grep(pattern = "group_3[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 3]),
                              100*(rp[, grep(pattern = "group_4[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 4]),
                              100*(rp[, grep(pattern = "group_5[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 5]),
                              100*(rp[, grep(pattern = "group_6[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 6]),
                              100*(rp[, grep(pattern = "group_7[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 7]),
                              100*(rp[, grep(pattern = "group_8[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 8]),
                              100*(rp[, grep(pattern = "group_9[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 9]),
                              100*(rp[, grep(pattern = "group_10[[:graph:]]*whiteheadstemborer", colnames(rp), perl = TRUE)]/panicles[, 10])), 1, mean), 1)

## Aggregate data frames

nonsystemic <- data.frame(visit[rep(seq_len(nrow(visit)), 7), ],
                          c(rep("Bacterial Blight", length(blb)),
                            rep("Bacterial Leaf Streak", length(bls)),
                            rep("Brown Spot", length(bst)),
                            rep("Leaf Blast", length(lba)),
                            rep("Narrow Brown Spot", length(nbs)),
                            rep("Leaf Scald", length(lsc)),
                            rep("Red Stripe", length(rsp))),
                          c(blb, bls, bst, lba, nbs, lsc, rsp))

systemic <- data.frame(visit[rep(seq_len(nrow(visit)), 7), ],
                       c(rep("Orange Leaf", length(olf)),
                         rep("Grassy Stunt", length(grs)),
                         rep("Ragged Stunt", length(rgd)),
                         rep("Tungro", length(tun)),
                         rep("Yellow Dwarf", length(ylo)),
                         rep("Bug Burn", length(bbn)),
                         rep("Hopperburn", length(hbn))),
                       c(olf, grs, rgd, tun, ylo, bbn, hbn))
# remove any records missing a proper value for area affected
systemic <- subset(systemic, !is.na(systemic[, 11]))

panicle.injuries <- data.frame(visit[rep(seq_len(nrow(visit)), 8), ],
                               c(rep("Bird", length(bird)),
                                 rep("False Smut", length(fsm)),
                                 rep("Dirty Panicle", length(dip)),
                                 rep("Neck Blast", length(nba)),
                                 rep("Rice Bug", length(rbg)),
                                 rep("Rice Grain Bug", length(rgb)),
                                 rep("Whitehead (Black Bug)", length(whtbb)),
                                 rep("Whitehead (Stem Borer)", length(whtsb))),
                               c(bird, fsm, dip, nba, rbg, rgb, whtbb, whtsb))

tiller.injuries <- data.frame(visit[rep(seq_len(nrow(visit)), 5), ],
                              c(rep("Bakanae", length(bak)),
                                rep("Sheath Blight", length(shb)),
                                rep("Sheath Rot", length(shr)),
                                rep("Stem Rot", length(str)),
                                rep("Deadheart", length(dht))),
                              c(bak, shb, shr, str, dht))

leaf.insect <- data.frame(visit[rep(seq_len(nrow(visit)), 5), ],
                          c(rep("Defoliator", length(def)),
                            rep("Leaffolder", length(lfd)),
                            rep("Leaf Miner", length(lfm)),
                            rep("Thrip", length(thp)),
                            rep("Whorl Maggot", length(whm))),
                          c(dht, lfd, lfm, thp, whm))

weed.area <- data.frame(visit[rep(seq_len(nrow(visit)), 2), ],
                        c(rep("Weed Above", length(weedabove)),
                          rep("Weed Below", length(weedbelow))),
                        c(weedabove, weedbelow))

weed.type <- data.frame(visit[rep(seq_len(nrow(visit)), 4), ],
                        c(rep("Broadleaves", length(broadleaf)),
                          rep("Grasses", length(grass)),
                          rep("Sedges", length(sedge)),
                          rep("Small Weeds", length(small))),
                        c(broadleaf, grass, sedge, small))

source("Functions/PRISM_Weed_spp.R")

weed <- na.omit(melt(weed.spp, id = "MFID"))
weed.spp <- left_join(weed.spp, visit, by = "MFID")

colnames(nonsystemic) <-
  colnames(systemic) <-
  colnames(panicle.injuries) <-
  colnames(tiller.injuries) <-
  colnames(weed.area) <-
  colnames(weed.type) <-
  colnames(leaf.insect) <- c("region",
                             "province",
                             "municipality",
                             "lat",
                             "lon",
                             "MFID",
                             "season",
                             "Assessment_Stage",
                             "Crop_Stage",
                             "Ecosystem",
                             "Region.Municipality",
                             "Disease",
                             "Damage")

nonsystemic <- subset(nonsystemic, !is.na(region))
systemic <- subset(systemic, !is.na(region))
panicle.injuries <- subset(panicle.injuries, !is.na(region))
tiller.injuries <- subset(tiller.injuries, !is.na(region))
weed.area <- subset(weed.area, !is.na(region))
weed.type <- subset(weed.type, !is.na(region))
leaf.insect <- subset(leaf.insect, !is.na(region))

#### Calculate Mean Injuries ####
injuries <- c("bird",
              "blb",
              "bls",
              "bst",
              "bak",
              "broadleaf",
              "bbn",
              "def",
              "dht",
              "dip",
              "fsm",
              "grass",
              "grs",
              "hbn",
              "lba",
              "lfd",
              "lfm",
              "lsc",
              "nba",
              "nbs",
              "olf",
              "rbg",
              "rgb",
              "rgd",
              "rsp",
              "sedge",
              "shb",
              "shr",
              "small",
              "str",
              "thp",
              "tun",
              "weedabove",
              "weedbelow",
              "whm",
              "whtbb",
              "whtsb",
              "ylo",
              "whm")

my.list <- list()

# Summarize injury values by municipality
for (i in 1:length(injuries)) {
  injury.id <- injuries[i]
  list.name <- as.character(injury.id)
  j <- summaryBy(get(paste(injury.id)) ~ region + Ecosystem + Assessment_Stage,
                 data.frame(visit, get(paste(injury.id))),
                 FUN = c(length, mean, min, max))
  values <- j

  my.list[[list.name]] <- values
}

final.data <- do.call(rbind, my.list)
final.data$injury <- row.names(final.data)

# order by Region/Municipality/Visit
final.data <- arrange(final.data, region, Ecosystem, Assessment_Stage)
final.data <- subset(final.data, !is.na(region))

# Replace values with NA for clarity
final.data[, 4][final.data[, 4] == "NaN"] <- NA
final.data[, 5][final.data[, 5] == "Inf"] <- NA
final.data[, 6][final.data[, 6] == "-Inf"] <- NA

# Drop the ".no" from the injury name
final.data[, 8] <- gsub(".", "|", final.data[, 8], fixed = TRUE)

final.data <- cbind(final.data,
                    data.table::fread(paste(final.data[, 8], collapse = "\n"),
                                      sep = "|", dec = "."))
final.data <- final.data[, -c(8, 10)] # drop the extra columns that we don't want

names(final.data) <- c("Region",
                       "Ecosystem",
                       "Assessment Stage",
                       "No. Fields",
                       "Mean",
                       "Min",
                       "Max",
                       "Injury")
final.data[, 5:7] <- round(final.data[, 5:7], 2)

#eos
