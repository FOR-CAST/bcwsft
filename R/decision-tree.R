## The fuel-typing decision tree, transpiled from bcwft2018.py
## FuelTyping.decisionTree (Greene, GPL-3) by data-raw/transpile.py. The function
## below is GENERATED -- do not hand-edit; re-run the transpiler if the upstream
## Python changes, then re-validate with the golden test (data-raw/generate_golden.py).
##
## Translation notes: Python None -> R NA; `X in [...]` / `in coniferList` -> %in%;
## `is (not) None` -> (!)is.na; `.startswith()` -> startsWith(); checkDomConifers() ->
## bcwsft_check_dom_conifers(); potentially-NA booleans (is_vegetated / is_forested)
## -> isTRUE(). Branch labels are transpiler line indices (traceability only; behaviour
## is validated against the reference Python, not the labels).

## Construct a decision-tree result.
.ft <- function(branch, fuel_type, ft_modifier = NA_real_) {
  list(branch = branch, fuel_type = fuel_type, ft_modifier = as.numeric(ft_modifier))
}

## Fall-through leaf (Python tree returns None): NA fuel type.
.unmatched <- function(branch) {
  list(branch = branch, fuel_type = NA_character_, ft_modifier = NA_real_)
}

#' Run the fuel-typing decision tree on a prepared state
#'
#' @param s named list of VRI attributes augmented with derived predicates
#'   (`is_vegetated`, `is_forested`, `is_logged`, `is_burned`, `harv_lag`,
#'   `dist_lag`, `pct_cnfr`, `dry_wet`, `stocking`, `season`), as assembled by
#'   [bcwsft_fuel_type_one()].
#' @return a list with `branch`, `fuel_type`, and `ft_modifier`.
#' @export
bcwsft_decision_tree <- function(s) {
  ## NON-VEGETATED SITE
  if ((!isTRUE(s$is_vegetated)) || (is.na(s$is_vegetated))) {
    ## SITE LOGGED
    if (isTRUE(s$is_logged)) {
      ## SITE HARVESTED WITHIN LAST 6 YEARS
      if (s$harv_lag <= 6) {
        if (s$COAST_INTERIOR_CD %eq% 'C') {
          return(.ft("L7", "S-3"))
        } else {
          return(.ft("L9", "S-1"))
          ## SITE HARVESTED WITHIN LAST 7-24 YEARS
        }
      } else if (s$harv_lag <= 24) {
        if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
          if (s$season %eq% 'dormant') {
            return(.ft("L14", "D-1"))
          } else {
            return(.ft("L16", "D-2"))
          }
        } else {
          if (s$season %eq% 'dormant') {
            return(.ft("L19", "O-1a"))
          } else {
            return(.ft("L21", "O-1b"))
            ## SITE HARVESTED LONGER THAN 24 YEARS AGO
          }
        }
      } else {
        if ((s$BEC_ZONE_CODE %in% c('CMA', 'IMA'))) {
          return(.ft("L25", "N"))
        } else if ((s$BEC_ZONE_CODE %in% c('BAFA', 'MH'))) {
          if (s$season %eq% 'dormant') {
            return(.ft("L28", "D-1"))
          } else {
            return(.ft("L30", "D-2"))
          }
        } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF', 'ICH')) && s$dry_wet %eq% 'wet') {
          return(.ft("L32", "C-5"))
        } else if ((s$BEC_ZONE_CODE %in% c('BWBS'))) {
          return(.ft("L34", "C-2"))
        } else if (s$BEC_ZONE_CODE %eq% 'SWB') {
          if (s$season %eq% 'dormant') {
            return(.ft("L37", "M-1", 50))
          } else {
            return(.ft("L39", "M-2", 50))
          }
        } else if (
          (s$BEC_ZONE_CODE %eq% 'SBS') ||
            (s$BEC_ZONE_CODE %eq% 'IDF' && s$dry_wet %eq% 'wet') ||
            (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'dry')
        ) {
          return(.ft("L42", "C-3"))
        } else if (
          (s$BEC_ZONE_CODE %in% c('SBPS', 'MS', 'ESSF')) ||
            ((s$BEC_ZONE_CODE %in% c('IDF', 'CDF')) && s$dry_wet %eq% 'dry')
        ) {
          return(.ft("L45", "C-7"))
        } else if ((s$BEC_ZONE_CODE %in% c('PP', 'BG'))) {
          if (s$season %eq% 'dormant') {
            return(.ft("L48", "O-1a"))
          } else {
            return(.ft("L50", "O-1b"))
          }
        } else if (s$BEC_ZONE_CODE %eq% 'CWH' && s$dry_wet %eq% 'dry') {
          if (s$season %eq% 'dormant') {
            return(.ft("L53", "M-1", 40))
          } else {
            return(.ft("L55", "M-2", 40))
            ## SITE UNLOGGED
          }
        }
      }
    } else {
      ## SITE RECENTLY BURNED
      if (isTRUE(s$is_burned) && (!is.na(s$dist_lag)) && (s$dist_lag < 11)) {
        if (s$dist_lag <= 3) {
          return(.ft("L61", "N"))
        } else if (s$dist_lag <= 6) {
          if (s$season %eq% 'dormant') {
            return(.ft("L64", "D-1"))
          } else {
            return(.ft("L66", "D-2"))
          }
        } else if (s$dist_lag <= 10) {
          if (s$season %eq% 'dormant') {
            return(.ft("L69", "O-1a"))
          } else {
            return(.ft("L71", "O-1b"))
            ## SITE NOT RECENTLY BURNED
          }
        }
      } else {
        if ((s$BCLCS_LEVEL_2 %in% c('L', NA))) {
          if (!is.na(s$SPECIES_CD_1)) {
            if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
              if (s$season %eq% 'dormant') {
                return(.ft("L78", "D-1"))
              } else {
                return(.ft("L80", "D-2"))
              }
            } else {
              if (s$season %eq% 'dormant') {
                return(.ft("L83", "O-1a"))
              } else {
                return(.ft("L85", "O-1b"))
              }
            }
          } else {
            return(.ft("L87", "N"))
          }
        } else {
          return(.ft("L89", "N"))
          ## SITE VEGETATED
        }
      }
    }
  } else if (isTRUE(s$is_vegetated)) {
    ## SITE FORESTED
    if (isTRUE(s$is_forested)) {
      ## SITE RECENTLY BURNED
      if (isTRUE(s$is_burned) && (!is.na(s$dist_lag)) && (s$dist_lag <= 10)) {
        if ((!is.na(s$pct_cnfr)) && (s$pct_cnfr >= 60)) {
          if ((!is.na(s$CROWN_CLOSURE)) && (s$CROWN_CLOSURE > 40)) {
            if (s$dist_lag <= 3) {
              return(.ft("L100", "N"))
            } else if (s$dist_lag <= 6) {
              if (s$season %eq% 'dormant') {
                return(.ft("L103", "D-1"))
              } else {
                return(.ft("L105", "D-2"))
              }
            } else {
              return(.ft("L107", "C-5"))
            }
          } else {
            if (s$dist_lag <= 1) {
              return(.ft("L110", "N"))
            } else if (s$dist_lag <= 6) {
              if (s$season %eq% 'dormant') {
                return(.ft("L113", "D-1"))
              } else {
                return(.ft("L115", "D-2"))
              }
            } else {
              if (s$season %eq% 'dormant') {
                return(.ft("L118", "O-1a"))
              } else {
                return(.ft("L120", "O-1b"))
              }
            }
          }
        } else {
          if (s$dist_lag <= 1) {
            return(.ft("L123", "N"))
          } else {
            if (s$season %eq% 'dormant') {
              return(.ft("L126", "D-1"))
            } else {
              return(.ft("L128", "D-2"))
              ## SITE NOT RECENTLY BURNED
            }
          }
        }
      } else {
        if ((is.na(s$SPECIES_CD_1)) || (is.na(s$SPECIES_PCT_1)) || (s$SPECIES_PCT_1 %eq% 0)) {
          return(.ft("L134", "VegForestNoBurn_Species-ERROR"))
          ## PURE/SINGLE SPECIES STANDS
        } else if (s$SPECIES_PCT_1 >= 80) {
          if (s$SPECIES_CD_1 %in% bcwsft_conifer_list) {
            ## PURE LODGEPOLE PINE STANDS
            if ((s$SPECIES_CD_1 %in% c('PL', 'PLI', 'PLC', 'PJ', 'PXJ', 'P'))) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 7) {
                return(.ft("L141", "S-1"))
              } else {
                if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                  if (
                    ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF', 'MH'))) ||
                      ((s$BEC_ZONE_CODE %in% c('ICH')) && s$dry_wet %eq% 'wet')
                  ) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L147", "D-1"))
                    } else {
                      return(.ft("L149", "D-2"))
                    }
                  } else {
                    return(.ft("L151", "C-7"))
                  }
                } else {
                  if (s$PROJ_HEIGHT_1 < 4) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L155", "O-1a"))
                    } else {
                      return(.ft("L157", "O-1b"))
                    }
                  } else if (s$PROJ_HEIGHT_1 <= 12) {
                    if (s$stocking > 8000) {
                      return(.ft("L160", "C-4"))
                    } else {
                      return(.ft("L162", "C-3"))
                    }
                  } else if (s$PROJ_HEIGHT_1 > 12) {
                    if (s$CROWN_CLOSURE < 40) {
                      if ((s$BEC_ZONE_CODE %in% c('BG', 'PP', 'IDF', 'MS'))) {
                        return(.ft("L166", "C-7"))
                      } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
                        return(.ft("L168", "C-5"))
                      } else {
                        return(.ft("L170", "C-3"))
                      }
                    } else {
                      if (s$EARLIEST_NONLOGGING_DIST_TYPE %eq% 'IBM') {
                        if (s$dist_lag <= 5) {
                          if (!is.na(s$STAND_PERCENTAGE_DEAD) && s$STAND_PERCENTAGE_DEAD > 50) {
                            return(.ft("L175", "M-3", 65))
                          } else if (
                            !is.na(s$STAND_PERCENTAGE_DEAD) && s$STAND_PERCENTAGE_DEAD >= 25
                          ) {
                            return(.ft("L177", "C-2"))
                          } else {
                            return(.ft("L179", "C-3"))
                          }
                        } else {
                          if (!is.na(s$STAND_PERCENTAGE_DEAD) && s$STAND_PERCENTAGE_DEAD > 50) {
                            return(.ft("L182", "C-2"))
                          } else if (
                            !is.na(s$STAND_PERCENTAGE_DEAD) && s$STAND_PERCENTAGE_DEAD >= 25
                          ) {
                            return(.ft("L184", "C-3"))
                          } else {
                            return(.ft("L186", "C-3"))
                          }
                        }
                      } else {
                        return(.ft("L188", "C-3"))
                        ## PURE PONDEROSA PINE STANDS
                      }
                    }
                  }
                }
              }
            } else if (s$SPECIES_CD_1 %eq% 'PY') {
              if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                if (!is.na(s$harv_lag) && s$harv_lag <= 10) {
                  return(.ft("L193", "S-1"))
                } else {
                  if (s$PROJ_HEIGHT_1 < 4) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L197", "O-1a"))
                    } else {
                      return(.ft("L199", "O-1b"))
                    }
                  } else if (s$PROJ_HEIGHT_1 <= 12) {
                    if (s$stocking > 8000) {
                      return(.ft("L202", "C-4"))
                    } else if (s$stocking >= 3000) {
                      return(.ft("L204", "C-3"))
                    } else {
                      return(.ft("L206", "C-7"))
                    }
                  } else if (s$PROJ_HEIGHT_1 <= 17) {
                    if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                      return(.ft("L209", "C-3"))
                    } else if (s$BCLCS_LEVEL_5 %eq% 'OP') {
                      return(.ft("L211", "C-7"))
                    }
                  } else {
                    return(.ft("L213", "C-7"))
                  }
                }
              } else if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                if (!is.na(s$STAND_PERCENTAGE_DEAD) && s$STAND_PERCENTAGE_DEAD >= 40) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L217", "O-1a"))
                  } else {
                    return(.ft("L219", "O-1b"))
                  }
                } else {
                  if (!is.na(s$harv_lag) && s$harv_lag <= 10) {
                    return(.ft("L222", "S-1"))
                  } else {
                    return(.ft("L224", "C-7"))
                    ## PURE OTHER PINE STANDS
                  }
                }
              }
            } else if ((s$SPECIES_CD_1 %in% c('PA', 'PF', 'PW'))) {
              if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                return(.ft("L228", "C-3"))
              } else if ((s$BCLCS_LEVEL_5 %in% c('SP', 'OP'))) {
                if (s$stocking >= 900) {
                  return(.ft("L231", "C-3"))
                } else if (s$stocking >= 600) {
                  return(.ft("L233", "C-7"))
                } else {
                  return(.ft("L235", "C-5"))
                  ## PURE DOUGLAS-FIR STANDS
                }
              }
            } else if ((s$SPECIES_CD_1 %in% c('FD', 'FDC', 'FDI', 'F'))) {
              ## SITE HARVESTED WITHIN LAST 6 YEARS
              if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                if (
                  ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                    (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                ) {
                  return(.ft("L242", "S-3"))
                } else {
                  return(.ft("L244", "S-1"))
                  ## SITE HARVESTED LONGER THAN 6 YEARS AGO
                }
              } else {
                if (!is.na(s$PROJ_HEIGHT_1) && s$PROJ_HEIGHT_1 < 4) {
                  if (
                    ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                      (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                  ) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L251", "D-1"))
                    } else {
                      return(.ft("L253", "D-2"))
                    }
                  } else {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L256", "O-1a"))
                    } else {
                      return(.ft("L258", "O-1b"))
                    }
                  }
                } else if (!is.na(s$PROJ_HEIGHT_1) && s$PROJ_HEIGHT_1 >= 4) {
                  if (!is.na(s$CROWN_CLOSURE) && s$CROWN_CLOSURE > 55) {
                    if (s$PROJ_HEIGHT_1 <= 12) {
                      if (
                        ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                          (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                      ) {
                        return(.ft("L264", "C-3"))
                      } else {
                        if (((!is.na(s$STAND_PERCENTAGE_DEAD)) && (s$STAND_PERCENTAGE_DEAD > 34))) {
                          return(.ft("L268", "C-4"))
                        } else {
                          return(.ft("L270", "C-3"))
                        }
                      }
                    } else if (s$PROJ_HEIGHT_1 > 12) {
                      if (
                        ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                          (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                      ) {
                        return(.ft("L274", "C-5"))
                      } else {
                        return(.ft("L276", "C-7"))
                      }
                    }
                  } else if (is.na(s$CROWN_CLOSURE) || s$CROWN_CLOSURE >= 26) {
                    if (
                      ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                        (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                    ) {
                      return(.ft("L280", "C-5"))
                    } else {
                      return(.ft("L282", "C-7"))
                    }
                  } else {
                    if (
                      ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                        (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                    ) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L287", "D-1"))
                      } else {
                        return(.ft("L289", "D-2"))
                      }
                    } else {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L292", "O-1a"))
                      } else {
                        return(.ft("L294", "O-1b"))
                      }
                    }
                  }
                } else {
                  return(.ft("L296", "VegForestNoBurnPureFd_ProjHeight-ERROR"))
                  ## PURE ENGELMANN SPRUCE STANDS
                }
              }
            } else if (s$SPECIES_CD_1 %eq% 'SE') {
              if (!is.na(s$harv_lag) && s$harv_lag <= 10) {
                return(.ft("L300", "S-2"))
              } else {
                if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L304", "D-1"))
                  } else {
                    return(.ft("L306", "D-2"))
                  }
                } else if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                  return(.ft("L308", "C-2"))
                } else if (s$BCLCS_LEVEL_5 %eq% 'OP') {
                  return(.ft("L310", "C-3"))
                  ## PURE SITKA SPRUCE STANDS
                }
              }
            } else if (s$SPECIES_CD_1 %eq% 'SS') {
              if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                return(.ft("L314", "S-3"))
              } else {
                if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L318", "D-1"))
                  } else {
                    return(.ft("L320", "D-2"))
                  }
                } else if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                  return(.ft("L322", "C-5"))
                  ## PURE BLACK OR WHITE SPRUCE STANDS
                }
              }
            } else if ((s$SPECIES_CD_1 %in% c('SB', 'SW'))) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 10) {
                return(.ft("L326", "S-2"))
              } else {
                if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                  return(.ft("L329", "C-2"))
                } else if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                  if ((s$BEC_ZONE_CODE %in% c('BWBS', 'SWB'))) {
                    return(.ft("L332", "C-1"))
                  } else {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L335", "M-1", 30))
                    } else {
                      return(.ft("L337", "M-2", 30))
                      ## PURE SPRUCE (UNKNOWN OR HYBRID) STANDS
                    }
                  }
                }
              }
            } else if (startsWith(s$SPECIES_CD_1, 'S')) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 7) {
                return(.ft("L342", "S-2"))
              } else {
                if ((s$BEC_ZONE_CODE %in% c('BWBS', 'SWB'))) {
                  if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                    return(.ft("L346", "C-2"))
                  } else {
                    return(.ft("L348", "C-1"))
                  }
                } else {
                  if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                    return(.ft("L351", "C-7"))
                  } else {
                    if ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF'))) {
                      return(.ft("L354", "C-5"))
                    } else {
                      if (s$PROJ_HEIGHT_1 < 4) {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L358", "O-1a"))
                        } else {
                          return(.ft("L360", "O-1b"))
                        }
                      } else if (s$PROJ_HEIGHT_1 >= 4) {
                        if (s$BCLCS_LEVEL_5 %eq% 'OP') {
                          return(.ft("L363", "C-3"))
                        } else if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                          return(.ft("L365", "C-2"))
                        } else {
                          return(.ft("L369", "VegForestPureOtherSpruceInterior_NoBCLCSLv5-ERROR"))
                        }
                      } else {
                        return(.ft("L373", "VegForestPureOtherSpruceInterior_ProjHeight-ERROR"))
                        ## PURE REDCEDAR, YELLOW CEDAR OR HEMLOCK STANDS
                      }
                    }
                  }
                }
              }
            } else if ((s$SPECIES_CD_1 %in% c('C', 'CW', 'Y', 'YC', 'H', 'HM', 'HW', 'HXM'))) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                return(.ft("L377", "S-3"))
              } else {
                if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                  if (s$PROJ_HEIGHT_1 < 4) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L382", "D-1"))
                    } else {
                      return(.ft("L384", "D-2"))
                    }
                  } else if (s$PROJ_HEIGHT_1 <= 15) {
                    return(.ft("L386", "C-3"))
                  } else if (s$PROJ_HEIGHT_1 > 15) {
                    if (s$PROJ_AGE_1 < 60) {
                      return(.ft("L389", "C-3"))
                    } else if (s$PROJ_AGE_1 <= 99) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L392", "M-1", 30))
                      } else {
                        return(.ft("L394", "M-2", 30))
                      }
                    } else {
                      return(.ft("L396", "C-5"))
                    }
                  }
                } else if (s$BCLCS_LEVEL_5 %eq% 'OP') {
                  return(.ft("L398", "C-5"))
                } else if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L401", "D-1"))
                  } else {
                    return(.ft("L403", "D-2"))
                    ## PURE TRUE FIR STANDS
                  }
                }
              }
            } else if (startsWith(s$SPECIES_CD_1, 'B')) {
              if (s$SPECIES_CD_1 %eq% 'BG') {
                return(.ft("L407", "C-7"))
              } else if (s$SPECIES_CD_1 %eq% 'BA') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L410", "M-1", 30))
                } else {
                  return(.ft("L412", "M-2", 30))
                }
              } else {
                if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                  return(.ft("L415", "C-7"))
                } else {
                  return(.ft("L417", "C-5"))
                  ## PURE YEW STANDS
                }
              }
            } else if ((s$SPECIES_CD_1 %in% c('T', 'TW'))) {
              return(.ft("L420", "C-5"))
              ## PURE JUNIPER STANDS
            } else if ((s$SPECIES_CD_1 %in% c('J', 'JR'))) {
              if (s$season %eq% 'dormant') {
                return(.ft("L424", "O-1a"))
              } else {
                return(.ft("L426", "O-1b"))
              }
            } else {
              return(.ft("L430", "VegForestedPureSpeciesStand_Species-ERROR"))
            }
          } else {
            if (s$season %eq% 'dormant') {
              return(.ft("L433", "D-1"))
            } else {
              return(.ft("L435", "D-2"))
              ## MIXED-SPECIES STANDS
            }
          }
        } else if (s$SPECIES_PCT_1 < 80) {
          ## MIXED-SPECIES DECIDUOUS STANDS
          if (s$pct_cnfr <= 20) {
            if (s$season %eq% 'dormant') {
              return(.ft("L441", "D-1"))
            } else {
              return(.ft("L443", "D-2"))
              ## MIXED-SPECIES CONIFER OR MIXEDWOOD STANDS
            }
          } else if (s$pct_cnfr > 20) {
            ## 21-40% CONIFER = DECIDUOUS DOMINATED MIXEDWOOD STANDS
            if (s$pct_cnfr <= 40) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                return(.ft("L449", "S-2"))
              } else {
                ## DOMINANT CONIFER = BLACK, WHITE, ENGELMANN, OR HYBRID SPRUCE
                if (
                  bcwsft_check_dom_conifers(
                    s,
                    c('SB', 'SW', 'SE', 'SX', 'SXB', 'SXE', 'SXL', 'SXS', 'SXW', 'SXX')
                  )
                ) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L455", "M-1", s$pct_cnfr))
                  } else {
                    return(.ft("L457", "M-2", s$pct_cnfr))
                    ## DOMINANT CONIFER = UNKNOWN SPRUCE
                  }
                } else if (bcwsft_check_dom_conifers(s, c('S'))) {
                  if (s$COAST_INTERIOR_CD %eq% 'C') {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L462", "M-1", s$pct_cnfr * 0.5))
                    } else {
                      return(.ft("L464", "M-2", s$pct_cnfr * 0.5))
                    }
                  } else {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L467", "M-1", s$pct_cnfr))
                    } else {
                      return(.ft("L469", "M-2", s$pct_cnfr))
                      ## DOMINANT CONIFER = ANY OTHER CONIFER
                    }
                  }
                } else {
                  if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L474", "M-1", s$pct_cnfr * 0.5))
                    } else {
                      return(.ft("L476", "M-2", s$pct_cnfr * 0.5))
                    }
                  } else {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L479", "M-1", s$pct_cnfr * 0.7))
                    } else {
                      return(.ft("L481", "M-2", s$pct_cnfr * 0.7))
                      ## 41-65% CONIFER = CONIFER_DOMINATED MIXEDWOOD STANDS
                    }
                  }
                }
              }
            } else if (s$pct_cnfr <= 65) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                return(.ft("L485", "S-1"))
              } else {
                ## DOMINANT CONIFER = LODGEPOLE PINE
                if (bcwsft_check_dom_conifers(s, c('PL', 'PLI', 'PLC', 'PJ', 'PXJ', 'P'))) {
                  if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L491", "M-1", s$pct_cnfr * 0.6))
                    } else {
                      return(.ft("L493", "M-2", s$pct_cnfr * 0.6))
                    }
                  } else if (s$BCLCS_LEVEL_5 %eq% 'OP') {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L496", "M-1", s$pct_cnfr * 0.7))
                    } else {
                      return(.ft("L498", "M-2", s$pct_cnfr * 0.7))
                    }
                  } else if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L501", "M-1", s$pct_cnfr * 0.8))
                    } else {
                      return(.ft("L503", "M-2", s$pct_cnfr * 0.8))
                      ## DOMINANT CONIFER = PONDEROSA PINE
                    }
                  }
                } else if (bcwsft_check_dom_conifers(s, c('PY'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L507", "M-1", s$pct_cnfr * 0.6))
                  } else {
                    return(.ft("L509", "M-2", s$pct_cnfr * 0.6))
                    ## DOMINANT CONIFER = OTHER PINE
                  }
                } else if (bcwsft_check_dom_conifers(s, c('PA', 'PF', 'PW'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L513", "M-1", s$pct_cnfr * 0.5))
                  } else {
                    return(.ft("L515", "M-2", s$pct_cnfr * 0.5))
                    ## DOMINANT CONIFER = DOUGLAS-FIR
                  }
                } else if (bcwsft_check_dom_conifers(s, c('F', 'FD', 'FDC', 'FDI'))) {
                  if ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF', 'ICH'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L520", "M-1", s$pct_cnfr * 0.5))
                    } else {
                      return(.ft("L522", "M-2", s$pct_cnfr * 0.5))
                    }
                  } else {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L525", "M-1", s$pct_cnfr * 0.6))
                    } else {
                      return(.ft("L527", "M-2", s$pct_cnfr * 0.6))
                      ## DOMINANT CONIFER = ENGELMANN SPRUCE
                    }
                  }
                } else if (bcwsft_check_dom_conifers(s, c('SE'))) {
                  if (s$BCLCS_LEVEL_5 %eq% 'SP') {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L532", "M-1", s$pct_cnfr * 0.6))
                    } else {
                      return(.ft("L534", "M-2", s$pct_cnfr * 0.6))
                    }
                  } else if ((s$BCLCS_LEVEL_5 %in% c('OP', 'DE'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L537", "M-1", s$pct_cnfr * 0.9))
                    } else {
                      return(.ft("L539", "M-2", s$pct_cnfr * 0.9))
                      ## DOMINANT CONIFER = SITKA SPRUCE
                    }
                  }
                } else if (bcwsft_check_dom_conifers(s, c('SS'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L543", "M-1", s$pct_cnfr * 0.4))
                  } else {
                    return(.ft("L545", "M-2", s$pct_cnfr * 0.4))
                    ## DOMINANT CONIFER = BLACK OR WHITE SPRUCE
                  }
                } else if (bcwsft_check_dom_conifers(s, c('SB', 'SW'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L549", "M-1", s$pct_cnfr))
                  } else {
                    return(.ft("L551", "M-2", s$pct_cnfr))
                    ## DOMINANT CONIFER = UNKNOWN OR HYBRID SPRUCE
                  }
                } else if (
                  bcwsft_check_dom_conifers(
                    s,
                    c('SX', 'SXB', 'SXE', 'SXL', 'SXS', 'SXW', 'SXX', 'S')
                  )
                ) {
                  if ((s$BEC_ZONE_CODE %in% c('BWBS', 'SWB'))) {
                    if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L557", "M-1", s$pct_cnfr))
                      } else {
                        return(.ft("L559", "M-2", s$pct_cnfr))
                      }
                    } else if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L562", "M-1", s$pct_cnfr * 0.6))
                      } else {
                        return(.ft("L564", "M-2", s$pct_cnfr * 0.6))
                      }
                    }
                  } else {
                    if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L568", "M-1", s$pct_cnfr * 0.6))
                      } else {
                        return(.ft("L570", "M-2", s$pct_cnfr * 0.6))
                      }
                    } else if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                      if (s$COAST_INTERIOR_CD %eq% 'I') {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L574", "M-1", s$pct_cnfr * 0.8))
                        } else {
                          return(.ft("L576", "M-2", s$pct_cnfr * 0.8))
                        }
                      } else if (s$COAST_INTERIOR_CD %eq% 'C') {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L579", "M-1", s$pct_cnfr * 0.5))
                        } else {
                          return(.ft("L581", "M-2", s$pct_cnfr * 0.5))
                        }
                      }
                    } else {
                      return(.ft("L585", "VegForestMixedSpeciesCnfrLT65_BCLCSLv5-ERROR"))
                      ## DOMINANT CONIFER = REDCEDAR, YELLOW CEDAR OR HEMLOCK
                    }
                  }
                } else if (
                  bcwsft_check_dom_conifers(s, c('C', 'CW', 'Y', 'YC', 'H', 'HM', 'HW', 'HXM'))
                ) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L589", "M-1", s$pct_cnfr * 0.4))
                  } else {
                    return(.ft("L591", "M-2", s$pct_cnfr * 0.4))
                    ## DOMINANT CONIFER = FIR
                  }
                } else if (bcwsft_check_dom_conifers(s, c('B', 'BA', 'BG', 'BL'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L595", "M-1", s$pct_cnfr * 0.6))
                  } else {
                    return(.ft("L597", "M-2", s$pct_cnfr * 0.6))
                    ## DOMINANT CONIFER = YEW
                  }
                } else if (bcwsft_check_dom_conifers(s, c('T', 'TW'))) {
                  return(.ft("L600", "C-5"))
                  ## DOMINANT CONIFER = JUNIPER
                } else if (bcwsft_check_dom_conifers(s, c('J', 'JR'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L604", "O-1a"))
                  } else {
                    return(.ft("L606", "O-1b"))
                    ## 66-80% CONIFER = CONIFER_DOMINATED MIXEDWOOD STANDS
                  }
                }
              }
            } else if (s$pct_cnfr <= 80) {
              if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                return(.ft("L610", "S-1"))
              } else {
                ## DOMINANT CONIFER = LODGEPOLE PINE
                if (bcwsft_check_dom_conifers(s, c('PL', 'PLI', 'PLC', 'PJ', 'PXJ', 'P'))) {
                  if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L616", "M-1", s$pct_cnfr * 0.5))
                    } else {
                      return(.ft("L618", "M-2", s$pct_cnfr * 0.5))
                    }
                  } else if ((s$BCLCS_LEVEL_5 %in% c('OP'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L621", "M-1", s$pct_cnfr * 0.7))
                    } else {
                      return(.ft("L623", "M-2", s$pct_cnfr * 0.7))
                    }
                  } else if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L626", "M-1", s$pct_cnfr * 0.8))
                    } else {
                      return(.ft("L628", "M-2", s$pct_cnfr * 0.8))
                      ## DOMINANT CONIFER = PONDEROSA PINE
                    }
                  }
                } else if (bcwsft_check_dom_conifers(s, c('PY'))) {
                  return(.ft("L631", "C-7"))
                  ## DOMINANT CONIFER = OTHER PINE
                } else if (bcwsft_check_dom_conifers(s, c('PA', 'PF', 'PW'))) {
                  return(.ft("L634", "C-5"))
                  ## DOMINANT CONIFER = DOUGLAS-FIR
                } else if (bcwsft_check_dom_conifers(s, c('F', 'FD', 'FDC', 'FDI'))) {
                  if (
                    ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF'))) ||
                      (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                  ) {
                    return(.ft("L639", "C-5"))
                  } else {
                    if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L643", "M-1", s$pct_cnfr * 0.7))
                      } else {
                        return(.ft("L645", "M-2", s$pct_cnfr * 0.7))
                      }
                    } else {
                      return(.ft("L647", "C-7"))
                      ## DOMINANT CONIFER = ENGELMANN SPRUCE
                    }
                  }
                } else if (bcwsft_check_dom_conifers(s, c('SE'))) {
                  if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L652", "M-1", s$pct_cnfr * 0.6))
                    } else {
                      return(.ft("L654", "M-2", s$pct_cnfr * 0.6))
                    }
                  } else if ((s$BCLCS_LEVEL_5 %in% c('OP', 'DE'))) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L657", "M-1", s$pct_cnfr * 0.7))
                    } else {
                      return(.ft("L659", "M-2", s$pct_cnfr * 0.7))
                      ## DOMINANT CONIFER = SITKA SPRUCE
                    }
                  }
                } else if (bcwsft_check_dom_conifers(s, c('SS'))) {
                  return(.ft("L662", "C-5"))
                  ## DOMINANT CONIFER = BLACK OR WHITE SPRUCE
                } else if (bcwsft_check_dom_conifers(s, c('SB', 'SW'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L666", "M-1", s$pct_cnfr))
                  } else {
                    return(.ft("L668", "M-2", s$pct_cnfr))
                    ## DOMINANT CONIFER = UNKNOWN OR HYBRID SPRUCE
                  }
                } else if (
                  bcwsft_check_dom_conifers(
                    s,
                    c('SX', 'SXB', 'SXE', 'SXL', 'SXS', 'SXW', 'SXX', 'S')
                  )
                ) {
                  if ((s$BEC_ZONE_CODE %in% c('BWBS', 'SWB'))) {
                    if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L674", "M-1", s$pct_cnfr))
                      } else {
                        return(.ft("L676", "M-2", s$pct_cnfr))
                      }
                    }
                    if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L679", "M-1", s$pct_cnfr * 0.6))
                      } else {
                        return(.ft("L681", "M-2", s$pct_cnfr * 0.6))
                      }
                    }
                  } else {
                    if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L685", "M-1", s$pct_cnfr * 0.6))
                      } else {
                        return(.ft("L687", "M-2", s$pct_cnfr * 0.6))
                      }
                    } else {
                      if (s$COAST_INTERIOR_CD %eq% 'I') {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L691", "M-1", s$pct_cnfr * 0.8))
                        } else {
                          return(.ft("L693", "M-2", s$pct_cnfr * 0.8))
                        }
                      } else {
                        return(.ft("L695", "C-5"))
                        ## DOMINANT CONIFER = REDCEDAR, YELLOW CEDAR OR HEMLOCK
                      }
                    }
                  }
                } else if (
                  bcwsft_check_dom_conifers(s, c('C', 'CW', 'Y', 'YC', 'H', 'HM', 'HW', 'HXM'))
                ) {
                  return(.ft("L698", "C-5"))
                  ## DOMINANT CONIFER = FIR
                } else if (bcwsft_check_dom_conifers(s, c('B', 'BA', 'BG', 'BL'))) {
                  return(.ft("L701", "C-7"))
                  ## DOMINANT CONIFER = YEW
                } else if (bcwsft_check_dom_conifers(s, c('T', 'TW'))) {
                  return(.ft("L704", "C-5"))
                  ## DOMINANT CONIFER = JUNIPER
                } else if (bcwsft_check_dom_conifers(s, c('J', 'JR'))) {
                  return(.ft("L707", "C-7"))
                  ## 81-100% CONIFER = PURE CONIFER, MIXED-SPECIES STANDS
                }
              }
            } else if (s$pct_cnfr <= 100) {
              ## DOMINANT CONIFER = LODGEPONE PINE
              if ((s$SPECIES_CD_1 %in% c('P', 'PL', 'PLI', 'PLC', 'PJ', 'PXJ'))) {
                if (!is.na(s$harv_lag) && s$harv_lag <= 7) {
                  return(.ft("L713", "S-1"))
                } else {
                  if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                    if (
                      ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF', 'MH'))) ||
                        (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                    ) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L719", "D-1"))
                      } else {
                        return(.ft("L721", "D-2"))
                      }
                    } else {
                      return(.ft("L723", "C-7"))
                    }
                  } else {
                    if (s$PROJ_HEIGHT_1 < 4) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L727", "O-1a"))
                      } else {
                        return(.ft("L729", "O-1b"))
                      }
                    } else if (s$PROJ_HEIGHT_1 >= 4) {
                      if (startsWith(s$SPECIES_CD_2, 'S') || startsWith(s$SPECIES_CD_2, 'B')) {
                        if (s$PROJ_HEIGHT_1 <= 12) {
                          if (s$stocking > 8000) {
                            return(.ft("L735", "C-4"))
                          } else {
                            return(.ft("L737", "C-3"))
                          }
                        } else {
                          if (s$CROWN_CLOSURE < 40) {
                            if ((s$BEC_ZONE_CODE %in% c('BG', 'PP', 'IDF', 'MS'))) {
                              return(.ft("L741", "C-7"))
                            } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
                              return(.ft("L743", "C-5"))
                            } else {
                              return(.ft("L745", "C-3"))
                            }
                          } else {
                            if (s$EARLIEST_NONLOGGING_DIST_TYPE %eq% 'IBM') {
                              if (s$dist_lag <= 5) {
                                if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                                  if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD > 50))
                                  ) {
                                    return(.ft("L752", "M-3", 65))
                                  } else if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD >= 25))
                                  ) {
                                    return(.ft("L755", "C-2"))
                                  } else {
                                    return(.ft("L757", "C-2"))
                                  }
                                } else {
                                  if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD > 50))
                                  ) {
                                    return(.ft("L761", "M-3", 65))
                                  } else if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD >= 25))
                                  ) {
                                    return(.ft("L764", "C-2"))
                                  } else {
                                    return(.ft("L766", "C-3"))
                                  }
                                }
                              } else {
                                if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                                  if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD > 50))
                                  ) {
                                    return(.ft("L771", "C-2"))
                                  } else if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD >= 25))
                                  ) {
                                    return(.ft("L774", "C-2"))
                                  } else {
                                    return(.ft("L776", "C-3"))
                                  }
                                } else {
                                  if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD > 50))
                                  ) {
                                    return(.ft("L780", "C-2"))
                                  } else if (
                                    ((!is.na(s$STAND_PERCENTAGE_DEAD)) &&
                                      (s$STAND_PERCENTAGE_DEAD >= 25))
                                  ) {
                                    return(.ft("L783", "C-3"))
                                  } else {
                                    return(.ft("L785", "C-3"))
                                  }
                                }
                              }
                            } else {
                              return(.ft("L787", "C-3"))
                            }
                          }
                        }
                      } else {
                        if (s$CROWN_CLOSURE < 40) {
                          if ((s$BEC_ZONE_CODE %in% c('IDF', 'PP', 'BG', 'SBPS', 'MS'))) {
                            return(.ft("L791", "C-7"))
                          } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'CDF', 'ICH'))) {
                            return(.ft("L793", "C-5"))
                          } else {
                            return(.ft("L795", "C-3"))
                          }
                        } else {
                          return(.ft("L797", "C-3"))
                          ## DOMINANT CONIFER = PONDEROSA PINE
                        }
                      }
                    }
                  }
                }
              } else if ((s$SPECIES_CD_1 %in% c('PY'))) {
                if ((!is.na(s$harv_lag)) && (s$harv_lag <= 7)) {
                  return(.ft("L801", "S-1"))
                } else {
                  if (s$PROJ_HEIGHT_1 < 4) {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L805", "O-1a"))
                    } else {
                      return(.ft("L807", "O-1b"))
                    }
                  } else {
                    if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                      return(.ft("L810", "C-3"))
                    } else {
                      return(.ft("L812", "C-7"))
                      ## DOMINANT CONIFER = OTHER PINE
                    }
                  }
                }
              } else if ((s$SPECIES_CD_1 %in% c('PA', 'PF', 'PW'))) {
                if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                  return(.ft("L816", "C-3"))
                } else {
                  if (s$stocking >= 900) {
                    return(.ft("L819", "C-3"))
                  } else if (s$stocking >= 600) {
                    return(.ft("L821", "C-7"))
                  } else {
                    return(.ft("L823", "C-5"))
                    ## DOMINANT CONIFER = DOUGLAS-FIR
                  }
                }
              } else if (startsWith(s$SPECIES_CD_1, 'F')) {
                if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                  if (
                    ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                      (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                  ) {
                    return(.ft("L829", "S-3"))
                  } else {
                    return(.ft("L831", "S-1"))
                  }
                } else {
                  if (s$PROJ_HEIGHT_1 < 4) {
                    if (
                      ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                        (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                    ) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L837", "D-1"))
                      } else {
                        return(.ft("L839", "D-2"))
                      }
                    } else {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L842", "O-1a"))
                      } else {
                        return(.ft("L844", "O-1b"))
                      }
                    }
                  } else {
                    if (s$CROWN_CLOSURE > 55) {
                      if (s$PROJ_HEIGHT_1 <= 12) {
                        if (
                          ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                            (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                        ) {
                          return(.ft("L850", "C-5"))
                        } else {
                          if (
                            ((!is.na(s$STAND_PERCENTAGE_DEAD)) && (s$STAND_PERCENTAGE_DEAD > 34))
                          ) {
                            return(.ft("L854", "C-4"))
                          } else {
                            if (s$SPECIES_CD_2 %eq% 'PY') {
                              return(.ft("L857", "C-7"))
                            } else {
                              return(.ft("L859", "C-3"))
                            }
                          }
                        }
                      } else if (s$PROJ_HEIGHT_1 > 12) {
                        if (
                          ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                            (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                        ) {
                          return(.ft("L863", "C-5"))
                        } else {
                          return(.ft("L865", "C-7"))
                        }
                      }
                    } else if (s$CROWN_CLOSURE >= 26 || is.na(s$CROWN_CLOSURE)) {
                      if (
                        ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                          (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                      ) {
                        return(.ft("L869", "C-5"))
                      } else {
                        return(.ft("L871", "C-7"))
                      }
                    } else {
                      if (
                        ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                          (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                      ) {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L876", "D-1"))
                        } else {
                          return(.ft("L878", "D-2"))
                        }
                      } else {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L881", "O-1a"))
                        } else {
                          return(.ft("L883", "O-1b"))
                          ## DOMINANT CONIFER = SPRUCE
                        }
                      }
                    }
                  }
                }
              } else if (startsWith(s$SPECIES_CD_1, 'S')) {
                if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                  if (
                    ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) ||
                      (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
                  ) {
                    return(.ft("L889", "S-3"))
                  } else {
                    return(.ft("L891", "S-2"))
                  }
                } else {
                  if (s$SPECIES_CD_1 %eq% 'SE') {
                    if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                      return(.ft("L895", "C-7"))
                    } else {
                      if ((s$SPECIES_CD_2 %in% c('BL', 'B', 'PL', 'P', 'PLI'))) {
                        if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                          return(.ft("L899", "C-2"))
                        } else {
                          return(.ft("L901", "C-3"))
                        }
                      } else if ((s$SPECIES_CD_2 %in% c('HW', 'HM', 'CW', 'YC'))) {
                        if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                          return(.ft("L904", "C-3"))
                        } else {
                          return(.ft("L906", "C-5"))
                        }
                      } else {
                        return(.ft("L908", "C-3"))
                      }
                    }
                  } else if ((s$SPECIES_CD_1 %in% c('SS'))) {
                    return(.ft("L910", "C-5"))
                  } else if ((s$SPECIES_CD_1 %in% c('SB'))) {
                    if ((s$BCLCS_LEVEL_5 %in% c('DE', 'OP'))) {
                      return(.ft("L913", "C-2"))
                    } else {
                      if ((s$BEC_ZONE_CODE %in% c('BWBS'))) {
                        return(.ft("L916", "C-1"))
                      } else {
                        return(.ft("L918", "C-3"))
                      }
                    }
                  } else {
                    if ((s$BEC_ZONE_CODE %in% c('BWBS'))) {
                      if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                        return(.ft("L922", "C-2"))
                      } else if ((s$BCLCS_LEVEL_5 %in% c('OP'))) {
                        return(.ft("L924", "C-3"))
                      } else {
                        return(.ft("L926", "C-1"))
                      }
                    } else {
                      if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'CDF'))) {
                        if (
                          ((s$SPECIES_CD_2 %in% c('BL', 'B'))) || startsWith(s$SPECIES_CD_2, 'P')
                        ) {
                          if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                            return(.ft("L933", "C-7"))
                          } else {
                            return(.ft("L935", "C-3"))
                          }
                        } else {
                          return(.ft("L937", "C-5"))
                        }
                      } else {
                        if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                          return(.ft("L940", "C-7"))
                        } else {
                          if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                            return(.ft("L943", "C-2"))
                          } else {
                            if (!is.na(s$STAND_PERCENTAGE_DEAD) && s$STAND_PERCENTAGE_DEAD > 34) {
                              return(.ft("L946", "C-2"))
                            } else {
                              if (((s$SPECIES_CD_2 %in% c('PL', 'PLI', 'P')))) {
                                return(.ft("L949", "C-2"))
                              } else {
                                return(.ft("L951", "C-3"))
                                ## DOMINANT CONIFER = REDCEDAR, YELLOW CEDAR OR HEMLOCK
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              } else if (
                startsWith(s$SPECIES_CD_1, 'C') ||
                  startsWith(s$SPECIES_CD_1, 'Y') ||
                  startsWith(s$SPECIES_CD_1, 'H')
              ) {
                if (!is.na(s$harv_lag) && s$harv_lag <= 6) {
                  return(.ft("L956", "S-3"))
                } else {
                  if (s$BCLCS_LEVEL_5 %eq% 'DE') {
                    if (s$PROJ_HEIGHT_1 < 4) {
                      if (s$season %eq% 'dormant') {
                        return(.ft("L961", "D-1"))
                      } else {
                        return(.ft("L963", "D-2"))
                      }
                    } else if (s$PROJ_HEIGHT_1 <= 15) {
                      return(.ft("L965", "C-3"))
                    } else {
                      if (s$PROJ_AGE_1 < 60) {
                        return(.ft("L968", "C-3"))
                      } else if (s$PROJ_AGE_1 <= 99) {
                        if (s$season %eq% 'dormant') {
                          return(.ft("L971", "M-1", 40))
                        } else {
                          return(.ft("L973", "M-2", 40))
                        }
                      } else {
                        return(.ft("L975", "C-5"))
                      }
                    }
                  } else if ((s$BCLCS_LEVEL_5 %in% c('OP'))) {
                    return(.ft("L977", "C-5"))
                  } else {
                    if (s$season %eq% 'dormant') {
                      return(.ft("L980", "D-1"))
                    } else {
                      return(.ft("L982", "D-2"))
                      ## DOMINANT CONIFER = GRAND FIR
                    }
                  }
                }
              } else if ((s$SPECIES_CD_1 %in% c('BG'))) {
                return(.ft("L985", "C-7"))
                ## DOMINANT CONIFER = AMABALIS FIR
              } else if ((s$SPECIES_CD_1 %in% c('BA'))) {
                if ((s$SPECIES_CD_2 %in% c('SE', 'SW', 'S'))) {
                  return(.ft("L989", "C-3"))
                } else {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L992", "M-1", 40))
                  } else {
                    return(.ft("L994", "M-2", 40))
                    ## DOMINANT CONIFER = OTHER FIR
                  }
                }
              } else if ((s$SPECIES_CD_1 %in% c('B', 'BL'))) {
                if (s$COAST_INTERIOR_CD %eq% 'C') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L999", "M-1", 40))
                  } else {
                    return(.ft("L1001", "M-2", 40))
                  }
                } else {
                  if ((s$BCLCS_LEVEL_5 %in% c('SP'))) {
                    return(.ft("L1004", "C-7"))
                  } else if ((s$BCLCS_LEVEL_5 %in% c('DE'))) {
                    if ((s$SPECIES_CD_2 %in% c('SE', 'SW', 'S'))) {
                      return(.ft("L1007", "C-2"))
                    } else {
                      return(.ft("L1009", "C-3"))
                    }
                  } else {
                    return(.ft("L1011", "C-3"))
                    ## DOMINANT CONIFER = YEW
                  }
                }
              } else if ((s$SPECIES_CD_1 %in% c('T', 'TW'))) {
                return(.ft("L1014", "C-5"))
                ## DOMINANT CONIFER = JUNIPER
              } else if ((s$SPECIES_CD_1 %in% c('J', 'JR'))) {
                return(.ft("L1017", "C-7"))
                ## NON-FORESTED SITE
              }
            }
          }
        }
      }
    } else {
      ## SITE RECENTLY BURNED
      if (isTRUE(s$is_burned) && (!is.na(s$dist_lag)) && (s$dist_lag < 11)) {
        if (s$dist_lag <= 1) {
          return(.ft("L1024", "N"))
        } else if (s$dist_lag <= 3) {
          if (s$season %eq% 'dormant') {
            return(.ft("L1027", "D-1"))
          } else {
            return(.ft("L1029", "D-2"))
          }
        } else {
          if (s$season %eq% 'dormant') {
            return(.ft("L1032", "O-1a"))
          } else {
            return(.ft("L1034", "O-1b"))
            ## SITE NOT RECENTLY BURNED
          }
        }
      } else {
        ## SITE LOGGED
        if (isTRUE(s$is_logged)) {
          if (!is.na(s$SPECIES_CD_1)) {
            if (s$harv_lag <= 7) {
              if (startsWith(as.character(s$SPECIES_CD_1), 'P')) {
                return(.ft("L1042", "S-1"))
              } else if (
                startsWith(as.character(s$SPECIES_CD_1), 'S') ||
                  startsWith(as.character(s$SPECIES_CD_1), 'B')
              ) {
                return(.ft("L1044", "S-2"))
              } else if (
                ((s$SPECIES_CD_1 %in% c('CW', 'YC'))) ||
                  startsWith(as.character(s$SPECIES_CD_1), 'H')
              ) {
                return(.ft("L1046", "S-3"))
              } else if (startsWith(as.character(s$SPECIES_CD_1), 'FD')) {
                if ((s$BEC_ZONE_CODE %in% c('CWH', 'ICH'))) {
                  return(.ft("L1049", "S-3"))
                } else {
                  return(.ft("L1051", "S-1"))
                }
              } else {
                return(.ft("L1053", "S-1"))
              }
            } else if (s$harv_lag <= 24) {
              if (
                ((s$BEC_ZONE_CODE %in% c('CWH', 'MH'))) ||
                  (s$BEC_ZONE_CODE %eq% 'ICH' && s$dry_wet %eq% 'wet')
              ) {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1058", "D-1"))
                } else {
                  return(.ft("L1060", "D-2"))
                }
              } else {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1063", "O-1a"))
                } else {
                  return(.ft("L1065", "O-1b"))
                }
              }
            } else {
              if ((s$BEC_ZONE_CODE %in% c('CMA', 'IMA'))) {
                return(.ft("L1068", "N"))
              } else if (s$BEC_ZONE_CODE %eq% 'BAFA') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1071", "D-1"))
                } else {
                  return(.ft("L1073", "D-2"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'CWH') {
                if (s$dry_wet %eq% 'dry') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1077", "M-1", 40))
                  } else {
                    return(.ft("L1079", "M-2", 40))
                  }
                } else {
                  return(.ft("L1081", "C-5"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'BWBS') {
                return(.ft("L1083", "C-2"))
              } else if (s$BEC_ZONE_CODE %eq% 'SWB') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1086", "M-1", 50))
                } else {
                  return(.ft("L1088", "M-2", 50))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'SBS') {
                return(.ft("L1090", "C-3"))
              } else if (s$BEC_ZONE_CODE %eq% 'SBPS') {
                return(.ft("L1092", "C-7"))
              } else if (s$BEC_ZONE_CODE %eq% 'MS') {
                return(.ft("L1094", "C-3"))
              } else if (s$BEC_ZONE_CODE %eq% 'IDF') {
                if (s$dry_wet %eq% 'dry') {
                  return(.ft("L1097", "C-7"))
                } else {
                  return(.ft("L1099", "C-3"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'PP') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1102", "O-1a"))
                } else {
                  return(.ft("L1104", "O-1b"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'BG') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1107", "O-1a"))
                } else {
                  return(.ft("L1109", "O-1b"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'MH') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1112", "D-1"))
                } else {
                  return(.ft("L1114", "D-2"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'ESSF') {
                return(.ft("L1116", "C-3"))
              } else if (s$BEC_ZONE_CODE %eq% 'CDF') {
                if (s$dry_wet %eq% 'dry') {
                  return(.ft("L1119", "C-7"))
                } else {
                  return(.ft("L1121", "C-5"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'ICH') {
                if (s$dry_wet %eq% 'dry') {
                  return(.ft("L1124", "C-3"))
                } else {
                  return(.ft("L1126", "C-5"))
                }
              } else {
                return(.ft("L1130", "VegNonForestUnburnedLoggedGT24HasSpecies_BEC-ERROR"))
              }
            }
          } else {
            if (s$harv_lag <= 5) {
              return(.ft("L1133", "S-1"))
            } else if (s$harv_lag <= 24) {
              if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1137", "D-1"))
                } else {
                  return(.ft("L1139", "D-2"))
                }
              } else {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1142", "O-1a"))
                } else {
                  return(.ft("L1144", "O-1b"))
                }
              }
            } else {
              if ((s$BEC_ZONE_CODE %in% c('CMA', 'IMA'))) {
                return(.ft("L1147", "N"))
              } else if (s$BEC_ZONE_CODE %eq% 'BAFA') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1150", "D-1"))
                } else {
                  return(.ft("L1152", "D-2"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'CWH') {
                if (s$dry_wet %eq% 'dry') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1156", "M-1", 40))
                  } else {
                    return(.ft("L1158", "M-2", 40))
                  }
                } else {
                  return(.ft("L1160", "C-5"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'BWBS') {
                return(.ft("L1162", "C-2"))
              } else if (s$BEC_ZONE_CODE %eq% 'SWB') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1165", "M-1", 25))
                } else {
                  return(.ft("L1167", "M-2", 25))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'SBS') {
                return(.ft("L1169", "C-3"))
              } else if (s$BEC_ZONE_CODE %eq% 'SBPS') {
                return(.ft("L1171", "C-7"))
              } else if (s$BEC_ZONE_CODE %eq% 'MS') {
                return(.ft("L1173", "C-7"))
              } else if (s$BEC_ZONE_CODE %eq% 'IDF') {
                if (s$dry_wet %eq% 'dry') {
                  return(.ft("L1176", "C-7"))
                } else {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1179", "M-1", 50))
                  } else {
                    return(.ft("L1181", "M-2", 50))
                  }
                }
              } else if (s$BEC_ZONE_CODE %eq% 'PP') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1184", "O-1a"))
                } else {
                  return(.ft("L1186", "O-1b"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'BG') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1189", "O-1a"))
                } else {
                  return(.ft("L1191", "O-1b"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'MH') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1194", "D-1"))
                } else {
                  return(.ft("L1196", "D-2"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'ESSF') {
                return(.ft("L1198", "C-7"))
              } else if (s$BEC_ZONE_CODE %eq% 'CDF') {
                if (s$dry_wet %eq% 'dry') {
                  return(.ft("L1201", "C-7"))
                } else {
                  return(.ft("L1203", "C-5"))
                }
              } else if (s$BEC_ZONE_CODE %eq% 'ICH') {
                if (s$dry_wet %eq% 'dry') {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1207", "M-1", 40))
                  } else {
                    return(.ft("L1209", "M-2", 40))
                  }
                } else {
                  return(.ft("L1211", "C-5"))
                }
              } else {
                return(.ft("L1215", "VegNonForestUnburnedLoggedGT24NoSpecies_BEC-ERROR"))
                ## SITE NOT LOGGED
              }
            }
          }
        } else {
          if (!is.na(s$SPECIES_CD_1)) {
            if ((s$BEC_ZONE_CODE %in% c('CMA', 'IMA'))) {
              return(.ft("L1220", "N"))
            } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH', 'BAFA'))) {
              if (s$season %eq% 'dormant') {
                return(.ft("L1223", "D-1"))
              } else {
                return(.ft("L1225", "D-2"))
              }
            } else {
              if (s$season %eq% 'dormant') {
                return(.ft("L1228", "O-1a"))
              } else {
                return(.ft("L1230", "O-1b"))
              }
            }
          } else {
            if (s$INVENTORY_STANDARD_CD %eq% 'F') {
              if ((s$NON_PRODUCTIVE_CD %in% c(11, 12, 13))) {
                if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1236", "D-1"))
                  } else {
                    return(.ft("L1238", "D-2"))
                  }
                } else {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1241", "O-1a"))
                  } else {
                    return(.ft("L1243", "O-1b"))
                  }
                }
              } else if (s$NON_PRODUCTIVE_CD %eq% 35) {
                return(.ft("L1245", "W"))
              } else if (s$NON_PRODUCTIVE_CD %eq% 42) {
                return(.ft("L1247", "N"))
              } else if ((s$NON_PRODUCTIVE_CD %in% c(60, 62, 63))) {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1250", "O-1a"))
                } else {
                  return(.ft("L1252", "O-1b"))
                }
              } else if (is.na(s$NON_PRODUCTIVE_CD)) {
                if ((s$BEC_ZONE_CODE %in% c('CMA', 'IMA'))) {
                  return(.ft("L1255", "N"))
                } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1258", "D-1"))
                  } else {
                    return(.ft("L1260", "D-2"))
                  }
                } else {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1263", "O-1a"))
                  } else {
                    return(.ft("L1265", "O-1b"))
                  }
                }
              } else {
                return(.ft("L1267", "N"))
              }
            } else {
              if ((s$LAND_COVER_CLASS_CD_1 %in% c('LA', 'RE', 'RI', 'OC'))) {
                return(.ft("L1270", "W"))
              } else if (s$LAND_COVER_CLASS_CD_1 %eq% 'HG') {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1273", "O-1a"))
                } else {
                  return(.ft("L1275", "O-1b"))
                }
              } else if ((s$LAND_COVER_CLASS_CD_1 %in% c('BY', 'BM', 'BL'))) {
                if (s$season %eq% 'dormant') {
                  return(.ft("L1278", "D-1"))
                } else {
                  return(.ft("L1280", "D-2"))
                }
              } else if ((s$LAND_COVER_CLASS_CD_1 %in% c('SL', 'ST', 'HE', 'HF', NA))) {
                if ((s$BEC_ZONE_CODE %in% c('CMA', 'IMA'))) {
                  return(.ft("L1283", "N"))
                } else if ((s$BEC_ZONE_CODE %in% c('CWH', 'MH', 'ICH'))) {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1286", "D-1"))
                  } else {
                    return(.ft("L1288", "D-2"))
                  }
                } else {
                  if (s$season %eq% 'dormant') {
                    return(.ft("L1291", "O-1a"))
                  } else {
                    return(.ft("L1293", "O-1b"))
                  }
                }
              } else {
                return(.ft("L1295", "N"))
              }
            }
          }
        }
      }
    }
  }
  .unmatched("py-fallthrough")
}
