#' Data analysis for diagnostic samples during the control phase
#'
#' Function to perform the data analysis, necessary for the analysis of the
#' diagnostic samples during the control phase
#'
#' This function is used internally to prepare the input data for the output
#' (tables and plots) in the sub menu "Diagnostic tests control phase" of the
#' ADAPT App.
#'
#' @param herd_summary Data frame; EuFMDIS output file "Herd summary"
#' @param farm_summary Data frame; EuFMDIS output file "Farm summary"
#' @param par_diag_control_ffd_prev numeric between 0 and 100; design
#' prevalence for the computation of the sample size according to freedom from
#' disease
#' @param par_diag_control_ffd_certainty numeric between 0 and 100; desired
#' accuracy for the computation of the sample size according to freedom from
#' disease
#' @param par_diag_control_edta positive integer; Number of blood samples (EDTA)
#' per symptomatic suspect holding
#' @param par_diag_control_serum positive integer; Number of blood samples
#' (serum) per symptomatic suspect holding
#' @param par_diag_control_bulk_milk positive integer; Number of bulk milk
#' samples per dairy farm
#' @param par_diag_control_lesions_smrum positive integer; Number of acute
#' lesion samples for small ruminants per farm
#' @param par_diag_control_lesions_pigs positive integer; Number of acute
#' lesion samples for pigs per farm
#' @param par_diag_control_lesions_cattle positive integer; Number of acute
#' lesion samples for cattle per farm
#' @param rel_cols_farm_summary_dc character vector of column names of the
#' data frame \code{farm_summary} that are required for the analysis
#' @param rel_cols_herd_summary_dc character vector of column names of the
#' data frame \code{herd_summary} that are required for the analysis
#' @param herd_types_dairy character vector listing the different herd types
#' that are associated with dairy herds
#' @param herd_types_small_ruminants character vector listing the different
#' herd types that are associated with small ruminant herds
#' @param herd_types_pigs character vector listing the different herd types
#' that are associated with pig herds
#' @param herd_types_cattle character vector listing the different herd types
#' that are associated with cattle herds
#'
#' @return Returns an aggregated data frame with one line per simulation run.
#' The data frame contains auxilliary variables needed to appriximate the
#' number of diagnostic samples required during the control phase as well as
#' the estimated values for number of bulk milk samples (n_bulk_milk), acute
#' lesions (n_acute_lesion), swabs (n_swabs), blood samples for edta analysis
#' (n_blood_edta) and serum analysis (n_blood_serum).
#' @importFrom magrittr %>%
#' @importFrom rlang .data

# See also Ticket T021
#                 T036
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-27
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
create_diag_control <- function(
    herd_summary, 
    farm_summary,
    par_diag_control_ffd_prev,
    par_diag_control_ffd_certainty,
    par_diag_control_edta,
    par_diag_control_serum,
    par_diag_control_bulk_milk,
    par_diag_control_lesions_smrum,
    par_diag_control_lesions_pigs,
    par_diag_control_lesions_cattle,
    rel_cols_farm_summary_dc,
    rel_cols_herd_summary_dc,
    herd_types_dairy,
    herd_types_small_ruminants,
    herd_types_pigs,
    herd_types_cattle
){
  
  # Normalise column names:
  herd_summary$herd_type <- 
      tolower(
          gsub("[ -]+", "_", herd_summary$herd_type)
      )
  
  # Compute total means ========================================================
  
  # Total proportion of dairy farms in herd summary:
  prop_dairy_overall <- mean(herd_summary$herd_type %in% herd_types_dairy)
  
  # Total proportion of cattle farms in herd summary:
  prop_cattle_overall <- mean(herd_summary$herd_type %in% herd_types_cattle)
  
  # Total proportion of pig farms in herd summary:
  prop_pigs_overall <- mean(herd_summary$herd_type %in% herd_types_pigs)
  
  # Total proportion of small ruminant farms in herd summary:
  prop_cattle_smrum <- mean(
      herd_summary$herd_type %in% herd_types_small_ruminants
  )
  
  # Total proportion of suspect farms that show symptoms in herd summary:
  prop_sus_symptoms_overall <- 
      sum(
          (herd_summary$reason_diagnosed == "SH") & 
              (herd_summary$day_clinical > -1)
      )/sum(herd_summary$reason_diagnosed == "SH")
  
  
  # Herd sizes -----------------------------------------------------------------
  
  # Total average herd size in herd summary:
  mean_n_animals_overall <- mean(herd_summary$herd_size)
  
  # Total average herd size of suspect farms in herd summary:
  ind <- grepl("^SH\\s*", herd_summary$reason_diagnosed)
  if (any(ind)){
    mean_n_animals_sus_overall <- 
        mean(herd_summary$herd_size[ind], na.rm = TRUE)
  } else {
    mean_n_animals_sus_overall <- mean_n_animals_overall
  }
  
  # Total average herd size of contact farms in herd summary:
  ind <- grepl("^CH\\s*", herd_summary$reason_diagnosed)
  if (any(ind)){
    mean_n_animals_contact_overall <- 
        mean(herd_summary$herd_size[ind], na.rm = TRUE)
  } else {
    mean_n_animals_contact_overall <- mean_n_animals_overall
  }
  
  # Sample size freedom from disease (FFD) -------------------------------------
  
  # Scale dorn parameters to interval [0,1]
  prev_ffd <- par_diag_control_ffd_prev/100
  acc_ffd <- par_diag_control_ffd_certainty/100
  
  # Total average FFD sample size in herd summary:
  mean_n_sample_ffd_overall <- 
      mean(
          compute_sample_size_vectorised(
              N = herd_summary$herd_size, 
              prev = prev_ffd,
              accuracy = acc_ffd
          ),
          na.rm = TRUE
      )
  
  # Total average FFD sample size for suspect farms in herd summary:
  ind <- grepl("^SH\\s*", herd_summary$reason_diagnosed)
  if (any(ind)){
    mean_n_sample_ffd_sus_overall <-
        mean(
            compute_sample_size_vectorised(          
                N = herd_summary$herd_size[ind],
                prev = prev_ffd,
                accuracy = acc_ffd
            ),
            na.rm = TRUE
        )
  } else {
    mean_n_sample_ffd_sus_overall <- mean_n_sample_ffd_overall
  }
  
  # Total average FFD sample size for contact farms in herd summary:
  ind <- grepl("^CH\\s*", herd_summary$reason_diagnosed)
  if (any(ind)){
    mean_n_sample_ffd_contact_overall <- 
        mean(
            compute_sample_size_vectorised( 
                N = herd_summary$herd_size[ind],
                prev = prev_ffd,
                accuracy = acc_ffd
            ),
            na.rm = TRUE
        )
  } else {
    mean_n_sample_ffd_contact_overall <- mean_n_sample_ffd_overall
  }
  
  
  # Fixed sample size EDTA -----------------------------------------------------
  
  n_sample_fixed_EDTA <- par_diag_control_edta
  
  # Total average fixed sample size in herd summary (minimum of herd size and 
  # sample size):
  mean_n_sample_fixed_EDTA_overall <- 
      mean(
          pmin(herd_summary$herd_size, n_sample_fixed_EDTA),
          na.rm = TRUE
      )
  
  # Total average fixed sample size for suspect farms in herd summary 
  # (minimum of herd size and sample size):
  ind <- grepl("^SH\\s*", herd_summary$reason_diagnosed)
  if (any(ind)){
    mean_n_sample_fixed_EDTA_sus_overall <- 
        mean(
            pmin(
                n_sample_fixed_EDTA,
                herd_summary$herd_size[ind]
            ),
            na.rm = TRUE
        )
  } else {
    mean_n_sample_fixed_EDTA_sus_overall <- mean_n_sample_fixed_EDTA_overall
  }
  
  # Fixed sample size serum ----------------------------------------------------
  
  n_sample_fixed_serum <- par_diag_control_serum
  
  # Total average fixed sample size in herd summary (minimum of herd size and 
  # sample size):
  mean_n_sample_fixed_serum_overall <- 
      mean(
          pmin(herd_summary$herd_size, n_sample_fixed_serum),
          na.rm = TRUE
      )
  
  # Total average fixed sample size for suspect farms in herd summary 
  # (minimum of herd size and sample size):
  ind <- grepl("^SH\\s*", herd_summary$reason_diagnosed)
  if (any(ind)){
    mean_n_sample_fixed_serum_sus_overall <- 
        mean(
            pmin(
                n_sample_fixed_serum,
                herd_summary$herd_size[ind]
            ), 
            na.rm = TRUE
        )    
  } else {
    mean_n_sample_fixed_serum_sus_overall <- mean_n_sample_fixed_serum_overall
  }
  
  
  # Compute average per run (where available) ==================================
  
  temp <- 
      herd_summary %>%
      dplyr::select(tidyselect::all_of(rel_cols_herd_summary_dc)) %>%
      dplyr::group_by(.data$run) %>%
      dplyr::summarise(
          prop_dairy = ifelse(
              any(.data$herd_type %in% herd_types_dairy),
              mean(.data$herd_type %in% herd_types_dairy),
              prop_dairy_overall
          ),
          prop_smrum = ifelse(
              any(.data$herd_type %in% herd_types_small_ruminants),
              mean(.data$herd_type %in% herd_types_small_ruminants),
              prop_cattle_smrum
          ),
          prop_pigs = ifelse(
              any(.data$herd_type %in% herd_types_pigs),
              mean(.data$herd_type %in% herd_types_pigs),
              prop_pigs_overall
          ),
          prop_cattle = ifelse(
              any(.data$herd_type %in% herd_types_cattle),
              mean(.data$herd_type %in% herd_types_cattle),
              prop_cattle_overall
          ),
          prop_sus_symptoms = ifelse(
              any(.data$reason_diagnosed == "SH"),
              sum((.data$reason_diagnosed == "SH") & (.data$day_clinical > -1))/
                  sum(.data$reason_diagnosed == "SH"),
              prop_sus_symptoms_overall
          ),
          mean_n_animals_sus = ifelse(
              any(grepl("^SH\\s*", .data$reason_diagnosed)),
              mean(.data$herd_size[grepl("^SH\\s*", .data$reason_diagnosed)]),
              mean_n_animals_sus_overall
          ),
          mean_n_animals_contact = ifelse(
              any(grepl("^CH\\s*", .data$reason_diagnosed)),
              mean(.data$herd_size[grepl("^CH\\s*", .data$reason_diagnosed)]),
              mean_n_animals_contact_overall
          ),
          mean_n_sample_ffd_sus = ifelse(
              any(grepl("^SH\\s*", .data$reason_diagnosed)),
              compute_sample_size_vectorised(   
                      N = .data$herd_size[grepl("^SH\\s*", .data$reason_diagnosed)],
                      prev = prev_ffd,
                      accuracy = acc_ffd
                  ) %>%
                  mean(na.rm = TRUE),
              mean_n_sample_ffd_sus_overall
          ),
          mean_n_sample_ffd_contact = ifelse(
              any(grepl("^CH\\s*", .data$reason_diagnosed)),
              compute_sample_size_vectorised(   
                      N = .data$herd_size[grepl("^CH\\s*", .data$reason_diagnosed)],
                      prev = prev_ffd,
                      accuracy = acc_ffd
                  ) %>%
                  mean(na.rm = TRUE),
              mean_n_sample_ffd_contact_overall
          ),
          mean_n_sample_fixed_EDTA_sus = ifelse(
              any(grepl("^SH\\s*", .data$reason_diagnosed)),
              .data$herd_size[grepl("^SH\\s*", .data$reason_diagnosed)] %>%
                  pmin(n_sample_fixed_EDTA) %>%
                  mean(na.rm = TRUE),
              mean_n_sample_fixed_EDTA_sus_overall
          ),
          mean_n_sample_fixed_serum_sus = ifelse(
              any(grepl("^SH\\s*", .data$reason_diagnosed)),
              .data$herd_size[grepl("^SH\\s*", .data$reason_diagnosed)] %>%
                  pmin(n_sample_fixed_serum) %>%
                  mean(na.rm = TRUE),
              mean_n_sample_fixed_serum_sus_overall
          )
      )
  
  # Output value:
  out <- 
      farm_summary %>%
      dplyr::select(dplyr::all_of(rel_cols_farm_summary_dc)) %>%
      dplyr::full_join(temp, by = "run") %>%
      dplyr::mutate(
          run = .data$run,
          n_bulk_milk = round((.data$chs + .data$shs)*.data$prop_dairy*par_diag_control_bulk_milk),
          n_acute_lesion = round(
              .data$shs*.data$prop_sus_symptoms*(
                    .data$prop_smrum*par_diag_control_lesions_smrum +
                    .data$prop_pigs*par_diag_control_lesions_pigs + 
                    .data$prop_cattle*par_diag_control_lesions_cattle
                    )
          ),
          n_swabs = 
              round(.data$shs*(1 - .data$prop_sus_symptoms)*.data$mean_n_sample_ffd_sus) +  
              round(.data$chs*.data$mean_n_sample_ffd_contact),
          n_blood_edta  = 
              round(.data$shs*(.data$prop_sus_symptoms)*.data$mean_n_sample_fixed_EDTA_sus) +  
              round(.data$shs*(1 - .data$prop_sus_symptoms)*.data$mean_n_sample_ffd_sus) +  
              round(.data$chs*.data$mean_n_sample_ffd_contact),
          n_blood_serum  = 
              round(.data$shs*(.data$prop_sus_symptoms)*.data$mean_n_sample_fixed_serum_sus) +  
              round(.data$shs*(1 - .data$prop_sus_symptoms)*.data$mean_n_sample_ffd_sus) +  
              round(.data$chs*.data$mean_n_sample_ffd_contact)
      )   
  return(out)
}
