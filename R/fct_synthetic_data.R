#' Generate Synthetic CDISC Clinical Trial Data
#'
#' Creates simulated ADaM datasets (ADSL, ADAE, ADLB, ADPD) for dashboard visualization.
#'
#' @param n_subjects Integer. Number of subjects to generate. Default 150.
#' @param seed Integer. Random seed for reproducibility. Default 42.
#'
#' @return A list with tibbles: `adsl`, `adae`, `adlb`, `adpd`
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @noRd
generate_clinical_data <- function(n_subjects = 150, seed = 42) {
  set.seed(seed)
  
  site_ids <- sprintf("SITE-%03d", 101:108)
  arms <- c("Active Drug 100mg", "Active Drug 200mg", "Placebo")
  
  adsl <- tibble::tibble(
    USUBJID = sprintf("STUDY01-%s-%04d", sample(site_ids, n_subjects, replace = TRUE), 1:n_subjects),
    STUDYID = "CLIN-2026-01",
    SITEID  = substr(USUBJID, 9, 16),
    AGE     = round(stats::rnorm(n_subjects, mean = 55, sd = 11)),
    SEX     = sample(c("M", "F"), n_subjects, replace = TRUE, prob = c(0.48, 0.52)),
    RACE    = sample(c("White", "Black/African American", "Asian", "Other"), n_subjects, replace = TRUE, prob = c(0.68, 0.18, 0.10, 0.04)),
    ARM     = sample(arms, n_subjects, replace = TRUE, prob = c(0.35, 0.35, 0.30)),
    RANDDT  = as.Date("2025-01-10") + sort(sample(0:180, n_subjects, replace = TRUE)),
    EOSSTT  = sample(c("Ongoing", "Completed", "Discontinued: Adverse Event", "Discontinued: Lack of Efficacy", "Discontinued: Withdrawal"),
                     n_subjects, replace = TRUE, prob = c(0.40, 0.45, 0.07, 0.05, 0.03)),
    SAFFL   = "Y"
  ) %>%
    dplyr::mutate(
      AGE = pmin(pmax(AGE, 18), 85),
      TRTSDT = RANDDT + 1,
      TRTEDT = dplyr::if_else(grepl("Discontinued", EOSSTT), TRTSDT + sample(14:90, dplyr::n(), replace = TRUE), TRTSDT + 180)
    )

  soc_pt_list <- list(
    "Gastrointestinal disorders" = c("Nausea", "Diarrhoea", "Vomiting", "Abdominal pain"),
    "Nervous system disorders"   = c("Headache", "Dizziness", "Somnolence"),
    "Hepatobiliary disorders"   = c("Hyperbilirubinaemia", "Hepatic enzyme increased", "Jaundice"),
    "General disorders"          = c("Fatigue", "Pyrexia", "Asthenia"),
    "Skin and subcutaneous"      = c("Rash", "Pruritus", "Alopecia"),
    "Infections and infestations"= c("Upper respiratory tract infection", "Nasopharyngitis", "Urinary tract infection")
  )
  
  adae_list <- list()
  for (i in 1:nrow(adsl)) {
    sub <- adsl[i, ]
    prob_ae <- if (sub$ARM == "Placebo") 0.45 else 0.75
    if (stats::runif(1) < prob_ae) {
      n_ae <- stats::rpois(1, lambda = if (sub$ARM == "Placebo") 1.2 else 2.5)
      n_ae <- max(1, min(n_ae, 5))
      
      for (k in 1:n_ae) {
        soc <- sample(names(soc_pt_list), 1)
        pt  <- sample(soc_pt_list[[soc]], 1)
        onset_offset <- sample(1:(as.numeric(sub$TRTEDT - sub$TRTSDT)), 1)
        astdt <- sub$TRTSDT + onset_offset
        aendt <- astdt + sample(2:14, 1)
        
        toxgr <- sample(1:4, 1, prob = c(0.55, 0.30, 0.12, 0.03))
        aeser <- dplyr::if_else(toxgr >= 3 || stats::runif(1) < 0.05, "Y", "N")
        aerel <- dplyr::if_else(sub$ARM != "Placebo" & stats::runif(1) < 0.60, "Related", "Not Related")
        
        adae_list[[length(adae_list) + 1]] <- tibble::tibble(
          USUBJID  = sub$USUBJID,
          ARM      = sub$ARM,
          AEBODSYS = soc,
          AEDECOD  = pt,
          AETOXGR  = toxgr,
          AESEV    = c("Mild", "Moderate", "Severe", "Life-threatening")[toxgr],
          AESER    = aeser,
          AEREL    = aerel,
          ASTDT    = astdt,
          AENDT    = aendt
        )
      }
    }
  }
  adae <- dplyr::bind_rows(adae_list)

  visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12", "Week 24")
  visit_nums <- c(0, 2, 4, 8, 12, 24)
  
  adlb_list <- list()
  for (i in 1:nrow(adsl)) {
    sub <- adsl[i, ]
    liver_signal <- (sub$ARM != "Placebo") && (stats::runif(1) < 0.06)
    
    base_alt <- stats::rnorm(1, mean = 22, sd = 5)
    base_ast <- stats::rnorm(1, mean = 20, sd = 4)
    base_bili<- stats::rnorm(1, mean = 0.6, sd = 0.15)
    base_crea<- stats::rnorm(1, mean = 0.9, sd = 0.15)
    
    for (v_idx in seq_along(visits)) {
      v_name <- visits[v_idx]
      v_num  <- visit_nums[v_idx]
      mult <- if (liver_signal && v_num >= 4) stats::runif(1, 2.5, 6.0) else stats::rnorm(1, 1.0, 0.15)
      
      alt_val <- pmax(5, base_alt * mult)
      ast_val <- pmax(5, base_ast * mult * 0.9)
      bili_val <- pmax(0.1, base_bili * (if (liver_signal && v_num >= 8) stats::runif(1, 2.0, 3.5) else 1.0))
      crea_val <- pmax(0.3, base_crea * stats::rnorm(1, 1.0, 0.1))
      
      adlb_list[[length(adlb_list) + 1]] <- tibble::tibble(
        USUBJID  = sub$USUBJID,
        ARM      = sub$ARM,
        VISIT    = v_name,
        VISITNUM = v_num,
        PARAMCD  = c("ALT", "AST", "BILI", "CREAT"),
        PARAM    = c("Alanine Aminotransferase (U/L)", "Aspartate Aminotransferase (U/L)", "Total Bilirubin (mg/dL)", "Serum Creatinine (mg/dL)"),
        AVAL     = c(alt_val, ast_val, bili_val, crea_val),
        ANRLO    = c(7, 8, 0.2, 0.5),
        ANRHI    = c(35, 35, 1.2, 1.2),
        BASE     = c(base_alt, base_ast, base_bili, base_crea)
      )
    }
  }
  adlb <- dplyr::bind_rows(adlb_list) %>%
    dplyr::mutate(
      ULN_RATIO = AVAL / ANRHI,
      CHG = AVAL - BASE
    )

  adpd <- tibble::tibble(
    SITEID = site_ids,
    INVESTIGATOR = paste("Dr.", c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis")),
    PLANNED_ENROLL = sample(20:30, length(site_ids), replace = TRUE),
    OPEN_QUERIES = sample(2:25, length(site_ids), replace = TRUE),
    PROTOCOL_DEVIATIONS = sample(1:12, length(site_ids), replace = TRUE)
  )

  list(adsl = adsl, adae = adae, adlb = adlb, adpd = adpd)
}

#' Generate Synthetic Clinical Trial Operations & Patient Journey Data
#'
#' Simulates CDISC/ADSL-aligned subject disposition, milestones, rescreening,
#' treatment arms, and longitudinal event timelines for swimmer plots.
#'
#' @param n_subjects Integer. Number of subjects to generate. Default 140.
#' @param seed Integer. Random seed for reproducibility. Default 2026.
#'
#' @return A list with data frames: `subjects` and `events`
#' @import dplyr
#' @import lubridate
#' @export
generate_clinical_trial_data <- function(n_subjects = 140, seed = 2026) {
  set.seed(seed)

  sites <- c("Site 101 - Boston", "Site 102 - Chicago", "Site 103 - Houston", "Site 104 - Seattle", "Site 105 - Atlanta")
  arms <- c("Arm A (Drug 100mg)", "Arm B (Drug 200mg)", "Placebo")

  # Base start date range (1 year window)
  base_date <- as.Date("2025-01-15")

  data_list <- vector("list", n_subjects)
  events_list <- list()

  for (i in 1:n_subjects) {
    usubjid <- sprintf("SUBJ-%03d", i)
    site <- sample(sites, 1, prob = c(0.25, 0.20, 0.20, 0.20, 0.15))

    # 1. ICF Obtained
    icf_date <- base_date + sample(0:300, 1)

    # 2. Screening
    screen_days <- sample(2:10, 1)
    screen_date <- icf_date + screen_days
    is_screen_success <- stats::runif(1) > 0.22 # ~78% screen pass rate

    screen_fail_reason <- NA_character_
    rescreen_flag <- "No"
    rescreen_date <- as.Date(NA)

    if (!is_screen_success) {
      screen_fail_reason <- sample(
        c("Inclusion Criteria Not Met", "Exclusion Criteria Met", "Lab Abnormality", "Patient Withdrawal", "Lost to Follow-Up"),
        1,
        prob = c(0.40, 0.25, 0.15, 0.10, 0.10)
      )
      # Some failures can rescreen
      if (stats::runif(1) < 0.35 && screen_fail_reason %in% c("Lab Abnormality", "Patient Withdrawal")) {
        rescreen_flag <- "Yes"
        rescreen_date <- screen_date + sample(14:30, 1)
        # 60% of rescreens eventually pass
        if (stats::runif(1) < 0.60) {
          is_screen_success <- TRUE
          screen_date <- rescreen_date
        }
      }
    }

    # Defaults
    randomized_flag <- "No"
    rand_date <- as.Date(NA)
    assigned_arm <- "Not Randomized"
    treatment_status <- "Not Applicable"
    treatment_start_date <- as.Date(NA)
    treatment_end_date <- as.Date(NA)
    not_treated_reason <- NA_character_
    current_status <- if (is_screen_success) "Pending Randomization" else "Screen Failed"
    duration_weeks <- round(as.numeric(difftime(screen_date, icf_date, units = "weeks")), 1)

    # 3. Randomization
    if (is_screen_success) {
      if (stats::runif(1) > 0.05) { # 95% of screen successes get randomized
        randomized_flag <- "Yes"
        rand_date <- screen_date + sample(3:12, 1)
        assigned_arm <- sample(arms, 1)

        # 4. Treatment initiation
        is_treated <- stats::runif(1) > 0.07 # 93% of randomized start treatment
        if (is_treated) {
          treatment_status <- "Treatment Started"
          treatment_start_date <- rand_date + sample(1:5, 1)

          # Treatment duration and completion
          tx_outcome <- sample(
            c("On Treatment", "Treatment Completed", "Discontinued - AE", "Discontinued - Disease Progression", "Discontinued - Consent Withdrawn"),
            1,
            prob = c(0.45, 0.30, 0.12, 0.08, 0.05)
          )
          current_status <- tx_outcome

          if (tx_outcome == "On Treatment") {
            tx_weeks <- sample(8:48, 1)
            treatment_end_date <- treatment_start_date + lubridate::weeks(tx_weeks)
          } else if (tx_outcome == "Treatment Completed") {
            tx_weeks <- 52 # 1-year protocol
            treatment_end_date <- treatment_start_date + lubridate::weeks(52)
          } else {
            tx_weeks <- sample(3:26, 1)
            treatment_end_date <- treatment_start_date + lubridate::weeks(tx_weeks)
          }

          duration_weeks <- round(as.numeric(difftime(treatment_end_date, icf_date, units = "weeks")), 1)

          # Events for swimmer plot
          # Milestone: First Dose
          events_list[[length(events_list) + 1]] <- data.frame(
            usubjid = usubjid,
            event_week = round(as.numeric(difftime(treatment_start_date, icf_date, units = "weeks")), 1),
            event_type = "First Dose",
            event_label = paste("First Dose:", treatment_start_date),
            stringsAsFactors = FALSE
          )

          # Adverse Events Grade 3+
          if (stats::runif(1) < 0.28) {
            ae_week <- round(stats::runif(1, 2, max(3, tx_weeks - 1)), 1)
            events_list[[length(events_list) + 1]] <- data.frame(
              usubjid = usubjid,
              event_week = ae_week,
              event_type = "Severe AE (Grade 3+)",
              event_label = paste("Grade 3+ AE at Wk", ae_week),
              stringsAsFactors = FALSE
            )
          }

          # Clinical Response (Partial / Complete)
          if (assigned_arm != "Placebo" && stats::runif(1) < 0.40) {
            resp_week <- sample(c(12, 24, 36), 1)
            if (resp_week < tx_weeks) {
              events_list[[length(events_list) + 1]] <- data.frame(
                usubjid = usubjid,
                event_week = resp_week,
                event_type = "Objective Tumor Response",
                event_label = paste("Confirmed PR/CR at Wk", resp_week),
                stringsAsFactors = FALSE
              )
            }
          }
        } else {
          treatment_status <- "Not Treated"
          current_status <- "Not Treated"
          not_treated_reason <- sample(
            c("Withdrew Consent Pre-Dose", "Intercurrent Illness", "Physician Decision", "Protocol Violation"),
            1,
            prob = c(0.4, 0.3, 0.2, 0.1)
          )
          duration_weeks <- round(as.numeric(difftime(rand_date + 7, icf_date, units = "weeks")), 1)
        }
      }
    }

    # Milestone event: ICF
    events_list[[length(events_list) + 1]] <- data.frame(
      usubjid = usubjid,
      event_week = 0,
      event_type = "ICF Obtained",
      event_label = paste("Consent:", icf_date),
      stringsAsFactors = FALSE
    )

    # Milestone event: Randomization
    if (randomized_flag == "Yes") {
      events_list[[length(events_list) + 1]] <- data.frame(
        usubjid = usubjid,
        event_week = round(as.numeric(difftime(rand_date, icf_date, units = "weeks")), 1),
        event_type = "Randomized",
        event_label = paste("Randomized to", assigned_arm),
        stringsAsFactors = FALSE
      )
    }

    data_list[[i]] <- data.frame(
      usubjid = usubjid,
      site = site,
      icf_date = icf_date,
      screen_date = screen_date,
      screen_status = if (is_screen_success) "Screen Success" else "Screen Failure",
      screen_fail_reason = ifelse(is.na(screen_fail_reason), "N/A", screen_fail_reason),
      rescreen_flag = rescreen_flag,
      rescreen_date = rescreen_date,
      randomized_flag = randomized_flag,
      randomization_date = rand_date,
      arm = assigned_arm,
      treatment_status = treatment_status,
      not_treated_reason = ifelse(is.na(not_treated_reason), "N/A", not_treated_reason),
      treatment_start_date = treatment_start_date,
      treatment_end_date = treatment_end_date,
      current_status = current_status,
      duration_weeks = duration_weeks,
      stringsAsFactors = FALSE
    )
  }

  df_subjects <- do.call(rbind, data_list)
  df_events <- do.call(rbind, events_list)

  list(subjects = df_subjects, events = df_events)
}
