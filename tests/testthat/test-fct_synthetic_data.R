test_that("generate_clinical_trial_data returns expected structure and dimensions", {
  res <- generate_clinical_trial_data(n_subjects = 50, seed = 123)

  expect_type(res, "list")
  expect_named(res, c("subjects", "events"), ignore.order = TRUE)

  subjects <- res$subjects
  events <- res$events

  expect_s3_class(subjects, "data.frame")
  expect_s3_class(events, "data.frame")
  expect_equal(nrow(subjects), 50)
  expect_gt(nrow(events), 0)

  # Check key subject columns
  expected_cols <- c(
    "usubjid", "site", "icf_date", "screen_date", "screen_status",
    "screen_fail_reason", "rescreen_flag", "rescreen_date",
    "randomized_flag", "randomization_date", "arm", "treatment_status",
    "not_treated_reason", "treatment_start_date", "treatment_end_date",
    "current_status", "duration_weeks"
  )
  expect_true(all(expected_cols %in% names(subjects)))
})

test_that("generate_clinical_trial_data is reproducible with identical seeds", {
  d1 <- generate_clinical_trial_data(n_subjects = 30, seed = 999)
  d2 <- generate_clinical_trial_data(n_subjects = 30, seed = 999)

  expect_identical(d1$subjects, d2$subjects)
  expect_identical(d1$events, d2$events)
})

test_that("generate_clinical_trial_data clinical integrity and logic constraints hold", {
  res <- generate_clinical_trial_data(n_subjects = 100, seed = 42)
  subjects <- res$subjects
  events <- res$events

  # Screen failure logic: only screen success or rescreen-passed can be randomized
  screen_failed <- subjects[subjects$screen_status == "Screen Failure", ]
  expect_true(all(screen_failed$randomized_flag == "No"))
  expect_true(all(screen_failed$arm == "Not Randomized"))

  # Non-randomized patients never have active treatment arm
  non_rand <- subjects[subjects$randomized_flag == "No", ]
  expect_true(all(non_rand$arm == "Not Randomized"))
  expect_true(all(is.na(non_rand$randomization_date)))

  # Randomized patients have valid arms
  rand <- subjects[subjects$randomized_flag == "Yes", ]
  valid_arms <- c("Arm A (Drug 100mg)", "Arm B (Drug 200mg)", "Placebo")
  expect_true(all(rand$arm %in% valid_arms))
  expect_false(any(is.na(rand$randomization_date)))

  # Dates consistency: screening is at or after ICF
  expect_true(all(subjects$screen_date >= subjects$icf_date))

  # Treatment start date is at or after randomization date
  treated <- subjects[!is.na(subjects$treatment_start_date), ]
  expect_true(all(treated$treatment_start_date >= treated$randomization_date))

  # Study duration in weeks is non-negative
  expect_true(all(subjects$duration_weeks >= 0))

  # Events milestones check
  expect_true("ICF Obtained" %in% events$event_type)
  expect_true(all(events$event_week >= 0))
})

test_that("generate_clinical_data returns valid CDISC ADaM datasets", {
  cdisc <- generate_clinical_data(n_subjects = 40, seed = 777)

  expect_type(cdisc, "list")
  expect_named(cdisc, c("adsl", "adae", "adlb", "adpd"), ignore.order = TRUE)

  # ADSL
  adsl <- cdisc$adsl
  expect_equal(nrow(adsl), 40)
  expect_true(all(c("USUBJID", "STUDYID", "SITEID", "AGE", "SEX", "ARM", "RANDDT") %in% names(adsl)))
  expect_true(all(adsl$AGE >= 18 & adsl$AGE <= 85))

  # ADAE
  adae <- cdisc$adae
  expect_s3_class(adae, "data.frame")
  expect_true(all(c("USUBJID", "AEDECOD", "AESEV", "AESER", "ASTDT") %in% names(adae)))
  if (nrow(adae) > 0) {
    expect_true(all(adae$AESEV %in% c("Mild", "Moderate", "Severe", "Life-threatening")))
    expect_true(all(adae$AESER %in% c("Y", "N")))
  }

  # ADLB (Laboratory signals)
  adlb <- cdisc$adlb
  expect_s3_class(adlb, "data.frame")
  expect_true(all(c("USUBJID", "PARAMCD", "AVAL", "ANRHI", "ANRLO", "ULN_RATIO") %in% names(adlb)))
  expect_true(all(c("ALT", "AST", "BILI", "CREAT") %in% unique(adlb$PARAMCD)))

  # ADPD (Protocol Deviations / Site operations)
  adpd <- cdisc$adpd
  expect_s3_class(adpd, "data.frame")
  expect_true(all(c("SITEID", "OPEN_QUERIES", "PROTOCOL_DEVIATIONS") %in% names(adpd)))
})
