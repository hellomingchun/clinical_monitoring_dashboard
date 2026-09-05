test_that("mod_overview UI and server initialize and compute metrics correctly", {
  # Test UI
  ui <- mod_overview_ui("overview_test")
  expect_s3_class(ui, "shiny.tag.list")
  ui_str <- as.character(ui)
  expect_match(ui_str, "Randomized Patients")
  expect_match(ui_str, "Cumulative Subject Enrollment Timeline")

  # Test Server
  data <- generate_clinical_data(n_subjects = 30, seed = 123)
  shiny::testServer(mod_overview_server, args = list(data = data), {
    session$flushReact()
    expect_equal(as.character(output$total_rand), as.character(nrow(data$adsl)))
    expect_equal(as.character(output$total_ongoing), as.character(sum(data$adsl$EOSSTT == "Ongoing")))
    expect_equal(as.character(output$total_saes), as.character(sum(data$adae$AESER == "Y")))
    expect_equal(as.character(output$total_sites), as.character(dplyr::n_distinct(data$adsl$SITEID)))
  })
})

test_that("mod_ae_safety UI and server filter and render AE metrics", {
  # Test UI
  ui <- mod_ae_safety_ui("ae_test")
  expect_s3_class(ui, "shiny.tag.list")
  ui_str <- as.character(ui)
  expect_match(ui_str, "Adverse Events by System Organ Class")

  # Test Server
  data <- generate_clinical_data(n_subjects = 30, seed = 456)
  shiny::testServer(mod_ae_safety_server, args = list(data = data), {
    session$setInputs(sel_arm = "All Arms", sel_ser = "All Events", sel_rel = "All")
    session$flushReact()

    df <- adae_filtered()
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), nrow(data$adae))

    # Test serious only filter
    session$setInputs(sel_ser = "Serious Only (SAE)")
    session$flushReact()
    df_sae <- adae_filtered()
    expect_true(all(df_sae$AESER == "Y"))
  })
})

test_that("mod_labs UI and server render laboratory safety plots", {
  ui <- mod_labs_ui("labs_test")
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "Hy's Law Quadrant Analysis")

  data <- generate_clinical_data(n_subjects = 30, seed = 789)
  shiny::testServer(mod_labs_server, args = list(data = data), {
    session$setInputs(sel_param = "ALT")
    session$flushReact()

    # Verify lab data is present
    expect_s3_class(adlb, "data.frame")
    expect_gt(nrow(adlb), 0)
  })
})

test_that("mod_patient_profile UI and server display subject demographics and timelines", {
  ui <- mod_patient_profile_ui("profile_test")
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "Patient Adverse Events Timeline")

  data <- generate_clinical_data(n_subjects = 30, seed = 101)
  shiny::testServer(mod_patient_profile_server, args = list(data = data), {
    test_subj <- data$adsl$USUBJID[1]
    session$setInputs(sel_subject = test_subj)
    session$flushReact()

    sub <- selected_adsl()
    expect_equal(nrow(sub), 1)
    expect_equal(sub$USUBJID, test_subj)
  })
})

test_that("mod_site_performance UI and server calculate site metrics", {
  ui <- mod_site_performance_ui("site_test")
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "Site Recruitment Performance")

  data <- generate_clinical_data(n_subjects = 30, seed = 202)
  shiny::testServer(mod_site_performance_server, args = list(data = data), {
    session$flushReact()

    metrics <- site_metrics()
    expect_s3_class(metrics, "data.frame")
    expect_true(all(c("SITEID", "PLANNED_ENROLL", "ACTUAL_ENROLLED", "PROTOCOL_DEVIATIONS", "OPEN_QUERIES") %in% names(metrics)))
  })
})
