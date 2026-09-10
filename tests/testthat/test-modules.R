test_that("mod_disposition UI and server initialize and compute metrics correctly", {
  # Test UI
  ui <- mod_disposition_ui("disp_test")
  expect_s3_class(ui, "shiny.tag.list")
  ui_str <- as.character(ui)
  expect_match(ui_str, "ICF Obtained")
  expect_match(ui_str, "Subject Disposition Flow")

  # Test Server
  raw <- generate_clinical_trial_data(n_subjects = 40, seed = 123)
  df_subj <- raw$subjects
  shiny::testServer(mod_disposition_server, args = list(filtered_subjects = reactive(df_subj)), {
    session$flushReact()
    expect_equal(as.character(output$kpi_icf), as.character(nrow(df_subj)))
    expect_match(output$kpi_screen, "%")
    expect_match(output$kpi_rand, "^[0-9]+$")
    expect_match(output$kpi_treated, "[0-9]+ / [0-9]+")
    expect_equal(as.character(output$kpi_rescreen), as.character(sum(df_subj$rescreen_flag == "Yes")))
    expect_equal(as.character(output$kpi_on_treatment), as.character(sum(df_subj$current_status == "On Treatment")))
  })

  # Test Server with empty data
  shiny::testServer(mod_disposition_server, args = list(filtered_subjects = reactive(df_subj[0, ])), {
    session$flushReact()
    expect_equal(as.character(output$kpi_icf), "0")
    expect_equal(as.character(output$kpi_screen), "0%")
  })
})

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

    # Verify patient plots render without missing aesthetics error
    expect_false(is.null(output$plot_patient_ae_timeline))
    expect_false(is.null(output$plot_patient_trends))
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

test_that("mod_swimmer UI and server render swimmer plot", {
  ui <- mod_swimmer_ui("swimmer_test")
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "Swimmer Plot")

  raw <- generate_clinical_trial_data(n_subjects = 30, seed = 303)
  shiny::testServer(
    mod_swimmer_server,
    args = list(filtered_subjects = reactive(raw$subjects), df_all_events = raw$events),
    {
      session$flushReact()
      expect_false(is.null(output$swimmer_plot))
    }
  )
})

test_that("mod_patient_listing UI and server render table and download CSV", {
  ui <- mod_patient_listing_ui("listing_test")
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "Export CSV")

  raw <- generate_clinical_trial_data(n_subjects = 25, seed = 404)
  shiny::testServer(
    mod_patient_listing_server,
    args = list(filtered_subjects = reactive(raw$subjects)),
    {
      session$flushReact()
      expect_false(is.null(output$patient_table))

      tmp_file <- output$download_csv
      expect_true(file.exists(tmp_file))
      downloaded_data <- read.csv(tmp_file)
      expect_equal(nrow(downloaded_data), nrow(raw$subjects))
      expect_true("usubjid" %in% names(downloaded_data))
    }
  )
})

