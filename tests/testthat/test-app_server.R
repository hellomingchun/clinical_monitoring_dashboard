test_that("app_server initializes and sets up reactive data correctly", {
  shiny::testServer(app_server, {
    session$flushReact()

    # Base reactive data is populated
    df <- filtered_subjects()
    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0)

    # Initial KPIs render properly
    expect_equal(as.character(output$kpi_icf), as.character(nrow(df)))
    expect_match(output$kpi_screen, "%")
    expect_match(output$kpi_rand, "^[0-9]+$")
    expect_match(output$kpi_treated, "[0-9]+ / [0-9]+")
  })
})

test_that("app_server filters reactively by clinical site", {
  shiny::testServer(app_server, {
    session$flushReact()
    initial_count <- nrow(filtered_subjects())

    # Set site filter
    target_site <- "Site 101 - Boston"
    session$setInputs(site_filter = target_site)
    session$flushReact()

    df_filtered <- filtered_subjects()
    expect_true(all(df_filtered$site == target_site))
    expect_lte(nrow(df_filtered), initial_count)
    expect_equal(as.character(output$kpi_icf), as.character(nrow(df_filtered)))

    # Reset to All Sites
    session$setInputs(site_filter = "All Sites")
    session$flushReact()
    expect_equal(nrow(filtered_subjects()), initial_count)
  })
})

test_that("app_server filters reactively by treatment arm", {
  shiny::testServer(app_server, {
    session$flushReact()

    # Filter to Placebo
    session$setInputs(arm_filter = "Placebo")
    session$flushReact()

    df_filtered <- filtered_subjects()
    expect_true(all(df_filtered$arm == "Placebo"))

    # Filter to Drug 100mg
    session$setInputs(arm_filter = "Arm A (Drug 100mg)")
    session$flushReact()
    expect_true(all(filtered_subjects()$arm == "Arm A (Drug 100mg)"))
  })
})

test_that("app_server filters reactively by subject status", {
  shiny::testServer(app_server, {
    session$flushReact()

    # Filter to On Treatment only
    session$setInputs(status_filter = c("On Treatment"))
    session$flushReact()

    df_filtered <- filtered_subjects()
    expect_true(all(df_filtered$current_status == "On Treatment"))

    # Filter to Screen Failed
    session$setInputs(status_filter = c("Screen Failed"))
    session$flushReact()
    expect_true(all(filtered_subjects()$current_status == "Screen Failed"))
  })
})

test_that("app_server filters reactively by rescreening status", {
  shiny::testServer(app_server, {
    session$flushReact()

    # Rescreened Only
    session$setInputs(rescreen_filter = "Rescreened Only")
    session$flushReact()
    expect_true(all(filtered_subjects()$rescreen_flag == "Yes"))

    # Never Rescreened
    session$setInputs(rescreen_filter = "Never Rescreened")
    session$flushReact()
    expect_true(all(filtered_subjects()$rescreen_flag == "No"))
  })
})

test_that("app_server filters reactively by date range", {
  shiny::testServer(app_server, {
    session$flushReact()
    all_dates <- filtered_subjects()$icf_date
    mid_date <- min(all_dates) + as.integer(difftime(max(all_dates), min(all_dates), units = "days") / 2)

    session$setInputs(date_range = c(min(all_dates), mid_date))
    session$flushReact()

    df_filtered <- filtered_subjects()
    expect_true(all(df_filtered$icf_date >= min(all_dates) & df_filtered$icf_date <= mid_date))
  })
})

test_that("app_server handles empty filter results gracefully", {
  shiny::testServer(app_server, {
    # Provide an impossible date window that matches 0 subjects
    session$setInputs(date_range = c(as.Date("2010-01-01"), as.Date("2010-01-02")))
    session$flushReact()

    expect_equal(nrow(filtered_subjects()), 0)
    expect_equal(as.character(output$kpi_icf), "0")
    expect_equal(as.character(output$kpi_screen), "0%")
  })
})

test_that("app_server download_csv handler generates valid CSV export", {
  shiny::testServer(app_server, {
    session$flushReact()

    tmp_file <- output$download_csv
    expect_true(file.exists(tmp_file))

    downloaded_data <- read.csv(tmp_file)
    expect_equal(nrow(downloaded_data), nrow(filtered_subjects()))
    expect_true("usubjid" %in% names(downloaded_data))
    expect_true("site" %in% names(downloaded_data))
  })
})
