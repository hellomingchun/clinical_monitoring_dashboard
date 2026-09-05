test_that("app_ui produces a valid Shiny UI tag list with required components", {
  ui <- app_ui()

  expect_s3_class(ui, "shiny.tag.list")

  # Render UI HTML tags to scalar string
  ui_str <- paste(as.character(htmltools::renderTags(ui)$html), collapse = " ")

  # Title check
  expect_match(ui_str, "Clinical Trial Operations &amp; Patient Journey Dashboard|Clinical Trial Operations & Patient Journey Dashboard")

  # Filter inputs presence in sidebar
  expect_match(ui_str, "site_filter")
  expect_match(ui_str, "arm_filter")
  expect_match(ui_str, "status_filter")
  expect_match(ui_str, "rescreen_filter")
  expect_match(ui_str, "date_range")
  expect_match(ui_str, "swimmer_top_n")
  expect_match(ui_str, "swimmer_sort")

  # Navigation and Tab panels
  expect_match(ui_str, "Overview &amp; Disposition")
  expect_match(ui_str, "Swimmer Plot")
  expect_match(ui_str, "Participant Listing")

  # Key KPI and Plot outputs
  expect_match(ui_str, "kpi_icf")
  expect_match(ui_str, "kpi_screen")
  expect_match(ui_str, "kpi_rand")
  expect_match(ui_str, "disposition_funnel_plot")
  expect_match(ui_str, "patient_table")
  expect_match(ui_str, "download_csv")
})

test_that("run_app creates a valid shinyApp object", {
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})
