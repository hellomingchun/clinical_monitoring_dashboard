test_that("app_sys finds installed files or returns empty string for nonexistent files", {
  # Looking for nonexistent file
  expect_equal(app_sys("nonexistent_test_file.xyz"), "")
})

test_that("get_golem_config reads configuration parameters correctly", {
  cfg_file <- system.file("golem-config.yaml", package = "clinicalmonitoringdashboard")
  if (cfg_file == "") {
    cfg_file <- file.path("..", "..", "inst", "golem-config.yaml")
  }
  if (!file.exists(cfg_file)) {
    cfg_file <- file.path("inst", "golem-config.yaml")
  }

  skip_if_not(file.exists(cfg_file), "golem-config.yaml not found")

  golem_name <- get_golem_config("golem_name", file = cfg_file)
  expect_equal(golem_name, "clinicalmonitoringdashboard")

  golem_version <- get_golem_config("golem_version", file = cfg_file)
  expect_equal(golem_version, "0.0.0.9000")

  # Default app_prod should be FALSE / "no"
  app_prod_default <- get_golem_config("app_prod", config = "default", file = cfg_file)
  expect_false(isTRUE(app_prod_default))

  # Production app_prod should be TRUE / "yes"
  app_prod_prod <- get_golem_config("app_prod", config = "production", file = cfg_file)
  expect_true(isTRUE(app_prod_prod))
})
