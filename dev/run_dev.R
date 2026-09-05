# Set options here
options(golem.app.prod = FALSE) # Set to TRUE for production

# Detach all loaded packages and clean environment
golem::detach_all_attached()

# Document and reload package
golem::document_and_reload()

# Run the application
run_app()
