# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

Sys.setlocale("LC_ALL","fr_FR.UTF-8")

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("shiny.port" = 3840, "shiny.host" = "0.0.0.0", "golem.app.prod" = TRUE)
mapdoapp::run_app() # add parameters here (if any)
