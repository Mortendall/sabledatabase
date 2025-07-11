#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(
      id = "inTabset",
      theme = bslib::bs_theme(version = 5),
      bslib::nav_panel(title = "Welcome",
                       mod_welcome_ui("welcome_1"),
                       value = "Welcome")
    ,
    bslib::nav_panel(title = "Metadata explorer",
                     mod_metadataexplorer_ui("metadataexplorer_1"),
                     value = "Metadata"
    ),
    bslib::nav_panel(title = "Data Explorer",
                     mod_dataexplorer_ui("dataexplorer_1"),
                     value = "Dataexplorer")
  ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sabledatabase"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
