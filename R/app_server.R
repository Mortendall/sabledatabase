#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  parentsession <- session
  dataobject <- shiny::reactiveValues()
  sabledatabase <- duckdb::dbConnect(
    duckdb::duckdb(
      dbdir = here::here("data/sabledata.duckdb")
    ))
  dataobject$metadata <- dm::tbl(sabledatabase, "metadata") |>
    dplyr::collect()

  mod_metadataexplorer_server("metadataexplorer_1",
                              dataobject,
                              sabledatabase,
                              parentsession)
  mod_welcome_server("welcome_1",
                     parentsession)
  mod_dataexplorer_server("dataexplorer_1",
                          dataobject,
                          sabledatabase,
                          parentsession)

  shiny::onStop(function(){
    cat("Closing database connection")
    duckdb::dbDisconnect(sabledatabase)
  })
}
