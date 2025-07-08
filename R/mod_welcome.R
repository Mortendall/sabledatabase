#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    row_heights = c(1,2,1),
    col_widths = c(3,6,3),
    shiny::column(12),
    shiny::tagList(
      shiny::column(12),
      bslib::card(shiny::tagList(
        shiny::h4("Welcome to the Sabledatabase"),
        shinyWidgets::actionBttn(
          inputId = ns("metadata"),
          label = "Browse the metadata catalogue",
          style = "gradient"
        ))),
      shiny::column(12)
    ),
    shiny::column(12)
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id, parentsession){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shiny::observeEvent(input$metadata,{
      shiny::updateTabsetPanel(
        session = parentsession,
        inputId = "inTabset",
        selected = "Metadata"
      )
    })

  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
