#' metadataexplorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metadataexplorer_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shiny::uiOutput(
      outputId = ns("select_study")
    )
  )
}

#' metadataexplorer Server Functions
#'
#' @noRd
mod_metadataexplorer_server <- function(id,
                                        dataobject,
                                        sabledatabase,
                                        parentsession){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$select_study <- shiny::renderUI({
      #turn age-scales in metadata into a proper numerical scale
      dataobject$scaledata <- dataobject$metadata |>
        dplyr::filter(!stringr::str_detect(`Age (weeks)`,pattern = "-")) |>
        dplyr::mutate(`Age (weeks)` = as.numeric(`Age (weeks)`)) |>
        dplyr::filter(!is.na(`Age (weeks)`)) |>
        dplyr::pull(`Age (weeks)`)


      bslib::layout_columns(
        col_widths = c(4,4,4),
        row_heights = c(3,3,3,3),
        shiny::column(12,
                      shiny::tagList(
                        bslib::card(
                        shiny::selectizeInput(
                          inputId = ns("Gender"),
                          choices = unique(dataobject$metadata$Gender),
                          label = "Select Gender",
                          options = list(dropdownParent = 'body')
                        )
                      ),
                      bslib::card(
                        shiny::selectizeInput(
                          inputId = ns("Strain"),
                          choices = unique(dataobject$metadata$Strain),
                          label = "Select Strain",
                          options = list(dropdownParent = 'body')
                        )
                      ),
                      bslib::card(
                        shiny::selectizeInput(
                          inputId = ns("Diet"),
                          choices = unique(dataobject$metadata$Diet),
                          label = "Select diet",
                          options = list(dropdownParent = 'body')
                        )
                      )))
        ,
        shiny::tagList(
          shiny::column(12,
                        shiny::tagList(
                          bslib::card(
                            shiny::sliderInput(
                              inputId = ns("Age"),
                              label = "Select age range",
                              min = min(dataobject$scaledata),
                              max = max(dataobject$scaledata),
                              value = c(min(dataobject$scaledata),
                                        max(dataobject$scaledata))

                            )
                          )
                        ))
        ),
        shiny::tagList(
          shiny::column(12)
        )
      )
    })
  })
}

## To be copied in the UI
# mod_metadataexplorer_ui("metadataexplorer_1")

## To be copied in the server
# mod_metadataexplorer_server("metadataexplorer_1")
