#' dataexplorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataexplorer_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        shiny::uiOutput(
          outputId = ns("controlUI")
        )
      )),
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        shiny::uiOutput(
          outputId = ns("graphUI")
        )
    )
  ),
  bslib::layout_column_wrap(
    width = 1/2,
    bslib::card(
      shiny::uiOutput(
        outputId = ns("summary_table")
        )),
      bslib::card(
          shiny::uiOutput(
            outputId = ns("exclusions")
          )
    )
  )
  )

}

#' dataexplorer Server Functions
#'
#' @noRd
mod_dataexplorer_server <- function(id, dataobject, sabledatabase, parentsession){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #####UI for graphs and control####

    output$controlUI <- shiny::renderUI({
      req(dataobject$selected_studies)
      shiny::tagList(
        shiny::tableOutput(
          outputId = ns("selected_studies")
        ),
        shiny::selectizeInput(
          inputId = ns("select_study"),
          label = "Select one of your studies to display",
          choices = dataobject$selected_studies$RMPP_ID,width = "40%",
          options = list(dropdownParent = "body"),
          selected = dplyr::first(dataobject$selected_studies$RMPP_ID)
        )
      )
    })
    #render selected studies table
    output$selected_studies <- shiny::renderTable({
      dataobject$selected_studies
    })

    #####register study selection####
    shiny::observeEvent(input$select_study,{
      req(dataobject$selected_studies)
      #req(input$display_parameter)
      metadata_study <- dataobject$metadata |>
        dplyr::filter(RMPP_ID == input$select_study) |>
        dplyr::mutate(System = paste0("sable",System),
                      Unique_ID = paste(System, Cage, sep = "_"))
      View(metadata_study)

      dataobject$study_data<- dm::tbl(sabledatabase, input$select_study) |>
                                         dplyr::collect() |>
        dplyr::mutate(Unique_ID = paste(system, cage_id,sep = "_")) |>
        dplyr::left_join( metadata_study,
                         by = c("Unique_ID"="Unique_ID")) |>
        dplyr::rename(Age = `Age (weeks)`)

      View(dataobject$study_data)


    })

    #####Graph UI####

    output$graphUI <- shiny::renderUI({
      req(dataobject$selected_studies)
      req(dataobject$study_data)

      col_options <- dplyr::slice_head(dataobject$study_data, n=1) |>
        dplyr::select(-Date_Time_1, -envirolightlux_1, -elapsed_h, -elapsed_min,
                      -bodytemp)
      col_options <- colnames(col_options)
      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("display_parameter"),
          label = "Select parameter to display",
          choices = col_options,
          selected = "vo2",
          options = list(dropdownParent = "body")
        ),
        shiny::selectizeInput(
          inputId = ns("color_by"),
          label = "Group by:",
          choices = c("Unique_ID", "cage_id", "system", "Gender", "Age",
                      "Genotype", "Strain", "Temperature", "Treatment"),
          options = list(dropdownParent = "body")
        ),
        shiny::plotOutput(
          outputId = ns("study_summary")
        )
      )
    })

    #####render plotly graph####

    output$study_summary <- shiny::renderPlot({
      req(input$display_parameter)
      req(input$select_study)
      req(input$color_by)
      req(dataobject$study_data)

     ggplot2::ggplot(dataobject$study_data,
                      ggplot2::aes_string(
                        x = "Date_Time_1",
                        y = input$display_parameter,
                        color = input$color_by
                      ))+
        ggplot2::geom_line()+
       ggplot2::theme_bw(base_size = 18,
                        base_line_size = 2)
    })

    #####UI for exclusion parameters####
    output$exclusions <- shiny::renderUI({
      req(dataobject$selected_studies)
      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("exclude_list"),
          label = "Select cages to exclude from overview",
          choices = unique(dataobject$study_data$Unique_ID),
          options = list(dropdownParent = "body"),
          multiple = TRUE
        ),
        shinyWidgets::actionBttn(
          inputId = ns("exclude_cages"),
          label = "Exclude cages",
          style = "jelly",
          width = "40%"
        ),
        shiny::tableOutput(
          outputId = ns("metadata")
        )
      )
    })

    #####Remove excluded cages####
    shiny::observeEvent(input$exclude_cages,{
      dataobject$study_data <- dataobject$study_data |>
        dplyr::filter(!Unique_ID %in% input$exclude_list)
    })

    output$summary_table <- shiny::renderUI({
      req(input$display_parameter)
      shiny::tableOutput(
        outputId = ns("summary_table_server")
      )
    })

    #####metadata table####
    output$summary_table_server <- shiny::renderTable({
      req(input$display_parameter)
     displayed_data <-  dataobject$study_data |>
       dplyr::group_by(Unique_ID) |>
       dplyr::summarise(Mean =mean(.data[[input$display_parameter]]),
                        stderr =sd(.data[[input$display_parameter]]))

     displayed_data
    })

  })
}

## To be copied in the UI
# mod_dataexplorer_ui("dataexplorer_1")

## To be copied in the server
# mod_dataexplorer_server("dataexplorer_1")
