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

      dataobject$study_data<- dm::tbl(sabledatabase, input$select_study) |>
                                         dplyr::collect() |>
        dplyr::mutate(Unique_ID = paste(system, cage_id,sep = "_")) |>
        dplyr::left_join( metadata_study,
                         by = c("Unique_ID"="Unique_ID")) |>
        dplyr::rename(Age = `Age (weeks)`)
    })

    #####Graph UI####

    output$graphUI <- shiny::renderUI({
      req(dataobject$selected_studies)
      req(dataobject$study_data)

      col_options <- dplyr::slice_head(dataobject$study_data, n=1) |>
        dplyr::mutate(`Age (weeks)`=NA) |>
        dplyr::select(-Date_Time_1, -envirolightlux_1, -elapsed_h, -elapsed_min,
                      -bodytemp, -colnames(dataobject$metadata))
      col_options <- colnames(col_options)



      shiny::fluidRow(
        shiny::column(width = 2,
                      shiny::selectizeInput(
                        inputId = ns("display_parameter"),
                        label = "Select parameter to display",
                        choices = col_options,
                        selected = "vo2",
                        options = list(dropdownParent = "body")
                      )
                      ),
        shiny::column(width = 2,
                      shiny::selectizeInput(
                        inputId = ns("color_by"),
                        label = "Group by:",
                        choices = c("Unique_ID", "cage_id", "system", "Gender", "Age",
                                    "Genotype", "Strain", "Temperature", "Treatment"),
                        options = list(dropdownParent = "body")
                      )
                      ),
        shiny::uiOutput(
          outputId = ns("y_controls")
        ),
        shiny::column(width = 2,
                      shinyWidgets::prettySwitch(
                        inputId = ns("summarize"),
                        label = "Display data summaried for selected group",
                        value = FALSE
                      )),
        shiny::plotOutput(
          outputId = ns("study_summary")
        ),
        shiny::uiOutput(
          outputId = ns("slidersettings")
        )
      )
    })

    output$y_controls <- shiny::renderUI({
      req(input$display_parameter)
      max_y <- dataobject$study_data |>
        dplyr::pull(input$display_parameter)
      max_y <- max(max_y)

      shiny::tagList(shiny::column(width = 2,
                    shiny::numericInput(
                      inputId = ns("miny"),
                      label = "set Y axis minimum",
                      min = 0,
                      max = max_y,
                      value = 0,
                      width = "80%"
                    )),
      shiny::column(width = 2,
                    shiny::numericInput(
                      inputId = ns("maxy"),
                      label = "set Y axis maximum",
                      min = 0,
                      max = max_y,
                      value = max_y,
                      width = "80%"
                    )))
    })

    #####render graph####

    output$study_summary <- shiny::renderPlot({
      req(input$display_parameter)
      req(input$select_study)
      req(input$color_by)
      req(dataobject$study_data)
      req(input$miny)
      req(input$maxy)
      req(input$selectrange)
      if(isFALSE(input$summarize)){
        summary_plot <- ggplot2::ggplot(dataobject$study_data,
                        ggplot2::aes_string(
                          x = "elapsed_min/60",
                          y = input$display_parameter,
                          color = input$color_by
                        ))+
          ggplot2::geom_line()+
          ggplot2::theme_bw(base_size = 18,
                            base_line_size = 2)+
          ggplot2::ylim(input$miny,
                        input$maxy)+
          ggplot2::xlab("Elapsed hours")+
        ggplot2::xlim(input$selectrange[1],
                      input$selectrange[2])
      }
      else{
        rendered_data <- dataobject$study_data |>
          dplyr::group_by(!!rlang::sym(input$color_by),elapsed_min) |>
          dplyr::summarise(mean = mean(.data[[input$display_parameter]],
                                       na.rm = TRUE))

        summary_plot <- ggplot2::ggplot(rendered_data,
                        ggplot2::aes_string(
                          x = "elapsed_min/60",
                          y = "mean",
                          color = input$color_by
                        ))+
          ggplot2::geom_line()+
          ggplot2::theme_bw(base_size = 18,
                            base_line_size = 2)+
          ggplot2::ylim(input$miny,
                        input$maxy)+
          ggplot2::ylab(input$displayed_parameter)+
          ggplot2::xlab("Elapsed hours")+
          ggplot2::xlim(input$selectrange[1],
                        input$selectrange[2])
      }

      summary_plot

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
          style = "gradient",
          size = "sm"
        ),
        shinyWidgets::actionBttn(
          inputId =ns("add_data"),
          label = "Add selected groups and data window to data processing",
          style = "gradient",
          size = "sm"
        ),
        shinyWidgets::actionBttn(
          inputId = ns("add_data"),
          label = "Go to data processing",
          style = "gradient",
          size = "sm"
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
                        stderr =stats::sd(.data[[input$display_parameter]]))

     displayed_data
    })

    #####Slider settings####
    output$slidersettings <- shiny::renderUI({
      req(dataobject$study_data)
        shiny::sliderInput(
          inputId = ns("selectrange"),
          label = "Select data range",
          min = min(dataobject$study_data$elapsed_min)/60,
          max = max(dataobject$study_data$elapsed_min)/60,
          value = c(min(dataobject$study_data$elapsed_min)/60,
                       max(dataobject$study_data$elapsed_min)/60
                       ),
          width = "350%",
          step = 1
        )
    })

  })
}

## To be copied in the UI
# mod_dataexplorer_ui("dataexplorer_1")

## To be copied in the server
# mod_dataexplorer_server("dataexplorer_1")
