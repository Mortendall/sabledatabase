#' assembled_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_assembled_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          shiny::uiOutput(
            outputId = ns("assembled_graph")
          )
        ),
        bslib::card(
          shiny::uiOutput(
            outputId = ns("circadian_average")
          )
        )
      )
    )

  )
}

#' assembled_data Server Functions
#'
#' @noRd
mod_assembled_data_server <- function(id,
                                      dataobject,
                                      sabledatabase,
                                      parentsession){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    #####UI for graph####
    output$assembled_graph <- shiny::renderUI({
      req(dataobject$assembled_data)
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
                      choices = c("ID", "cage_id", "system", "Gender", "Age",
                                  "Genotype", "Strain", "Temperature", "Treatment", "System", "RMPP_ID"),
                      options = list(dropdownParent = "body"),
                      selected = "RMPP_ID"
                    )
      ),
      shiny::uiOutput(
        outputId = ns("y_controls")
      ),
      shiny::column(width = 2,
                    shinyWidgets::prettySwitch(
                      inputId = ns("summarize"),
                      label = "Display data summaried for selected group",
                      value = TRUE
                    )),
      shiny::plotOutput(
        outputId = ns("assembled_figure")
      ),
      shiny::plotOutput(
        outputId = ns("circadian_average")
      ))
    })

    #####Server for graph####
    output$assembled_figure <- shiny::renderPlot({
      req(dataobject$assembled_data)
      req(input$miny)
      req(input$maxy)
      if(isFALSE(input$summarize)){
      summary_plot <- ggplot2::ggplot(dataobject$assembled_data,
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
        ggplot2::xlab("Elapsed hours")

      }
      else{
        rendered_data <- dataobject$assembled_data |>
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
          ggplot2::xlab("Elapsed hours")
      }
      summary_plot
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

    #####circadian_average####
    #CHECK WHY THIS ISNT WORKING
    output$circadian_average <- shiny::renderUI({
      req(dataobject$assembled_data)

    })

    output$circadian_average <- shiny::renderPlot({
      req(input$miny)
      req(input$maxy)
      req(dataobject$assembled_data)

      circadian_data <- dataobject$assembled_data |>
        dplyr::group_by(!!rlang::sym(input$color_by), hour ) |>
        dplyr::summarise(mean = mean(.data[[input$display_parameter]],
                                     na.rm = TRUE))

      ggplot2::ggplot(circadian_data,
                      ggplot2::aes_string(
                        x = "hour",
                        y = "mean",
                        color = input$color_by
                      ))+
        ggplot2::geom_line()+
        ggplot2::theme_bw(base_size = 18,
                          base_line_size = 2)+
        ggplot2::ylim(input$miny,
                      input$maxy)+
        ggplot2::ylab(input$display_parameter)+
        ggplot2::xlab("Hour")+
        ggplot2::ggtitle("Circadian Summary")+
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18)

        )


    })

  })
}

## To be copied in the UI
# mod_assembled_data_ui("assembled_data_1")

## To be copied in the server
# mod_assembled_data_server("assembled_data_1")
