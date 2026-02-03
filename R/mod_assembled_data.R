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
            outputId = ns("caloric_content")
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

    #####add caloric content to data####

    output$caloric_content <- shiny::renderUI({
      req(dataobject$assembled_data)
      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("select_variable"),
          label = "Select variable for grouping",
          choices = c("cage_id", "system", "Gender", "Age",
                      "Genotype", "Strain", "Temperature", "Treatment"),
          selected = "Treatment",
          multiple = FALSE
        ),
        shinyWidgets::actionBttn(
          inputId = ns("add_diet"),
          label = "Add diet to group",
          style = "jelly"
        ),
        rhandsontable::rHandsontableOutput(
        outputId = ns("detected_groups")
        ))

    })

    output$detected_groups <- rhandsontable::renderRHandsontable({


      dataobject$groups <- dataobject$assembled_data |>
        dplyr::select(input$select_variable) |>
        dplyr::distinct(.keep_all = FALSE) |>
        dplyr::mutate(diet = NA) |>
        dplyr::rename("Groups"=input$select_variable)

      rhandsontable::rhandsontable(
        data = dataobject$groups,
        rowHeaders = FALSE
      ) |>
        rhandsontable::hot_col(
          col = "Groups",
          readOnly = T
        ) |>
        rhandsontable::hot_col(
          col = "diet",
          type = "dropdown",
          source = c("chow",
                     "HFD",
                     "HFHS")
        )
    })

    #####add caloric content to diet data####

    shiny::observeEvent(input$add_diet,{
      dataobject$groups <- rhandsontable::hot_to_r(input$detected_groups)

      if(any(is.na(dataobject$groups))){
        shinyWidgets::sendSweetAlert(
          title = "Group assignment missing",
          text = "One or more groups are missing diet assignments",
          type = "error"
          )
      }
      else{
        #####FIX THIS ERROR####
        variable_name <- input$select_variable
        dataobject$assembled_data <- dplyr::left_join(
          dataobject$assembled_data,
          dataobject$groups,
          by = c("Treatment" = "Groups")
        )
        shiny::showNotification("Groups added")
      }
    })

  })
}

## To be copied in the UI
# mod_assembled_data_ui("assembled_data_1")

## To be copied in the server
# mod_assembled_data_server("assembled_data_1")
