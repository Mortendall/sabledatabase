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
        outputId = ns("summary_plot")
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
        dplyr::rename(Age = `Age (weeks)`) |>
        dplyr::filter(!is.na(System))
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
          inputId = ns("data_processing"),
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
#ADD PROPER LABELING OF AXIS
    output$summary_plot <- shiny::renderUI({
      req(input$display_parameter)
      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("select_xy"),
          label = "Select a parameter for the Y-axis",
          choices = c("ee", "feed", "vo2", "vco2"),
          selected= "ee",
          options = list(dropdownParent = "body"),
          multiple = FALSE
        ),
        shiny::selectizeInput(
          inputId = ns("select_coloring_xy"),
          label = "Select coloring for plot",
          choices = c("Unique_ID", "Gender", "Age",
                      "Genotype", "Strain", "Temperature", "Treatment"),
          selected = "Treatment",
          options = list(dropdownParent = "body")
        ),
        plotly::plotlyOutput(
          outputId = ns("summary_plot_server")
        ),
        shiny::tableOutput(
          outputId = ns("anovatable")
        )
      )

    })

    #####summary plot####
    output$summary_plot_server <- plotly::renderPlotly({
      req(input$display_parameter)
      metadata_study <- dataobject$metadata |>
        dplyr::filter(RMPP_ID == input$select_study) |>
        dplyr::mutate(System = paste0("sable",System),
                      Unique_ID = paste(System, Cage, sep = "_")) |>
      dplyr::rename(Age = `Age (weeks)`) |>
        dplyr::filter(!is.na(System))

     displayed_data <-  dataobject$study_data |>
       dplyr::group_by(Unique_ID) |>
       dplyr::summarise(ee = mean(ee),
                        feed = mean(feed),
                        bodymass = mean(bodymass),
                        vo2 = mean(vo2),
                        vco2 = mean(vco2)) |>
       dplyr::left_join( metadata_study,
                         by = c("Unique_ID"="Unique_ID"))

     xy_plot <- ggplot2::ggplot(
       data = displayed_data,
       mapping = ggplot2::aes_string(
         x = "bodymass",
         y  = input$select_xy,
         color = input$select_coloring_xy,
         label = "Unique_ID"
       )
     )+
       ggplot2::geom_point(size = 8)+
       ggplot2::geom_smooth(method = "lm",
                            se = FALSE)+
       ggplot2::theme_bw()

     xy_plotly <- plotly::ggplotly(xy_plot)

     xy_plotly

    })

    output$anovatable <- shiny::renderTable({
      req(input$display_parameter)
      metadata_study <- dataobject$metadata |>
        dplyr::filter(RMPP_ID == input$select_study) |>
        dplyr::mutate(System = paste0("sable",System),
                      Unique_ID = paste(System, Cage, sep = "_")) |>
        dplyr::rename(Age = `Age (weeks)`) |>
        dplyr::filter(!is.na(System))

      displayed_data <-  dataobject$study_data |>
        dplyr::group_by(Unique_ID) |>
        dplyr::summarise(ee = mean(ee),
                         feed = mean(feed),
                         bodymass = mean(bodymass),
                         vo2 = mean(vo2),
                         vco2 = mean(vco2)) |>
        dplyr::left_join( metadata_study,
                          by = c("Unique_ID"="Unique_ID"))

      anova_table <- displayed_data |> rstatix::anova_test(

       formula =  stats::as.formula(paste(input$select_xy,
                                          "~bodymass * ",
                                          input$select_coloring_xy,
                                          sep = ""))
      )

      rstatix::get_anova_table(anova_table)
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

    #####Add selected data to further processing####
    shiny::observeEvent(input$add_data,{
      #check if there is no assembled data
      if(is.null(dataobject$assembled_data)){
        dataobject$assembled_data <- dataobject$study_data |>
          dplyr::filter(elapsed_min/60 >= input$selectrange[1]&
                          elapsed_min/60 <= input$selectrange[2])|>
          dplyr::mutate(hour =lubridate::hour(Date_Time_1),
                        ID = paste(RMPP_ID, Unique_ID, sep = "_"))
        shiny::showNotification("Data added to assembled data list. Please go
                                to assembled data to explore further",
                                duration = 7)
      }
      else{
        #check if some columns are only present in one dataset, such as running wheels
        if(isTRUE(all(colnames(dataobject$assembled_data)%in% colnames(dataobject$study_data))&
                  all(colnames(dataobject$study_data)%in% colnames(dataobject$assembled_data))
                  )){
          selected_data <- dataobject$study_data |>
            dplyr::filter(elapsed_min/60 >= input$selectrange[1]&
                            elapsed_min/60 <= input$selectrange[2])
          dataobject$assembled_data <- dplyr::rows_append(dataobject$assembled_data,
                                                          selected_data)|>
            dplyr::mutate(hour =lubridate::hour(Date_Time_1),
                          ID = paste(RMPP_ID, Unique_ID, sep = "_"))
          shiny::showNotification("Data added to assembled data list. Please go
                                to assembled data to explore further",
                                duration = 7)

        }
        #if cols are missing from either dataset, add them as NA
        else{
          dataobject$study_data <- dataobject$study_data |>
            dplyr::mutate(ID = paste(RMPP_ID, Unique_ID, sep = "_"))
          cols_assembled <- colnames(dataobject$assembled_data)
          cols_dataset <- colnames(dataobject$study_data)

          #check assembled data
          missing_cols_assembled <- cols_dataset[!cols_dataset %in% cols_assembled]
          if(length(missing_cols_assembled)!=0){
            for (i in 1:length(missing_cols_assembled)){
              new_col <- missing_cols_assembled[i]
              dataobject$assembled_data <- dataobject$assembled_data |>
                dplyr::mutate(!! paste0(new_col) := as.numeric(NA))
            }
          }

          #check selected data
          missing_cols_dataset <- cols_assembled[!cols_assembled %in% cols_dataset]
          selected_data <- dataobject$study_data
          if (length(missing_cols_dataset)!=0){
            for (i in 1:length(missing_cols_dataset)){
              new_col <- missing_cols_dataset[i]
              selected_data <- selected_data |>
                dplyr::mutate(!! paste0(new_col) := as.numeric(NA))
            }
          }

          selected_data <- selected_data |>
            dplyr::filter(elapsed_min/60 >= input$selectrange[1]&
                            elapsed_min/60 <= input$selectrange[2])
          dataobject$assembled_data <- dplyr::rows_append(dataobject$assembled_data,
                                                          selected_data) |>
            dplyr::mutate(hour =lubridate::hour(Date_Time_1))
          shiny::showNotification("Data added to assembled data list. Please go
                                to assembled data to explore further",
                                duration = 7)
        }

      }
    })

    #go to next page

    shiny::observeEvent(input$data_processing,{
      shiny::updateTabsetPanel(
        session = parentsession,
        inputId = "inTabset",
        selected = "Assembleddata"
      )
    })

  })
}

## To be copied in the UI
# mod_dataexplorer_ui("dataexplorer_1")

## To be copied in the server
# mod_dataexplorer_server("dataexplorer_1")
