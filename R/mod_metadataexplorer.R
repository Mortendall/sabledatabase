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
#####UI for select study####
    output$select_study <- shiny::renderUI({
      #turn age-scales in metadata into a proper numerical scale
      dataobject$scaledata <- dataobject$metadata |>
        dplyr::filter(!stringr::str_detect(`Age (weeks)`,pattern = "-")) |>
        dplyr::mutate(`Age (weeks)` = as.numeric(`Age (weeks)`)) |>
        dplyr::filter(!is.na(`Age (weeks)`)) |>
        dplyr::pull(`Age (weeks)`)

      #create UI
      shiny::tagList(
      bslib::layout_column_wrap(
        bslib::card(
          shiny::h5("Select the criteria you want to filter studies by. Once you
                    are happy with the parameters, press \"find studies\" ")
        )
      ),
      bslib::layout_columns(
        col_widths = c(4,4,4),
        row_heights = c(3,3,3,3),
        shiny::column(12,
                      shiny::tagList(
                        bslib::card(
                        shiny::selectizeInput(
                          inputId = ns("Gender"),
                          choices = unique(dataobject$metadata$Gender),
                          label = "Select Gender(s)",
                          options = list(dropdownParent = 'body'),
                          multiple = TRUE,
                          selected = c("Male", "Female")
                        )
                      ),
                      bslib::card(
                        shiny::selectizeInput(
                          inputId = ns("Strain"),
                          choices = unique(dataobject$metadata$Strain),
                          label = "Select Strain(s)",
                          options = list(dropdownParent = 'body'),
                          multiple = TRUE,
                          selected = "C57BL/6N"
                        )
                      ),
                      bslib::card(
                        shiny::selectizeInput(
                          inputId = ns("Diet"),
                          choices = unique(dataobject$metadata$Diet),
                          label = "Select diet(s)",
                          options = list(dropdownParent = 'body'),
                          selected = "chow",
                          multiple = TRUE
                        )
                      ),
                      bslib::card(
                        shinyWidgets::actionBttn(
                          inputId = ns("find_studies"),
                          label = "Find studies with selected parameters",
                          style = "fill"
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
                          ),
                          bslib::card(
                            shiny::tagList(
                              shiny::checkboxInput(
                                inputId = ns("display_genotype"),
                                label = "Enable genotype selection?",
                                value = FALSE
                            ),
                            shiny::uiOutput(
                              outputId = ns("genotypeUI")
                            )
                            )
                          ),
                          bslib::card(
                            shiny::selectizeInput(
                              inputId = ns("Temperature"),
                              choices = unique(dataobject$metadata$Temperature),
                              options = list(dropdownParent = 'body'),
                              multiple = TRUE,
                              label = "Select one or more temperature degrees",
                              selected = c("4","22","23", "RT")
                            )
                          )
                        ))
        ),
        shiny::tagList(
          shiny::column(12,
                        bslib::card(
                          shiny::tagList(
                            shiny::checkboxInput(
                              inputId = ns("display_treatment"),
                              label = "Enable selection of treatments?",
                              value = FALSE
                            ),
                            shiny::uiOutput(
                              outputId = ns("treatmentUI")
                            )
                          )
                        ),
                        bslib::card(
                          shiny::selectizeInput(
                            inputId = ns("System"),
                            choices = unique(dataobject$metadata$System),
                            label = "Select Sable system",
                            options = list(dropdownParent = "body"),
                            multiple = TRUE,
                            selected = c("1","2","3","4")
                          )
                        )
                        )
        )
      ),
      bslib::layout_column_wrap(
        bslib::card(
          shiny::uiOutput(
            outputId = ns("results")
          )
        )
      ))
    })

    #####UI for results####
    output$results <- shiny::renderUI({
      req(dataobject$selected_studies)
      shiny::tagList(
        shiny::tableOutput(
          outputId = ns("resultstable")
        ),
        shinyWidgets::actionBttn(
          inputId = ns("continue"),
          label = "Continue with selected studies",
          style = "jelly"
        )
        )
    })
    #####UI for treatment options####
    output$treatmentUI <- shiny::renderUI({
      if(isTRUE(input$display_treatment)){
        shiny::selectizeInput(
          inputId = ns("Treatment"),
          choices = unique(dataobject$metadata$Treatment),
          label = "Select treatments",
          options = list(dropdownParent = "body"),
          multiple = TRUE,
          selected = unique(dataobject$metadata$Treatment)
        )
      }
    })

    ######UI for genotype options####
    output$genotypeUI <- shiny::renderUI({
      if(isTRUE(input$display_genotype)){
        shiny::selectizeInput(
          inputId = ns("Genotype"),
          choices = unique(dataobject$metadata$Genotype),
          label = "Select Genotype(s)",
          options = list(dropdownParent = 'body'),
          multiple = TRUE,
          selected = unique(dataobject$metadata$Genotype)
        )
      }
    })

    #####server for resultsTable####
    output$resultstable <- shiny::renderTable({
      dataobject$selected_studies
    })

    #####Filter studies####

    shiny::observeEvent(input$find_studies,{

      selected_studies <- dataobject$metadata |>
        dplyr::filter(Gender %in% input$Gender&
                        Strain %in% input$Strain&
                        Diet %in% input$Diet&
                        Temperature %in% input$Temperature&
                        System %in% input$System
                        )

      if(isTRUE(input$display_genotype)){
        selected_studies <- selected_studies |>
          dplyr::filter(Genotype %in% input$Genotype)
      }

      if(isTRUE(input$display_treatment)){
        selected_studies <- selected_studies |>
          dplyr::filter(Treatment %in% input$Treatment)
      }

      #convert age to numeric
      if(length(selected_studies[,1]!=0)){
        selected_studies <- selected_studies |>
          dplyr::mutate(numeric_age = as.numeric(stringr::str_remove(`Age (weeks)`,
                                                                     pattern = "-[:digit:]+"))) |>
          dplyr::filter(numeric_age>= input$Age[1]&
                          numeric_age<=input$Age[2]) |>
          dplyr::select(-numeric_age)

        #generate summary of selected studies

        selected_studies <- selected_studies |>
          dplyr::group_by(RMPP_ID) |>
          dplyr::summarise(
            System = paste(unique(System),
                           collapse = ", "),
            Gender = paste(unique(Gender),
                           collapse = ", "),
            Age = paste(unique(`Age (weeks)`),
                        collapse = ", "),
            Genotype = paste(unique(Genotype),
                             collapse = ", "),
            Strain = paste(unique(Strain),
                           collapse = ", "),
            Diet = paste(unique(Diet),
                         collapse = ", "),
            Temperature = paste(unique(Temperature),
                                collapse = ", "),
            Treatment = paste(unique(Treatment),
                              collapse = ", ")
          )
        dataobject$selected_studies<- selected_studies
      }

    })

    #####Go to next page####
    shiny::observeEvent(input$continue,{
      shiny::updateTabsetPanel(
        session = parentsession,
        inputId = "inTabset",
        selected = "Dataexplorer"
      )
    })

  })
}

## To be copied in the UI
# mod_metadataexplorer_ui("metadataexplorer_1")

## To be copied in the server
# mod_metadataexplorer_server("metadataexplorer_1")
