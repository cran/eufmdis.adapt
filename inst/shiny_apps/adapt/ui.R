# User interface for the ADAPT App
# Version 05
# 2023-03-20

# Define user interface:
shiny::shinyUI(
  shinydashboard::dashboardPage(

    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # HEADER -------------------------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # dashboardHeader(title = "EuFMDiS Output Analysis"),

    shinydashboard::dashboardHeader(
      title = "EuFMDiS Output Analysis",
      tags$li(
        a(
          href = 'https://www.vetmeduni.ac.at/en/',
          img(
            src = 'logo_vetmeduni_negativ.png',
            title = "University of Veterinary Medicine, Vienna",
            height = "38px"
          ),
          style = "padding-top:6px; padding-bottom:6px;"
        ),
        class = "dropdown"
      ),
      tags$li(
        a(
          href = 'https://www.ages.at/en/',
          img(
            src = 'ages_logo_webfarbe_transparent.png',
            title = "Austrian Agency for Health and Food Safety",
            height = "50px"
          ),
          style = "padding-top:0px; padding-bottom:0px;"
        ),
        class = "dropdown"
      ),
      tags$li(
        a(
          href = 'https://www.eufmd.info/',
          img(
            src = 'Logo_EuFMD_2023.png',
            title = "European Commission for the Control of Foot-and-Mouth-Disease",
            height = "40px"
          ),
          style = "padding-top:5px; padding-bottom:5px;"
        ),
        class = "dropdown"
      ),
      tags$li(
        a(
          href = 'https://european-union.europa.eu/index_en',
          img(
            src = 'EN-Funded_by_the_EU-NEG.png',
            title = "Funded by the European Union",
            height = "30px"
          ),
          style = "padding-top:10px; padding-bottom:10px;"
        ),
        class = "dropdown"
      )
    ),

    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # SIDEBAR ------------------------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Icons from https://fontawesome.com/

    shinydashboard::dashboardSidebar(
      shiny::fileInput(
        inputId = "upload",
        label = "Upload EuFMDiS output Files",
        buttonLabel = "Select files...",
        multiple = TRUE,
        accept = ".csv"
      ),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Configuration",
          tabName = "config",
          icon = icon("chart-bar"),
          selected = FALSE
        ),

        shinydashboard::menuItem(
          "Epidemiology",
          tabName = "epidemiology",
          # icon = icon("chart-line"),
          icon = icon("virus"),
          selected = TRUE,
          startExpanded = TRUE,
          menuSubItem("General data", tabName = "epi_general", selected = TRUE),
          menuSubItem("Data per farm type", tabName = "epi_type")
        ),

        shinydashboard::menuItem(
          "Diagnostic tests control phase",
          tabName = "diag_control",
          icon = icon("vial"),
          selected = FALSE
        ),

        shinydashboard::menuItem(
          "Economic Analysis",
          tabName = "econ",
          icon = icon("euro-sign"),
          # icon = icon("chart-line"),
          selected = FALSE
        ),

        shinydashboard::menuItem(
          "Post Outbreak Management",
          tabName = "postoutbr",
          icon = icon("eye"),
          # icon = icon("chart-line"),
          selected = FALSE
        )

      ), # end sidebarMenu

      htmltools::div(
        shiny::downloadButton(
          outputId = "download_all_tables",
          label = "Download all tables"
        ),
        class = "download_button"
      )

    ), # end dashboardSidebar


    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # BODY ---------------------------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      shinydashboard::tabItems(

        # TAB: CONFIGURATION ===================================================

        shinydashboard::tabItem(
          tabName = "config",

          # Row: Table + plot distribution
          shiny::fluidRow(
            shinydashboard::box(
              title = "Configuration of Simulation",
              htmlOutput(outputId = "check_tab_config"),
              DT::dataTableOutput(outputId = "tab_config")
            )
          ) # End fluidRow
        ), # End tabItem


        # TAB: EPIDEMIOLOGY -> GENERAL DATA ====================================

        shinydashboard::tabItem(
          tabName = "epi_general",

          # ------------------------
          # Row: For Debugging only:
          # ------------------------
          # fluidRow(
          #   box(
          #     title = "Debugging",
          #     verbatimTextOutput(outputId = "debug")
          #   )
          # ), # End fluidRow

          # Row: Table + plot distribution
          shiny::fluidRow(
            shinydashboard::box(
              title = "General analysis",
              shiny::htmlOutput(outputId = "check_tab_epi_general"),
              DT::dataTableOutput(outputId = "table_epi_general")
            ),
            shinydashboard::box(
              title = "View distribution",
              shiny::selectInput(
                inputId = "par_plot_epi_general",
                label = "Choose parameter",
                choices = parameters_epi_general$par %>%
                  setNames(parameters_epi_general$Parameter),
                selected = NULL,
                multiple = FALSE
              ),
              shiny::downloadButton(
                outputId = "download_plot_epi_general",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_epi_general"),
              shiny::plotOutput(outputId = "plot_epi_general")
            )
          ), # End fluidRow

          # Row: Line plots
          shiny::fluidRow(
            shinydashboard::box(
              title = "Time series",
              shiny::selectInput(
                inputId = "par_plot_epi_line",
                label = "Choose parameter",
                choices = parameters_epi_line_plots$Parameter,
                selected = NULL,
                multiple = FALSE
              ),
              shiny::radioButtons(
                inputId = "aggr_plot_epi_line",
                label = "",
                choices = c("Per day", "Cumulative"),
                selected = "Per day"
              ),
              shiny::downloadButton(
                outputId = "download_plot_epi_line",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_epi_line"),
              shiny::plotOutput(outputId = "plot_epi_line"),
              width = 12
            )
          ) # End fluidRow
        ), # End tabItem


        # TAB: EPIDEMIOLOGY -> DATA PER FARM TYPE ==============================

        shinydashboard::tabItem(
          tabName = "epi_type",

          # Row: Picker farm type
          shiny::fluidRow(
            shinydashboard::box(
              # selectInput(
              #   inputId = "par_epi_farm_type",
              #   label = "Choose farm type",
              #   choices = names_farm_types$par %>%
              #     setNames(names_farm_types$Parameter),
              #   selected = NULL,
              #   multiple = FALSE
              # ),
              shiny::uiOutput("picker_farm_types"),
              width = 12
            )
          ), # End fluidRow

          # Row: Table + plot distribution
          shiny::fluidRow(
            shinydashboard::box(
              title = "Results per farm type",
              shiny::htmlOutput(outputId = "check_table_epi_types"),
              DT::dataTableOutput(outputId = "table_epi_types")
            ),
            shinydashboard::box(
              title = "View distribution per farm type",
              shiny::selectInput(
                inputId = "par_plot_epi_type",
                label = "Choose parameter",
                choices = parameters_epi_types$par %>%
                  setNames(parameters_epi_types$Parameter),
                selected = NULL,
                multiple = FALSE
              ),
              shiny::downloadButton(
                outputId = "download_plot_epi_types",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_epi_types"),
              shiny::plotOutput(outputId = "plot_epi_types")
            )
          ) # End fluidRow
        ), # End tabItem


        # TAB: ECONOMIC ANALYSIS ===============================================

        shinydashboard::tabItem(
          tabName = "econ",

          # Row: Picker farm type
          shiny::fluidRow(
            box(
              uiOutput("picker_countries"),
              width = 12
            )
          ), # End fluidRow

          # Row: Table + plot distribution
          shiny::fluidRow(
            shinydashboard::box(
              title = "Results for a country",
              htmlOutput(outputId = "check_table_economy"),
              DT::dataTableOutput(outputId = "table_economy")
            ),
            shinydashboard::box(
              title = "View distribution for a country",
              shiny::selectInput(
                inputId = "par_distr_econ",
                label = "Choose parameter",
                choices = parameters_economy$par %>%
                  setNames(parameters_economy$Parameter),
                selected = NULL,
                multiple = FALSE
              ),
              shiny::downloadButton(
                outputId = "download_plot_distr_econ",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_distr_econ"),
              shiny::plotOutput(outputId = "plot_distr_econ")
            )
          ), # End fluidRow

          # Row: Barcharts
          shiny::fluidRow(
            shinydashboard::box(
              title = "Compare cost points",
              shinyWidgets::pickerInput(
                inputId = "par_barchart_econ",
                label = "Select cost points to display",
                choices = parameters_economy$par[parameters_economy$barchart == "x"] %>%
                  setNames(
                    parameters_economy$Parameter[parameters_economy$barchart == "x"]
                  ),
                selected = parameters_economy$par[parameters_economy$barchart == "x"],
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              shiny::downloadButton(
                outputId = "download_plot_barchart_econ",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_barchart_econ"),
              shiny::plotOutput(outputId = "plot_barchart_econ"),
              width = 12
            )
          ), # End fluidRow

        ), # End tabItem

        # TAB: POST OUTBREAK MANAGEMENT ========================================

        shinydashboard::tabItem(
          tabName = "postoutbr",

          # Row: Picker farm type
          shiny::fluidRow(
            shinydashboard::box(
              title = "General analysis",
              htmlOutput(outputId = "check_table_postoutbr"),
              DT::dataTableOutput(outputId = "table_postoutbr")
            ),
            shinydashboard::box(
              title = "View distribution",
              shiny::selectInput(
                inputId = "par_plot_postoutbr",
                label = "Choose parameter",
                choices = parameters_postoutbr$par %>%
                  setNames(parameters_postoutbr$Parameter),
                selected = NULL,
                multiple = FALSE
              ),
              shiny::downloadButton(
                outputId = "download_plot_distr_postoutbr",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_distr_postoutbr"),
              shiny::plotOutput(outputId = "plot_distr_postoutbr")
            )
          ) # End fluidRow

        ), # End tabItem


        # TAB: DIAGNOSTICS -> CONTROL PHASE ====================================

        shinydashboard::tabItem(
          tabName = "diag_control",

          # Row: Description and Parameter input:
          shiny::fluidRow(
            shinydashboard::box(
              title = "Description of sampling scheme during control phase",
              HTML(text_diag_control)
            ),
            shinydashboard::box(
              title = "Set parameters for sampling scheme",
              shiny::numericInput(
                inputId = "par_diag_control_bulk_milk",
                label = "(a) Number of bulk milk samples per dairy farm",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_bulk_milk") %>%
                  dplyr::pull("Value")
                ,
                min = 0
              ),
              shiny::numericInput(
                inputId = "par_diag_control_lesions_smrum",
                label = "(b) Number of acute lesion samples small ruminants per farm",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_lesions_smrum") %>%
                  dplyr::pull("Value"),
                min = 0
              ),
              shiny::numericInput(
                inputId = "par_diag_control_lesions_pigs",
                label = "(c) Number of acute lesion samples pigs per farm",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_lesions_pigs") %>%
                  dplyr::pull("Value"),
                min = 0
              ),
              shiny::numericInput(
                inputId = "par_diag_control_lesions_cattle",
                label = "(d) Number of acute lesion samples cattle per farm",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_lesions_cattle") %>%
                  dplyr::pull("Value"),
                min = 0
              ),
              shiny::numericInput(
                inputId = "par_diag_control_edta",
                label = "(e) Number of blood samples (EDTA) per symptomatic suspect holding",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_edta") %>%
                  dplyr::pull("Value"),
                min = 0
              ),
              shiny::numericInput(
                inputId = "par_diag_control_serum",
                label = "(f) Number of blood samples (serum) per symptomatic suspect holding",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_serum") %>%
                  dplyr::pull("Value"),
                min = 0
              ),
              shiny::numericInput(
                inputId = "par_diag_control_ffd_prev",
                label = "(g) Minimal detectable prevalence (in %)",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_ffd_prev") %>%
                  dplyr::pull("Value"),
                min = 1,
                max = 100
              ),
              shiny::numericInput(
                inputId = "par_diag_control_ffd_certainty",
                label = "(h) Probability of detection (in %)",
                value = defaults_diag_control %>%
                  dplyr::filter(Parameter == "par_diag_control_ffd_certainty") %>%
                  dplyr::pull("Value"),
                min = 1,
                max = 100
              )
            )
          ), # End fluidRow

          # Row: Table + plot distribution
          shiny::fluidRow(
            shinydashboard::box(
              title = "General analysis",
              htmlOutput(outputId = "check_tab_diag_control"),
              DT::dataTableOutput(outputId = "tab_diag_control")
            ),
            shinydashboard::box(
              title = "View distribution",
              shiny::selectInput(
                inputId = "par_plot_diag_control",
                label = "Choose parameter",
                choices = parameters_diag_control$par %>%
                  setNames(parameters_diag_control$Parameter),
                selected = NULL,
                multiple = FALSE
              ),
              shiny::downloadButton(
                outputId = "download_plot_distr_diag_control",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_distr_diag_control"),
              shiny::plotOutput(outputId = "plot_distr_diag_control")
            )
          ), # End fluidRow

          # Row: Barcharts
          shiny::fluidRow(
            shinydashboard::box(
              title = "Compare numbers of samples",
              shinyWidgets::pickerInput(
                inputId = "par_barchart_diag_control",
                label = "Select samples to display",
                choices = parameters_diag_control$par %>%
                  setNames(
                    parameters_diag_control$Parameter
                  ),
                selected = parameters_diag_control$par,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              shiny::downloadButton(
                outputId = "download_plot_barchart_diag_control",
                label = "Download",
                icon = NULL
              ),
              shiny::htmlOutput(outputId = "check_plot_barchart_diag_control"),
              shiny::plotOutput(outputId = "plot_barchart_diag_control"),
              width = 12
            )
          ) # End fluidRow
        ) # End tabItem

      ) # End tabItems

    ) # End dashboardBody
  ) # End dashboardPage
) # End shinyUI
