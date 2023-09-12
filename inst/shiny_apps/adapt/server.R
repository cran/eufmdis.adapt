# Server function for the ADAPT App
# Version 05
# 2023-03-20

shiny::shinyServer(
  function(input, output) {

    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # IMPORT USER DATA ---------------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Upload EuFMDiS output files:
    control_summary <- shiny::reactive({
      if (!is.null(input$upload)){
        import_data_file(
          pattern = "control_summary",
          names_files = input$upload$name,
          paths_files = input$upload$datapath,
          def_columns = def_columns
        )
      } else {
        NULL
      }
    })

    # # Extract sim name:
    # sim_name <- reactive(extract_sim_name(input$upload))

    farm_summary <- shiny::reactive({
      if (!is.null(input$upload)){
        import_data_file(
          pattern = "farm_summary",
          names_files = input$upload$name,
          paths_files = input$upload$datapath,
          def_columns = def_columns
        )
      } else {
        NULL
      }
    })

    cost_summary <- shiny::reactive({
      if (!is.null(input$upload)){
        out <- import_data_file(
          pattern = "cost_summary",
          names_files = input$upload$name,
          paths_files = input$upload$datapath,
          def_columns = def_columns
        )
        
        # Rename columns for country "EU":
        par_orig <- parameters_economy$par_EU
        par_rename <- parameters_economy$par
        for (i in seq(along.with = par_orig)){
          names(out) <- gsub(
            pattern = paste0("^([a-z]{2}_)", par_orig[i]),
            replace = paste0("\\1", par_rename[i]),
            x = names(out)
          )
        }
        out
      } else {
        NULL
      }
    })

    farm_daily <- shiny::reactive({
      if (!is.null(input$upload)){
        out <- import_data_file(
          pattern = "farm_daily",
          names_files = input$upload$name,
          paths_files = input$upload$datapath,
          def_columns = def_columns
        )
        if (!is.null(out)){
          out <-
            out %>%
            dplyr::group_by(run) %>%
            dplyr::mutate(
              new_ihs = discumulate_data(cum_ihs),
              new_surv_done = discumulate_data(total_surv_done),
              new_culls_done = discumulate_data(total_culls_done),
              new_disp_done = discumulate_data(total_disp_done),
              new_vacc_done = discumulate_data(total_vacc_done),
            ) %>%
            dplyr::ungroup()
        }
      } else {
        NULL
      }
    })

    herd_summary <- shiny::reactive({
      if (!is.null(input$upload)){
        import_data_file(
          pattern = "herd_summary",
          names_files = input$upload$name,
          paths_files = input$upload$datapath,
          def_columns = def_columns
        )
      } else {
        NULL
      }
    })

    config_summary <- shiny::reactive({
      if (!is.null(input$upload)){
        import_data_file(
          pattern = "config_summary",
          names_files = input$upload$name,
          paths_files = input$upload$datapath,
          def_columns = def_columns,
          transpose = TRUE
        )
      } else {
        NULL
      }
    })


    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CREATE HELPER DATA FRAMES ------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    # METADATA =================================================================

    # Which countries are found in the EuFMDiS data:
    countries <- shiny::reactive({
      if (!is.null(cost_summary())){
        names(cost_summary()) %>%
          grep("^[a-z]{2}_", ., value = TRUE) %>%
          sub("_.*", "", .) %>%
          unique() %>%
          setdiff(c("control"))
      } else {
        NULL
      }
    })


    # CREATE LONG DATA FRAMES ==================================================

    farm_summary_types_long <- shiny::reactive({
      if (!is.null(farm_summary())){
        create_long_data_frame(
          dat = farm_summary(),
          categories = names_farm_types$par,
          name_categories = "farm_type"
        )
      } else {
        NULL
      }
    })

    farm_daily_types_long <- shiny::reactive({
      if (!is.null(farm_daily())){
        create_long_data_frame(
          dat = farm_daily(),
          categories = names_farm_types$par,
          name_categories = "farm_type"
        )
      } else {
        NULL
      }
    })

    cost_summary_countries_long <- shiny::reactive({
      if (!is.null(cost_summary())){        
        
        out <- create_long_data_frame(
          dat = cost_summary(),
          categories = countries(),
          name_categories = "country",
          starts_with = TRUE
        ) %>%
          dplyr::mutate(
            country = ifelse(country != "eu", country, "all")
          )
        
        if (!("all" %in% out$country)){
          out_all <- 
            out %>%
            dplyr::group_by(run) %>%
            dplyr::summarise(
              across(-country, sum)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(country = "all")
          out <- dplyr::bind_rows(out_all, out)
        }
        
        # Return value:
        out
      } else {
        NULL
      }
    })


    # CREATE JOINED/AGGREGATED DATA FRAMES =====================================


    # General data Epi ---------------------------------------------------------

    rel_cols_farm_summary <- c(
      "run",
      "ihs",
      "total_animals_culled",
      "pzhs",
      "szhs",
      "vhs",
      "total_vacc_animals",
      "cattle_vacc",
      "sheep_vacc",
      "pigs_vacc"
    )
    rel_cols_control_summary <- c(
      "run",
      "control_duration"
    )
    rel_cols_farm_daily <- c(
      "run",
      "cum_pzhs",
      "cum_szhs"
    )
    epi_general <-
      shiny::reactive({
        if (!is.null(farm_summary()) & !is.null(control_summary())){
          farm_summary() %>%
            dplyr::select(dplyr::all_of(rel_cols_farm_summary)) %>%
            dplyr::full_join(
              dplyr::select(
                control_summary(),
                dplyr::all_of(rel_cols_control_summary)
              ),
              by = "run"
            )
        } else {
          NULL
        }
      })


    # Epi data for line plots --------------------------------------------------

    rel_cols_farm_daily2 <- c(
      "run",
      "day",
      "new_ihs",
      "new_surv_done",
      "new_culls_done",
      "new_disp_done",
      "new_vacc_done"
    )
    epi_line_plots <-
      shiny::reactive({
        if (!is.null(farm_daily())){
          farm_daily() %>%
            dplyr::select(dplyr::all_of(rel_cols_farm_daily2)) %>%
            tidyr::pivot_longer(
              -c(run, day),
              values_to = "value_daily",
              names_to = "par"
            ) %>%
            tidyr::complete(run, day, par, fill = list("value_daily" = 0)) %>%
            dplyr::arrange(run, day) %>%
            dplyr::group_by(run, par) %>%
            dplyr::mutate(value_cum = cumsum(value_daily)) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(
              cols = starts_with("value"),
              names_to = "aggr",
              values_to = "value"
            ) %>%
            dplyr::group_by(day, par, aggr) %>%
            dplyr::summarise(
              q025 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q975 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::arrange(day)
        } else {
          NULL
        }
      })


    # Farm type data - Epi -----------------------------------------------------

    rel_cols_farm_summary_types_long <- c(
      "run",
      "farm_type",
      "ihs",
      "farms_culled",
      "animals_culled",
      "vhs"
    )
    rel_cols_farm_daily_types_long <- c(
      "run",
      "farm_type",
      "num_pzhs",
      "num_szhs"
    )
    epi_types <-
      shiny::reactive({
        if (!is.null(farm_daily_types_long()) & !is.null(farm_summary_types_long())){
          farm_daily_types_long() %>%
            dplyr::select(dplyr::all_of(rel_cols_farm_daily_types_long)) %>%
            dplyr::group_by(run, farm_type) %>%
            dplyr::summarize(
              dplyr::across(dplyr::everything(), max),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::full_join(
              dplyr::select(
                farm_summary_types_long(),
                dplyr::all_of(rel_cols_farm_summary_types_long)
              ),
              by = c("run", "farm_type")
            )
        } else {
          NULL
        }
      })


    # Diagnostics - Control phase ----------------------------------------------

    # Columns farm summary:
    rel_cols_farm_summary_dc <- c(
      "run",
      "chs",
      "shs"
    )

    # Columns herd summary:
    rel_cols_herd_summary_dc <- c(
      "run",
      "reason_diagnosed",
      "day_clinical",
      "herd_type",
      "herd_size"
    )

    # Herd types:
    herd_types_cattle <-
      names_farm_types %>%
      dplyr::filter(cattle != "") %>%
      dplyr::pull(par)
    herd_types_dairy <-
      names_farm_types %>%
      dplyr::filter(dairy != "") %>%
      dplyr::pull(par)
    herd_types_small_ruminants <-
      names_farm_types %>%
      dplyr::filter(small_rum != "") %>%
      dplyr::pull(par)
    herd_types_pigs <-
      names_farm_types %>%
      dplyr::filter(pigs != "") %>%
      dplyr::pull(par)

    diag_control <- shiny::reactive({
      if (!is.null(farm_summary()) & !is.null(herd_summary())){
        create_diag_control(
          herd_summary = herd_summary(),
          farm_summary = farm_summary(),
          par_diag_control_ffd_prev = input$par_diag_control_ffd_prev,
          par_diag_control_ffd_certainty = input$par_diag_control_ffd_certainty,
          par_diag_control_edta = input$par_diag_control_edta,
          par_diag_control_serum = input$par_diag_control_serum,
          par_diag_control_bulk_milk = input$par_diag_control_bulk_milk,
          par_diag_control_lesions_smrum = input$par_diag_control_lesions_smrum,
          par_diag_control_lesions_pigs = input$par_diag_control_lesions_pigs,
          par_diag_control_lesions_cattle = input$par_diag_control_lesions_cattle,
          rel_cols_farm_summary_dc,
          rel_cols_herd_summary_dc,
          herd_types_dairy,
          herd_types_small_ruminants,
          herd_types_pigs,
          herd_types_cattle
        )
      } else {
        NULL
      }
    })


    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CREATE OUTPUT DATA FRAMES ------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Table Configuration:
    tab_config <-
      shiny::reactive({
        if (!is.null(config_summary()) ){
          temp <-
            config_summary() %>%
            dplyr::select(dplyr::all_of(parameters_config$par)) %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "par")
          parameters_config %>%
            dplyr::select(Parameter, par) %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par)
        } else {
          NULL
        }
      })


    # Table Epidemiology -> General data -> General Analysis:
    tab_epi_general <-
      shiny::reactive({
        if (!is.null(epi_general()) ){
          temp <-
            epi_general() %>%
            tidyr::pivot_longer(-run, values_to = "value", names_to = "par") %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(
              Median = median(value, na.rm = TRUE),
              # IQR = IQR(value, na.rm = TRUE),
              q2.5 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q97.5 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                -par,
                ~tidyr::replace_na(., 0)
              )
            )
          # Output:
          parameters_epi_general %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 0)))

        } else {
          NULL
        }
      })


    # Table Epidemiology -> Data per farm type -> Results per farm type:
    tab_epi_types <-
      shiny::reactive({
        if (!is.null(epi_types()) ){
          temp <-
            epi_types() %>%
            dplyr::filter(farm_type == input$par_epi_farm_type) %>%
            dplyr::select(-farm_type) %>%
            tidyr::pivot_longer(-run, values_to = "value", names_to = "par") %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(
              Median = median(value, na.rm = TRUE),
              # IQR = IQR(value, na.rm = TRUE),
              q2.5 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q97.5 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                -par,
                ~tidyr::replace_na(., 0)
              )
            )
          # Output:
          parameters_epi_types %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 0)))
        } else {
          NULL
        }
      })


    # Table Economic Analysis -> Results per country:
    tab_economy <-
      shiny::reactive({
        if (!is.null(cost_summary_countries_long()) & !is.null(input$eco_country)){
          temp <-
            cost_summary_countries_long() %>%
            dplyr::filter(country == input$eco_country) %>%
            dplyr::select(dplyr::all_of(c("run", parameters_economy$par))) %>%
            tidyr::pivot_longer(-run, values_to = "value", names_to = "par") %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(
              Median = median(value, na.rm = TRUE),
              # IQR = IQR(value, na.rm = TRUE),
              q2.5 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q97.5 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                -par,
                ~tidyr::replace_na(., 0)
              )
            )
          # Output:
          parameters_economy %>%
            dplyr::select(Parameter, par) %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 0)))
        } else {
          NULL
        }
      })


    # Table Post Outbreak Management -> General Analysis:
    tab_postoutbr <-
      shiny::reactive({
        if (!is.null(control_summary()) ){
          temp <-
            control_summary() %>%
            dplyr::select(dplyr::all_of(c("run", parameters_postoutbr$par))) %>%
            tidyr::pivot_longer(-run, values_to = "value", names_to = "par") %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(
              Median = median(value, na.rm = TRUE),
              # IQR = IQR(value, na.rm = TRUE),
              q2.5 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q97.5 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                -par,
                ~tidyr::replace_na(., 0)
              )
            )
          # Output:
          parameters_postoutbr %>%
            dplyr::select(Parameter, par) %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 0)))
        } else {
          NULL
        }
      })

    # Table Diagnostics -> Control phase -> General Analysis:
    tab_diag_control <-
      shiny::reactive({
        if (!is.null(diag_control()) ){
          temp <-
            diag_control() %>%
            dplyr::select(dplyr::all_of(c("run", parameters_diag_control$par))) %>%
            tidyr::pivot_longer(-run, values_to = "value", names_to = "par") %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(
              Median = median(value, na.rm = TRUE),
              # IQR = IQR(value, na.rm = TRUE),
              q2.5 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q97.5 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                -par,
                ~tidyr::replace_na(., 0)
              )
            )
          # Output:
          parameters_diag_control %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 0)))
        } else {
          NULL
        }
      })

    # Table Diagnostics -> POM - Surveillance -> General Analysis:
    tab_diag_pom_surv <-
      shiny::reactive({
        if (!is.null(diag_pom_surv()) ){
          temp <-
            diag_pom_surv() %>%
            dplyr::select(dplyr::all_of(c("run", parameters_diag_pom_surv$par))) %>%
            tidyr::pivot_longer(-run, values_to = "value", names_to = "par") %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(
              Median = median(value, na.rm = TRUE),
              # IQR = IQR(value, na.rm = TRUE),
              q2.5 = quantile(value, 0.025, na.rm = TRUE),
              q25 = quantile(value, 0.25, na.rm = TRUE),
              q75 = quantile(value, 0.75, na.rm = TRUE),
              q97.5 = quantile(value, 0.975, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                -par,
                ~tidyr::replace_na(., 0)
              )
            )
          # Output:
          parameters_diag_pom_surv %>%
            dplyr::left_join(temp, by = "par") %>%
            dplyr::select(-par) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 0)))
        } else {
          NULL
        }
      })


    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # EXPORT OUTPUT DATA FRAMES ------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    output$download_all_tables <- shiny::downloadHandler(
      filename = function() {
        #paste0(input$dataset, ".csv")
        paste0(
          "All_tables_",
          format(Sys.time(), "%Y%m%d_%H%m"),
          ".xlsx"
        )
      },
      content = function(file) {
        # Create output list ---------------------------------------------------

        # The data in tables:
        list_data <-
          list(
            "Configuration" = tab_config(),
            "Epidemiology" = tab_epi_general(),
            "Economic analysis" = tab_economy(),
            "Post outbreak mgmt" = tab_postoutbr(),
            "Diagnostics contol phase" = tab_diag_control()#,
            #"Diagnostics POM" =tab_diag_pom_surv()
          )

        # Messages if input files are missing:
        list_messages <-
          list(
            "Configuration" = check_availability(
              list(config_summary = config_summary())
            ),
            "Epidemiology" = check_availability(
              list(
                farm_summary = farm_summary(),
                control_summary = control_summary()
              )
            ),
            "Economic analysis" = check_availability(
              list(cost_summary = cost_summary())
            ),
            "Post outbreak mgmt" = check_availability(
              list(control_summary = control_summary())
            ),
            "Diagnostics contol phase" = check_availability(
              list(
                farm_summary = farm_summary(),
                herd_summary = herd_summary()
              )
            )
          ) %>%
          lapply(function(y) as.character(y) %>% sub(".*>(.*)<.*", "\\1", .))

        # Schreibe gewaehltes Land in den Titel der oekonomischen Analyse:
        ind_econ <- which(names(list_data) == "Economic analysis")
        names(list_data)[ind_econ] <-
          paste(
            names(list_data)[ind_econ],
            toupper(input$eco_country)
          )

        # Combine data and messages:
        # Data is available, else message
        is_missing <- lapply(list_data, is.null) %>% unlist()
        list_out <- lapply(
          seq(along.with = is_missing),
          function(i){
            if(!is_missing[i]){
              list_data[[i]]
            } else {
              list_messages[[i]]
            }
          }
        ) %>%
          setNames(names(list_data))

        # Create xlsx-File:
        style_header <- openxlsx::createStyle(
          textDecoration = "bold",
          fgFill = "#ecf0f5"#FFF5D7
        )
        openxlsx::write.xlsx(
          x = list_out,
          file = file,
          headerStyle  = style_header,
          colWidths = "auto"
        )
      }
    )



    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CREATE OUTPUT PLOTS ------------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Plot Epidemiology -> General data -> View Distribution
    plot_epi_general <- shiny::reactive({
      if (!is.null(epi_general()) ){
        rel_dat <-
          epi_general() %>%
          dplyr::pull(all_of(input$par_plot_epi_general))
        rel_parameter <-
          parameters_epi_general %>%
          dplyr::filter(
            par == input$par_plot_epi_general
          ) %>%
          dplyr::pull(Parameter)

        plot_distribution(
          x = rel_dat,
          parameter = rel_parameter,
          main = "Distribution for all farm types"
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Epidemiology -> General data -> Time series
    plot_epi_line <- shiny::reactive({
      if (!is.null(epi_line_plots()) ){
        # Parameterbezeichung fuer Ausgabe im Plot:
        rel_parameter_long <- input$par_plot_epi_line

        # Welcher Parametername im Datensatz ist relevant:
        rel_par_data <-
          parameters_epi_line_plots %>%
          dplyr::filter(Parameter == rel_parameter_long) %>%
          dplyr::pull(par)

        # Filtere relevante Daten:
        if (input$aggr_plot_epi_line == "Cumulative"){
          rel_dat <-
            epi_line_plots() %>%
            dplyr::filter(
              par == rel_par_data,
              aggr == "value_cum"
            )
        } else {
          rel_dat <-
            epi_line_plots() %>%
            dplyr::filter(
              par == rel_par_data,
              aggr == "value_daily"
            )
        }
        plot_time_series(
          x = rel_dat,
          parameter = rel_parameter_long,
          main = paste(
            "The bold line represents the daily median, the coloured area",
            "represents the daily 95% range of variation."
          )
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Epidemiology -> Data per farm type -> View Distribution per farm type
    plot_epi_types <- shiny::reactive({
      if (!is.null(epi_types()) ){
        rel_dat <-
          epi_types() %>%
          dplyr::filter(farm_type == input$par_epi_farm_type) %>%
          dplyr::pull(all_of(input$par_plot_epi_type))
        rel_parameter <-
          parameters_epi_types %>%
          dplyr::filter(
            par == input$par_plot_epi_type
          ) %>%
          dplyr::pull(Parameter)
        rel_farm_type <-
          names_farm_types %>%
          dplyr::filter(par == input$par_epi_farm_type) %>%
          dplyr::pull(Parameter)

        plot_distribution(
          x = rel_dat,
          parameter = rel_parameter,
          main = paste("Distribution for farm type", rel_farm_type)
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Economic Analysis -> View Distribution for a country
    plot_distr_econ <- shiny::reactive({
      if (!is.null(cost_summary_countries_long()) ){
        rel_dat <-
          cost_summary_countries_long() %>%
          dplyr::filter(country == input$eco_country) %>%
          dplyr::pull(all_of(input$par_distr_econ))
        rel_parameter <-
          parameters_economy %>%
          dplyr::filter(
            par == input$par_distr_econ
          ) %>%
          dplyr::pull(Parameter)
        rel_country <- toupper(input$eco_country)

        plot_distribution(
          x = rel_dat,
          parameter = rel_parameter,
          main = paste("Distribution for country:", rel_country)
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Economic Analysis -> Compare Cost Points
    plot_barchart_econ <- shiny::reactive({
      if (!is.null(cost_summary_countries_long()) ){

        rel_cols_long <-
          parameters_economy$Parameter[
            match(input$par_barchart_econ, parameters_economy$par)
          ]

        data_barchart_eco <-
          cost_summary_countries_long() %>%
          dplyr::filter(country == input$eco_country) %>%
          dplyr::select(dplyr::all_of(input$par_barchart_econ)) %>%
          lapply(
            function(x){
              c(
                median(x, na.rm = TRUE),
                quantile(x, 0.025, na.rm = TRUE),
                quantile(x, 0.975, na.rm = TRUE)
              )
            }
          ) %>%
          do.call('rbind', .) %>%
          as.data.frame() %>%
          setNames(c("median", "q2.5", "q97.5")) %>%
          dplyr::mutate(par = rel_cols_long)

        plot_barchart_euros(
          x = data_barchart_eco,
          country = input$eco_country
        )

      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Post Outbreak Management -> View Distribution
    plot_distr_postoutbr <- shiny::reactive({
      if (!is.null(control_summary()) ){
        rel_dat <-
          control_summary() %>%
          dplyr::pull(all_of(input$par_plot_postoutbr))
        rel_parameter <-
          parameters_postoutbr %>%
          dplyr::filter(
            par == input$par_plot_postoutbr
          ) %>%
          dplyr::pull(Parameter)

        plot_distribution(
          x = rel_dat,
          parameter = rel_parameter,
          main = paste("Distribution for all farm types")
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Diagnostics -> Control phase -> Distribution:
    plot_distr_diag_control <- shiny::reactive({
      if (!is.null(diag_control()) ){
        rel_dat <-
          diag_control() %>%
          dplyr::pull(all_of(input$par_plot_diag_control))
        rel_parameter <-
          parameters_diag_control %>%
          dplyr::filter(
            par == input$par_plot_diag_control
          ) %>%
          dplyr::pull(Parameter)

        plot_distribution(
          x = rel_dat,
          parameter = rel_parameter,
          main = "Distribution for all farm types"
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Diagnostics -> Control phase -> Bar chart:
    plot_barchart_diag_control<- shiny::reactive({
      if (!is.null(diag_control()) ){

        rel_pars <-
          parameters_diag_control$Parameter[
            match(input$par_barchart_diag_control, parameters_diag_control$par)
          ]

        data_barchart_diag_control <-
          tab_diag_control() %>%
          dplyr::filter(Parameter %in% rel_pars)

        plot_barchart(x = data_barchart_diag_control)

      } else {
        NULL
      }
      grDevices::recordPlot()
    })


    # Plot Diagnostics -> POM Surveillance -> Distribution:
    plot_distr_diag_pom_surv <- shiny::reactive({
      if (!is.null(diag_pom_surv()) ){
        rel_dat <-
          diag_pom_surv() %>%
          dplyr::pull(all_of(input$par_plot_diag_pom_surv))
        rel_parameter <-
          parameters_diag_pom_surv %>%
          dplyr::filter(
            par == input$par_plot_diag_pom_surv
          ) %>%
          dplyr::pull(Parameter)

        plot_distribution(
          x = rel_dat,
          parameter = rel_parameter,
          main = "Distribution for all farm types"
        )
      } else {
        NULL
      }
      grDevices::recordPlot()
    })

    # Plot Diagnostics -> POM Surveillance -> Bar chart:
    plot_barchart_diag_pom_surv<- shiny::reactive({
      if (!is.null(diag_pom_surv()) ){

        rel_pars <-
          parameters_diag_pom_surv$Parameter[
            match(input$par_barchart_diag_pom_surv, parameters_diag_pom_surv$par)
          ]

        data_barchart_diag_pom_surv <-
          tab_diag_pom_surv() %>%
          dplyr::filter(Parameter %in% rel_pars)

        plot_barchart(x = data_barchart_diag_pom_surv)

      } else {
        NULL
      }
      grDevices::recordPlot()
    })

    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CREATE REACTIVE WIDGETS --------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    output$picker_farm_types <- shiny::renderUI({
      if (!is.null(farm_summary_types_long())){
        # Which farm types are in the data:
        rel_farm_types <- unique(farm_summary_types_long()$farm_type)
        rel_names_farm_types <-
          names_farm_types %>%
          dplyr::filter(par %in% rel_farm_types)
        # Create picker:
        selectInput(
          inputId = "par_epi_farm_type",
          label = "Choose farm type",
          choices = rel_names_farm_types$par %>%
            setNames(rel_names_farm_types$Parameter),
          selected = NULL,
          multiple = FALSE
        )
      } else {
        # If no data is loaded yet, display all available farm types in picker
        selectInput(
          inputId = "par_epi_farm_type",
          label = "Choose farm type",
          choices = names_farm_types$par %>%
            setNames(names_farm_types$Parameter),
          selected = NULL,
          multiple = FALSE
        )
      }
    })

    output$picker_countries <- shiny::renderUI({
      selectInput(
        inputId = "eco_country",
        label = "Choose country",
        choices = c("all", setdiff(countries(), "eu")) %>%
          setNames(c("All", toupper(setdiff(countries(), "eu")))) %>%
          as.list(),
        selected = "all",
        multiple = FALSE
      )
    })

    output$picker_vaccination_strategy <- shiny::renderUI({
      if (is.null(epi_general())){
        NULL
      } else if (any(epi_general()$vhs > 0)){
        selectInput(
          inputId = "vaccination_strategy",
          label = "Choose vaccination strategy",
          choices = c(
            "vaccinate to retain",
            "vaccinate to salvage/vaccinate to waste"
          ),
          selected = "vaccinate to retain",
          multiple = FALSE
        )
      } else {
        NULL
      }
    })


    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CREATE OUTPUT ELEMENTS ---------------------------------------------------
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # TAB ECONOMIC ANALYSIS ====================================================

    output$check_tab_config <- shiny::renderUI({
      check_availability(
        list(
          config_summary = config_summary()
        )
      )
    })
    output$tab_config <-
      DT::renderDataTable(
        DT::datatable(
          tab_config(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "configuration", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        )
      )


    # TAB EPIDEMIOLOGY -> GENERAL DATA =========================================

    output$check_tab_epi_general <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          control_summary = control_summary()
        )
      )
    })
    output$table_epi_general <-
      DT::renderDataTable(
        DT::datatable(
          tab_epi_general(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "epi_general", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ) %>%
          format_numbers_DT(-1, currency = '', digits = 0)
      )
    output$check_plot_epi_general <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          control_summary = control_summary()
        )
      )
    })
    output$plot_epi_general <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_epi_general()))
    )
    output$download_plot_epi_general <- shiny::downloadHandler(
      filename = function() {
        paste("epi_general_distribution.png")
      },
      content = function(file) {
        grDevices::png(file)
        grDevices::replayPlot(plot_epi_general())
        grDevices::dev.off()
      }
    )
    output$check_plot_epi_line <- shiny::renderUI({
      check_availability(
        list(
          farm_daily = farm_daily()
        )
      )
    })
    output$plot_epi_line <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_epi_line()))
    )
    output$download_plot_epi_line <- shiny::downloadHandler(
      filename = function() {
        paste("epi_general_time_series.png")
      },
      content = function(file) {
        grDevices::png(file, width = 900, height = 480)
        grDevices::replayPlot(plot_epi_line())
        grDevices::dev.off()
      }
    )


    # TAB EPIDEMIOLOGY -> DATA PER FARM TYPE ===================================

    output$check_table_epi_types <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          farm_daily = farm_daily()
        )
      )
    })
    output$table_epi_types <-
      DT::renderDataTable(
        DT::datatable(
          tab_epi_types(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "epi_per_farm_type", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ) %>%
          format_numbers_DT(-1, currency = '', digits = 0)
      )

    output$check_plot_epi_types <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          farm_daily = farm_daily()
        )
      )
    })
    output$plot_epi_types <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_epi_types()))
    )
    output$download_plot_epi_types <- shiny::downloadHandler(
      filename = function() {
        paste("epi_types_distribution.png")
      },
      content = function(file) {
        grDevices::png(file)
        grDevices::replayPlot(plot_epi_types())
        grDevices::dev.off()
      }
    )


    # TAB ECONOMIC ANALYSIS ====================================================

    output$check_table_economy <- shiny::renderUI({
      check_availability(
        list(
          cost_summary = cost_summary()
        )
      )
    })
    output$table_economy <-
      DT::renderDataTable(
        DT::datatable(
          tab_economy(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "economy", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ) %>%
          format_numbers_DT(-1, currency = '', digits = 0)
      )

    output$check_plot_distr_econ <- shiny::renderUI({
      check_availability(
        list(
          cost_summary = cost_summary()
        )
      )
    })
    output$plot_distr_econ <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_distr_econ()))
    )
    output$download_plot_distr_econ <- shiny::downloadHandler(
      filename = function() {
        paste("econ_distr.png")
      },
      content = function(file) {
        grDevices::png(file)
        grDevices::replayPlot(plot_distr_econ())
        grDevices::dev.off()
      }
    )

    output$check_plot_barchart_econ <- shiny::renderUI({
      check_availability(
        list(
          cost_summary = cost_summary()
        )
      )
    })
    output$plot_barchart_econ <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_barchart_econ()))
    )
    output$download_plot_barchart_econ <- shiny::downloadHandler(
      filename = function() {
        paste("econ_barchart.png")
      },
      content = function(file) {
        grDevices::png(file, width = 900, height = 480)
        grDevices::replayPlot(plot_barchart_econ())
        grDevices::dev.off()
      }
    )


    # TAB POST OUTBREAK MANAGEMENT =============================================

    output$check_table_postoutbr <- shiny::renderUI({
      check_availability(
        list(
          control_summary = control_summary()
        )
      )
    })
    output$table_postoutbr <-
      DT::renderDataTable(
        DT::datatable(
          tab_postoutbr(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "post_mgmt", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ) %>%
          format_numbers_DT(-1, currency = '', digits = 0)
      )

    output$check_plot_distr_postoutbr <- shiny::renderUI({
      check_availability(
        list(
          control_summary = control_summary()
        )
      )
    })
    output$plot_distr_postoutbr <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_distr_postoutbr()))
    )
    output$download_plot_distr_postoutbr <- shiny::downloadHandler(
      filename = function() {
        paste("pom_distr.png")
      },
      content = function(file) {
        grDevices::png(file)
        grDevices::replayPlot(plot_distr_postoutbr())
        grDevices::dev.off()
      }
    )


    # TAB DIAGNOSTICS -> CONTROL PHASE =========================================

    output$check_tab_diag_control <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          herd_summary = herd_summary()
        )
      )
    })
    output$tab_diag_control <-
      DT::renderDataTable(
        DT::datatable(
          tab_diag_control(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "diag_control", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames= FALSE
        ) %>%
          format_numbers_DT(-1, currency = '', digits = 0)
      )

    output$check_plot_distr_diag_control <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          herd_summary = herd_summary()
        )
      )
    })
    output$plot_distr_diag_control <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_distr_diag_control()))
    )
    output$download_plot_distr_diag_control <- shiny::downloadHandler(
      filename = function() {
        paste("diag_control_distr.png")
      },
      content = function(file) {
        grDevices::png(file)
        grDevices::replayPlot(plot_distr_diag_control())
        grDevices::dev.off()
      }
    )

    output$check_plot_barchart_diag_control <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          herd_summary = herd_summary()
        )
      )
    })
    output$plot_barchart_diag_control <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_barchart_diag_control()))
    )
    output$download_plot_barchart_diag_control <- shiny::downloadHandler(
      filename = function() {
        paste("diag_control_barchart.png")
      },
      content = function(file) {
        grDevices::png(file, width = 900, height = 480)
        grDevices::replayPlot(plot_barchart_diag_control())
        grDevices::dev.off()
      }
    )


    # TAB DIAGNOSTICS -> POM SURVEILLANCE ======================================

    output$text_diag_pom_surv_vacc <- shiny::renderUI({
      if (is.null(epi_general())){
        HTML("")
      } else if (!any(epi_general()$vhs > 0)){
        HTML("")
      } else if (input$vaccination_strategy == "vaccinate to retain"){
        HTML(text_diag_pom_surv_vacc_retain)
      } else {
        HTML(text_diag_pom_surv_vacc_remove)
      }
    })

    output$check_tab_diag_pom_surv <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          herd_summary = herd_summary()
        )
      )
    })
    output$tab_diag_pom_surv <-
      DT::renderDataTable(
        DT::datatable(
          tab_diag_pom_surv(),
          extensions = 'Buttons',
          options = list(
            dom = 'Brtip',
            buttons = list(
              list(extend = 'excel', title = "diag_control", text = 'Download')
            ),
            paging = FALSE,
            info = FALSE
          ),
          rownames= FALSE
        ) %>%
          format_numbers_DT(-1, currency = '', digits = 0)
      )

    output$check_plot_distr_diag_pom_surv <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          herd_summary = herd_summary()
        )
      )
    })
    output$plot_distr_diag_pom_surv <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_distr_diag_pom_surv()))
    )
    output$download_plot_distr_diag_pom_surv <- shiny::downloadHandler(
      filename = function() {
        paste("diag_control_pom_surv.png")
      },
      content = function(file) {
        grDevices::png(file)
        grDevices::replayPlot(plot_distr_diag_pom_surv())
        grDevices::dev.off()
      }
    )

    output$check_plot_barchart_diag_pom_surv <- shiny::renderUI({
      check_availability(
        list(
          farm_summary = farm_summary(),
          herd_summary = herd_summary()
        )
      )
    })
    output$plot_barchart_diag_pom_surv <- shiny::renderPlot(
      grDevices::replayPlot(shiny::req(plot_barchart_diag_pom_surv()))
    )
    output$download_plot_barchart_diag_pom_surv <- shiny::downloadHandler(
      filename = function() {
        paste("diag_pom_surv_barchart.png")
      },
      content = function(file) {
        grDevices::png(file, width = 900, height = 480)
        grDevices::replayPlot(plot_barchart_diag_pom_surv())
        grDevices::dev.off()
      }
    )


    # OUTPUT DEBUGGING =========================================================

    output$debug <- shiny::renderPrint({
      str(cost_summary())
    })

  } # End Server function definition
) # Closing shinyServer function

