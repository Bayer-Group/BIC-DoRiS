#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import bslib
#' @import shinyBS
#' @import DT
#' @import tidyr
#' @import dplyr
#' @noRd

library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyBS)
library(DT)
library(tidyr)
library(dplyr)

#' server part of doris app

app_server <- function(input, output, session) {

  #### REACTIVE OBJECTS ####
  # Flags for user interface (conditionalPanel)
  data_upload <- shiny::reactiveValues(val = FALSE)
  output$data_flag <- shiny::reactive({data_upload$val})
  shiny::outputOptions(output, "data_flag", suspendWhenHidden = FALSE)

  target_and_factors <- shiny::reactiveValues(val = FALSE)
  output$target_and_factors_flag <- shiny::reactive({target_and_factors$val})
  shiny::outputOptions(output, "target_and_factors_flag", suspendWhenHidden = FALSE)

  shiny::observeEvent(c(input$select_Factors, input$select_targetVariable), {
    if (!is.null(input$select_Factors) & input$select_targetVariable != "") {
      target_and_factors$val <- TRUE
    } else {
      target_and_factors$val <- FALSE
    }
  })

  shiny::observeEvent(doris_data(), {
    if (dim(doris_data())[1] > 0) {
        data_upload$val <- TRUE
    }
  })

  options_on_off <- shiny::reactiveValues(val = TRUE)

  shiny::observeEvent(input$optionButton, {
    if(options_on_off$val) {
      options_on_off$val <- FALSE
    } else {
      options_on_off$val <- TRUE
    }
  })

  output$option_on_off <- shiny::reactive({options_on_off$val})

  shiny::outputOptions(output, "option_on_off", suspendWhenHidden = FALSE)

  graph_on_off <- shiny::reactiveValues(val = TRUE)

  shiny::observeEvent(input$graphButton, {
    if (graph_on_off$val) {
      graph_on_off$val <- FALSE
    } else {
      graph_on_off$val <- TRUE
    }
  })

  output$graph_on_off <- shiny::reactive({graph_on_off$val})
  shiny::outputOptions(output, "graph_on_off", suspendWhenHidden = FALSE)

  table_on_off <- shiny::reactiveValues(val = TRUE)
  shiny::observeEvent(input$tableButton, {
    if(table_on_off$val) {
      table_on_off$val <- FALSE
    } else {
      table_on_off$val <- TRUE
    }
  })
  output$table_on_off <- reactive({table_on_off$val})
  shiny::outputOptions(output, "table_on_off", suspendWhenHidden = FALSE)

  settings_upload <- shiny::reactiveValues(val = FALSE)
  output$settings_flag <- shiny::reactive({settings_upload$val})
  shiny::outputOptions(output, "settings_flag", suspendWhenHidden = FALSE)
  shiny::observeEvent(input$submit, {
    if(dim(doris_data())[1] > 0) {
      if (input$select_targetVariable != "") {
        settings_upload$val <- TRUE
      }
    }
  })

  ####... reactive object doris_data() ####
  doris_data <- shiny::reactive({
    if (!is.null(shiny::req(input$file_upload))) {
      path_ending <- utils::tail(strsplit(input$file_upload$datapath,"/.")[[1]], n = 1)
      if (path_ending %in% c(".rdata",".RData",".Rdata")) {
        tmp <- get(load(shiny::req(input$file_upload$datapath)))
      }
      if (path_ending == ".rds") {
        tmp <- readRDS(input$file_upload$datapath)
      }
      if (path_ending == ".csv") {
        tmp <- read.csv(input$file_upload$datapath, header = TRUE)
      }
      if (path_ending %in% c("sas7bdat", "sas7cdat")) {
        tmp <- haven::read_sas(input$file_upload$datapath)
      }
    } else {
       tmp <- NULL
    }
    tmp
  })


  ####... reactive object Factor_reac() ####
  Factor_reac <- shiny::eventReactive(input$submit, {
    shiny::req(doris_data())
    shiny::req(input$select_Factors)
    Factor_tmp <- doris_data() %>%
      dplyr::select(!!!rlang::syms(input$select_Factors))
    Factor_tmp <- as.data.frame(unclass(Factor_tmp),stringsAsFactors = TRUE)
    Factor_tmp
  })
  ####... reactive object dose_reac() ####
  dose_reac <- shiny::reactive({
    shiny::req(doris_data())
    dose_tmp <- as.factor(doris_data()$dose)
    dose_tmp
  })

  ####... reactive object targetVariable_reac() ####
  targetVariable_reac <- shiny::eventReactive(input$submit, {
    shiny::req(doris_data())
    shiny::req(input$select_targetVariable)
    targetVariable_tmp <- doris_data() %>%
      dplyr::pull(!!rlang::sym(input$select_targetVariable))
    targetVariable_tmp
  })

  #### SIDEBAR ####
  output$select_Factors <- shiny::renderUI({
   #check for numeric columns
   choices <- doris_data() %>%
     utils::type.convert(as.is = TRUE) %>%
     dplyr::select(where(is.character)) %>%
     colnames()

    shinyWidgets::pickerInput(
      inputId = "select_Factors",
      label = "Select subgroup variable(s)",
      choices = choices,
      selected = NULL,
      multiple = TRUE,
      options = list('actions-box' = TRUE)
    )
  })

  output$select_targetVariable <- shiny::renderUI({
   #check for numeric columns
   choices <- doris_data() %>%
     utils::type.convert(as.is = TRUE) %>%
     dplyr::select(where(is.numeric)) %>%
     dplyr::select(-dose) %>%
     colnames()


   choices <- c("", choices)
    shiny::selectizeInput(
      inputId = "select_targetVariable",
      label = "Target variable: ",
      choices = choices,
      selected = "",
      multiple = FALSE
    )
  })

  shiny::observeEvent(input$select_targetVariable, {
    output$targetVariable_check <- shiny::renderUI({
      if (input$select_targetVariable == "") {
        shiny::HTML(
          paste0(
            '<p style = "color:#E43157"> <i class="fa-solid fa-exclamation" style="color: #E43157">
            </i> Please select a target variable! </p>'
          )
        )
      } else {
        shiny::HTML(
          paste0(
            '<p style = "color:#08cf86"> <i class="fa-solid fa-check"> </i> </p>'
          )
        )
      }
    })
  })

  shiny::observeEvent(input$select_Factors, {
    output$selectFactors_check <- shiny::renderUI({
      if (is.null(input$select_Factors)) {
        shiny::HTML(
          paste0(
            '<p style = "color:#E43157"> <i class="fa-solid fa-exclamation" style="color: #E43157">
            </i> Please select a factor variable! </p>'
          )
        )
      } else {
        shiny::HTML(
          paste0(
            '<p style = "color:#08cf86"> <i class="fa-solid fa-check"> </i> </p>'
          )
        )
      }
    })
  },ignoreInit = FALSE,ignoreNULL=FALSE)

  #### OVERVIEW ####
  factors_and_levels <- shiny::reactive({
    shiny::req(input$graphic_select_subgroup1)
    shiny::req(input$graphic_select_subgroup2)
    if (input$graphic_select_subgroup2 %in% levels(Factor_reac()[,input$graphic_select_subgroup1])) {
      return(c(
        input$graphic_select_subgroup1,
        input$graphic_select_subgroup2
      ))
    }
  })

  #### GRAPHIC ####
  output$graphic_select_subgroup1 <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "graphic_select_subgroup1",
      label = "Subgroup factor",
      choices = input$select_Factors,
      multiple = TRUE,
      selected = input$select_Factors[1],
      options = list(maxItems = 1)
    )
  })

  shiny::observeEvent(c(input$graphic_select_subgroup1,input$DT_eval_rows_selected), {
    shiny::req(doris_data())
    choices <- levels(Factor_reac()[,input$graphic_select_subgroup1])

    if (is.null(input$DT_eval_rows_selected)) {
      selected <- choices[1]
    } else {
      dO <- dorisOverview(
        Factors = Factor_reac(),
        dose = dose_reac(),
        targetVariable = targetVariable_reac()
      )
      selected <- as.character(dO$levl[input$DT_eval_rows_selected])
    }

    shiny::updateSelectInput(
      session,
      inputId = "graphic_select_subgroup2",
      label = "subgroup level",
      choices = choices,
      selected = selected
    )
  })

  output$graphic_select_subgroup3<- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "graphic_select_subgroup3",
      label = "subgroup factor (2)",
      choices = input$select_Factors,
      multiple = TRUE,
      selected = input$select_Factors[2],
      options = list(maxItems = 1)
    )
  })

  output$graphic_select_subgroup4<- shiny::renderUI({

    shiny::req(doris_data())
    shiny::req(input$graphic_select_subgroup3)
    choices <- levels(Factor_reac()[,input$graphic_select_subgroup3])
    selected <- choices[1]
    shiny::selectInput(
      inputId = "graphic_select_subgroup4",
      label = "subgroup level (2)",
      choices = choices,
      selected = NULL
    )
  })

  output$DT_eval <-NULL

  shiny::observeEvent(input$submit, {
    output$overview_table <- DT::renderDataTable({
      shiny::req(doris_data())
      shiny::req(dose_reac())

      Factor_reac <- isolate(Factor_reac())
      targetVariable_reac <- isolate(targetVariable_reac())

      if (dim(Factor_reac())[1] > 0) {
        tmp <- dorisOverview(Factors = Factor_reac, dose = dose_reac(), targetVariable = targetVariable_reac)
        tmp2 <- as.data.frame(tmp$SGDL)
        if (length(input$select_Factors) > 1){
          rownames(tmp2) <- paste0(tmp$fact," : ", tmp$levl)
        } else if (length(input$select_Factors) == 1) {
          rownames(tmp2) <- paste0(tmp$fact," : ", tmp$levl)
        }

        DT::datatable(
          tmp2
        )
      }
    })

    output$SGM <- DT::renderDataTable({
      shiny::req(doris_data())
      shiny::req(dose_reac())

      Factor_reac <- isolate(Factor_reac())
      targetVariable_reac <- isolate(targetVariable_reac())

      tmp <- dorisOverview(
        Factors = Factor_reac,
        dose = dose_reac(),
        targetVariable = targetVariable_reac
      )
      tmp2 <- dorisEvaluation(
        Factors = Factor_reac,
        dose = dose_reac(),
        targetVariable = targetVariable_reac,
        pattern = pattern_reac$val,
        delta = rep(input$delta,length(unique(dose_reac()))),
        #alpha = input$alpha,
        method = input$alpha_method
      )
      tmp3 <- as.data.frame(tmp2$SGM)
      rownames(tmp3) <- paste0(tmp$fact," : ", tmp$levl)
      DT::datatable(
        round(tmp3,3)
      )
    })

    output$DSC <- DT::renderDataTable({
      shiny::req(doris_data())
      shiny::req(dose_reac())

      Factor_reac <- isolate(Factor_reac())
      targetVariable_reac <- isolate(targetVariable_reac())

      tmp <- dorisOverview(
        Factors = Factor_reac,
        dose = dose_reac(),
        targetVariable = targetVariable_reac
      )

      tmp2 <- dorisEvaluation(
        Factors = Factor_reac,
        dose = dose_reac(),
        targetVariable = targetVariable_reac,
        pattern = pattern_reac$val,
        delta = rep(input$delta,length(unique(dose_reac()))),
        #alpha = input$alpha,
        method = input$alpha_method
      )
      tmp3 <- as.data.frame(tmp2$DSC)
      rownames(tmp3) <- paste0(tmp$fact," : ", tmp$levl)
      DT::datatable(
        round(tmp3,3)
      )
    })


    pattern_options <- shiny::reactiveValues(
      Factors = NULL,
      targetVariable = NULL,
      delta = NULL,
      alpha = NULL,
      method = NULL
    )

  shiny::observe({
    calc_permutation_manual()
    calc_permutation_automatic()
  })


  output$DT_eval <- DT::renderDataTable({
    #requirements
    shiny::req(doris_data())
    shiny::req(dose_reac())

    Factor_reac <- shiny::isolate(Factor_reac())
    targetVariable_reac <- shiny::isolate(targetVariable_reac())

    if (input$pattern_choice == "manual") {
      tmp_test <- shiny::req(calc_evaluation_manual())
    } else {
      tmp_test <- shiny::req(calc_evaluation_automatic())
    }

    #permutation
    tmp_test2 <- NULL
    if (input$perform_permutation) {
      if (input$pattern_choice == "manual") {
        if (!is.null(shiny::isolate(calc_permutation_manual()))) {
          tmp_test2 <- shiny::req(shiny::isolate(calc_permutation_manual()))
        }
      } else {
        if (!is.null(shiny::isolate(calc_permutation_automatic()))) {
          tmp_test2 <- shiny::req(shiny::isolate(calc_permutation_automatic()))
        }
      }
    }
    if (!is.null(tmp_test)) {
      rownames(tmp_test$mean_list[[1]])

      tmp3_test <- as.data.frame(
        cbind(
          paste0(rownames(tmp_test$mean_list[[1]])),
          round(tmp_test$tv_df,3),
          round(tmp_test$tv_list[[1]],3)
        )
      )
      colnames(tmp3_test) <- c("subgroups:levels", "total truth values", paste0("dose : ",round(as.numeric(levels(dose_reac())),4)))


      if (input$perform_permutation) {
        if (!is.null(tmp_test2)) {
        pval <- dorisCalcPvalue(
           tmp_list  = tmp_test2,
           truth_value = tmp_test$tv_df
        )
        tmp3_test <- as.data.frame(
        cbind(
          paste0(rownames(tmp_test$mean_list[[1]])),
          round(pval,4),
          round(tmp_test$tv_df,3),
          round(tmp_test$tv_list[[1]],3)
          )
        )
        colnames(tmp3_test) <- c("subgroups:levels","p-value", "total truth values", paste0("dose : ",levels(dose_reac())))
        }
      }

      tmp <- dorisOverview(
        Factors = Factor_reac,
        dose = dose_reac(),
        targetVariable = targetVariable_reac
      )
      tmp2 <- dorisEvaluation(
        Factors = Factor_reac,
        dose = dose_reac(),
        targetVariable = as.numeric(targetVariable_reac),
        pattern = pattern_reac$val,
        delta = rep(input$delta,length(unique(dose_reac()))),
        #alpha = input$alpha,
        method = input$alpha_method
      )
      names(tmp2$DTV) <- paste0("truth-value: dose ",names(tmp2$DTV))

      f_colZ <- grDevices::colorRamp(c("#f2f2f2","#f5aa20"))

      DT::datatable(
        tmp3_test
        , filter = 'top', selection = 'single'
      ) %>%
      formatStyle(
        'total truth values',
        backgroundColor = styleInterval(
          seq(0, 1, by=0.05),
          grDevices::rgb(f_colZ(seq(0, 1, length.out = 22)), maxColorValue = 255)
        )
      )
    }
    })
  })

  calc_evaluation_manual <- shiny::reactive({
    tmp <- dorisAutoPattern(
      Factors = shiny::req(Factor_reac()),
      dose = shiny::req(dose_reac()),
      targetVariable = shiny::req(targetVariable_reac()),
      delta = shiny::req(input$delta),
      # alpha = shiny::req(input$alpha),
      method = shiny::req(input$alpha_method),
      nperm = 1,
      pattern_choice = "manual",
      pattern = shiny::req(pattern_reac$val),
      perform_perm = FALSE,
      weights = weights_reac$val,
      compare_pattern = input$compare_pattern
    )
    tmp
  })

  calc_evaluation_automatic <- shiny::reactive({
    tmp <- dorisAutoPattern(
      Factors = shiny::req(Factor_reac()),
      dose = shiny::req(dose_reac()),
      targetVariable = shiny::req(targetVariable_reac()),
      delta = shiny::req(input$delta),
      # alpha = shiny::req(input$alpha),
      method = shiny::req(input$alpha_method),
      nperm = 1,
      pattern_choice = "automatic",
      perform_perm = FALSE,
      weights = weights_reac$val,
      compare_pattern = input$compare_pattern
    )
    tmp
  })

  calc_permutation_manual <- shiny::reactive({
    if (input$perform_permutation) {
      if (input$pattern_choice == "manual") {
        tmp <- dorisAutoPattern(
          Factors = shiny::req(Factor_reac()),
          dose = shiny::req(dose_reac()),
          targetVariable = shiny::req(targetVariable_reac()),
          delta = shiny::req(input$delta),
          # alpha = shiny::req(input$alpha),
          method = shiny::req(input$alpha_method),
          nperm = req(input$number_permutation),
          pattern_choice = "manual",
          pattern = shiny::req(pattern_reac$val),
          perform_perm = TRUE,
          weights = weights_reac$val,
          compare_pattern = input$compare_pattern
        )
      } else {
       tmp<-NULL
      }
    } else {tmp <- NULL}
    tmp
  })

  calc_permutation_automatic <- shiny::reactive({
    if (input$perform_permutation) {
    if (input$pattern_choice == "automatic") {
      tmp <- dorisAutoPattern(
        Factors = shiny::req(Factor_reac()),
        dose = shiny::req(dose_reac()),
        targetVariable = shiny::req(targetVariable_reac()),
        delta = shiny::req(input$delta),
        # alpha = shiny::req(input$alpha),
        method = shiny::req(input$alpha_method),
        nperm = req(input$number_permutation),
        pattern_choice = "automatic",
        perform_perm = TRUE,
        weights = weights_reac$val,
        compare_pattern = input$compare_pattern
      )
    } else {
       tmp<-NULL
      }
    } else {tmp <- NULL}
    tmp
  })



  shiny::observeEvent(input$DT_eval_rows_selected, {
    dO <- dorisOverview(
      Factors = Factor_reac(),
      dose = dose_reac(),
      targetVariable = targetVariable_reac()
    )
    updateSelectizeInput(
      session,
      inputId = "graphic_select_subgroup1",
      selected = dO$fact[input$DT_eval_rows_selected]
    )
    shiny::updateCheckboxInput(
      session,
      inputId ="add_second_factor",
      value = FALSE
    )
  })

  output$DTV <- DT::renderDataTable({
    shiny::req(doris_data())
    shiny::req(Factor_reac())
    shiny::req(dose_reac())
    shiny::req(targetVariable_reac())

    tmp <- dorisOverview(
      Factors = Factor_reac(),
      dose = dose_reac(),
      targetVariable = targetVariable_reac()
    )
    tmp2 <- dorisEvaluation(
      Factors = Factor_reac(),
      dose = dose_reac(),
      targetVariable = targetVariable_reac(),
      pattern = pattern_reac$val, delta = rep(input$delta,length(unique(dose_reac()))),
      # alpha = input$alpha,
      method = input$alpha_method
    )
    tmp3 <- as.data.frame(tmp2$DTV)
    rownames(tmp3) <- paste0(tmp$fact," : ", tmp$levl)
    DT::datatable(
      round(tmp3,3)
    )
  })

  output$stv_pmd <- DT::renderDataTable({
    shiny::req(doris_data())
    shiny::req(Factor_reac())
    shiny::req(dose_reac())
    shiny::req(targetVariable_reac())

    tmp <- dorisOverview(
      Factors = Factor_reac(),
      dose = dose_reac(),
      targetVariable = targetVariable_reac()
    )
    tmp2 <- dorisEvaluation(
      Factors = Factor_reac(),
      dose = dose_reac(),
      targetVariable = as.numeric(targetVariable_reac()),
      pattern = pattern_reac$val, delta = rep(input$delta,length(unique(dose_reac()))), #alpha = input$alpha,
      method = input$alpha_method)
    tmp3 <- as.data.frame(cbind(tmp2$stv,tmp2$pmd))
    rownames(tmp3) <- paste0(tmp$fact," : ", tmp$levl)
    DT::datatable(
      round(tmp3,3)
    )  %>% formatStyle('V2',
     target = "row", backgroundColor = styleEqual(c(1,0), c('#c8ff9e','#ffa1a4'))
     )
  })

  shiny::observe({
    shiny::req(doris_data())
    tmp <- length(unique(doris_data()$dose))
    for (i in 1:tmp) {
      id <- paste0('pattern_value', i)
      fluidRow(
      column(1,
      shiny::insertUI(
        selector = paste0('#placeholder',i),
        where = "beforeBegin",
        ui = pattern_ui(id)
      )
      )
      )
      shiny::callModule(pattern_server, id, number = reactive({i}))
    }
  })
  pattern_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
          tags$style(
            HTML(
              " .selectize-control.single .selectize-input:after{content: none;}"
            )
          ),
        tags$head(
          tags$style(
            HTML(
              ".selectize-input{
              height: 15px;
              }"
            )
          )
         ),
        shiny::uiOutput(ns("pattern"))
      )
  }

  pattern_server <- function(input, output, session, number) {
    number()
    output[['pattern']] <- shiny::renderUI({
      ns <- session$ns
      shiny::tags$div(
        id = environment(ns)[['namespace']],
        shiny::tagList(
          shiny::selectizeInput(
            inputId = ns("select"),
            label = "",
            choices = c("=", "<", ">"),
            selected = "="
          )
        )
      )
    })
  }

  #### WEIGHTS ####
  shiny::observe({
    shiny::req(doris_data())
    tmp <- length(unique(doris_data()$dose))
    nof <- doris_data() %>%
      dplyr::group_by(dose) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::pull(n)
    for (i in 1:tmp) {
      id <- paste0('weights_value', i)
      shiny::insertUI(
        selector = paste0('#place_holder',i),
        where = "beforeBegin",
        ui = weights_ui(id)
      )
      shiny::callModule(weights_server, id, number = reactive({i}), N.of.subjects = reactive({nof[i]}))
    }
  })

  weights_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::uiOutput(ns("weights"))
    )
  }

  weights_server <- function(input, output, session, number, N.of.subjects) {
    number()
    N.of.subjects()
    output[['weights']] <- shiny::renderUI({
      ns <- session$ns
      shiny::tags$div(
        id = environment(ns)[['namespace']],
        shiny::tagList(
          shiny::numericInput(
            inputId = ns("select"),
            label = paste0("Weight ", number()),
            value = N.of.subjects(),
            min = 0
          )
        )
      )
    })
  }

  ### split in permutation and non permutation
  shiny::observe({
    output$graphic_first_factor <- renderPlot({
      input$perform_permutation
      shiny::req(factors_and_levels())
      shiny::req(doris_data())
      shiny::req(dose_reac())
      shiny::req(input$lower_y)
      shiny::req(input$upper_y)

      Factor_reac <- isolate(Factor_reac())
      targetVariable_reac <- isolate(targetVariable_reac())

      if (length(factors_and_levels()) == 2) {
        fac1 <- factors_and_levels()[1]
        lev1 <- factors_and_levels()[2]
        fac2 <- NULL
        lev2 <- NULL
      } else if (length(factors_and_levels()) == 4) {
        fac1 <- factors_and_levels()[1]
        lev1 <- factors_and_levels()[2]
        fac2 <- factors_and_levels()[3]
        lev2 <- factors_and_levels()[4]
      } else {
        return(NULL)
      }

      dorisGraphData <- doris_preprocessGraph(
        Factors = Factor_reac,
        dose = doris_data()$dose,
        targetVariable = targetVariable_reac,
        factor_selected = fac1,
        subgroup_selected = lev1,
        factor_selected2 = fac2,
        subgroup_selected2 = lev2,
        pattern = pattern_reac$val,
        delta = input$delta
      )

      point_dat <- cbind(Factor_reac,dose = doris_data()$dose,targetVariable = targetVariable_reac)#[cbind(Factor_reac,doris_data()$dose,targetVariable_reac)[,fac1] == lev1,]

      fac1 <- factors_and_levels()[1]
      lev1 <- factors_and_levels()[2]

      if (input$pattern_choice == "automatic") {
        tmp_list <- calc_evaluation_automatic()
        tmp_list2 <- calc_permutation_automatic()
      } else {
        tmp_list <- calc_evaluation_manual()
        tmp_list2 <- calc_permutation_manual()
      }
      if(!is.null(tmp_list)) {
      index <- which(paste0(fac1, ": ",lev1) == rownames(tmp_list$mean_list[[1]]))

      dorisGraph_base2(
        dorisGraphData = dorisGraphData,
        lower = input$lower_y,
        upper = input$upper_y,
        subgroup = fac1,
        subgroup_level = lev1,
        add_complement_logical = input$add_complement,
        add_other_subgroups_logical = input$add_other_subgroups,
        add_overall_mean_logical = input$add_overall_mean,
        add_backgrounds_logical = input$add_backgrounds,
        compare_pattern = input$compare_pattern,
        add_points_logical = input$add_points,
        points_data = point_dat,
        pattern_choice_auto = (input$pattern_choice=="automatic"),
        add_permutation_infos = (input$add_permutation & input$perform_permutation),
        tmp_list = tmp_list2,
        index = index,
        jitter_points = input$add_jitter
      )
      }
    })
  })

  ## Histogram
  shiny::observe({
    #permutation
    tmp_test2 <- NULL
    tmp_test1 <- NULL
    if (input$perform_permutation) {

      if (input$pattern_choice == "manual") {
        if (!is.null(calc_permutation_manual())) {
          tmp_test2 <- shiny::req(shiny::isolate(calc_permutation_manual()))
          tmp_test1 <- shiny::req(shiny::isolate(calc_evaluation_manual()))
        }
      } else {
        if (!is.null(calc_permutation_automatic())) {
          tmp_test2 <- shiny::req(shiny::isolate(calc_permutation_automatic()))
          tmp_test1 <- shiny::req(shiny::isolate(calc_evaluation_automatic()))
        }
      }
    } else { tmp_test2 <- NULL}


    fac1 <- factors_and_levels()[1]
    lev1 <- factors_and_levels()[2]

    index <- which(paste0(fac1, ": ",lev1) == rownames(tmp_test2$mean_list[[1]]))

    if (!is.null(tmp_test2)) {
    if (length(tmp_test1$tv_df[index]) > 0) {
      if (!is.na(tmp_test1$tv_df[index])) {
      output$graphic_histogram <- renderPlot({
        dorisHistogram(
          tmp_list = tmp_test2,
          total_truth_val = tmp_test1$tv_df[index],
          index = index
        )
      }, height = 220)
      } else {
        output$graphic_histogram <- renderPlot({
        NULL
        }, height = 220)
      }
    } else {
         output$graphic_histogram <- renderPlot({
      NULL
      }, height = 220)
      }
    } else {
      output$graphic_histogram <- renderPlot({
      NULL
      }, height = 220)
    }
  })
  pattern_reac <- reactiveValues(val = NULL)
  weights_reac <- reactiveValues(val = NULL)
  seq_alpha_reac <- reactive({
    if (!is.null(input$seq_alpha_start) & !is.null(input$seq_alpha_end) & !is.null(input$seq_alpha_length)) {
      seq(input$seq_alpha_start,input$seq_alpha_end,length = input$seq_alpha_length)
    } else {
      NULL
    }
  })

  seq_delta_reac <- reactive({
     if (!is.null(input$seq_delta_start) & !is.null(input$seq_delta_end) & !is.null(input$seq_delta_length)) {
      seq(input$seq_delta_start,input$seq_delta_end,length = input$seq_delta_length)
    } else {
      NULL
    }
  })

  results_simulation2 <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$update_simulation2, {
    if (!is.null(seq_delta_reac()) & !is.null(seq_alpha_reac())) {
      alpha_seq <- seq_alpha_reac()

      delta_seq <- seq_delta_reac()

      runs <- input$distribution_number_runs2
      tmp <- matrix(NA, nrow=input$distribution_number_runs2,ncol=length(alpha_seq)*length(delta_seq))

      ind <- 1
      withProgress(message = "Simulating...", value = 0, {
      for(i in 1:length(alpha_seq)) {
        for (j in 1:length(delta_seq)) {
          tmp[,ind] <-
            dorisDistribution(
              Factors = Factor_reac(),
              dose = dose_reac(),
              targetVariable = targetVariable_reac(),
              pattern = pattern_reac$val,
              delta = rep(delta_seq[j], length(unique(dose))),
              alpha = alpha_seq[i],
              runs = input$distribution_number_runs2
            )
          incProgress(1/(length(alpha_seq)*length(delta_seq)), detail = paste("Simulation ", ind , " of ", length(alpha_seq)*length(delta_seq)))
          ind <- ind + 1
        }
      }
      })
      results_simulation2$val <- tmp
    }
  })

  # output$heatmap_graph <- renderPlot({
  #
  #   if (!is.null(results_simulation2$val)) {
  #     dorisHeatmap(
  #       data = results_simulation2$val,
  #       value = input$value,
  #       delta_seq = seq_delta_reac(),
  #       alpha_seq = seq_alpha_reac(),
  #       runs = input$distribution_number_runs2
  #     )
  #   }
  # })

  shiny::observe({
    tmp <- c()
      for(i in 1:length(levels(dose_reac()))) {
        tmp[i] <- input[[paste0("pattern_value",i,"-select")]]
      }
    pattern <- paste(tmp, sep = "", collapse = "")
    pattern_reac$val <- pattern
  })

  shiny::observe({
    tmp <- c()
      for(i in 1:length(levels(dose_reac()))) {
        tmp[i] <- input[[paste0("weights_value",i,"-select")]]
      }
    if (any(is.na(tmp))) {
      NULL
    } else {
      weights_reac$val <- tmp
    }
  })

  dorisDist <- shiny::eventReactive(input$update_simulation, {
    shiny::req(doris_data())
    shiny::req(Factor_reac())
    shiny::req(dose_reac())
    shiny::req(targetVariable_reac())
    tmp <- dorisDistribution(
      Factors = Factor_reac(),
      dose = dose_reac(),
      targetVariable = targetVariable_reac(),
      pattern = pattern_reac$val,
      delta = rep(input$delta,length(unique(dose_reac()))),
      #alpha = input$alpha,
      runs = input$distribution_number_runs
    )
    tmp
  })

  output$lower_y <- shiny::renderUI({
    low_y <- min(shiny::req(as.numeric(targetVariable_reac())), na.rm = TRUE)
    shiny::numericInput(
      inputId = "lower_y",
      label = "Lower limit y:",
      value = low_y,
      step = 0.1
    )
  })

  output$upper_y <- shiny::renderUI({
    upper_y = max(as.numeric(targetVariable_reac()), na.rm = TRUE)
    shiny::numericInput(
      inputId = "upper_y",
      label = "Upper limit y:",
      value = upper_y,
      step = 0.1
    )
  })

  output$dist <- shiny::renderDataTable(
    as.data.frame(table(shiny::req(dorisDist())))
  )

  output$includeCSS <- shiny::renderUI({
      shiny::tags$head(
        tags$style(
          HTML("
             .navbar-nav > li > a, .navbar-brand {
              padding-top:4px !important;
              padding-bottom:0 !important;
              color: white;
              height: 60px;
             }
            .navbar { background-color: #00617F;}
            .navbar-default .navbar-nav > li > a {color:white;}
            .navbar-default .navbar-nav > .active > a,
            .navbar-default .navbar-nav > .active > a:focus,
            .navbar-default .navbar-nav > .active > a:hover {color: white; background-color: #004459;}
            .navbar-default .navbar-nav > li > a:hover {color: orange;}

            body {
              background-color: #ffffff;
              color: #000000;
            }
            .tabbable > .nav > li[class=active]    > a {
              background-color: #f2f2f2; color:#383838
            }
          ")
        )
      )
    })


  shiny::observeEvent(data_upload$val, {
    shinyBS::updateCollapse(
      session,
      id = "collapse_variables",
      open = c(
        shiny::HTML('<p style="color:white; font-size:100%;"> Variables: </p>')
      )
    )
  })



  # change color of the Create/Upload Plots Buttons
  output$cont1 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#submit{color: white; background-color: #08cf86;}'))
      )
    )
  })

  shiny::observeEvent(input$submit, {
    output$cont1 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#submit{color: #ffffff; background-color:#e3e3e3;}'))
        )
      )
    })
  })

  shiny::observeEvent(c(input$select_Factors, input$select_targetVariable), {
    output$cont1 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(
            HTML(
              '#submit{color: white; background-color: #08cf86;}'
            )
          )
        )
      )
    })
  })

  shiny::observeEvent(input$optionButton,{
     output$cont_option <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#optionButton{
          position: absolute;
           right: 110px;
           top: 5px;
           height: 40px;
           width: 40px;
           border-radius: 50%;
           border: 1px solid white;}'))
        )
      )
    })
  })
}
