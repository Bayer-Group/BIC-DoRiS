#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  shiny::addResourcePath('www',system.file('app/www', package = "doris"))
  shiny::navbarPage(title = shiny::div(
    HTML(
      "<h3 style ='color:white;'> <span style ='height: 100px;'>DoRiS</span> - Do<span style ='color:orange'>se</span> R<span style ='color:orange'>esponse</span> i<span style ='color:orange'>n</span> S<span style ='color:orange'>ubgroups              </span></h3>"
    ),
    shiny::actionButton(
      inputId = "optionButton",
      label = "",
      icon = icon("gears"),
      style = "position: absolute;
         right: 110px;
         background-color: #004459;
         color: white;
         top: 5px;height: 40px;
         width: 40px;
         border-radius: 50%;
         border: 1px solid white;"
    ),
    # shiny::uiOutput({"cont_option"}),
    shiny::actionButton(
      inputId = "tableButton",
      label = "",
      icon = icon("table"),
      style =
        "position: absolute;
        right: 20px;top: 5px;
        height: 40px;
        background-color: #004459;
        color: white;
        width: 40px;
        border-radius: 50%;
        border: 1px solid white;"
    ),
    shiny::actionButton(
      inputId = "graphButton",
      label = "",
      icon = icon("chart-line"),
      style =
        "position: absolute;
        right: 65px;
        top: 5px;
        background-color: #004459;
        color: white;
        height: 40px;
        width: 40px;
        border-radius: 50%;
        border: 1px solid white;"
    )
  ),
  windowTitle = "DoRiS",
  id = "navpanel",
  shiny::tabPanel(
    "Scatter Plots",
    value = "home",
    icon = icon("chart-line"),
    shiny::uiOutput('includeCSS'),
    bsplus::use_bs_popover(),
    bsplus::use_bs_tooltip(),
    shiny::conditionalPanel(condition = "output.option_on_off == true",
      shiny::wellPanel(style = paste0("background-color: #fafafa; border-color: black; border: 5px; border-radius: 5px;"),
        shiny::fluidRow(
          shiny::column(3,
            shiny::fluidRow(
              shiny::column(10,
                shiny::fileInput(
                  inputId = "file_upload",
                  label = "Please upload doris data"
                )
              ),
              shiny::column(2,
                shiny::tags$style(
                  ".btn-doris_custom {background-color: #00617F; color: #FFF;}"
                ),
                helpTextDropdownButton(
                  id = "MyDropdown2",
                  text = HTML(
                    paste0("
                      <h4>Data Format</h4>
                      <h5>Currently supported data formats are:</h5>
                      <h5>R files ('.rdata','.RData','.Rdata','.rds'),
                           SAS files ('.sas7bdat','.sas7cdat') and
                           CSV files ('.csv')</h5>
                      <h4>Requirements</h4>
                      <h5>The variable 'dose' needs to be in the uploaded data frame.
                      The target variable requires numeric format, the subgroup (factor) variable(s) require character format.
                      </h5>
                     "
                    )
                  ),
                  right = FALSE
                )
              )
            )
          ),
          shiny::column(3,
            shiny::conditionalPanel(condition = "output.data_flag == true",
              shiny::fluidRow(
                shiny::column(10,
                  shiny::uiOutput("select_targetVariable")
                ),
                shiny::column(2,
                  helpTextDropdownButton(
                    id = "dropdown_target",
                    text = HTML(
                      paste0("
                      <h4> Target variable </h4>
                      <h5> Please select one target variable for analysis with DoRiS, e.g an efficacy or safety endpoint you are interested in.
                      </h5>

                      ")
                    ),
                    right = FALSE
                  )
                ),
                shiny::column(12,
                  shiny::uiOutput("targetVariable_check")
                )
              )
            ),
            shiny::fluidRow(
              shiny::conditionalPanel(condition = "output.data_flag == true",
                shiny::column(10,
                  shiny::uiOutput("select_Factors")
                ),
                shiny::column(2,
                  helpTextDropdownButton(
                    id = "dropdown_factors",
                    text = HTML(
                      paste0("
                        <h4> Subgroups </h4>
                        <h5>
                          Please select the subgroup variables to be included in the DoRiS analysis.
                          You have to select at least one subgroup variable,
                          but several variables can also be analyzed at the same time.
                          There is no restriction to the number of subgroup variables.
                          After choosing all variables press the ‘submit’ button.
                        </h5>
                      ")
                    ),
                  right = FALSE
                  )
                ),
                shiny::column(12,
                  shiny::uiOutput("selectFactors_check")
                ),
                shiny::fluidRow(
                  shiny::column(12, offset = 2,
                    shiny::conditionalPanel(condition = "output.target_and_factors_flag == true",
                      shiny::actionButton(
                        inputId = "submit",
                        label = "Submit",
                        icon = icon("refresh")
                      ),
                      shiny::uiOutput('cont1')
                    )
                  )
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.settings_flag == true",
            shiny::column(3,
              shiny::fluidRow(
                 shiny::column(10,
                  shiny::radioButtons(
                    inputId = "compare_pattern",
                    label = "Compare to:",
                    choices = c("overall","subgroup complement"),
                    selected = "subgroup complement",
                    inline = FALSE
                  )
                ),
                shiny::column(2,
                  helpTextDropdownButton(
                    id = "dropdown_compare_pattern",
                    text = HTML(
                      paste0("
                        <h4> Compare Pattern </h4>
                        <h5>
                        Select if you want to compare the dose response curve in the subgroup(s)
                        to the overall dose response curve or if you want to compare it to the dose response curve of the complement of the subgroup.
                        </h5>
                      ")
                    ),
                  right = TRUE
                  )
                ),
                shiny::column(10,
                  shiny::radioButtons(
                    inputId = "pattern_choice",
                    label = "Pattern Choice",
                    choices = c("manual","automatic"),
                    selected = "manual",
                    inline = TRUE
                  )
                ),
                shiny::column(2,
                  helpTextDropdownButton(
                    id = "dropdown_pattern_choice",
                    text = HTML(
                      paste0("<h4> Pattern Choice</h4>
                        <h5>
                        You can either select manual or automatic pattern choice.
                        If you select manual pattern choice pattern selection
                        boxes will appear, one for each dose level.
                        You can select a pattern you are interested in.
                        For each box you have the choice of ‘<’, ‘=’ or ‘>’.
                        For example, if you have three dose groups – placebo,
                        low dose and high dose - and select a pattern = > > you
                        are looking for a subgroup with a deviating dose
                        response curve that shows equal response after placebo
                        and greater response after the active doses.
                        I you select automatic pattern choice the pattern with
                        the largest deviation between the dose response curve
                        in the subgroup and in the reference group is
                        automatically selected and displayed for each selected subgroup.
                        </h5>
                      ")
                    ),
                  right = TRUE
                  )
                ),
                shiny::conditionalPanel(condition = "input.pattern_choice == 'manual'",
                  shiny::column(10,
                    shiny::fluidRow(
                      shiny::column(1,
                        shiny::div(id = "placeholder1")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder2")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder3")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder4")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder5")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder6")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder7")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder8")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder9")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder10")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder11")
                      ),
                      shiny::column(1,
                         shiny::div(id = "placeholder12")
                      )
                    )
                  )#,
                  # shiny::column(2,
                  #    helpTextDropdownButton(
                  #     id = "dropdown_pattern",
                  #     text = HTML(
                  #       paste0("
                  #       <h4> Pattern </h4>
                  #       ")
                  #     ),
                  # right = TRUE
                  #   )
                  # )
                )
              ),
              shiny::fluidRow(
                shiny::column(10,
                  shiny::numericInput(
                    inputId = "delta",
                    label = HTML("<p style = 'color: orange;'>&delta; (clinically relevant effect size):</p>"),
                    value = 0.1,
                    min = 0,
                    step = 0.1
                  )
                ),
                shiny::column(2,
                  helpTextDropdownButton(
                    id = "dropdown_delta",
                    text = HTML(
                      paste0("
                      <h4> &delta; (clinically relevant effect size): </h4>
                      <h5>
                      Specify a clinically meaningful difference delta
                      for the target variable. What difference in the target
                      variable between the dose response curves would be
                      clinically meaningful to potentially follow-up on
                      individualized dosing? This ensures that only
                      clinically relevant deviations are shown.
                      </h5>
                      ")
                    ),
                  right = TRUE
                  )
                )
              )
            ),
            shiny::column(3,
              shiny::fluidRow(
                  shiny::column(10,
                    shiny::radioButtons(
                      inputId = "alpha_method",
                      label = "Select method",
                      choices = c("Minimum", "Mean", "Weighted Mean"),
                      selected = "Weighted Mean",
                      inline = FALSE
                    )
                  ),
                  shiny::column(2,
                     helpTextDropdownButton(
                      id = "dropdown_alpha_method",
                      text = HTML(
                        paste0("
                        <h4> Method </h4>
                        <h5>
                          Fuzzy logic gives us a truth value for the required difference delta in each dose group to be true. Select the method to combine the truth values of the dose groups to get the total truth value for the complete pattern of deviation between the dose response curves.
                          Minimum: The total truth value for the complete pattern of deviation is determined as the minimum truth value over all dose groups.
                          Mean: The total truth value for the complete pattern of deviation is determined as the average of the truth values over all dose groups.
                          Weighted mean: The total truth value for the complete pattern of deviation is determined as the average of the truth values over all dose groups weighted by the number of subjects in the dose groups (default) or by any other weighting scheme chosen by the user.
                        </h5>
                        ")
                      ),
                  right = TRUE
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::conditionalPanel(condition = "input.alpha_method == 'Weighted Mean'",
                    shiny::column(10,
                      shiny::fluidRow(
                        shiny::column(4,
                          shiny::div(id = "place_holder1")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder2")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder3")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder4")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder5")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder6")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder7")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder8")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder9")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder10")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder11")
                        ),
                        shiny::column(4,
                           shiny::div(id = "place_holder12")
                        )
                      )
                    )#,
                  #   shiny::column(2,
                  #      helpTextDropdownButton(
                  #       id = "dropdown_weighted_means",
                  #       text = HTML(
                  #         paste0("
                  #         <h4> Weighted means </h4>
                  #         ")
                  #       ),
                  # right = TRUE
                  #     )
                  #   )
                  )
                ),
                # shiny::fluidRow(
                #   shiny::column(10,
                #     shiny::numericInput(
                #       inputId = "alpha",
                #       label = HTML("&alpha; (level of significance):"),
                #       value = 0.05,
                #       min = 0,
                #       max = 1,
                #       step = 0.05
                #     )
                #   ),
                #   shiny::column(2,
                #      helpTextDropdownButton(
                #       id = "dropdown_alpha",
                #       text = HTML(
                #         paste0("
                #         <h4> &alpha; (level of significance): </h4>
                #         ")
                #       ),
                #   right = TRUE
                #     )
                #   )
                # ),
                shiny::fluidRow(
                  shiny::column(10,
                    shiny::checkboxInput(
                      inputId = "perform_permutation",
                      label = HTML("Perform permutation"),
                      value = FALSE
                    )
                  ),
                  shiny::column(2,
                     helpTextDropdownButton(
                      id = "dropdown_permutation",
                      text = HTML(
                        paste0("
                        <h4> Perform permutation: </h4>
                        <h5>
                        Tick the ‘Perform permutation’ box to start the calculations including permutation tests.
                        On the ‘evaluation’ tab, a list of all subgroups can be seen on the left side in the DoRiS application.
                        It can be sorted by the total truth values to display the best fitting subgroup at the top.
                        Click on the arrows next to the column headers for sorting.
                        On the right side the dose response curve for the respective subgroup is
                        displayed together with a histogram of the total truth values from the permutations.
                        On the ‘description’ tab you can see summary statistics by subgroup:
                        the number of subjects by dose group, subgroup means and difference of subgroup means to reference group means.
                        </h5>
                        <h5 style = 'color: red;'>
                        If you want to change the settings for a new analysis remove the tick,
                        change the settings and set the tick again to re-start the calculation with the new settings.
                        </h5>

                        ")
                      ),
                      right = TRUE
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(10,
                    shiny::numericInput(
                      inputId = "number_permutation",
                      label = HTML("Number permutation"),
                      value = 1000,
                      min = 1,
                      step =1
                    )#,
                    # shiny::actionButton(
                    #   inputId = "button_permutation",
                    #   label = "Perform permutation!",
                    #   icon = icon("shuffle")
                    # )
                  ),
                  shiny::column(2,
                     helpTextDropdownButton(
                      id = "dropdown_number_permutations",
                      text = HTML(
                        paste0("
                        <h4> Number permutation: </h4>
                        <h5>
                        Select the number of permutations to calculate a p-value.
                        The subgroup affiliations withing each dose group are permutated
                        and a total truth value is calculated for each permutation.
                        The location of the observed total truth value in
                        the distribution of simulated values is used to
                        identify remarkable subgroups and derive an exploratory p-value.
                        The p-value corresponds to the fraction of truth values greater
                        than the observed truth value. No adjustment for multiple testing is performed.
                        </h5>
                        ")
                      ),
                  right = TRUE
                    )
                  )
                )
              )
            )
          )
        )
      ),
      shiny::conditionalPanel(condition = "output.settings_flag == false",
        HTML(
          paste0(
            "<h2 style ='color: #f5aa20;'> Welcome to DoRiS application </h2>
              <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> To proceed with the app please </li></h5>
                <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i> upload your data in the panel above,  </li></h5>
                <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i> select a target variable, </li></h5>
                <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i> your factor variable(s) and submit. </li></h5>
              <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> For more information about the data structure or app-options please click the nearest question mark symbol.</li></h5>
              <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> For more information about the concept of the doris application, please see the 'About'-tab. </li></h5>
            "
          )
        )
      ),
      shiny::conditionalPanel(condition = "output.settings_flag == true",
      shiny::fluidRow(
        shiny::conditionalPanel(condition = "output.table_on_off == true",
          shiny::column(6,
            shiny::wellPanel(style = paste0("background-color: #fafafa; border-color: black; border: 5px; border-radius: 5px;"),
              shiny::conditionalPanel(condition = "output.settings_flag == true",
                shiny::fluidRow(
                  shiny::tabsetPanel(
                    selected = "Evaluation",
                    shiny::tabPanel("Description",
                      shiny::tabsetPanel(
                        shiny::tabPanel(
                          "Number of Subjects",
                          shiny::column(12,
                            DT::dataTableOutput("overview_table"),
                            style = "overflow-y: scroll;overflow-x: scroll;"
                          )
                        ),
                        shiny::tabPanel("Subgroup means",
                          shiny::column(12,
                            DT::dataTableOutput("SGM"),
                            style = "overflow-y: scroll;overflow-x: scroll;"
                          )
                        ),
                        shiny::tabPanel("Diff. Subgroup means & clinical trial means",
                          shiny::column(12,
                            DT::dataTableOutput("DSC"),
                            style = "overflow-y: scroll;overflow-x: scroll;"
                          )
                        )
                      )
                    ),
                    shiny::tabPanel("Evaluation",
                      shiny::column(12,
                        DT::dataTableOutput("DT_eval"),
                        style = "overflow-y: scroll;overflow-x: scroll;"
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        shiny::conditionalPanel(condition = "output.graph_on_off == true",
          shiny::column(6,
            shiny::wellPanel(style = paste0("background-color: #fafafa; border-color: black; border: 5px; border-radius: 5px;"),
              shiny::conditionalPanel(condition = "output.settings_flag == true",
                shiny::fluidRow(
                  shiny::column(8,
                    shiny::plotOutput(
                     "graphic_first_factor",
                     width = "100%"
                    ),
                    conditionalPanel(condition = "input.perform_permutation == true",
                      shiny::plotOutput(
                        "graphic_histogram",
                        width = "100%"
                      )
                    )
                  ),
                  shiny::column(4,
                    tagList(
                      shiny::uiOutput("graphic_select_subgroup1"),
                      shiny::selectInput(
                        inputId = "graphic_select_subgroup2",
                        label = "subgroup level",
                        choices = NULL,
                        selected = NULL
                      ),
                      shiny::uiOutput("upper_y"),
                      shiny::uiOutput("lower_y"),
                      shiny::checkboxInput(
                        inputId = "add_overall_mean",
                        label = HTML(paste("<p style = 'color:black;'> Add overall mean")),
                        value = TRUE
                      ),
                      shiny::checkboxInput(
                        inputId = "add_complement",
                        label = HTML(paste0("<p style = 'color:#08cf86'> Add subgroup complement line </p>")),
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "add_other_subgroups",
                        label = HTML(paste0("<p style = 'color:#424242'> Add for all other subgroup level(s) </p>")),
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "add_backgrounds",
                        label = HTML(paste0("<p style = 'color:#f5aa20'> Add background(s) for the truth values </p>")),
                        value = FALSE
                      ),
                      shiny::checkboxInput(
                        inputId = "add_points",
                        label = HTML(paste0("<p style = 'color:#424242'> Add points to graph</p>")),
                        value = TRUE
                      ),
                      shiny::conditionalPanel(condition = "input.add_points == true",
                        shiny::checkboxInput(
                          inputId = "add_jitter",
                          label = HTML(paste0("<p style = 'color:#424242'> Jitter points </p>")),
                          value = TRUE
                        )
                      ),
                      shiny::conditionalPanel(condition = "input.perform_permutation == true",
                        shiny::checkboxInput(
                          inputId = "add_permutation",
                          label = HTML(paste0("<p style = 'color:#424242'> Add permuted subgroup means to graph</p>")),
                          value = FALSE
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )#,
  # shiny::tabPanel(
  #   "About",
  #   value = "about",
  #   icon = icon("exclamation-circle"),
  #   HTML("
  #    <h3 style ='color: #f5aa20;'> Introduction </h3>
  #     <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i>  The scope of this application is
  #       to decide  wheter personalized dosing is practically possible in drug development. </li></h5>
  #     <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i>  If this turns out to be true it is the goal to develop a technique to </li></h5>
  #     <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i>       detect those compounds which are suitable for personalized dosing </li></h5>
  #     <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i>       determines the dose regimen for a single patient / subgroup of patients based on baseline variables</li></h5>
  #     <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i>  Primarily we focus on 'ready to use' drugs with doses to be seleceted from phase 2b dose finding studies </li></h5>
  #    <h3 style ='color: #f5aa20;'> Personalized dosing </h3>
  #    <img src ='www/Example1.png' alt ='Graphic not available' width='650' height = '500'/>
  #    <h3 style ='color: #f5aa20;'> Finding remarkable subgroups among hundreds </h3>
  #
  #    <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> We look for subgroups with mean curves that differ from
  #           the mean curve of the complement / overall study. </li></h5>
  #    <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> Those differences can be specified with a pattern. </li></h5>
  #    <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> For each dose level we can say that the subgroup mean of the
  #    target variable shall be </li></h5>
  #       <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i> less than (<), </li></h5>
  #       <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i> equal to (=) or </li></h5>
  #       <h5> &nbsp &nbsp <i class='fa-solid fa-circle' style ='color: #2B6636'></i> greater than (>) </li></h5>
  #       <img src ='www/Example2.png'  alt ='Graphic not available' width='650' height = '500'/>
  #     <h5> the complement / study mean of the target variable. </h5>
  #    <h5><i class='fa-solid fa-circle' style ='color: #00617F'></i> Then, we look automatically for those subgroups which match with the pattern. </li></h5>
  #    <h3 style ='color: #f5aa20;'> Fuzzy logic </h3>
  #    <h3 style ='color: #f5aa20;'> Example </h3>
  #
  #   ")
  # )
)
}

