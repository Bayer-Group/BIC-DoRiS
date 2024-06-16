#' Pre-processing function for dorisGraph_base function
#'
#' @param Factors data frame with factors in columns
#' @param dose vector with dose levels
#' @param targetVariable vector with target variable (binary or metric)
#' @param factor_selected first selected factor
#' @param subgroup_selected first selected subgroup
#' @param factor_selected2 second selected factor
#' @param subgroup_selected2 second selected subgroup
#' @param pattern selected pattern
#' @param delta vector with fuzzy logic threshold values

doris_preprocessGraph <- function(
  Factors,
  dose,
  targetVariable,
  factor_selected,
  subgroup_selected,
  factor_selected2,
  subgroup_selected2,
  pattern,
  delta
) {

  dose <- as.numeric(dose)
  targetVariable <- as.numeric(targetVariable)
  reduced_data <- cbind(Factors[c(factor_selected,factor_selected2)], dose, targetVariable)
  pat <- unlist(strsplit(pattern, split=""))

  summary_overall <- reduced_data %>%
    dplyr::group_by(dose) %>%
    dplyr::summarise(
      N_overall = n(),
      mean = mean(targetVariable, na.rm =TRUE),
      .groups = "keep"
    )

  filtered_data <- reduced_data %>%
    dplyr::filter(!!rlang::sym(factor_selected) == subgroup_selected) %>%
    dplyr::filter(!is.na(targetVariable))
  complement_data <- reduced_data %>%
    dplyr::filter(!!rlang::sym(factor_selected) != subgroup_selected) %>%
    dplyr::filter(!is.na(targetVariable))
  if (!is.null(subgroup_selected2)& !is.null(factor_selected2)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(!!rlang::sym(factor_selected2) == subgroup_selected2)
    complement_data <-  reduced_data %>%
      dplyr::filter(!(!!rlang::sym(factor_selected) == subgroup_selected & !!rlang::sym(factor_selected2) == subgroup_selected2))
  }

  summary_subgroup <- filtered_data %>%
    dplyr::group_by(dose) %>%
    dplyr::summarise(
      N_subgroup = n(),
      mean_subgroup = mean(targetVariable, na.rm =TRUE),
      .groups = "keep"
    )

  summary_complement <- complement_data %>%
    dplyr::group_by(dose) %>%
    dplyr::summarise(
      N_complement = n(),
      mean_complement = mean(targetVariable, na.rm =TRUE),
      .groups = "keep"
    )

  summary_pattern <- tibble(dose = summary_overall$dose, pattern =  pat)

  #merge all data sets
  summary <- merge(
    merge(summary_overall,summary_subgroup, all = TRUE),
    merge(summary_complement,summary_pattern,all = TRUE),
    all = TRUE
  )
  summary$N_subgroup[is.na(summary$N_subgroup)] <- 0

  # Summary2 <- summary %>%
  #   dplyr::mutate(
  #     overall_delta_lower = case_when(
  #       pattern == "<" ~ mean ,
  #       pattern == ">" ~ mean + delta,
  #       pattern == "=" ~ mean - delta,
  #       TRUE ~ mean
  #     ),
  #     overall_delta_upper = case_when(
  #       pattern == "<" ~ mean - delta,
  #       pattern == ">" ~ mean,
  #       pattern == "=" ~ mean + delta,
  #       TRUE ~ mean
  #     ),
  #     complement_delta_lower = case_when(
  #       pattern == "<" ~ mean_complement ,
  #       pattern == ">" ~ mean_complement + delta,
  #       pattern == "=" ~ mean_complement - delta,
  #       TRUE ~ mean
  #     ),
  #     complement_delta_upper = case_when(
  #       pattern == "<" ~ mean_complement - delta,
  #       pattern == ">" ~ mean_complement,
  #       pattern == "=" ~ mean_complement + delta,
  #       TRUE ~ mean
  #     ),
  #     overall_difference = mean - mean_subgroup,
  #     complement_difference = mean_complement - mean_subgroup#,
  #   )

  Summary2 <- summary %>%
    dplyr::mutate(
      overall_delta_lower_less =  mean,
      overall_delta_lower_greater = mean + delta,
      overall_delta_lower_equal = mean - delta,
      overall_delta_upper_less =  mean - delta,
      overall_delta_upper_greater = mean,
      overall_delta_upper_equal = mean + delta,

      complement_delta_lower_less =  mean_complement,
      complement_delta_lower_greater = mean_complement + delta,
      complement_delta_lower_equal = mean_complement - delta,
      complement_delta_upper_less =  mean_complement - delta,
      complement_delta_upper_greater = mean_complement,
      complement_delta_upper_equal = mean_complement + delta,

      overall_difference = mean - mean_subgroup,
      complement_difference = mean_complement - mean_subgroup
    )


  Summary3 <- Summary2 %>%
    dplyr::rowwise() %>%
    dplyr::filter(!is.na(mean_subgroup)) %>%
    dplyr::mutate(
      truth_value_overall = case_when(
        pattern == ">" ~ lt(overall_difference, - delta) ,
        pattern == "<" ~ gt(overall_difference, delta),
        pattern == "=" ~ eq(overall_difference, -delta, delta),
        TRUE ~ mean
      ),
      truth_value_complement = case_when(
        pattern == ">"  ~ lt(complement_difference,-delta) ,
        pattern == "<" ~ gt(complement_difference,delta),
        pattern == "=" ~ eq(complement_difference,-delta,delta),
        TRUE ~ mean
      )
    ) %>%
    dplyr::mutate(
      less_overall = lt(overall_difference,-delta) ,
      great_overall = gt(overall_difference,delta),
      equal_overall = eq(overall_difference,-delta,delta),
      less_complement = lt(complement_difference,-delta) ,
      great_complement = gt(complement_difference,delta),
      equal_complement = eq(complement_difference,-delta,delta)
    ) %>%
    dplyr::mutate(
      best_fit_overall = case_when(
        less_overall == max(less_overall,great_overall,equal_overall) ~ ">",
        equal_overall == max(less_overall,great_overall,equal_overall) ~ "=",
        great_overall == max(less_overall,great_overall,equal_overall) ~ "<",
      ),
      best_fit_complement = case_when(
        less_complement == max(less_complement,great_complement,equal_complement) ~ ">",
        equal_complement == max(less_complement,great_complement,equal_complement) ~ "=",
        great_complement == max(less_complement,great_complement,equal_overall) ~ "<",
      )
    )

    Summary3 <- merge(Summary2, Summary3, all = TRUE)

    other_subgroups <- unique(complement_data[c(factor_selected,factor_selected2)])
      for(i in 1:nrow(other_subgroups)) {
        other_subgroup <- reduced_data %>%
          dplyr::filter(!!rlang::sym(colnames(other_subgroups)[1]) == other_subgroups[i,1])
        if(!is.null(subgroup_selected2)& !is.null(factor_selected2)) {
          other_subgroup <- other_subgroup %>%
            dplyr::filter(!!rlang::sym(colnames(other_subgroups)[2]) == other_subgroups[i,2])
        }
      summary_others <- other_subgroup %>%
        dplyr::group_by(dose) %>%
        dplyr::summarise(
          !!paste0("N_other_",quo_name(i)) := n(),
          !!paste0("mean_other_",quo_name(i)) := mean(targetVariable, na.rm =TRUE),
          .groups = "keep"
        )
        summary_others <- dplyr::full_join(summary_overall, summary_others, by ="dose") %>% select(-c("mean","N_overall"))
        Summary3 <- merge(Summary3,summary_others)
      }
  Summary3[,startsWith(names(Summary3),"N_other")][is.na(Summary3[,startsWith(names(Summary3),"N_other")])] <- 0
  return(Summary3)
}
