#' Calculates best fitting pattern
#'
#' @param Factors data frame with factors in columns
#' @param dose vector with dose levels
#' @param targetVariable vector with target variable (binary or metric)
#' @param pattern vector to specify pattern of differences
#' @param delta vector with fuzzy logic threshold values
#' @param pattern_list vector with possible patterns in the form of e.g."=>>>="
#' @param method method used to calculate best fitting pattern

dorisAutoPattern <- function(
    Factors,
    dose,
    targetVariable,
    delta,
    method = "Minimum",
    nperm = 1000,
    sampleseed = 1234,
    pattern_choice,
    pattern = NULL,
    perform_perm = FALSE,
    weights = NULL,
    compare_pattern
  ) {

  if (!is.numeric(targetVariable)) {
    targetVariable <- as.numeric(targetVariable)
    if (!is.numeric(targetVariable)) {
      error("targetVariable needs to be numeric")
    }
  }

  # list with entries for every factor variable with
  # means for each level and dose
  if (method == "Weighted Mean" && is.null(weights)) {
    NULL
  } else {
  if (compare_pattern == "overall") {
    list_of_mean_per_dose <- lapply(
      names(Factors),
      function(f) {
        tapply(
         targetVariable,
         cbind(Factors[f],dose),
         mean,
         na.rm = TRUE,
         simplify = TRUE
        )
      }
    )
  } else if (compare_pattern == "subgroup complement") {

    list_of_mean_per_dose <- lapply(
      names(Factors),
      function(f) {
        lapply(
         levels(Factors[[f]]),
         function(y) {
          tmp <- tapply(
           targetVariable[which(Factors[f] != y)],
           dose[which(Factors[f] != y)],
           mean,
           na.rm = TRUE,
           simplify = TRUE
          )
          tmp <- as.data.frame(t(tmp))
          rownames(tmp) <- y
          tmp
         }
        )
      }
    )
    list_of_mean_per_dose <- lapply(list_of_mean_per_dose,
      function(x) {
        tmp<-Reduce(rbind,x)
      }
    )
  }

  if (pattern_choice == "automatic") {
    levels_and_pattern <- calc_best_pattern(
      Factors,
      targetVariable,
      dose,
      delta,
      order = 1:nrow(Factors),
      compare_pattern = compare_pattern
    )
  } else if (pattern_choice == "manual") {
   level_rownames <- paste(
      rep(
        names(Factors),
        sapply(list_of_mean_per_dose,function(x) length(dimnames(x)[[1]]))
      ),
      unlist(
        lapply(
          list_of_mean_per_dose,
          function(x) dimnames(x)[[1]]
        )
      ),
      sep = ": "
    )
    levels_and_pattern <- data.frame(
      best_pattern = rep(pattern, length(level_rownames))
    )
    rownames(levels_and_pattern) <- level_rownames
  }


  # #### Permutation test ####
  if (perform_perm) {
    tmp_mean_list <- vector("list", length = nperm)
    tmp_tv_list <- vector("list", length = nperm)
    tmp_tv <- c()
    for (i in 1:nperm) {
      tmp <- plyr::ddply(cbind(Factors,targetVariable,dose),"dose",function(x){
        cbind(
          targetVariable = sample(x$targetVariable,length(x$targetVariable),replace = FALSE),
          x[, "targetVariable" != colnames(x)]
        )
      })

      tmp_res <- calc_truth_values(
        tmp[names(Factors)],
        tmp$targetVariable,
        tmp$dose,
        delta,
        method = method,
        levels_and_pattern,
        weights = weights,
        compare_pattern = compare_pattern
      )
      tmp_tv <- cbind(tmp_tv, tmp_res$`tV results`$truth.Values)
      tmp_mean_list[[i]] <- tmp_res$`mean results`
      tmp_tv_list[[i]] <- tmp_res$`tv dataframe`
    }
  } else {
    tmp_mean_list <- vector("list", length = 1)
    tmp_tv_list <- vector("list", length = 1)
    tmp_tv <- c()
    tmp <- cbind(Factors,targetVariable,dose)
    tmp_res <- calc_truth_values(
      tmp[names(Factors)],
      tmp$targetVariable,
      tmp$dose,
      delta,
      method = method,
      levels_and_pattern,
      weights = weights,
      compare_pattern = compare_pattern
    )
      tmp_tv <- cbind(tmp_tv, tmp_res$`tV results`$truth.Values)
      tmp_mean_list[[1]] <- tmp_res$`mean results`
      tmp_tv_list[[1]] <- tmp_res$`tv dataframe`

    }
    index <- apply(tmp_tv_list[[1]], 1, function(x) {any(is.na(x))})
    tmp_tv[index] <- NA
    return(
      list(
        "tv_df" = tmp_tv,
        "tv_list" = tmp_tv_list,
        "mean_list" = tmp_mean_list
      )
    )
  }
}

