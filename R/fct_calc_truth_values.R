#' calculates truth values in dorisAutoPattern function
#'
#' @param Factors data frame with factors in columns
#' @param dose vector with dose levels
#' @param targetVariable vector with target variable (binary or metric)
#' @param delta vector with fuzzy logic threshold values
#' @param method method used to calculate best fitting pattern
#' @param best_pattern_and_truthValues best fitting patterns and truth values
#' @param weights weights for weigthes means
#' @param compare_pattern pattern comparision (complement or overall means)
#'

calc_truth_values <- function(
  Factors,
  targetVariable,
  dose,
  delta,
  method = "Minimum",
  best_pattern_and_truthValues = levels_and_pattern,
  weights,
  compare_pattern
) {

  dose_means <- tapply(targetVariable, dose, mean, na.rm = TRUE)

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
  if (compare_pattern == "subgroup complement") {

    list_of_mean_per_dose2 <- lapply(
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
    list_of_mean_per_dose2 <- lapply(list_of_mean_per_dose2,
      function(x) {
        tmp<-Reduce(rbind,x)
      }
    )
    Complement_means <- as.data.frame(Reduce(rbind,list_of_mean_per_dose2))
  }

  Subgroup_means <- as.data.frame(Reduce(rbind,list_of_mean_per_dose))

  # dimnames(Subgroup_means)[[2]]=paste("dose",round(as.numeric(levels(dose)),3),sep="")

  row.names(Subgroup_means) <- paste(
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
  ### differences: subgroup means - clinical trial means or subgroup means - complement mean

  if (compare_pattern == "subgroup complement") {
    Subgroup_differences  <- Subgroup_means - Complement_means
  } else {
   Subgroup_differences  <- as.data.frame(
    t(
      apply(Subgroup_means, 1, function(x) x - dose_means)
    )
   )
  }

  alltvmatr <- apply(
    Subgroup_differences,
    1,
    function(x) sapply(x,gettvpdose,delta), simplify = FALSE
  )

  ## idea: get tv for given pattern
 truthValues <- rep(NA,length(alltvmatr))
 truthValues_per_dose <- data.frame(matrix(NA,dim(Subgroup_means)[1],dim(Subgroup_means)[2]))

 for (j in 1:length(alltvmatr)) {
   tmp <- c()
    for(i in 1:ncol(alltvmatr[[j]])) {
      tmp <- c(tmp,
        alltvmatr[[j]][,i][which(!is.na(match(names(alltvmatr[[j]][,i]),
        substr(best_pattern_and_truthValues$best_pattern[j],i,i))))]
      )
    }

    if (length(tmp) == 0) {
      truthValues_per_dose[j,] <- rep(NA,ncol(alltvmatr[[j]]))
    } else {
      #method
      truthValues_per_dose[j,] <- tmp
      if (method == "Minimum") {
        if (!all(is.na(tmp))) {
          tv <- min(tmp, na.rm = TRUE)
        } else {
          tv <- NA
        }
      } else if (method == "Mean") {
        if (!all(is.na(tmp))) {
          tv <- mean(tmp, na.rm = TRUE)
        } else {
          tv <-  NA
        }
      } else if (method == "Weighted Mean"){
        if (!all(is.na(tmp))) {
        tv <- weighted.mean(tmp, w = weights, na.rm =TRUE)
        } else {
          tv <-  NA
        }
      } else {
        tv <- rep(NA,ncol(alltvmatr[[j]]))
      }
    }
    truthValues[j] <- tv
 }

  return(
    list(
      "tV results" =
        data.frame("Subgroups" = names(alltvmatr), "truth Values" = truthValues),
      "mean results" =
        Subgroup_means,
      "tv dataframe" =
        truthValues_per_dose
    )
  )
}
