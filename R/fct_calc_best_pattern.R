#' calculates the best fitting pattern
#'
#' @param Factors data frame with factors in columns
#' @param targetVariable vector with target variable (binary or metric)
#' @param dose vector with dose levels
#' @param delta vector with fuzzy logic threshold values
#' @param order order of the factors
#' @param compare_pattern pattern comparision (complement or overall means)
#'
calc_best_pattern <- function(
  Factors,
  targetVariable,
  dose,
  delta,
  order = 1:nrow(Factors),
  compare_pattern
) {

  dose_means <- tapply(targetVariable, dose, mean, na.rm = TRUE)
  if (is.factor(dose)) {
    pattern_length <- length(levels(dose))
  } else {
    pattern_length <- length(unique(dose))
  }

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

  possible_patterns <- expand.grid(
    rep(
      list(c("<","=",">")),
      pattern_length
    )
  )

  possible_patterns_char <- apply(possible_patterns, 2, as.character)


  ### combine items of h to a data frame
  Subgroup_means <- as.data.frame(Reduce(rbind,list_of_mean_per_dose))

  dimnames(Subgroup_means)[[2]]=paste("dose",round(as.numeric(levels(dose)),3),sep="")
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

  dimnames(Subgroup_differences)[[2]] <- paste(
    "dose", round(as.numeric(levels(dose)), 3), sep = ""
  )

  alltvmatr <- apply(
    Subgroup_differences,
    1,
    function(x) sapply(x,gettvpdose,delta), simplify = FALSE
  )


  best_pattern_and_truthValues <- lapply(alltvmatr,
    function(y) {
      if(all(!is.na(y))) {
        data.frame(best_pattern = paste(
          rownames(y)[
            apply(y, 2, function(x) {
              unlist(which(!is.na(suppressWarnings(match(x,max(x,na.rm = TRUE))))))
            })
          ]
          , collapse = ""
        )
        )
      } else {
        data.frame(best_pattern = NA)
      }
    }
  )
  best_pattern_and_truthValues <- do.call("rbind",best_pattern_and_truthValues)

  return(best_pattern_and_truthValues)
}
