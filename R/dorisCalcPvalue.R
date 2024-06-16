#' calculates pvalue for permutations in doris app
#'
#' @param tmp_list reactive calc_permutation_automatic or calc_permutation_manual-object from doris app
#' @param total_truth_val numeric value of total truth value

dorisCalcPvalue <- function(
    tmp_list,
    truth_value
) {

  pval <- c()
  for (i in 1:length(truth_value)) {
    if (!all(is.na(tmp_list$tv_df[i, ]))){
      pval[i] <- 1 - (ecdf(tmp_list$tv_df[i, ])(truth_value[i]))
    } else {
     pval[i] <- NA
    }
  }
  pval
}

