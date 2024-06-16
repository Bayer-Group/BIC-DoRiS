#' Draws a histogram of permuted total truth values in doris app
#'
#' @param tmp_list reactive calc_permutation_automatic or calc_permutation_manual-object from doris app
#' @param total_truth_val numeric value of total truth value
#' @param index numeric index vector

dorisHistogram <- function(
    tmp_list,
    total_truth_val,
    index
) {
  if (is.numeric(index) & length(index) > 0) {
    f_colZ <- grDevices::colorRamp(c("#f2f2f2","#f5aa20"))
    par(mfrow = c(1,1), mar = c(4,4,0.5,0.5))
    hist(
      tmp_list$tv_df[index,],
      col = grDevices::rgb(f_colZ(seq(0, 1, length.out = 20)), maxColorValue = 255),
      breaks= seq(0, 1, length.out = 21),
      border = "#ffffff",
      main = NULL,
      ylab = "frequency",
      xlab = "total truth values"
    )

    abline(v = total_truth_val, lty = 2, col = "#424242")
  }
}


