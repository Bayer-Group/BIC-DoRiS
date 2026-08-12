#' Main graph function in doris
#'
#' @param dorisGraphData data frame from preprocessing function
#' @param lower lower y-axis limit
#' @param upper upper y-axis limit
#' @param subgroup character with selected subgroup variable name
#' @param subgroup_level character with selected subgroup level name
#' @param compare_pattern selected pattern
#' @param add_subgroup logical for adding subgroup mean lines
#' @param add_overall_mean_logical logical for adding mean lines
#' @param add_complement_logical logical for adding complement lines
#' @param add_other_subgroups_logical logical for adding other subgroups
#' @param add_backgrounds_logical logical for background
#' @param add_points_logical logical for adding points
#' @param points_data raw data points
#' @param pattern_choice_auto retained for compatibility;
#'   truth-value backgrounds always follow \code{tmp$pattern} from preprocessing
#'   (manual entry or automatic best pattern for the selected subgroup).
#' @param add_permutation_infos logical for permutation infos
#' @param jitter_points logical for jitter points
#' @param tmp_list automatic evaluation data
#' @param index index variable
#'

dorisGraph_base2 <- function(
  dorisGraphData,
  lower,
  upper,
  subgroup,
  subgroup_level,
  compare_pattern,
  add_subgroup,
  add_overall_mean_logical,
  add_complement_logical,
  add_other_subgroups_logical,
  add_backgrounds_logical,
  add_points_logical = FALSE,
  points_data,
  pattern_choice_auto = TRUE,
  add_permutation_infos = FALSE,
  jitter_points,
  tmp_list,
  index
) {
  # No longer needed. Evaluate anyway to avoid errors.
  force(pattern_choice_auto)

  bar_width <- diff(range(dorisGraphData$dose)) / 100
  delta_width <- diff(range(dorisGraphData$dose)) / 35

  par(mfrow = c(1, 1), mar = c(2.2, 2.2, 3.5, 1.2))

  plot(
    x = dorisGraphData$dose,
    y = dorisGraphData$mean,
    type = "n",
    xaxt = "n",
    ylim = c(lower, upper),
    xlim = c(
      min(dorisGraphData$dose) - (3 * bar_width),
      max(dorisGraphData$dose) + (3 * bar_width)
    ),
    ylab = "",
    xlab = "",
    bg = "#424242",
  )
  axis(1, at = dorisGraphData$dose)

  # Truth-value backgrounds: use actual pattern from preprocessing
  # (manual or automatic pattern).
  for (i in sort(unique(dorisGraphData$dose))) {
    tmp <- dorisGraphData[dorisGraphData$dose == i, , drop = FALSE]
    # If multiple rows per dose are present, pick one row for background drawing
    if (nrow(tmp) > 1L) {
      oks <- !is.na(tmp$mean_subgroup)
      if (any(oks)) {
        tmp <- tmp[which(oks)[1L], , drop = FALSE]
      } else {
        tmp <- tmp[1L, , drop = FALSE]
      }
    }

    # After merging, pattern may only exist as pattern.x.
    pcol <- if ("pattern" %in% names(tmp)) {
      tmp$pattern[1L]
    } else if ("pattern.x" %in% names(tmp)) {
      tmp$pattern.x[1L]
    } else {
      NA
    }
    sym <- tryCatch(as.character(pcol), error = function(e) NA_character_)

    if (
      isTRUE(add_backgrounds_logical) &&
        !is.na(sym) &&
        sym %in% c("=", "<", ">")
    ) {
      bg_ramp <- switch(
        sym,
        "=" = grDevices::colorRamp(c("#f2f2f2", "#f5aa20", "#f2f2f2")),
        "<" = grDevices::colorRamp(c("#f5aa20", "#f2f2f2")),
        ">" = grDevices::colorRamp(c("#f2f2f2", "#f5aa20"))
      )

      if (compare_pattern == "overall") {
        seq_delta <- switch(
          sym,
          "=" = seq(
            tmp$overall_delta_lower_equal,
            tmp$overall_delta_upper_equal,
            length.out = 501L
          ),
          "<" = seq(
            tmp$overall_delta_lower_less,
            tmp$overall_delta_upper_less,
            length.out = 501L
          ),
          ">" = seq(
            tmp$overall_delta_lower_greater,
            tmp$overall_delta_upper_greater,
            length.out = 501L
          )
        )
      } else {
        seq_delta <- switch(
          sym,
          "=" = seq(
            tmp$complement_delta_lower_equal,
            tmp$complement_delta_upper_equal,
            length.out = 501L
          ),
          "<" = seq(
            tmp$complement_delta_lower_less,
            tmp$complement_delta_upper_less,
            length.out = 501L
          ),
          ">" = seq(
            tmp$complement_delta_lower_greater,
            tmp$complement_delta_upper_greater,
            length.out = 501L
          )
        )
      }

      seq_delta <- sort(unique(c(seq_delta)))
      seq_delta <- seq_delta[is.finite(seq_delta)]

      # Truth-value background colours must match the full fuzzy-logic band
      # (seq_delta), not be re-scaled to the visible ylim. Colour grading
      # would appear at the wrong y whenever the plot window cuts off part of the band.
      if (length(seq_delta) >= 2L) {
        n_full <- length(seq_delta)
        # One ramp position per interval between consecutive seq_delta values
        ramp_t <- seq(0, 1, length.out = n_full - 1L)
        in_y <- dplyr::between(seq_delta, lower, upper)
        seq_delta2 <- seq_delta[in_y]
        # nr rectangles need nr gradient stops (avoid length mismatch vs fixed 500).
        nr <- length(seq_delta2) - 1L
        if (nr >= 1L) {
          idx_first <- which(in_y)[1L]
          k_interval <- idx_first + seq_len(nr) - 1L
          cols_grad <- grDevices::rgb(
            bg_ramp(ramp_t[k_interval]),
            maxColorValue = 255
          )
          graphics::rect(
            xleft = i - bar_width,
            xright = i + bar_width,
            ybottom = seq_delta2[-1L],
            ytop = seq_delta2[-length(seq_delta2)],
            xpd = NA,
            col = cols_grad,
            border = NA
          )
        }
      }

      if (length(seq_delta) >= 1L) {
        seq_last <- utils::tail(seq_delta, 1L)
        graphics::rect(
          xleft = i - bar_width,
          xright = i + bar_width,
          ybottom = lower,
          ytop = max(seq_delta[1L], lower),
          xpd = NA,
          col = ifelse(sym == "<", "#f5aa20", "#f2f2f2"),
          border = NA
        )
        graphics::rect(
          xleft = i - bar_width,
          xright = i + bar_width,
          ybottom = min(seq_last, upper),
          ytop = upper,
          xpd = NA,
          col = ifelse(sym == ">", "#f5aa20", "#f2f2f2"),
          border = NA
        )

        if (sym == "=") {
          segments(
            x0 = i - delta_width,
            x1 = i + delta_width,
            y0 = c(seq_delta[1L], mean(seq_delta), seq_last),
            y1 = c(seq_delta[1L], mean(seq_delta), seq_last),
            col = c("#cccccc", "#d99802", "#cccccc"),
          )
        } else if (sym == "<") {
          segments(
            x0 = i - delta_width,
            x1 = i + delta_width,
            y0 = c(min(seq_delta), max(seq_delta)),
            y1 = c(min(seq_delta), max(seq_delta)),
            col = c("#d99802", "#cccccc")
          )
        } else {
          segments(
            x0 = i - delta_width,
            x1 = i + delta_width,
            y0 = c(min(seq_delta), max(seq_delta)),
            y1 = c(min(seq_delta), max(seq_delta)),
            col = c("#cccccc", "#d99802")
          )
        }
      }
    }
    if (add_overall_mean_logical) {
      mtext(
        paste0("N=", tmp$N_overall),
        side = 3,
        at = i,
        line = 2,
        col = "#424242",
        cex = 1
      )
    }
    if (add_subgroup) {
      mtext(
        paste0("n=", tmp$N_subgroup),
        side = 3,
        at = i,
        line = 1,
        col = "dodgerblue",
        cex = 1
      )
    }
    if (add_complement_logical) {
      mtext(
        paste0("n=", tmp$N_complement),
        side = 3,
        at = i,
        line = 0,
        col = "#08cf86",
        cex = 1
      )
    }
  }

  if (isTRUE(add_permutation_infos) && !is.null(tmp_list)) {
    df <- Reduce(rbind, lapply(tmp_list$mean_list, function(x) x[index, ]))
    if (!any(is.na(df))) {
      perm_ramp <- grDevices::colorRamp(c("#eeeeee", "#f5aa20"))
      col_df <- grDevices::rgb(
        perm_ramp(tmp_list$tv_df[index, ]),
        maxColorValue = 255
      )
      for (j in seq_along(col_df)) {
        lines(
          x = dorisGraphData$dose,
          y = df[j, ],
          col = paste0(col_df, 40)[j],
          lwd = 1.5
        )
      }
    }
  }
  # draw mean lines
  if (add_overall_mean_logical) {
    lines(
      x = dorisGraphData$dose,
      y = dorisGraphData$mean,
      type = "l",
      col = "#424242",
      lwd = 2,
      pch = 16
    )
    points(
      dorisGraphData$dose,
      dorisGraphData$mean,
      cex = sqrt(
        20 * (dorisGraphData$N_overall / max(dorisGraphData$N_overall)) / pi
      ),
      col = "#00000080",
      pch = 19
    )
  }

  if (add_points_logical) {
    #points_data
    if (jitter_points) {
      jitter_width <- diff(range(dorisGraphData$dose)) / 10

      tmp <- points_data[points_data[, subgroup] != subgroup_level, ]$dose
      tmp_sub <- points_data[points_data[, subgroup] == subgroup_level, ]$dose

      for (i in dorisGraphData$dose) {
        tmp[tmp == i & !is.na(tmp)] <- points_data[
          points_data[, subgroup] != subgroup_level,
        ]$dose[
          points_data[points_data[, subgroup] != subgroup_level, ]$dose == i &
            !is.na(
              points_data[points_data[, subgroup] != subgroup_level, ]$dose
            )
        ] +
          seq(
            -jitter_width / 8,
            jitter_width / 8,
            length = length(points_data[
              points_data[, subgroup] != subgroup_level,
            ]$dose[
              points_data[points_data[, subgroup] != subgroup_level, ]$dose ==
                i &
                !is.na(
                  points_data[points_data[, subgroup] != subgroup_level, ]$dose
                )
            ])
          ) +
          (jitter_width / 4)
        tmp_sub[tmp_sub == i & !is.na(tmp_sub)] <- points_data[
          points_data[, subgroup] == subgroup_level,
        ]$dose[
          points_data[points_data[, subgroup] == subgroup_level, ]$dose == i &
            !is.na(
              points_data[points_data[, subgroup] == subgroup_level, ]$dose
            )
        ] +
          seq(
            -jitter_width / 8,
            jitter_width / 8,
            length = length(points_data[
              points_data[, subgroup] == subgroup_level,
            ]$dose[
              points_data[points_data[, subgroup] == subgroup_level, ]$dose ==
                i &
                !is.na(
                  points_data[points_data[, subgroup] == subgroup_level, ]$dose
                )
            ])
          ) -
          (jitter_width / 4)
      }
      points(
        tmp,
        points_data[points_data[, subgroup] != subgroup_level, ]$targetVariable,
        cex = 1,
        col = "#08cf8690",
        pch = 18
      )

      points(
        tmp_sub,
        points_data[points_data[, subgroup] == subgroup_level, ]$targetVariable,
        cex = 1,
        col = "#1e90ff90",
        pch = 18
      )
    } else {
      jitter_width <- diff(range(dorisGraphData$dose)) / 10
      points(
        points_data[points_data[, subgroup] != subgroup_level, ]$dose +
          (jitter_width / 4),
        points_data[points_data[, subgroup] != subgroup_level, ]$targetVariable,
        cex = 1,
        col = "#08cf8690",
        pch = 18
      )
      points(
        points_data[points_data[, subgroup] == subgroup_level, ]$dose -
          (jitter_width / 4),
        points_data[points_data[, subgroup] == subgroup_level, ]$targetVariable,
        cex = 1,
        col = "#1e90ff90",
        pch = 18
      )
    }
  }

  #draw subgroup lines
  if (add_subgroup) {
    lines(
      x = dorisGraphData$dose,
      y = dorisGraphData$mean_subgroup,
      type = "l",
      col = "#1e90ffe2",
      lwd = 2,
      pch = 16
    )
    points(
      dorisGraphData$dose,
      dorisGraphData$mean_subgroup,
      cex = sqrt(
        20 * (dorisGraphData$N_subgroup / max(dorisGraphData$N_overall)) / pi
      ),
      #cex = sqrt(dorisGraphData$N_subgroup /pi)/2,
      col = "#1e90ffe2",
      pch = 19
    )
  }
  if (add_other_subgroups_logical) {
    for (i in 1:sum(startsWith(names(dorisGraphData), "N_other_"))) {
      lines(
        x = dorisGraphData$dose,
        y = unlist(dorisGraphData[paste0("mean_other_", i)]),
        type = "l",
        col = "#a6baaf60",
        lwd = 2,
        lty = 2,
        pch = 16
      )
      points(
        x = dorisGraphData$dose,
        y = unlist(dorisGraphData[paste0("mean_other_", i)]),
        cex = sqrt(
          20 *
            (unlist(dorisGraphData[paste0("N_other_", i)]) /
              max(dorisGraphData$N_overall)) /
            pi
        ),
        #cex= sqrt(unlist(dorisGraphData[paste0("N_other_",i)]) /pi)/2,
        col = "#a6baaf60",
        pch = 19
      )
    }
  }

  if (add_complement_logical) {
    lines(
      x = dorisGraphData$dose,
      y = dorisGraphData$mean_complement,
      type = "l",
      col = "#08cf86e2",
      lwd = 3,
      lty = 1,
      pch = 16
    )
    points(
      x = dorisGraphData$dose,
      y = dorisGraphData$mean_complement,
      cex = sqrt(
        20 * (dorisGraphData$N_complement / max(dorisGraphData$N_overall)) / pi
      ),
      #cex= sqrt(dorisGraphData$N_complement /pi)/2,
      col = "#08cf86e2",
      pch = 19
    )
  }
}
