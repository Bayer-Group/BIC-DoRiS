#' Main graph function in doris
#'
#' @param dorisGraphData data frame from preprocessing function
#' @param lower lower y-axis limit
#' @param upper upper y-axis limit
#' @param compare_pattern selected pattern
#' @param add_overall_mean_logical logical for adding mean lines
#' @param add_complement_logical logical for adding complement lines
#' @param add_other_subgroups_logical logical for adding other subgroups
#' @param add_backgrounds_logical logical for background
#' @param add_points_logical logical for adding points
#' @param points_data raw data points
#' @param pattern_choice_auto best fitting pattern
#' @param add_permutation_infos logical for permutation infos
#' @param jitter_points logical for jitter points
#' @param tmp_list
#' @param index
#'

dorisGraph_base2 <- function(
  dorisGraphData,
  lower,
  upper,
  subgroup,
  subgroup_level,
  compare_pattern,
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

  bar_width <- diff(range(dorisGraphData$dose))/100
  delta_width <- diff(range(dorisGraphData$dose))/35

  par(mfrow = c(1,1),mar=c(2.2,2.2,3.5,1.2))

  plot(
    x = dorisGraphData$dose,
    y = dorisGraphData$mean,
    type = "n",
    xaxt = "n",
    ylim=c(lower,upper),
    xlim = c(min(dorisGraphData$dose) - (3*bar_width),max(dorisGraphData$dose) + (3*bar_width)),
    ylab = "",
    xlab= "",
    bg = "#424242",
  )
  axis(1, at = dorisGraphData$dose)

  #backgrounds:
  for (i in sort(unique(dorisGraphData$dose))) {
    tmp <- dorisGraphData[dorisGraphData$dose == i,]

     if (add_backgrounds_logical) {
      if (pattern_choice_auto) {
        if (tmp$best_fit_overall == "=") {
          f_colZ <- grDevices::colorRamp(c("#f2f2f2","#f5aa20","#f2f2f2"))
        } else if (tmp$best_fit_overall == "<") {
          f_colZ <- grDevices::colorRamp(c("#f5aa20","#f2f2f2"))
        } else if (tmp$best_fit_overall== ">") {
          f_colZ <- grDevices::colorRamp(c("#f2f2f2","#f5aa20"))
        } else {
          print("error")
        }
      } else {
        if (tmp$pattern == "=") {
          f_colZ <- grDevices::colorRamp(c("#f2f2f2","#f5aa20","#f2f2f2"))
        } else if (tmp$pattern == "<") {
          f_colZ <- grDevices::colorRamp(c("#f5aa20","#f2f2f2"))
        } else if (tmp$pattern == ">") {
          f_colZ <- grDevices::colorRamp(c("#f2f2f2","#f5aa20"))
        } else {
          print("error")
        }
      }

    if (compare_pattern == "overall") {
      if (pattern_choice_auto) {
        if (tmp$best_fit_overall == "=") {
          seq_delta <- seq(
            tmp$overall_delta_lower_equal,
            tmp$overall_delta_upper_equal,
            length.out = 501
          )
        }
        if (tmp$best_fit_overall == "<") {
          seq_delta <- seq(
            tmp$overall_delta_lower_less,
            tmp$overall_delta_upper_less,
            length.out = 501
          )
        }
        if (tmp$best_fit_overall == ">") {
          seq_delta <- seq(
            tmp$overall_delta_lower_greater,
            tmp$overall_delta_upper_greater,
            length.out = 501
          )
        }
      } else {
        if (tmp$pattern == "=") {
          seq_delta <- seq(
            tmp$overall_delta_lower_equal,
            tmp$overall_delta_upper_equal,
            length.out = 501
          )
        }
        if (tmp$pattern == "<") {
          seq_delta <- seq(
            tmp$overall_delta_lower_less,
            tmp$overall_delta_upper_less,
            length.out = 501
          )
        }
        if (tmp$pattern == ">") {
          seq_delta <- seq(
            tmp$overall_delta_lower_greater,
            tmp$overall_delta_upper_greater,
            length.out = 501
          )
        }
      }
    } else {
      if (pattern_choice_auto) {
        if (tmp$best_fit_overall == "=") {
          seq_delta <- seq(
            tmp$complement_delta_lower_equal,
            tmp$complement_delta_upper_equal,
            length.out = 501
          )
        }
        if (tmp$best_fit_complement == "<") {
          seq_delta <- seq(
            tmp$complement_delta_lower_less,
            tmp$complement_delta_upper_less,
            length.out = 501
          )
        }
        if (tmp$best_fit_complement == ">") {
          seq_delta <- seq(
            tmp$complement_delta_lower_greater,
            tmp$complement_delta_upper_greater,
            length.out = 501
          )
        }
      } else {
        if (tmp$pattern == "=") {
          seq_delta <- seq(
            tmp$complement_delta_lower_equal,
            tmp$complement_delta_upper_equal,
            length.out = 501
          )
        }
        if (tmp$pattern == "<") {
          seq_delta <- seq(
            tmp$complement_delta_lower_less,
            tmp$complement_delta_upper_less,
            length.out = 501
          )
        }
        if (tmp$pattern == ">") {
          seq_delta <- seq(
            tmp$complement_delta_lower_greater,
            tmp$complement_delta_upper_greater,
            length.out = 501
          )
        }
      }
    }

    seq_delta <- sort(seq_delta)
    seq_delta2 <- seq_delta[between(seq_delta,lower,upper)]

    graphics::rect(
      xleft =  i-bar_width,
      xright = i+bar_width,
      ybottom = seq_delta2[-1],
      ytop = seq_delta2[-length(seq_delta2)],
      xpd = NA,
      col = grDevices::rgb(f_colZ(seq(0, 1, length.out = 500)), maxColorValue = 255)[between(seq_delta,lower,upper)],
      border = NA
    )


     if (!pattern_choice_auto) {
    # lower part
      graphics::rect(
        xleft =  i-bar_width,
        xright = i+bar_width,
        ybottom = lower,
        ytop = max(seq_delta[1],lower),
        xpd = NA,
        col = ifelse(tmp$pattern == "<","#f5aa20","#f2f2f2"),
        border = NA
      )
      #upper part
      graphics::rect(
        xleft =  i-bar_width,
        xright = i+bar_width,
        ybottom = min(last(seq_delta),upper),
        ytop = upper,
        xpd = NA,
        col = ifelse(tmp$pattern == ">", "#f5aa20","#f2f2f2"),
        border = NA
      )

       if(tmp$pattern == "=") {
         segments(
          x0=  i-delta_width,
          x1 = i+delta_width,
          y0= c(seq_delta[1],mean(seq_delta),last(seq_delta)),
          y1 = c(seq_delta[1],mean(seq_delta),last(seq_delta)),
          col = c("#cccccc","#d99802","#cccccc"),
        )
       } else {
         if(tmp$pattern == "<") {
           col_ <- c("#d99802","#cccccc")
         } else {
            col_ <- c("#cccccc","#d99802")
         }
         segments(
          x0=  i-delta_width,
          x1 = i+delta_width,
          y0= c(min(seq_delta), max(seq_delta)),
          y1 = c(min(seq_delta), max(seq_delta)),
          col =col_
        )
       }
     } else {
        # lower part
      graphics::rect(
        xleft =  i-bar_width,
        xright = i+bar_width,
        ybottom = lower,
        ytop = max(seq_delta[1],lower),
        xpd = NA,
        col = ifelse(tmp$best_fit_overall == "<","#f5aa20","#f2f2f2"),
        border = NA
      )
      #upper part
      graphics::rect(
        xleft =  i-bar_width,
        xright = i+bar_width,
        ybottom = min(last(seq_delta),upper),
        ytop = upper,
        xpd = NA,
        col = ifelse(tmp$best_fit_overall == ">", "#f5aa20","#f2f2f2"),
        border = NA
      )

       if(tmp$best_fit_overall == "=") {
         segments(
          x0=  i-bar_width,
          x1 = i+bar_width,
          y0= c(seq_delta[1],mean(seq_delta),last(seq_delta)),
          y1 = c(seq_delta[1],mean(seq_delta),last(seq_delta)),
          col = c("#cccccc","#d99802","#cccccc"),
        )
       } else {
          if(tmp$best_fit_overall == "<") {
           col_ <- c("#d99802","#cccccc")
         } else {
            col_ <- c("#cccccc","#d99802")
         }
         segments(
          x0=  i-bar_width,
          x1 = i+bar_width,
          y0= c(min(seq_delta), max(seq_delta)),
          y1 = c(min(seq_delta), max(seq_delta)),
          col = col_
        )
       }
     }
   }
    mtext(
      paste0("N=", tmp$N_overall),
      side = 3,
      at = i,
      line = 2,
      col = "#424242",
      cex = 1
    )
    mtext(
      paste0("n=", tmp$N_subgroup),
      side = 3,
      at = i,
      line = 1,
      col = "dodgerblue",
      cex = 1
    )
    if(add_backgrounds_logical){
    # if(compare_pattern == "overall mean") {
    #   truth_value <- tmp$truth_value_overall
    #   best_pattern <- paste(dorisGraphData$best_fit_overall, collapse = " ")
    # } else {
    #   truth_value <- tmp$truth_value_complement
    #   best_pattern <- paste(dorisGraphData$best_fit_complement, collapse = " ")
    # }

    # mtext(
    #   paste0("tv=", round(truth_value,2)),
    #   side =  1,
    #   at = i,
    #   line = 2,
    #   col = "#f5aa20",
    #   cex = 1
    # )
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
  #}
   if (add_permutation_infos) {
    df <- Reduce(rbind,lapply(tmp_list$mean_list,function(x){x[index,]}))
     if (any(is.na(df))) {

    } else {
    f_colZ <- grDevices::colorRamp(c("#eeeeee","#f5aa20"))
    col_df <- grDevices::rgb(f_colZ(tmp_list$tv_df[index,]), maxColorValue = 255)
    for(i in 1:length(col_df)) {
      lines(x = dorisGraphData$dose, y = df[i,], col = paste0(col_df, 40)[i],lwd = 1.5)
    }
   }
  }
  # draw mean lines
  #dorisGraphData$N_overall/  max(dorisGraphData$N_overall)
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
        cex= sqrt(20*(dorisGraphData$N_overall/max(dorisGraphData$N_overall))/pi),
        col ="#00000080",
        pch = 19
      )
  }


  if (add_points_logical) {
    #points_data
    if (jitter_points) {
      jitter_width <- diff(range(dorisGraphData$dose))/20
      tmp <- points_data[points_data[,subgroup] != subgroup_level,]$dose
      tmp_sub <- points_data[points_data[,subgroup] == subgroup_level,]$dose
      for (i in dorisGraphData$dose) {
        tmp[tmp == i & !is.na(tmp)] <- points_data[points_data[,subgroup] != subgroup_level,]$dose[points_data[points_data[,subgroup] != subgroup_level,]$dose == i & !is.na(points_data[points_data[,subgroup] != subgroup_level,]$dose)] + seq(-jitter_width, jitter_width, length = length(points_data[points_data[,subgroup] != subgroup_level,]$dose[points_data[points_data[,subgroup] != subgroup_level,]$dose == i & !is.na(points_data[points_data[,subgroup] != subgroup_level,]$dose)]))
        tmp_sub[tmp_sub == i & !is.na(tmp_sub)] <- points_data[points_data[,subgroup] == subgroup_level,]$dose[points_data[points_data[,subgroup] == subgroup_level,]$dose == i & !is.na(points_data[points_data[,subgroup] == subgroup_level,]$dose)] + seq(-jitter_width, jitter_width, length = length(points_data[points_data[,subgroup] == subgroup_level,]$dose[points_data[points_data[,subgroup] == subgroup_level,]$dose == i & !is.na(points_data[points_data[,subgroup] == subgroup_level,]$dose)]))

      }
      points(
        tmp,
        points_data[points_data[,subgroup] != subgroup_level,]$targetVariable,
        cex= 1,
        col ="#08cf86e2",
        pch = 19
      )
      points(
        tmp_sub,
        points_data[points_data[,subgroup] == subgroup_level,]$targetVariable,
        cex= 1,
        col ="#1e90ffe2",
        pch = 19
      )
    } else {
      points(
        points_data[points_data[,subgroup] != subgroup_level,]$dose,
        points_data[points_data[,subgroup] != subgroup_level,]$targetVariable,
        cex= 1,
        col ="#08cf86e2",
        pch = 19
      )
      points(
        points_data[points_data[,subgroup] == subgroup_level,]$dose,
        points_data[points_data[,subgroup] == subgroup_level,]$targetVariable,
        cex= 1,
        col ="#1e90ffe2",
        pch = 19
      )
    }
  }


  #draw subgroup lines

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
      cex = sqrt(20*(dorisGraphData$N_subgroup/max(dorisGraphData$N_overall))/pi),
      #cex = sqrt(dorisGraphData$N_subgroup /pi)/2,
      col ="#1e90ffe2",
      pch = 19
    )

   if (add_other_subgroups_logical) {
     for(i in 1:sum(startsWith(names(dorisGraphData),"N_other_"))) {
      lines(
        x = dorisGraphData$dose,
        y = unlist(dorisGraphData[paste0("mean_other_",i)]),
        type = "l",
        col = "#a6baaf60",
        lwd = 2,
        lty = 2,
        pch = 16
      )
        points(
          x = dorisGraphData$dose,
          y = unlist(dorisGraphData[paste0("mean_other_",i)]),
          cex = sqrt(20*(unlist(dorisGraphData[paste0("N_other_",i)])/max(dorisGraphData$N_overall))/pi),
          #cex= sqrt(unlist(dorisGraphData[paste0("N_other_",i)]) /pi)/2,
          col ="#a6baaf60",
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
          cex = sqrt(20*(dorisGraphData$N_complement/max(dorisGraphData$N_overall))/pi),
          #cex= sqrt(dorisGraphData$N_complement /pi)/2,
          col ="#08cf86e2",
          pch = 19
        )
    }
}

