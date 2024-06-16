#' calculates distribution of the number of subgroups identified as remarkable for a given trial with resampling
#'
#' @param Factors data frame with factors in columns
#' @param dose vector with dose levels
#' @param targetVariable vector with target variable (binary or metric)
#' @param pattern vector to specify pattern of differences
#' @param delta vector with fuzzy logic threshold values
#' @param alpha pattern matching threshold
#' @param runs number of simulation runs
#' @return vector with frequencies of identified subgroups
#' @export

dorisDistribution <- function(Factors,dose,targetVariable,pattern,delta,alpha = 0.05,runs){

    y <- rep(NA, length(targetVariable))

    rep_runs <- replicate(runs,{
        D <- cbind(Factors,dose,targetVariable)
        D_tmp <- D %>%
                dplyr::group_by(dose) %>%
                mutate(target2 = sample(targetVariable,length(targetVariable), replace = TRUE),
                        dose2 = dose) %>% ungroup() %>% as.data.frame()
            D_tmp <- D_tmp[,c("dose2","target2")]
            D_tmp <- D_tmp[sample(rownames(D_tmp[,c("dose2","target2")])),]
        x <- D_tmp$dose2
        y <- D_tmp$target2

        H = dorisEvaluation(Factors,x,y,pattern,delta,alpha)
        sum(H$pmd, na.rm = TRUE)

        }
    )

    return(
        unlist(rep_runs)
    )
}
