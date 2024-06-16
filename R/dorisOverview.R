#' dose response in subgroups
#'
#' Function \code{dorisOverview} provides an overview of the data
#' and the information to identify numbered subgroups, as provided
#' by dorisEvaluation. The "overview" is seperated from the "evaluation"
#' to keep \code{dorisEvaluation} efficient for simulations.
#'
#' @param Factors         data frame with factors in columns
#' @param dose            vector with dose levels
#' @param targetVariable  vector with target variable (binary or metric)

#' @return a list with three items (lowercase for vectors, uppercase for data frames):
#' * \code{fact} names of factors
#' * \code{levl} factor levels
#' * \code{SGDL} crosstab of subgroups by dose levels
#' @export

dorisOverview <- function(Factors,dose,targetVariable){

    k=length(unique(dose))

    h=lapply(Factors,
        function(f)
            table(f,dose)
    )

    if (dim(Factors)[2] == 1) {
        H <- data.frame(matrix(h[[1]],dim(h[[1]])[1],dim(h[[1]])[2]))
        colnames(H) <- unique(dose)
        rownames(H) <- rownames(h[[1]])
    } else {
    H = as.data.frame(Reduce(rbind,h))
    }
    ln=unlist(lapply(Factors, function(f){
        sort(unique(f))
    }))
    names(ln) <- NULL

    fn=unlist(
        mapply(
            function(f,n){
                rep(n,length(na.omit(unique(f))))
            },
            Factors,
            names(Factors)
        )
    )
    names(fn)=NULL

    names(H)=paste0("dose: ", colnames(H))
    row.names(H)=NULL

    list(fact=fn,levl=ln,SGDL=H)
}
