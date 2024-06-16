#' Function \code{dorisEvaluation} identifies remarkable subgroups.
#'
#' @param Factors data frame with factors in columns
#' @param dose vector with dose levels
#' @param targetVariable vector with target variable (binary or metric)
#' @param pattern vector to specify pattern of differences
#' @param delta vector with fuzzy logic threshold values
#' @param alpha pattern matching threshold
#' @return a list with six items (lowercase: vectors, uppercase: data frames):
#' * \code{ctm} clinical trial means
#' * \code{SGM} subgroup means
#' * \code{DSC} differences \code{SGM - ctm}
#' * \code{DTV} dose level truth values
#' * \code{stv} subgroup truth value
#' * \code{pmd} pattern matching decision
#' @export

dorisEvaluation <- function(Factors,dose,targetVariable,pattern,delta, alpha = 0.05,
                            method = "Minimum"){

    targetVariable <- as.numeric(targetVariable)
    ### number of non-missing values
    nmv <- function(x) length(x[!is.na(x)])

### number of dose levels
    k=length(unique(dose))

### ON THE CLINICAL TRIAL LEVEL
### patients per dose level
    cN=tapply(targetVariable,dose,nmv)
### trial means per dose level
    ctm=tapply(targetVariable,dose,mean,na.rm=TRUE)
    names(ctm)=paste("dl",1:k,sep="")

### subgroup means
### create a list with one item per factor
### each item is a matrix: factor levels in rows, dose levels in columns
### matrix elements are means
    h=lapply(names(Factors),
        function(f){
            tapply(targetVariable,cbind(Factors[f],dose),mean,na.rm=TRUE,simplify=TRUE)
        }
    )
    ### combine items of h to a data frame
    sm=as.data.frame(Reduce(rbind,h))

    dimnames(sm)[[2]]=paste("dl",1:k,sep="")
    row.names(sm)=NULL

    # set missing values to zero
    sm_nm <- sm
    sm_nm[is.na(sm_nm)] <- 0
    ### combine items of h to a data frame
    SGM=as.data.frame(Reduce(rbind,h))
    dimnames(SGM)[[2]]=paste("dl",1:k,sep="")
    row.names(SGM)=NULL

    # set missing values to zero
    SGM_nm <- SGM
    SGM_nm[is.na(SGM_nm)] <- 0
    ### THE COMPLEMENTS
    ### number of patients per dose level
    h=Reduce(rbind,replicate(nrow(sm),cN,simplify=FALSE))
    kn=h-sm_nm
### means per dose level
    h=Reduce(rbind,replicate(nrow(SGM_nm),ctm,simplify=FALSE))
    CPM=((sm_nm +kn)*h-sm_nm *SGM_nm )/kn

### differences: subgroup means - clinical trial means
    DSC=as.data.frame(t(apply(SGM,1,function(x) x-ctm)))
    dimnames(DSC)[[2]]=paste("dl",1:k,sep="")

### truth values per dose level within in subgroup
    DTV=as.data.frame(t(apply(DSC,1,truthValues,pattern,delta)))
    dimnames(DTV)[[2]]=paste(round(as.numeric(levels(unique(dose))),3),sep="")

### truth values for the subgroups
    if (method == "Minimum") {
        stv=apply(DTV,1,min)
    } else if (method == "Sum") {
        stv=apply(DTV,1,sum)
    } else if (method == "Mean") {
        stv=apply(DTV,1,mean)
    } else if (method == "Weighted Mean") {
        stv=apply(DTV,1,mean)
    }

### pattern matching decision
    pmd=as.numeric(stv >= alpha)

    list(ctm=ctm,
         SGM=SGM,
         CPM=CPM,
         DSC=DSC,
         DTV=DTV,
         stv=stv,
         pmd=pmd
    )
}
