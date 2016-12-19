#'@include All-classes.R
NULL

#' Permutation test
#' 
#' Test how the classification performs compared to random (eg. permuted) data.
#' 
#' This is a test suit and will return a summarized object. The default of the
#' parameter 'iter' is set quite low, and in principle the more iterations the 
#' better, or until the pvalue converges to a specifc value. If no pre-permuted
#' data has been supplied by the user, then the internal permutation method
#' will perform a sampling without replacement within each dimension. 
#' 
#' @name permute
#' @rdname permute
#' @aliases permute permute,matrix-method
#' @param mat matrix with samples on rows, PCs in columns. 
#' Ordered PCs, with PC1 to the left.
#' @param iter integer number of iterations to be performed.
#' @param groups vector in same order as rows in matrix
#' @param df degrees of freedom, passed to smooth.spline
#' @param verbose makes function more talkative
#' @param projmethod 'pcp' or 'mlp'
#' @param user.permutations user defined permutation matrix
#' @param seed random seed to be used by the internal permutation
#' @param scores.real the real score
#' @param scores.vec all permuted scores
#' @param x matrix for the function permute, otherwise it is a
#'   PermutationResults object
#' @param n data to extract from ClassifiedPoints (NULL gives all)
#' @param y default plot param, which should be set to NULL
#' @param .Object internal object 
#' @param object ClassifiedPoints Object
#' @param ... arguments to pass on
#' @param recursive dont use (belongs to default generic of combine 'c()')
#' @return The permute function returns an object of class PermutationResults
#' @author Jesper R. Gadin and Jason T. Serviss
#' @keywords permutation
#' @examples
#' 
#' #use pcp method
#' data(pcpMatrix)
#' groups <- rownames(pcpMatrix)
#'
#' #run function
#' iterations <- 10
#' pe <- permute(
#'     mat=pcpMatrix,
#'     groups=groups,
#'     iter=iterations,
#'     projmethod="pcp"
#' )
#' 
#' #use mlp method
#' data(mlpMatrix)
#' groups <- rownames(mlpMatrix)
#' pe <- permute(
#'     mat=mlpMatrix,
#'     groups=groups,
#'     iter=iterations,
#'     projmethod="mlp"
#' )
#' 
#'
#' #getData accessor
#' getData(pe)
#'
#' #getData accessor specific
#' getData(pe, "scores.vec")
#'
#' #get pvalue
#' pvalue(pe)
#'
#' #plot result
#' plot(pe)
#'
#' #combine three (parallell) jobs on the same matrix 
#' pe2 <- c(pe, pe, pe)
#'
NULL

#' @rdname permute
#' @export
setGeneric("permute", function(mat, ...
    ){ standardGeneric("permute")})

#' @rdname permute
#' @export
setMethod("permute", "matrix",
    function(
        mat,
        groups,
        projmethod="pcp",
        iter=100,
        user.permutations=NULL,
        seed=3,
        df=5,
        verbose=TRUE,
        ...
    ){

    if(verbose) message("initializing permutation analysis\n")
    
    #check which projectionmethod to use
    if(projmethod=="pcp") {
        projm <- pcp
    } else if(projmethod=="mlp") {
        projm <- mlp
    }

    ##check for NA groups
    if(any(is.na(groups))) groups[is.na(groups)] <- "NA"

    uniq.groups <- combn(unique(groups), 2)
    uniq.groups <- apply(uniq.groups, 2, sort)

    if(!is.null(user.permutations)){
        user.permutations <- .user.permutationsCheck(
            user.permutations,
            uniq.groups,
            iter
        )
        permats <- user.permutations
    }else{
        set.seed(seed)
        permats <- .permMatrix(iter, groups, uniq.groups, mat)
    }

    #score first the real one
    scores.real <- .scoreReal(mat, groups, projm, df)

    ##score permats
    scores.vec <- .scorePermats(permats, groups, uniq.groups, mat, projm, df)

    invisible(lapply(1:length(scores.vec), function(x, scores.real)
        message(
            length(scores.vec[[x]]),
            " iterations were sucessfully completed for comparison ",
            names(scores.real[x])
        ), scores.real=scores.real
    ))

    #remove temporary projm
    rm(projm)

    new("PermutationResults",
        scores.real=scores.real,
        scores.vec=scores.vec
    )

})

###############################################################################
#
# helpers as units
#
###############################################################################

##build permutation matrix
.permMatrix <- function(iterations, groups, uniq.groups, mat) {
    permats <- lapply(1:ncol(uniq.groups), function(y)
        lapply(1:iterations, function(x)
            sapply(1:ncol(mat[groups %in% uniq.groups[, y], ]), function(i)
                mat[groups %in% uniq.groups[, y], i] <-
                sample(mat[groups %in% uniq.groups[, y], i], replace=FALSE)
            )
        )
    )

    names(permats) <- lapply(1:ncol(uniq.groups), function(x)
        paste(uniq.groups[, x], collapse = " vs ")
    )

    return(permats)
}

##score real ones
.scoreReal <- function(mat, groups, projm, df) {
    ob <- projm(mat, groups, df=df)
    cl <- classify(ob)
    scores.real <- lapply(
        getData(cl, "scores"),
        function(y)
            max(y)
    )

    scores.real <- scores.real[
        order(names(scores.real))
    ]

    return(scores.real)
}

##score permats
.scorePermats <- function(permats, groups, uniq.groups, mat, projm, df) {
    perm.scores <- lapply(1:length(permats), function(x, permats)
        as.numeric(
            lapply(1:length(permats[[1]]), function(y)
                .combinedFunction(
                    y,
                    x,
                    groups,
                    uniq.groups,
                    permats,
                    mat,
                    projm,
                    df
                )
            )
        ), permats=permats
    )
    
    perm.scores <- lapply(1:length(perm.scores),
        function(x)
            perm.scores[[x]][is.na(perm.scores[[x]]) == FALSE])
    
    names(perm.scores) <- names(permats)

    perm.scores <- perm.scores[order(names(perm.scores))]

    return(perm.scores)
}

##current iteration max score; used by .scorePermats
.combinedFunction <- function(y, x, groups, uniq.groups, permats, mat, projm, df) {
    if( y%%500 == 0){message("iteration ", y, " for comparison ", x, "\n")}

    PcpOut <- tryCatch({projm(permats[[x]][[y]],
        groups=groups[groups %in% uniq.groups[, x]], df=df)},
        warning=function(w) {return(NA)},
        error=function(w) {return(NA)}
    )

    if(class(PcpOut) == "Pcp" | class(PcpOut) == "Mlp") {
        maxScore <- max(unlist(classify(PcpOut)@scores))
        return(maxScore)
    } else {
        return(NA)
    }
}

##check user.permutations
.user.permutationsCheck <- function(user.permutations, uniq.groups, iter) {
    
    if(!is.list(user.permutations) | !is.list(user.permutations[[1]])) {
        message("The permutation matrix you input is not a list.")
        stop(paste("Check the vignette for details concerning the ",
            "structure of the user.permutations argument.",sep=""))
    }
    
    if(length(user.permutations) != ncol(uniq.groups)) {
        stop(paste("All groups comparisons are not present",
        "in your permutation matrix", sep=" "))
    }
    if(exists("iterations") & length(user.permutations[[1]]) != iter) {
        message(paste("The number of iterations does not ",
            "match the iterations in your permutation matrix.", sep=""))
    warning("You may have made a mistake when producing the permutation")
    }
    if(is.null(names(user.permutations))) {
        names(user.permutations) <- lapply(1:ncol(uniq.groups), function(x)
            paste(uniq.groups[, x], collapse = " vs ")
        )
    }
    return(user.permutations)
}

##plots
.plots <- function(pdf, scores.real, steps = c(1:6)) {
    if(!is.null(pdf)){
        plot(scores.real, steps)
    }
}

