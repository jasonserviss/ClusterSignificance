#'@include ClusterSignificance-package.R
NULL

#' @rdname pcp
#' @export
.Pcp <- setClass("Pcp", representation(
    groups="character",
    points.orig="matrix",
    line="matrix",
    points.onedim="numeric",
    dimnames="character",
    index="integer",
    group.color="matrix"
))

#' @rdname pcp
setGeneric("getData", function(x, ...
){ standardGeneric("getData")})

#' @rdname pcp
#' @export
setMethod("getData", "Pcp", function(x, n=NULL)
    {
        if(is.null(n)){
                l <- lapply(slotNames(x),function(y,o){slot(o, y)},o=x)
                names(l) <- slotNames(x)
                l
        } else if(class(n)=="character"){
                slot(x,n)
        }
    }
)

#' @rdname mlp
#' @export
.Mlp <- setClass("Mlp", representation(
    groups="character",
    points.orig="matrix",
    #points.norm="matrix",
    line="matrix",
    points.onedim="numeric",
    dimnames="character",
    #Mlp specific
    points.origo="matrix",
    group.color="matrix"
))

#' @rdname mlp
#' @export
setMethod("getData", "Mlp", function(x, n=NULL)
    {

        if(is.null(n)){
            l <- lapply(slotNames(x),function(y,o){slot(o, y)},o=x)
            names(l) <- slotNames(x)
            l
        } else if(class(n)=="character"){
            slot(x,n)
        }
    }
)

#' @rdname classify
#' @export
.ClassifiedPoints <- setClass("ClassifiedPoints", 
    representation(
        scores="list",
        scores.points="vector",
        scores.index="vector",
        ROC="list",
        group.color="matrix"
))

#' @rdname classify
#' @export
setMethod("getData", "ClassifiedPoints", function(x, n=NULL)
    {

        if(is.null(n)){
            l <- lapply(slotNames(x),function(y,o){slot(o, y)},o=x)
            names(l) <- slotNames(x)
            l
        } else if(class(n)=="character"){
            slot(x,n)
        }
    }
)

#' @rdname permute
#' @export
.PermutationResults <- setClass("PermutationResults", 
    representation(
    scores.real="list",
    scores.vec="list",
    group.color="matrix"
    #p.value="numeric"
))

#' @rdname permute
#' @export
setMethod("getData", "PermutationResults", function(x, n=NULL)
    {

        if(is.null(n)){
            l <- lapply(slotNames(x),function(y,o){slot(o, y)},o=x)
            names(l) <- slotNames(x)
            l
        } else if(class(n)=="character"){
            slot(x,n)
        }
    }
)

#' @rdname permute
#' @export
setMethod("c", "PermutationResults", function(x, ..., recursive=FALSE)
    {

        #check that scores real are the same in all objects
        scores.real=unlist(lapply(list(x, ...),
            function(x){unlist(getData(x, "scores.real"))}))
            #print(scores.real)
            #print(scores.real == getData(x,"scores.real"))
            if(any(!(scores.real == getData(x,"scores.real"))))
            stop("scores.real must be same to merge objects")
            #until we remake things to arrays, this will do in the meantime
            cnames <- names(getData(x, "scores.real"))
            clen <- length(getData(x, "scores.real"))
            lst <- lapply(list(x, ...),
                function(x){
                    as.data.frame(
                        getData(x, "scores.vec"),
                        col.names=1:clen
                    )
                })
                df <- do.call("rbind",lst)
                scores.lst <- lapply(1:ncol(df),function(x){df[[x]]})
                names(scores.lst) <- cnames
    
        new("PermutationResults",
            #p.value=0.11,
            scores.real=getData(x,"scores.real"),
            scores.vec=scores.lst
            )
    }
)

#' @rdname permute
setGeneric("pvalue", function(x, ...
){ standardGeneric("pvalue")})

#' @rdname permute
#' @export
setMethod("pvalue", "PermutationResults", function(x,...)
    {
        scores.vec <- getData(x,"scores.vec")
        .calculateP(scores.vec, getData(x,"scores.real"))
    }
)

##calculate p-value
.calculateP <- function(scores.vec, scores.real) {
    #scores.vec <- list(scores.vec)
    p.value <- lapply(
        1:length(scores.vec),
            function(x, scores.vec)
                ifelse(
                    sum(scores.vec[[x]] >= scores.real[[x]])/
                        length(scores.vec[[x]]) == 0,
                    10^-log10(length(scores.vec[[x]])),
                    sum(scores.vec[[x]] >= scores.real[[x]])/
                    length(scores.vec[[x]])
                ), scores.vec = scores.vec
    )

    p.value <- as.numeric(p.value)
    names(p.value) <- names(scores.real)
    return(p.value)
}
