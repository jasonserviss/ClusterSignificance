#'@include ClusterSignificance-package.R
NULL

#' @rdname pcp
#' @export
.Pcp <- setClass("Pcp", representation(
    classes       = "character",
    points.orig   = "matrix",
    line          = "matrix",
    points.onedim = "numeric",
    dimnames      = "character",
    index         = "integer",
    class.color   = "matrix"
))

#' @rdname pcp
setGeneric("getData", function(x, ...
){ standardGeneric("getData")})

#' @rdname pcp
#' @export
setMethod("getData", "Pcp", function(
    x,
    n = NULL
){
        if(is.null(n)){
                l <- lapply(slotNames(x), function(y,o) {slot(o, y)}, o = x)
                names(l) <- slotNames(x)
                l
        } else if(class(n) == "character"){
                slot(x, n)
        }
    }
)

#' @rdname mlp
#' @export
.Mlp <- setClass("Mlp", representation(
    classes       = "character",
    points.orig   = "matrix",
    line          = "matrix",
    points.onedim = "numeric",
    dimnames      = "character",
    points.origo  = "matrix",
    class.color   = "matrix"
))

#' @rdname mlp
#' @export
setMethod("getData", "Mlp", function(
    x,
    n = NULL
){

        if(is.null(n)){
            l <- lapply(slotNames(x), function(y, o) {slot(o, y)}, o = x)
            names(l) <- slotNames(x)
            l
        } else if(class(n) == "character"){
            slot(x,n)
        }
    }
)

#' @rdname classify
#' @export
.ClassifiedPoints <- setClass("ClassifiedPoints", 
    representation(
        scores        = "list",
        scores.points = "vector",
        scores.index  = "vector",
        ROC           = "list",
        AUC           = "numeric",
        class.color   = "matrix"
))

#' @rdname classify
#' @export
setMethod("getData", "ClassifiedPoints", function(
    x,
    n = NULL
){

        if(is.null(n)){
            l <- lapply(slotNames(x), function(y, o) {slot(o, y)}, o = x)
            names(l) <- slotNames(x)
            l
        } else if(class(n) == "character"){
            slot(x, n)
        }
    }
)

#' @rdname permute
#' @export
.PermutationResults <- setClass("PermutationResults", 
    representation(
    scores.real = "list",
    scores.vec  = "list",
    group.color = "matrix"
))

#' @rdname permute
#' @export
setMethod("getData", "PermutationResults", function(
    x,
    n = NULL
){

        if(is.null(n)){
            l <- lapply(slotNames(x), function(y, o) {slot(o, y)}, o = x)
            names(l) <- slotNames(x)
            l
        } else if(class(n) == "character"){
            slot(x, n)
        }
    }
)

#' @rdname permute
#' @export
setMethod("c", "PermutationResults", function(
    x,
    ...,
    recursive = FALSE
){

        #check that scores real are the same in all objects
        scores.real = unlist(
            lapply(
                list(x, ...),
                    function(x)
                        unlist(getData(x, "scores.real"))
            )
        )
        if(any(!(scores.real == getData(x, "scores.real"))))
        stop("scores.real must be same to merge objects")

        #until we remake things to arrays, this will do in the meantime
        cnames <- names(getData(x, "scores.real"))
        s.vec <- vector("list", length(getData(x, "scores.vec")))

        for(y in list(x, ...)){
            s.vec <- lapply(
                1:length(getData(y, "scores.vec")),
                function(z)
                    c(s.vec[[z]], getData(y, "scores.vec")[[z]])
            )
        }
        names(s.vec) <- cnames
        new(
            "PermutationResults",
            scores.real = getData(x, "scores.real"),
            scores.vec = s.vec,
            group.color = getData(x, "group.color")
        )
    }
)

#' @rdname permute
setGeneric("pvalue", function(x, ...){
    standardGeneric("pvalue")}
)

#' @rdname permute
#' @export
setMethod("pvalue", "PermutationResults", function(
    x,
    ...
){
        scores.vec <- getData(x, "scores.vec")
        .calculateP(scores.vec, getData(x, "scores.real"))
})

##calculate CI
.calculateP <- function(
    scores.vec,
    scores.real
){
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

#' @rdname permute
setGeneric("conf.int", function(x, ...){
    standardGeneric("conf.int")}
)

#' @rdname permute
#' @export
#' @importFrom stats binom.test
setMethod("conf.int", "PermutationResults", function(
    x,
    conf.level = 0.99,
    ...
){
    .calculateCI(
        getData(x, "scores.vec"),
        getData(x, "scores.real"),
        conf.level
    )
})

.calculateCI <- function(
    scores.vec,
    scores.real,
    conf.level
){
    tPerm <- lapply(
        1:length(scores.vec),
            function(x, scores.vec)
                ifelse(
                    sum(scores.vec[[x]] >= scores.real[[x]])/
                        length(scores.vec[[x]]) == 0,
                    0,
                    sum(scores.vec[[x]] >= scores.real[[x]])
                ), scores.vec = scores.vec
    )
    
    CI <- sapply(
        1:length(tPerm),
        function(x)
            binom.test(
                tPerm[[x]],
                length(scores.vec[[x]]),
                conf.level = conf.level
            )$conf.int
    )
    
    colnames(CI) <- names(scores.vec)
    rownames(CI) <- c(
        paste("CI", gsub(".([0-9]*)", "\\1", conf.level), "%-low", sep = ""),
        paste("CI", gsub(".([0-9]*)", "\\1", conf.level), "%-high", sep = "")
    )
    return(CI)
}




