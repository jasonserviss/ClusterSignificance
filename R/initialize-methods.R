#'@include ClusterSignificance-package.R
NULL

#' @rdname pcp
#' @export 
setMethod("initialize","Pcp", function(
    .Object,
    ...,
    groups,
    points.orig,
    line,
    points.onedim,
    index
    ){
    callNextMethod(.Object,...,
    groups = groups,
    points.orig = points.orig,
    line = line,
    points.onedim = points.onedim,
    index = index)
    }
)

#' @rdname mlp
#' @export 
setMethod("initialize","Mlp", function(
    .Object, ..., groups, points.orig, line, points.onedim
    ){
    callNextMethod(.Object, ...,
    groups = groups,
    points.orig = points.orig,
    line = line,
    points.onedim = points.onedim)
    }
)

#' @rdname classify
#' @export 
setMethod("initialize","ClassifiedPoints", function(
    .Object,
    ...,
    scores,
    scores.points=scores.points,
    scores.index=scores.index,
    ROC
    ){
        callNextMethod(.Object, ...,
            scores=scores,
            scores.points=scores.points,
            scores.index=scores.index,
            ROC=ROC
        )
    }
)

#' @rdname permute
#' @export 
setMethod("initialize","PermutationResults", function(
    .Object,
    ...,
    scores.real,
    scores.vec
    ){
        callNextMethod(.Object, ...,
            scores.real=scores.real,
            scores.vec=scores.vec)
    }
)
