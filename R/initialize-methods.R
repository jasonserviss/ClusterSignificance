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
    index,
    group.color
    ){
    callNextMethod(.Object,...,
    groups = groups,
    points.orig = points.orig,
    line = line,
    points.onedim = points.onedim,
    index = index,
    group.color = group.color
	)
    }
)

#' @rdname mlp
#' @export 
setMethod("initialize","Mlp", function(
    .Object, ..., groups, points.orig, line, points.onedim, group.color
    ){
    callNextMethod(.Object, ...,
    groups = groups,
    points.orig = points.orig,
    line = line,
    points.onedim = points.onedim,
    group.color = group.color)
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
    ROC,
    group.color
    ){
        callNextMethod(.Object, ...,
            scores=scores,
            scores.points=scores.points,
            scores.index=scores.index,
            ROC=ROC,
			group.color=group.color
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
            scores.vec=scores.vec
			)
    }
)
