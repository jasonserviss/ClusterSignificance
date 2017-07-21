#'@include ClusterSignificance-package.R
NULL

#' @rdname pcp
#' @export 
setMethod("initialize","Pcp", function(
    .Object,
    ...,
    classes,
    points.orig,
    line,
    points.onedim,
    index,
    class.color
){
    callNextMethod(
        .Object,
        ...,
        classes = classes,
        points.orig = points.orig,
        line = line,
        points.onedim = points.onedim,
        index = index,
        class.color = class.color
    )
})

#' @rdname mlp
#' @export 
setMethod("initialize","Mlp", function(
    .Object,
    ...,
    classes,
    points.orig,
    line,
    points.onedim,
    class.color
){
    callNextMethod(
        .Object,
        ...,
        classes = classes,
        points.orig = points.orig,
        line = line,
        points.onedim = points.onedim,
        class.color = class.color
    )
})

#' @rdname classify
#' @export 
setMethod("initialize","ClassifiedPoints", function(
    .Object,
    ...,
    scores,
    scores.points=scores.points,
    scores.index=scores.index,
    ROC,
    AUC,
    class.color
){
    callNextMethod(
        .Object,
        ...,
        scores = scores,
        scores.points = scores.points,
        scores.index = scores.index,
        ROC = ROC,
        AUC = AUC,
        class.color = class.color
    )
})

#' @rdname permute
#' @export 
setMethod("initialize","PermutationResults", function(
    .Object,
    ...,
    scores.real,
    scores.vec
){
    callNextMethod(
        .Object,
        ...,
        scores.real = scores.real,
        scores.vec = scores.vec
    )
})
