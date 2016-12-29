#'@include All-classes.R
NULL

#internal show function
.showPcp <- function(
    object
){
    names <- slotNames(object)
    cat("Class:","Pcp\n")
    cat("Contains: \n")
    for(i in 1:length(names)){
        cat(paste(i,". ",names[i], "\n",sep=""))
    }
}
#' @rdname pcp
#' @export
setMethod("show", "Pcp", function(object){.showPcp(object)})

#internal show function
.showMlp <- function(
    object
){
    names <- slotNames(object)
    cat("Class:","Mlp\n")
    cat("Contains: \n")
    for(i in 1:length(names)){
        cat(paste(i,". ",names[i], "\n",sep=""))
    }
}

#' @rdname mlp
#' @export
setMethod("show", "Mlp", function(object){.showMlp(object)})

#internal show function
.showClassifiedPoints <- function(
    object
){
    names <- slotNames(object)
    cat("Class:","ClassifiedPoints\n")
    cat("Contains: \n")
    for(i in 1:length(names)){
        cat(paste(i,". ",names[i], "\n",sep=""))
    }
}

#' @rdname classify
#' @export
setMethod("show","ClassifiedPoints",
function(object){.showClassifiedPoints(object)})


#internal show function
.showPermutationResults <- function(
    object
){
    names <- slotNames(object)
    cat("Class:","PermutationResults\n")
    cat("Contains: \n")
    for(i in 1:length(names)){
        cat(paste(i,". ",names[i], "\n",sep=""))
    }
}

#' @rdname permute
#' @export
setMethod("show","PermutationResults",
function(object){.showPermutationResults(object)})
