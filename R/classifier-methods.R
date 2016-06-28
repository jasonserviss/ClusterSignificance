#'@include All-classes.R
NULL

#' Classification of the one dimensional points in a Pcp or Mlp object.
#' 
#' Classification based on ROC params (TN TP FP FN).
#' 
#' Tests all possible discrimination lines and picks the one with highest score
#' based on a score which is simply calculated by the formula
#' (TP - FP) + (TN - FN).
#'
#' The plot shows the distribution of scores for different discrimination lines.
#' Each line is a separator that has a score for the separation of the two 
#' groups, and the height of the line marks the score for this separation.
#' 
#' @name classify
#' @rdname classify
#' @aliases classify classify,Pcp-method classify,Mlp-method ClassifiedPoints 
#' @param x Pcp or Mlp Object for the function classify otherwise it is a 
#'  ClassifiedPoints object
#' @param n data to extract from ClassifiedPoints (NULL gives all)
#' @param y default plot param, which should be set to NULL
#' @param .Object internal object 
#' @param object ClassifiedPoints Object
#' @param scores final scores
#' @param scores.points sorted points 
#' @param scores.index index of sorted points
#' @param ROC parameters (TN, TP, FN and FP)
#' @param ... additional arguments to pass on
#' @return The classify function returns an object of class ClassifiedPoints
#' @author Jesper R. Gadin and Jason T. Serviss
#' @keywords classification
#' @examples
#' 
#' #use demo data
#' data(pcpMatrix)
#' groups <- rownames(pcpMatrix)
#'
#' #run function
#' prj <- pcp(pcpMatrix, groups)
#' cl <- classify(prj)
#'
#' #getData accessor
#' getData(cl)
#'
#' #getData accessor specific
#' getData(cl, "scores")
#'
#' #plot result
#' plot(cl)
#'
#' @exportMethod classify
#' 
NULL

#' @rdname classify
setGeneric("classify", function(x, ...
    ){ standardGeneric("classify")})

#' @rdname classify
setMethod("classify", "Pcp", function(x, ...
    ){
        .classifyWrapper(x, ...)
})                                    

#' @rdname classify
setMethod("classify", "Mlp", function(x, ...
    ){
        .classifyWrapper(x, ...)
})                                    

###############################################################################
#
# helpers as units
#
###############################################################################

.classifyWrapper <- function(x, ...){
    ##import and order data
    #points <-    x["points.onedim"]
    points <-    getData(x, "points.onedim")
    names(points) <- getData(x, "groups")
    sp <- sort(points,decreasing=FALSE, index.return=TRUE)
    order <- names(points)[sp$ix]

    #find all group comparisons to be made
    allCombos <- combn(unique(order),2)

    ##make a list holding matrixes of all group positions coded as 0 or 1
    codedMat <- .codedMatrix(allCombos, order)

    ##select the upper triangle and assign "2" to the values not to be
    ##included in the score calculation

    sepCalc <- .upperTriangle(mat = codedMat)
    grp1.mat <- sepCalc[[1]]
    grp2.mat <- sepCalc[[2]]

    ##calculate the true positives, false positives, false negatives, and true
    ##negatives
    truth <- .TpFpFnTn(grp1.mat, grp2.mat)
    TP <- truth[[1]]
    FP <- truth[[2]]
    FN <- truth[[3]]
    TN <- truth[[4]]

    ##score each seperation
    scores <- .Scores(TP = TP, FP = FP, FN = FN, TN = TN)

    #check if we get better score by changing the predicted class
    scores <- .Bscore(scores)

    ##name the scores variable
    names(scores) <- sapply(1:length(scores),
        function(x) paste(allCombos[,x], collapse = " vs "))

    #return ClassifiedPoints object
    new("ClassifiedPoints",
    scores = scores,
    scores.points=sp$x,
    scores.index=sp$ix,
    ROC = list(TP=TP,FP=FP,FN=FN,TN=TN)
    )

}

##make a list holding matrixes of all group positions coded as 0 or 1
.codedMatrix <- function(allCombos, order) {

    ##Outputs a list with one element per group seperation to be scored.
    ##Each list element contains a character vector.
    ##Each character vector contains the groups names. The group names are
    ##repeated the same numer of times as there are points for each group.
    groupList <- lapply(1:ncol(allCombos),
        function(oo, order)
            order[order %in% allCombos[, oo]], order=order)

    ##Code each group name as 0 or 1
    codedList <- lapply(1:length(groupList),
        function(pp, groupList)
            ifelse(
                groupList[[pp]] == unique(groupList[[pp]])[1],
                as.integer(0),
                as.integer(1)
            ), groupList=groupList
    )

    ##Assemble a martix for each group comparison to be scored where all
    ##possible seperations can be easily represented.
    mat <- lapply(1:length(codedList),
        function(oo)
            matrix(codedList[[oo]],
                ncol=length(codedList[[oo]]),
                nrow=length(codedList[[oo]])))
    return(mat)
}

##select the upper triangle adn assign 2 to values not to be included in the
##score calculation
.upperTriangle <- function(mat) {
    tfmat <- lapply(1:length(mat),
        function(ee, mat) upper.tri(mat[[ee]]), mat=mat)
    grp1.mat <- mat
    grp2.mat <- mat

    ##how to do this without altering a global variable
    grp1.mat <- lapply(1:length(grp1.mat),
        function(x, grp1.mat){
            grp1.mat[[x]][!tfmat[[x]]] <- 2
            grp1.mat[[x]]
        }, grp1.mat=grp1.mat
    )
        
    grp2.mat <- lapply(1:length(grp2.mat),
        function(x, grp2.mat){
            grp2.mat[[x]][tfmat[[x]]] <- 2
            grp2.mat[[x]]
        }, grp2.mat=grp2.mat
    )

    l <- list(grp1.mat, grp2.mat)
    return(l)
}

##calculate false/true positives/negatives
#TP - actual grp 1 classified as group one
#FP - actual grp 1 classified as group two
#FN - actual grp 2 classified as group one
#TN - actual grp 2 classified as group two
##outputs a list with 4 elements (TP, FP, FN, TN) where each element includes
#one list for each group comparison

.TpFpFnTn <- function(grp1.mat, grp2.mat){
    TP <- lapply(1:length(grp1.mat),
        function(x, y) apply(y[[x]][,-1] == 1,2,sum), y=grp1.mat
    )
    FP <- lapply(1:length(grp1.mat),
        function(x, y) apply(y[[x]][,-1] == 0,2,sum), y=grp1.mat
    )
    FN <- lapply(1:length(grp2.mat),
        function(x, y) apply(y[[x]][,-1] == 1,2,sum), y=grp2.mat
    )
    TN <- lapply(1:length(grp2.mat),
        function(x, y) apply(y[[x]][,-1] == 0,2,sum), y=grp2.mat
    )
    l <- list(TP, FP, FN, TN)
    return(l)
}

#calculate score for each group and classification
#scores <- (TP-FP) + (TN-FN)
.Scores <- function(TP, FP, FN, TN) {
    scores <- lapply(1:length(TP),
        function(xx)
            sapply(1:length(TP[[xx]]),
                function(y, TP, FP, FN, TN)
                    {
                        (TP[[xx]][[y]] - FP[[xx]][[y]]) +
                        (TN[[xx]][[y]] - FN[[xx]][[y]])
                    }, TP=TP, FP=FP, FN=FN, TN=TN
            )
    )
    return(scores)
}

##better score
.Bscore <- function(scores){
    scores <- lapply(1:length(scores),
        function(oo, scores)
            ifelse(scores[[oo]] < -scores[[oo]],
                -scores[[oo]],
                scores[[oo]]),
                scores=scores)
    return(scores)
}

