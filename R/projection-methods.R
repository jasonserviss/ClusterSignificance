#'@include All-classes.R
NULL

#' Projection of points into one dimension.
#'
#' Project points onto a principal curve.
#'
#' The resulting Pcp object containing results from a principal curve reduction
#' to one dimension. The group and the one dimensional points will be the
#' information needed to carry out a classification using the classify() 
#' function. As a help to illustrate the details of the dimension reduction,
#' the information from some critical steps is stored in the object. To
#' visually explore these there is a dedicated plot method for Pcp objects, use
#' plot().
#'
#' @name pcp
#' @rdname pcp
#' @aliases pcp pcp,matrix-method Pcp 
#' @param mat matrix with samples on rows, PCs in columns. Ordered PCs, 
#' with PC1 to the left.
#' @param groups vector in same order as rows in matrix
#' @param x matrix object for the function pcp otherwise it is a Pcp object
#' @param df degrees of freedom, passed to smooth.spline
#' @param group.color user assigned group coloring scheme
#' @param n data to extract from Pcp (NULL gives all)
#' @param y default plot param, which should be set to NULL
#' @param .Object internal object 
#' @param points.orig multidimensional points describing the original data 
#' @param line multidimensional points describing a line 
#' @param points.onedim a vector of points
#' @param index internal index from the projection 
#' @param steps 1,2,3,4,5,6 or "all"
#' @param object Pcp object
#' @param ... additional arguments to pass on
#' @return The pcp function returns an object of class Pcp
#' @author Jesper R. Gadin and Jason T. Serviss
#' @keywords projection
#' @examples
#'
#' #use demo data
#' data(pcpMatrix)
#' groups <- rownames(pcpMatrix)
#'
#' #run function
#' prj <- pcp(pcpMatrix, groups)
#'
#' #getData accessor
#' getData(prj)
#'
#' #getData accessor specific
#' getData(prj, "line")
#'
#' #plot the result (if dim >2, then plot in 3d)
#' plot(prj)
#'
#' #plot the result (if dim =2, then plot in 2d)
#' prj2 <- pcp(pcpMatrix[,1:2], groups)
#' plot(prj2)
#'
#' @exportMethod pcp
#'
#' @importFrom princurve principal.curve
#'

NULL

#' @rdname pcp
setGeneric("pcp", function(mat, ...
){ standardGeneric("pcp")})

#' @rdname pcp
setMethod("pcp", "matrix", function(mat, groups, df=NULL, group.color=NULL, ...
){

    #input checks
    .inputChecks(mat, groups)

    #check for column dimnames
    if(is.null(colnames(mat))){
        dimnames <- paste("dim",1:ncol(mat), sep="")
    } else{
        dimnames <- colnames(mat)
    }

    ##check for NA groups
    if(any(is.na(groups))) {
        groups[is.na(groups)] <- "NA"
    }

    #rownames(mat) <- groups

    ##run principal.curve and order and extract output
    if(is.null(df)) {df <- 5}
    prCurve <- .Curve(mat, groups, df)

    mat <- prCurve[[1]]
    line <- prCurve[[2]]
    vec.onedim <- prCurve[[3]]
    groups <- prCurve[[4]]
    index <- prCurve[[5]]

    #possible to remove in the future (not important to store group info twice)
    rownames(mat) <- groups
    rownames(line) <- groups

    #set color
    if(is.null(group.color)) group.color <- .setColors(groups)
    #if(!is.null(group.color)) group.color
    

    #return Pcp-object
    new("Pcp",
        groups=groups,
        points.orig=mat,
        line=line,
        points.onedim=vec.onedim,
        dimnames=dimnames,
        index=index,
        group.color=group.color
    )
})


#############################################################################
#                                                                           #
# helpers as units                                                          #
#                                                                           #
#############################################################################

##input checks
.inputChecks <- function(
    mat,
    groups
){
    checkList = list(FALSE, FALSE, FALSE)
    checkList[[1]] <- is.matrix(mat)
    checkList[[2]] <- is.character(groups)
    checkList[[3]] <- length(groups) == nrow(mat)
    checkList[[4]] <- ncol(mat) >= 3
    checkList[[5]] <- all(sapply(1:length(split(mat, groups)),
        function(x) length(split(mat, groups)[[x]])) > 4)
    if(any(unlist(checkList)) == FALSE) {
        stop(paste("There seems to be an error in your input, please refer to",
            "the vignette for details concerning input format", sep=""))
    }
}

#normalize points (intervall 0 to 1) and return matrix with normalized values
.normalizeMatrix <- function(
    mat
){
    (mat-min(mat))/(max(mat)-min(mat))
}

#run princurve, sort the output, and return
.Curve <- function(
    mat,
    groups,
    df,
    ...
){

    ##use princurve principal.curve to draw the principal curve
    prCurve <- principal.curve(mat, maxit=1000, df=df)

    #use index so the output are sorted based on the curve
    index <- prCurve$tag
    line <- prCurve$s[index, ]
    vec.onedim <- prCurve$lambda[index]

    #reorder original data
    groups <- groups[index]
    mat <- mat[index, ]


    return(list(mat, line, vec.onedim, groups, index))
}

#' Projection of points into one dimension.
#'
#' Project points onto the mean based line.
#'
#' Projection of the points onto a line between the mean of two groups.
#' Mlp is the abbreviation for 'mean line projection'. The
#' function accepts, at the moment, only two groups and two PCs at a
#' time.
#'
#'
#' An object containing results from a mean line projection reduction to one
#' dimension. 
#'
#' The group and the one dimensional points are the most important information
#' to carry out a classification using the classify() function. As a help
#' to illustrate the details of the dimension reduction, the information from
#' some critical steps are stored in the object. To visually explore these 
#' there is a dedicated plot method for Mlp objects, use plot().  
#'
#' @name mlp 
#' @rdname mlp
#' @aliases mlp mlp,matrix-method Mlp 
#' @param mat matrix with samples on rows, PCs in columns. 
#' Ordered PCs, with PC1 to the left.
#' @param groups vector in same order as rows in matrix
#' @param x matrix object for the function mlp otherwise it is a Mlp object
#' @param group.color user assigned group coloring scheme
#' @param n data to extract from Mlp (NULL gives all)
#' @param y default plot param, which should be set to NULL(default: NULL)
#' @param .Object internal object 
#' @param object Mlp object
#' @param points.orig multidimensional points describing the original data 
#' @param line multidimensional points describing a line 
#' @param points.onedim a vector of points
#' @param steps 1,2,3,4,5,6 or "all"
#' @param ... additional arguments to pass on
#' @return The mlp function returns an object of class Mlp
#' @author Jesper R. Gadin and Jason T. Serviss
#' @keywords projection
#' @examples
#'
#' #use demo data
#' data(mlpMatrix)
#' groups <- rownames(mlpMatrix)
#'
#' #run function
#' prj <- mlp(mlpMatrix, groups)
#'
#' #getData accessor
#' getData(prj)
#'
#' #getData accessor specific
#' getData(prj, "line")
#'
#' #plot result
#' plot(prj)
#'
#' @exportMethod mlp
#' @importFrom pracma crossn
#'
NULL

#' @rdname mlp
setGeneric("mlp", function(mat, ... 
    ){ standardGeneric("mlp")})

#' @rdname mlp
setMethod("mlp", "matrix", function(mat, groups, group.color=NULL, ...
    ){ 

    #check that there are only two groups
    if(!length(unique(groups))==2){
        stop("groups can only have two levels")
    }

    if(!(ncol(mat)==2)){
        stop("ncol(mat) has to be 2; accepts only two dimensions")
    }

    #check for column dimnames
    if(is.null(colnames(mat))){
        dimnames <- paste("dim",1:ncol(mat), sep="")
    } else{
        dimnames <- colnames(mat)
    }

    ##check for NA groups
    if(any(is.na(groups))) {
        groups[is.na(groups)] <- "NA"
    }

    rownames(mat) <- groups
    
    #find mean value for each group and dimension
    mat.means <- .groupAndDimensionMean(mat, groups)
    
    #what is the vector spanning the mean vector of 
    #group A and group B (limited to two groups)
    vec.regr <- .regressionVectorFromGroupMeans(mat.means)
    
    #move points so mean line passes through origin
    mat.origo <- .movePointsSoMeanLineGoesThroughOrigo2D(mat, mat.means)
    
    #project points on the vector from the group means 
    #(creating a line crossing origo)
    mat.proj <- 
        .projectMultidimensionalPointsOnMultidimensionalLine(
            mat.origo,
            vec.regr
        )

    #Calculate euclidean distance from origo to each point, which will 
    #reduce to one dimension 
    vec.onedim <- .reduceMultDimToOneDimAlongTheLine(mat.proj)

    #process color info
    if(is.null(group.color)) group.color <- .setColors(groups)

    #return Mlp-object
    new("Mlp",
        groups=groups,
        points.orig=mat,
        line=mat.proj, 
        points.onedim=vec.onedim,
        dimnames=dimnames,
        #Mlp specific
        points.origo=mat.origo,
        group.color=group.color
    )
}
)

#############################################################################
#                                                                           #
# helpers as units                                                          #
#                                                                           #
#############################################################################

#normalize points (intervall 0 to 1)
#returns matrix with normalized values
.normalizeMatrix <- function(
    mat
){
    
    (mat-min(mat))/(max(mat)-min(mat))
}

#calculate group and dimensional means
#returns matrix with groups on rows and dimensions on cols
.groupAndDimensionMean <- function(
    mat,
    groups
){
    
    matrix(
        unlist(
            apply(mat,2,function(x){
                lapply(split(x,groups), mean)
            })
        ),
        nrow=length(unique(groups)),
        dimnames=list(unique(groups), paste("dim",1:ncol(mat),sep=""))
    )
}

#calculate the line based on group means (now limited to solve only two groups)
#return the vector for the line from mean of group 2 to mean of group 1
.regressionVectorFromGroupMeans <- function(
    mat
){
    
    mat[1,] - mat[2,]
}

#project points on to a line passing through origin
.projectMultidimensionalPointsOnMultidimensionalLine <- function(
    mat,
    vec
){
    
    a <- mat%*%vec #matrix product operator
    b <- drop(vec%*%vec)
    return((a%*%vec) * (1/b))
}

#calculate the euclidean distance which will be the reduction to one dimension
#returns a vector with the euclidean distance
.reduceMultDimToOneDimAlongTheLine <- function(
    mat
){
    
    #because the move to origo through the y-intercept we can use the
    #x-axis to know which the direction the euclidean distance is supposed
    #to be calculated
    reduced <- rep(NA, nrow(mat))
    #group <- vector()
    if(any(mat[,1]<0)){
        reduced[mat[,1]<=0] <- -sqrt(apply(mat[mat[,1]<=0,,drop=FALSE]^2,1,sum))
        #group <- rownames(mat[mat[,1]<0,,drop=FALSE])
    }
    
    if(any(mat[,1]>0)){
        reduced[mat[,1]>0] <- sqrt(apply(mat[mat[,1]>0,,drop=FALSE]^2,1,sum))
        #group <- c(group,rownames(mat[mat[,1]>0,,drop=FALSE]))

    }
    #group names and order must be preserved here
    #names(reduced) <- group
    return(reduced)
}
#move the points so their mean line passes through origin
#returns a matrix, with changed y-values
.movePointsSoMeanLineGoesThroughOrigo2D <- function(
    points,
    meanMat
){
    
    ycept <- .axisIntercept2D(meanMat)[2]
    points[,2] <- points[,2] - ycept
    points
    
}
#calculates whre the x and y intercepts are.
#returns a vector of length two
.axisIntercept2D <- function(
    meanMat
){
    
    meanMat <- t(meanMat)
    yp2.mean <- meanMat[2,2]
    yp1.mean <- meanMat[2,1]
    xp2.mean <- meanMat[1,2]
    xp1.mean <- meanMat[1,1]

    #plot(c(xp1.mean,xp2.mean),c(yp1.mean, yp2.mean))

    ydiff <- yp2.mean-yp1.mean
    xdiff <- xp2.mean-xp1.mean

    if(xdiff == 0 & ydiff == 0){
        # the means ffrom the two groups are the same
        # impossible to draw a line
        # no mean difference at all between groups.
        stop("the means of the groups are the same, impossible to draw line")
    }

    if ( xdiff == 0 | ydiff == 0 ){
        slope <- 0
    } else {
        slope <- (ydiff/xdiff)
    }

    ycept <- yp2.mean-(slope*xp2.mean)
    xcept <- -ycept/slope

    ret <- c(xcept,ycept)
    names(ret) <- c("xcept","ycept")
    ret
}

#############################################################################
#                                                                           #
# helpers available for the future                                          #
#                                                                           #
#############################################################################

#calculates the vector perpendicular to a 2D line
#returns a matrix for the left and right side of the line vectors direction
.vectorMatrixOrthogonalToMeanPointLine2D <- function(
    origoLineVec,
    dist
){
    
    v <- origoLineVec
    h <- dist

    #left
    P3 = c(-v[2], v[1]) / sqrt(v[1]^2 + v[2]^2) * h
    #right
    P4 = c(-v[2], v[1]) / sqrt(v[1]^2 + v[2]^2) * -h
    
    matrix(c(P4,P3), dimnames=list(NULL,c("right","left")), ncol=2)
}

#calculates the distance from a line to a point (in this case origo)
#returns a scalar for the distance
.distMeanPointsToLineLineTo2D3D <- function(
    origoLineVec,
    means
){
    
    means <- t(means)
    #library(pracma)
    l <- origoLineVec
    lp <- means[,1]
    
    if(nrow(means)==2) {
        o <- c(0,0)
        lpo_vec <- o-lp
        mat <- matrix(c(lpo_vec,l),byrow=TRUE,ncol=2)
        cross <- lpo_vec[1]*l[2]-lpo_vec[2]*l[1]
    } else if(nrow(means)==3) {
        o <- c(0,0,0)
        lpo_vec <- o-lp
        mat <- matrix(c(lpo_vec,l),byrow=TRUE,ncol=3)
        cross <- crossn(mat)
    }
    
    dist <- sqrt(sum(cross^2))/sqrt(sum(l^2))
    dist
}
