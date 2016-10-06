#'@include All-classes.R
NULL


#' @rdname pcp
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scatterplot3d scatterplot3d
#'

setMethod("plot",c("Pcp", "missing"), function(x, y, steps="all",group.color=NULL, ...)
{

    if("all" %in% steps){
        #set mfrow 3:2
        oldparams <- par(mfrow=c(3,2))
        steps <- c(1,2,3,4,5,6)
    }
    
    groups <- getData(x,"groups")

    dim <- length(getData(x,"dimnames"))

    #check if colnames are present, then use them as axisnames
    #if(colnames())
    #axisnames <- col

	#set color
	if(is.null(group.color)) CM <- getData(x,"group.color")
	if(!is.null(group.color)) CM <- group.color
	#if(!is.null(group.color)) CM <- group.color

    if(dim==2){
        .plotPcp2D(x, steps, CM)
    }else{

        if(1 %in% steps){
            #original plot
            plot <- .step1(x, groups, CM)
        }
        
        if(2 %in% steps){
            #plot with normalized data and line
            .step2(x, groups, CM)
        }
        
        if(3 %in% steps){
            #show the change from old points to new points on the line
            .step3(x, groups, CM)
        }
        
        if(4 %in% steps){
            #show the projected points in 3 dimensions
            .step4(x, groups, CM)
        }
        
        if(5 %in% steps){
            ##show projection into one dimension
            .step5(x, groups, CM)
        }
        
        if(6 %in% steps){
            #show the move to origin.
            .step6(x, groups, CM)
        }
        
        #reset mfrow
        if("all" %in% steps){
            par(oldparams)
        }
    }
}
)

###############################################################################
#
# helpers as units
#
###############################################################################

#set color scheme
.setColors <- function(groups) {
    
    colors <- colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(groups)))
    CM <- col2rgb(colors, alpha = FALSE)
    colnames(CM) <- unique(groups)
    return(CM)
}

#original plot
.step1 <- function(x, groups, CM) {
    
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]
    sz <- splt[[3]]

    plot <- scatterplot3d(sx[[1]], sy[[1]], sz[[1]],
        color=rgb(
            CM["red",   names(sx[1])],
            CM["green", names(sx[1])],
            CM["blue",  names(sx[1])],
            200,
            maxColorValue=255
        ),
        pch=16,
        xlim=c(min(unlist(sx)), max(unlist(sx))),
        ylim=c(min(unlist(sy)), max(unlist(sy))),
        zlim=c(min(unlist(sz)), max(unlist(sz))),
        xlab=getData(x, "dimnames")[1],
        ylab=getData(x, "dimnames")[2],
        zlab=getData(x, "dimnames")[3],
        box=FALSE,
        y.margin.add = 0.25,
        cex.lab = 0.5,
        cex.axis = 0.5,
        cex.symbols = 1
    )

    invisible(
        sapply(2:length(unique(groups)), function(x)
            plot$points3d(
                sx[[x]], sy[[x]], sz[[x]],
                col=rgb(
                    CM["red",   names(sx[x])],
                    CM["green", names(sx[x])],
                    CM["blue",  names(sx[x])],
                    200,
                    maxColorValue=255
                ),
                pch=16
            )
        )
    )
    return(plot)
}

#plot with normalized data and line
.step2 <- function(x, groups, CM) {
    
    plot <- .step1(x, groups, CM)

    #draw line
    l <- getData(x,"line")
    plot$points3d(l[,1], l[,2], l[,3], type="l", pch=16, cex=0.2)

    return(plot)
}

#show the change from old points to new points on the line by dashing
.step3 <- function(x, groups, CM) {
    
    p <- getData(x, "points.orig")
    plot <- .step2(x, groups, CM)

    ##draw dashes
    p2 <- getData(x,"line")
    split <- apply(p, 2, function(x) split(x, rownames(p)))
    split2 <- apply(p2, 2, function(x) split(x, rownames(p2)))

    invisible(
        sapply(1:length(unlist(split2[[1]])), function(xx)
            plot$points3d(
                c(unlist(split[[1]])[[xx]], unlist(split2[[1]])[[xx]]),
                c(unlist(split[[2]])[[xx]], unlist(split2[[2]])[[xx]]),
                c(unlist(split[[3]])[[xx]], unlist(split2[[3]])[[xx]]),
                col=rgb(190,190,190,250, maxColorValue=255),
                type = "l"
            )
        )
    )
}

#show the projected points in 3 dimensions
.step4 <- function(x, groups, CM) {
    
    p <- getData(x,"line")
    splt <- apply(p, 2, function(x) split(x, rownames(p)))
    sx <- splt[[1]]
    sy <- splt[[2]]
    sz <- splt[[3]]
    
    od <- getData(x,"points.onedim")
    names(od) <- groups
    s <- split(od, names(od))
    
    plot <- scatterplot3d(sx[[1]], sy[[1]], sz[[1]],
        color=rgb(
            CM["red",   names(sx[1])],
            CM["green", names(sx[1])],
            CM["blue",  names(sx[1])],
            200,
            maxColorValue=255
        ),
        pch=16,
        xlim=c(
            min(c(unlist(s),unlist(sx))),
            max(c(unlist(s),unlist(sx)))
        ),
        ylim=c(
            min(unlist(sy),-0.2),
            max(unlist(sy))
        ),
        zlim=c(
            min(unlist(sz),-0.2),
            max(unlist(sz))
        ),
        xlab=getData(x, "dimnames")[1],
        ylab=getData(x, "dimnames")[2],
        zlab=getData(x, "dimnames")[3],
        box=FALSE,
        y.margin.add = 0.25,
        cex.lab = 0.5,
        cex.axis = 0.5,
        cex.symbols = 1
    )

    invisible(
        sapply(2:length(unique(groups)), function(j)
            plot$points3d(
                sx[[j]], sy[[j]], sz[[j]],
                col=rgb(
                    CM["red",   names(sx[j])],
                    CM["green", names(sx[j])],
                    CM["blue",  names(sx[j])],
                    200,
                    maxColorValue=255
                ),
                pch=16
            )
        )
    )
    return(plot)
}

##show projection into one dimension
.step5 <- function(x, groups, CM) {
    
    plot <- .step4(x, groups, CM)
    
    od <- getData(x,"points.onedim")
    names(od) <- groups
    s <- split(od, names(od))
    
    #plot points in one dimension
    invisible(
        sapply(1:length(s), function(yz)
            sapply(1:length(s[[yz]]), function(zy)
                plot$points3d(
                    s[[yz]][[zy]], 0, 0,
                    col=rgb(
                        CM["red",names(s[yz])],
                        CM["green",names(s[yz])],
                        CM["blue",names(s[yz])],
                        150,
                        maxColorValue=255
                    ),
                    pch=16,
                    #type = "h"
                )
            )
        )
    )

    ##draw dashes
    
    p <- getData(x,"line")
    splt <- apply(p, 2, function(x) split(x, rownames(p)))
    sx <- splt[[1]]
    sy <- splt[[2]]
    sz <- splt[[3]]
    
    invisible(
        sapply(1:length(unlist(sx)), function(xx)
            plot$points3d(
                c(unlist(sx)[[xx]], unlist(s)[[xx]]),
                c(unlist(sy)[[xx]], 0),
                c(unlist(sz)[[xx]], 0),
                col=rgb(190,190,190,250, maxColorValue=255),
                type = "l"
            )
        )
    )
}

#show the move to origin.
.step6 <- function(x, groups, CM) {
    
    od <- getData(x,"points.onedim")
    names(od) <- groups
    s <- split(od, names(od))

    #plot points in one dimension
    plot <- scatterplot3d(s[[1]][[1]], 0, 0,
        color=rgb(
            CM["red",   names(s[1])],
            CM["green", names(s[1])],
            CM["blue",  names(s[1])],
            0,
            maxColorValue=255
        ),
        pch=16,
        xlim=c(min(od), max(od)),
        ylim=c(-0.2, max(od)),
        zlim=c(-0.2, max(od)),
        xlab=getData(x, "dimnames")[1],
        ylab=getData(x, "dimnames")[2],
        zlab=getData(x, "dimnames")[3],
        y.ticklabs=c(rep("", length(x))),
        z.ticklabs=c(rep("", length(x))),
        box=FALSE,
        y.margin.add = 0.25,
        cex.lab = 0.5,
        cex.axis = 0.5,
        cex.symbols = 1,
        type="h"
    )

    invisible(
        sapply(1:length(s), function(yz)
            sapply(1:length(s[[yz]]), function(zy)
                plot$points3d(
                    s[[yz]][[zy]], 0, 0,
                    col=rgb(
                        CM["red",   names(s[yz])],
                        CM["green", names(s[yz])],
                        CM["blue",  names(s[yz])],
                        150,
                        maxColorValue=255
                    ),
                    pch=16,
                    type = "h"
                )
            )
        )
    )
}


#' @rdname classify
#' @export
setMethod("plot",c("ClassifiedPoints", "missing"), function(x, y, group.color=NULL, ...)
{
    # plot the points and create lines as separators and show above each line
    # the score obtained using that line as classifier.

    p <- getData(x,"scores.points")
    s <- getData(x,"scores")
    
    ##import colors used in projection plot
 #   colors <- 
 #       colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(names(p))))
 #                   
 #   CM <- col2rgb(colors, alpha = FALSE)
 #   colnames(CM) <- unique(names(p))

	#set color
	if(is.null(group.color)) CM <- getData(x,"group.color")
	if(!is.null(group.color)) CM <- group.color
    
    ##setup plot window
    steps <- length(s)
    setup <- n2mfrow(steps)
    oldparams <- par(mfrow=setup)
    
    ##plot
    for( ii in 1:steps ){
        score <- s[ii]
        name <- names(score)
        yrange <- range(score)
        yrange[1]<-0 ##otherwise the lowest score cannot be seen

        grp1 <- strsplit(name, " vs ")[[1]][[1]]
        grp2 <- strsplit(name, " vs ")[[1]][[2]]

        plot(
            0,
            xlim=c(min(p),max(p)),
            ylim=yrange,
            bty='n',
            pch='',
            ylab='',
            xlab='',
            main=name
        )

        y1 <- rep(yrange[1], length(p[names(p)==grp1]))
        y2 <- rep(yrange[1], length(p[names(p)==grp2]))

        lines(p[names(p)==grp1],y1,
            col=rgb(
                CM["red", grp1],
                CM["green", grp1],
                CM["blue", grp1],
                150,
                maxColorValue=255
            ),
            pch=16,
            type="p"
        )
    
        lines(
            p[names(p)==grp2],
            y2,
            col=rgb(
                CM["red", grp2],
                CM["green", grp2],
                CM["blue", grp2],
                150,
                maxColorValue=255
            ),
            pch=16,
            type="p"
        )

        # find points where to draw lines.
        # these points are in between the line points.
        # to use a vectorized solution we need to include a "temporary variable"
        p0 <- subset(p, names(p) == grp1 | names(p) == grp2)
        p1 <- c(0,p0)
        p2 <- c(p0,0)
        p.middle.points <- ((p1+p2)/2)[-c(1,length(p1))]

        #add lines up to scores value (which is on the y-axis)
        for (i in 1:length(p.middle.points)){
            lines(
                c(p.middle.points[i],p.middle.points[i]),
                c(yrange[1], score[[1]][i]),
                col="gray",
                type="l"
            )
        }

    }
}
)

#' @rdname permute
#' @export
setMethod("plot",c("PermutationResults", "missing"), function(x, y, ...)
{
    scores.vec <- getData(x,"scores.vec")
    score.reals <- getData(x,"scores.real")
    
    ##setup plot window
    steps <- length(scores.vec)
    setup <- n2mfrow(steps)
    oldparams <- par(mfrow=setup)

    #plot histogram of score distribution
    for( yy in 1:steps) {
        score <- score.reals[yy]
        name <- names(score)
        range <- range(score, scores.vec[yy])
        hist(c(scores.vec[[yy]]), xlim=range, main=name, xlab="scores")
        #add line for the "real" data score
        abline(v=score.reals[[yy]], col="red")
    }
}
)

#' @rdname mlp
#' @importFrom RColorBrewer brewer.pal
#' @export

setMethod("plot",c("Mlp", "missing"), function(x, y, steps="all", ...)
{

    if("all" %in% steps){
        #set mfrow 3:2
        oldparams <- par(mfrow=c(3,2))
        steps <- c(1,2,3,4,5,6)
    }
    
    groups <- getData(x, "groups")

    #check if colnames are present, then use them as axisnames
    #if(colnames())
    #axisnames <- col
    
    ##set up color scheme for all groups
    CM <- .setColors(groups)
    
    if(1 %in% steps){
        #original plot
        plot <- .MlpPlotStep1(x, groups, CM)
    }
    
    if(2 %in% steps){
        #plot with normalized data and line
        .MlpPlotStep2(x, groups, CM)
    }
    
    if(3 %in% steps){
        #show the change from old points to new points on the line by dashing
        .MlpPlotStep3(x, groups, CM)
    }
    
    if(4 %in% steps){
        #show the projected points in 3 dimensions
        .MlpPlotStep4(x, groups, CM)
    }
    
    if(5 %in% steps){
        ##show projection into one dimension
        .MlpPlotStep5(x, groups, CM)
    }
    
    if(6 %in% steps){
        #show the move to origin.
        .MlpPlotStep6(x, groups, CM)
    }
    
    #reset mfrow
    if("all" %in% steps){
        par(oldparams)
    }
}
)

###############################################################################
#
# helpers as units
#
###############################################################################

#set color scheme
.setColors <- function(groups) {
    
    colors <- colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(groups)))
    CM <- col2rgb(colors, alpha = FALSE)
    colnames(CM) <- unique(groups)
    return(CM)
}

#original plot
.MlpPlotStep1 <- function(x, groups, CM) {
    
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    col1=rgb(
        CM["red",1],
        CM["green",1],
        CM["blue",1],
        200,
        maxColorValue=255
    )
    
    col2=rgb(
        CM["red",2],
        CM["green",2],
        CM["blue",2],
        200,
        maxColorValue=255
    )

    plot(
        0,
        xlim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))),
        ylim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))), 
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    lines(sx[[1]], sy[[1]], col=col1, pch=16, type="p")
    lines(sx[[2]], sy[[2]], col=col2, pch=16, type="p")
}

#show how every point connects to the mean points
.MlpPlotStep2 <- function(x, groups, CM) {
    
    #same points as in step1
    .MlpPlotStep1(x, groups, CM)
    
    #add mean Values
    p <- getData(x,"points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    y1.mean <- mean(sy[[1]])
    y2.mean <- mean(sy[[2]])
    x1.mean <- mean(sx[[1]])
    x2.mean <- mean(sx[[2]])

    lines(x1.mean, y1.mean, col="black", pch=16, type="p")
    lines(x2.mean, y2.mean, col="black", pch=16, type="p")
    
    #draw the lines grp1
    for(i in 1:length(sy[[1]])){
        lines(
            c(x1.mean,sx[[1]][i]),
            c(y1.mean,sy[[1]][i]),
            col="grey",
            type="l",
            lty=2
        )
    }
    
    #draw the lines grp2
    for(i in 1:length(sy[[2]])){
        lines(
            c(x2.mean,sx[[2]][i]),
            c(y2.mean,sy[[2]][i]),
            col="grey",
            type="l",
            lty=2
        )
    }
}

#show move to point where the mean line goes through origo
.MlpPlotStep3 <- function(x, groups, CM) {
    
    #the first set of points
    p <- getData(x,"points.orig")
    po <- getData(x,"points.origo")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim=c(min(p,po),max(p,po)),
        ylim=c(min(p,po),max(p,po)),
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    #draw arrows
    #suppress warnings (usually happens when length of arrow is too short)
    #and not printing the arrow is not a big thing.
    for(i in 1:nrow(p)) {
        suppressWarnings(
            arrows(
                p[i,1],
                p[i,2],
                po[i,1],
                po[i,2],
                col="black",
                angle=10,
                length=0.1,
                lwd=1)
            )
    }
    
    #the second set of points 
    splt <- apply(po, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    col1=rgb(
        CM["red",1],
        CM["green",1],
        CM["blue",1],
        200,
        maxColorValue=255
    )
    
    col2=rgb(
        CM["red",2],
        CM["green",2],
        CM["blue",2],
        200,
        maxColorValue=255
    )

    lines(sx[[1]], sy[[1]], col=col1, pch=16, type="p")
    lines(sx[[2]], sy[[2]], col=col2, pch=16, type="p")

    #draw the mean line
    l <- getData(x, "line")
    lines(l[,1], l[,2], type="l", pch=16, cex=0.2)
}


#show the move to one dimension (on the line)
.MlpPlotStep4 <- function(x, groups, CM){
    
    po <- getData(x, "points.origo")
    splt <- apply(po, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim=c(min(po),max(po)),
        ylim=c(min(po),max(po)),
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    #the s
    splt <- apply(po, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    col1=rgb(
        CM["red",1],
        CM["green",1],
        CM["blue",1],
        200,
        maxColorValue=255
    )
    
    col2=rgb(
        CM["red",2],
        CM["green",2],
        CM["blue",2],
        200,
        maxColorValue=255
    )

    lines(sx[[1]], sy[[1]], col=col1, pch=16, type="p")
    lines(sx[[2]], sy[[2]], col=col2, pch=16, type="p")

    #draw the mean line
    l <- getData(x, "line")
    lines(l[,1], l[,2], type="l", pch=16, cex=0.2)

    ##draw lines
    po <- getData(x, "points.origo")
    pd <- getData(x, "line")

    for(i in 1:nrow(po)){
        lines(
            c(pd[i,1],po[i,1]),
            c(pd[i,2],po[i,2]),
            col="grey",
            type="l"
        )
    }
}

#show the result of the projection onto the line
.MlpPlotStep5 <- function(x, groups, CM) {
    
    p <- getData(x,"line")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    col1=rgb(
        CM["red",1],
        CM["green",1],
        CM["blue",1],
        200,
        maxColorValue=255
    )
    
    col2=rgb(
        CM["red",2],
        CM["green",2],
        CM["blue",2],
        200,
        maxColorValue=255
    )

    plot(
        0,
        xlim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))),
        ylim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))), 
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    lines(sx[[1]], sy[[1]], col=col1, pch=16, type="p")
    lines(sx[[2]], sy[[2]], col=col2, pch=16, type="p")
}

#show how the points behave when there is only one dimension in play
.MlpPlotStep6 <- function(x, groups, CM) {
    
    p <- getData(x,"points.onedim")
    sx <- split(p, groups)

    col1=rgb(
        CM["red",1],
        CM["green",1],
        CM["blue",1],
        200,
        maxColorValue=255
    )
    
    col2=rgb(
        CM["red",2],
        CM["green",2],
        CM["blue",2],
        200,
        maxColorValue=255
    )

    plot(
        0,
        xlim=c(min(unlist(sx)),max(unlist(sx))),
        ylim=c(-0.2,1),
        bty='n',
        pch='',
        ylab='',
        xlab='',
        yaxt='n'
    )
    
    lines(sx[[1]], rep(0,length(sx[[1]])), col=col1, pch=16, type="p")
    lines(sx[[2]], rep(0,length(sx[[2]])), col=col2, pch=16, type="p")
}

##plot Pcp projection with only 2 dims
.plotPcp2D <- function(x, steps, CM){
    
    if("all" %in% steps) {
        #set mfrow 3:2
        oldparams <- par(mfrow=c(3,2))
        steps <- c(1,2,3,4,5,6)
    }
    
    groups <- getData(x, "groups")

    #check if colnames are present, then use them as axisnames
    #if(colnames())
    #axisnames <- col
    
    ##set up color scheme for all groups
    #CM <- .setColors(groups)
    
    if(1 %in% steps){
        #original plot
        plot <- .Pcp2DPlotStep1(x, groups, CM)
    }
    
    if(2 %in% steps){
        #plot.. line
        .Pcp2DPlotStep2(x, groups, CM)
    }
    
    if(3 %in% steps){
        #show the change from old points to new points on the line by dashing
        .Pcp2DPlotStep3(x, groups, CM)
    }
    
    if(4 %in% steps){
        #show the projected points in 3 dimensions
        .Pcp2DPlotStep4(x, groups, CM)
    }
    
    if(5 %in% steps){
        ##show projection into one dimension
        .Pcp2DPlotStep5(x, groups, CM)
    }
    
    if(6 %in% steps){
        #show the move to origin.
        .Pcp2DPlotStep6(x, groups, CM)
    }
    
    #reset mfrow
    if("all" %in% steps){
        par(oldparams)
    }
}

###############################################################################
#
# helpers as units
#
###############################################################################
.Pcp2DPlotStep1 <- function(x, groups, CM) {
    
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]


    plot(
        0,
        xlim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))),
        ylim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))), 
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    for(i in 1:length(sx)) {
        lines(sx[[i]], sy[[i]],
            col=rgb(
                CM["red",i],
                CM["green",i],
                CM["blue",i],
                200,
                maxColorValue=255
            ),
            pch=16, type="p")
    }
}

#add line to points
.Pcp2DPlotStep2 <- function(x, groups, CM) {
    
    #same points as in step1
    .Pcp2DPlotStep1(x, groups, CM)
    
    #add mean Values
    l <- getData(x,"line")

    #draw line
    for(i in 1:(nrow(l)-1)) {
        lines(c(l[i,1],l[i+1,1]), c(l[i,2],l[i+1,2]))
    }
}

.Pcp2DPlotStep3 <- function(x, groups, CM) {
    
    #same points as in step1
    .Pcp2DPlotStep2(x, groups, CM)
    
    #add mean Values
    p <- getData(x,"points.orig")
    l <- getData(x,"line")

    #draw line from point to line point
    for(i in 1:(nrow(l)-1)) {
        lines(c(l[i,1],p[i,1]), c(l[i,2],p[i,2]))
    }
}

#show the result of the projection onto the line
.Pcp2DPlotStep4 <- function(x, groups, CM) {
    
    p <- getData(x,"line")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim=c(min(unlist(sx),unlist(sy)), max(unlist(sx),unlist(sy))),
        ylim=c(min(unlist(sx),unlist(sy)), max(unlist(sx),unlist(sy))),
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    for(i in 1:length(sx)){
        lines(sx[[i]], sy[[i]],
            col=rgb(
                CM["red",i],
                CM["green",i],
                CM["blue",i],
                200,
                maxColorValue=255
            ),
            pch=16, type="p")
    }
}


#show the result of the projection onto the line
.Pcp2DPlotStep5 <- function(x, groups, CM) {
    p <- getData(x,"line")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]
    
    p2 <- getData(x,"points.onedim")
    p2 <- matrix(c(p2,rep(0,length(p2))),ncol=2, dimnames=list(groups,NULL))
    splt2 <- apply(p2, 2, function(x) split(x, groups))
    sx2 <- splt2[[1]]
    sy2 <- splt2[[2]]

    plot(
        0,
        xlim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))),
        ylim=c(min(unlist(sx),unlist(sy)),max(unlist(sx),unlist(sy))), 
        bty='n',
        pch='',
        ylab='',
        xlab=''
    )
    
    #all points
    for(i in 1:length(sx)){
        lines(c(sx[[i]], sx2[[i]]), c(sy[[i]], sy2[[i]]),
            col=rgb(
                CM["red",i],
                CM["green",i],
                CM["blue",i],
                200,
                maxColorValue=255
            ),
            pch=16,
            type="p"
        )
    }
    
    for(i in 1:nrow(p)){
        lines(c(p[i,1], p2[i,1]),
            c(p[i,2], p2[i,2]),
            col="grey",
            type="l"
        )
    }
}

#show how the points behave when there is only one dimension in play
.Pcp2DPlotStep6 <- function(x, groups, CM) {
    p <- getData(x,"points.onedim")
    p <- matrix(c(p,rep(0,length(p))),ncol=2, dimnames=list(groups,NULL))
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim=c(min(unlist(sx)),max(unlist(sx))),
        ylim=c(-0.2,1),
        bty='n',
        pch='',
        ylab='',
        xlab='',
        yaxt='n'
    )
    
    for(i in 1:length(sx)){
        lines(sx[[i]], sy[[i]],
            col=rgb(
                CM["red",i],
                CM["green",i],
                CM["blue",i],
                200,
                maxColorValue=255
            ),
            pch=16,
            type="p"
        )
    }
}
