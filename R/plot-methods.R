#'@include All-classes.R
NULL


#' @rdname pcp
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom graphics legend
#'

setMethod(
    "plot",
    c(
        "Pcp",
        "missing"
    ),
    function(
        x,
        y,
        steps = "all",
        class.color = NULL,
        ...
    )
{
    group.color <- class.color
    ellipsis <- list(...)
    dim <- length(getData(x, "dimnames"))

    #set color
    if(is.null(group.color)) CM <- getData(x, "class.color")
    if(!is.null(group.color)) CM <- group.color

    if(dim == 2) {
        .plotPcp2D(x, steps, CM, ellipsis)
    } else {

        groups <- getData(x, "classes")
        oldparams <- par(no.readonly = TRUE)

        if("all" %in% steps){
            par(mfrow = c(3, 2), mar = c(5, 4, 5, 2) + 0.1)
            steps <- c(1, 2, 3, 4, 5, 6)
        }


        if(1 %in% steps){
            #original plot
            plot <- .step1(x, groups, CM, ellipsis)
            if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
        }
        
        if(2 %in% steps){
            #plot with normalized data and line
            .step2(x, groups, CM, ellipsis)
            if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
        }
        
        if(3 %in% steps){
            #show the change from old points to new points on the line
            .step3(x, groups, CM, ellipsis)
            if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
        }
        
        if(4 %in% steps){
            #show the projected points in 3 dimensions
            .step4(x, groups, CM, ellipsis)
            if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
        }
        
        if(5 %in% steps){
            ##show projection into one dimension
            .step5(x, groups, CM, ellipsis)
            if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
        }
        
        if(6 %in% steps){
            #show the move to origin.
            .step6(x, groups, CM, ellipsis)
            if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
        }

        #reset mfrow
        if(length(steps) == 6){
            .legend(CM, oldparams, ellipsis)
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

#add legend
.legend <- function(
    CM,
    oldparams,
    ellipsis,
    ...
){
    
    if(!is.null(oldparams)) {
        par(oldparams)
        par(usr = c(0,1,0,100), xpd = NA)
        inset <- -0.15
    } else {
        par(usr = c(0,1,0,100), xpd = NA)
        inset <- -0.05
    }
    
    
    alpha <- 1
    cex.legend <- 1
    pt.cex.legend <- 2
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex.legend" %in% names(ellipsis)) {
        cex.legend <- ellipsis[['cex.legend']]
    }
    if("pt.cex.legend" %in% names(ellipsis)) {
        pt.cex.legend <- ellipsis[['pt.cex.legend']]
    }

    if(ncol(CM) > 5) {
        horiz = FALSE
        ncol = 5
    } else {
        horiz = TRUE
        ncol = 1
    }
    
    legend(
        "top",
        legend = sort(colnames(CM)),
        horiz = horiz,
        col = rgb(
            t(CM[, sort(colnames(CM))]),
            maxColorValue = 255,
            alpha = alpha *255
        ),
        bty = 'n',
        border = 'white',
        pch = 16,
        pt.cex = pt.cex.legend,
        inset = inset,
        cex = cex.legend,
        ncol = ncol
    )
    par(oldparams)
}

#original plot
.step1 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]
    sz <- splt[[3]]

    cex.lab <- 0.5
    cex.axis <- 0.5
    cex.symbols <- 1
    alpha <- 0.75

    if("cex.lab" %in% names(ellipsis)) {
        cex.lab <- ellipsis[['cex.lab']]
    }
    if("cex.axis" %in% names(ellipsis)) {
        cex.axis <- ellipsis[['cex.axis']]
    }
    if("cex.symbols" %in% names(ellipsis)) {
        cex.symbols <- ellipsis[['cex.symbols']]
    }
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }

    plot <- scatterplot3d(sx[[1]], sy[[1]], sz[[1]],
        color = rgb(
            CM["red",   names(sx[1])],
            CM["green", names(sx[1])],
            CM["blue",  names(sx[1])],
            alpha * 255,
            maxColorValue = 255
        ),
        pch = 16,
        xlim = c(min(unlist(sx)), max(unlist(sx))),
        ylim = c(min(unlist(sy)), max(unlist(sy))),
        zlim = c(min(unlist(sz)), max(unlist(sz))),
        xlab = getData(x, "dimnames")[1],
        ylab = getData(x, "dimnames")[2],
        zlab = getData(x, "dimnames")[3],
        box = FALSE,
        mar = rep(3, 4)+0.1, #bottom, left, top, right
        y.margin.add = 0.1,
        cex.lab = cex.lab,
        cex.axis = cex.axis,
        cex.symbols = cex.symbols
    )

    invisible(
        sapply(2:length(unique(groups)), function(x)
            plot$points3d(
                sx[[x]], sy[[x]], sz[[x]],
                col = rgb(
                    CM["red",   names(sx[x])],
                    CM["green", names(sx[x])],
                    CM["blue",  names(sx[x])],
                    alpha * 255,
                    maxColorValue = 255
                ),
                pch = 16,
                cex = cex.symbols
            )
        )
    )
    return(plot)
}

#plot with normalized data and line
.step2 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    plot <- .step1(x, groups, CM, ellipsis, ...)

    #draw line
    l <- getData(x, "line")
    plot$points3d(l[,1], l[,2], l[,3], type = "l", pch = 16, cex = 0.2)

    return(plot)
}

#show the change from old points to new points on the line by dashing
.step3 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "points.orig")
    plot <- .step2(x, groups, CM, ellipsis, ...)

    ##draw dashes
    p2 <- getData(x, "line")
    split <- apply(p, 2, function(x) split(x, rownames(p)))
    split2 <- apply(p2, 2, function(x) split(x, rownames(p2)))

    invisible(
        sapply(1:length(unlist(split2[[1]])), function(xx)
            plot$points3d(
                c(unlist(split[[1]])[[xx]], unlist(split2[[1]])[[xx]]),
                c(unlist(split[[2]])[[xx]], unlist(split2[[2]])[[xx]]),
                c(unlist(split[[3]])[[xx]], unlist(split2[[3]])[[xx]]),
                col=rgb(190, 190, 190, 250, maxColorValue = 255),
                type = "l"
            )
        )
    )
}

#show the projected points in 3 dimensions
.step4 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "line")
    splt <- apply(p, 2, function(x) split(x, rownames(p)))
    sx <- splt[[1]]
    sy <- splt[[2]]
    sz <- splt[[3]]
    
    od <- getData(x, "points.onedim")
    names(od) <- groups
    s <- split(od, names(od))

    cex.lab <- 0.5
    cex.axis <- 0.5
    cex.symbols <- 1
    alpha <- 0.75
    
    if("cex.lab" %in% names(ellipsis)) {
        cex.lab <- ellipsis[['cex.lab']]
    }
    if("cex.axis" %in% names(ellipsis)) {
        cex.axis <- ellipsis[['cex.axis']]
    }
    if("cex.symbols" %in% names(ellipsis)) {
        cex.symbols <- ellipsis[['cex.symbols']]
    }
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }

    plot <- scatterplot3d(sx[[1]], sy[[1]], sz[[1]],
        color = rgb(
            CM["red",   names(sx[1])],
            CM["green", names(sx[1])],
            CM["blue",  names(sx[1])],
            alpha * 255,
            maxColorValue = 255
        ),
        pch = 16,
        xlim = c(
            min(c(unlist(s),unlist(sx))),
            max(c(unlist(s),unlist(sx)))
        ),
        ylim = c(
            min(unlist(sy),-0.2),
            max(unlist(sy))
        ),
        zlim = c(
            min(unlist(sz),-0.2),
            max(unlist(sz))
        ),
        xlab = getData(x, "dimnames")[1],
        ylab = getData(x, "dimnames")[2],
        zlab = getData(x, "dimnames")[3],
        box = FALSE,
        mar = rep(3, 4)+0.1, #bottom, left, top, right
        y.margin.add = 0.25,
        cex.lab = cex.lab,
        cex.axis = cex.axis,
        cex.symbols = cex.symbols
    )

    invisible(
        sapply(2:length(unique(groups)), function(j)
            plot$points3d(
                sx[[j]], sy[[j]], sz[[j]],
                col = rgb(
                    CM["red",   names(sx[j])],
                    CM["green", names(sx[j])],
                    CM["blue",  names(sx[j])],
                    alpha * 255,
                    maxColorValue = 255
                ),
                pch = 16,
                cex = cex.symbols
            )
        )
    )
    return(plot)
}

##show projection into one dimension
.step5 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    plot <- .step4(x, groups, CM, ellipsis, ...)
    
    od <- getData(x, "points.onedim")
    names(od) <- groups
    s <- split(od, names(od))
    
    alpha <- 0.75
    cex.symbols <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex.symbols" %in% names(ellipsis)) {
        cex.symbols <- ellipsis[['cex.symbols']]
    }
    #plot points in one dimension
    invisible(
        sapply(1:length(s), function(yz)
            sapply(1:length(s[[yz]]), function(zy)
                plot$points3d(
                    s[[yz]][[zy]], 0, 0,
                    col = rgb(
                        CM["red", names(s[yz])],
                        CM["green", names(s[yz])],
                        CM["blue", names(s[yz])],
                        alpha * 255,
                        maxColorValue = 255
                    ),
                    pch = 16,
                    cex = cex.symbols
                )
            )
        )
    )

    ##draw dashes
    
    p <- getData(x, "line")
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
                col = rgb(190, 190, 190, 250, maxColorValue = 255),
                type = "l"
            )
        )
    )
}

#show the move to origin.
.step6 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    od <- getData(x, "points.onedim")
    names(od) <- groups
    s <- split(od, names(od))

    cex.lab <- 0.5
    cex.axis <- 0.5
    cex.symbols <- 1
    alpha <- 0.75
    
    if("cex.lab" %in% names(ellipsis)) {
        cex.lab <- ellipsis[['cex.lab']]
    }
    if("cex.axis" %in% names(ellipsis)) {
        cex.axis <- ellipsis[['cex.axis']]
    }
    if("cex.symbols" %in% names(ellipsis)) {
        cex.symbols <- ellipsis[['cex.symbols']]
    }
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }

    #plot points in one dimension
    plot <- scatterplot3d(s[[1]][[1]], 0, 0,
        color = rgb(
            CM["red",   names(s[1])],
            CM["green", names(s[1])],
            CM["blue",  names(s[1])],
            0,
            maxColorValue = 255
        ),
        pch=16,
        xlim = c(min(od), max(od)),
        ylim = c(-0.2, max(od)),
        zlim = c(-0.2, max(od)),
        xlab = getData(x, "dimnames")[1],
        ylab = getData(x, "dimnames")[2],
        zlab = getData(x, "dimnames")[3],
        y.ticklabs = c(rep("", length(x))),
        z.ticklabs = c(rep("", length(x))),
        box = FALSE,
        mar = rep(3, 4) + 0.1, #bottom, left, top, right
        y.margin.add = 0.25,
        cex.lab = cex.lab,
        cex.axis = cex.axis,
        cex.symbols = cex.symbols,
        type = "h"
    )

    invisible(
        sapply(1:length(s), function(yz)
            sapply(1:length(s[[yz]]), function(zy)
                plot$points3d(
                    s[[yz]][[zy]], 0, 0,
                    col = rgb(
                        CM["red",   names(s[yz])],
                        CM["green", names(s[yz])],
                        CM["blue",  names(s[yz])],
                        alpha * 255,
                        maxColorValue = 255
                    ),
                    pch = 16,
                    type = "h",
                    cex = cex.symbols
                )
            )
        )
    )
}


#' @rdname classify
#' @param comparison Specify a comparison i.e.
#' ("grp1 vs grp2") and plot only that comparison.
#' @export
#' @importFrom graphics title

setMethod(
    "plot",
    c(
        "ClassifiedPoints",
        "missing"
    ),
    function(
        x,
        y,
        comparison = "all",
        class.color = NULL,
        ...
    )
{
    group.color <- class.color
    
    # plot the points and create lines as separators and show above each line
    # the score obtained using that line as classifier.

    p <- getData(x, "scores.points")
    s <- getData(x, "scores")
    
    #subset comparison
    if(comparison != "all") {
        if(!is.null(s[[comparison]])) {
            s <- s[comparison]
        } else {
            stop(
                "The specified comparison could not be found"
            )
        }
    }
    
    #set color
    if(is.null(group.color)) CM <- getData(x, "class.color")
    if(!is.null(group.color)) CM <- group.color
    
    ##setup plot window
    steps <- length(names(s))
    setup <- n2mfrow(steps)
    oldparams <- par(mfrow = setup)
    
    cex.points <- 1
    lwd <- 1
    alpha <- 0.75
    cex.lab <- 1
    cex.axis <- 1
    cex.main <- 1
    
    ellipsis <- list(...)
    if("cex.points" %in% names(ellipsis)) {
        cex.points <- ellipsis[['cex.points']]
    }
    if("lwd" %in% names(ellipsis)) {
        lwd <- ellipsis[['lwd']]
    }
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex.lab" %in% names(ellipsis)) {
        cex.lab <- ellipsis[['cex.lab']]
    }
    if("cex.axis" %in% names(ellipsis)) {
        cex.axis <- ellipsis[['cex.axis']]
    }
    if("cex.main" %in% names(ellipsis)) {
        cex.main <- ellipsis[['cex.main']]
    }

    ##plot
    for( ii in 1:steps ){
        score <- s[ii]
        name <- names(score)
        yrange <- range(score)
        yrange[1] <- 0 ##otherwise the lowest score cannot be seen

        grp1 <- strsplit(name, " vs ")[[1]][[1]]
        grp2 <- strsplit(name, " vs ")[[1]][[2]]

        plot(
            0,
            xlim = c(min(p), max(p)),
            ylim = yrange,
            bty = 'n',
            pch = '',
            ylab = 'score',
            xlab = '',
            cex.lab = cex.lab,
            cex.axis = cex.axis
        )
        
        title(main = name, cex.main = cex.main)

        y1 <- rep(yrange[1], length(p[names(p) == grp1]))
        y2 <- rep(yrange[1], length(p[names(p) == grp2]))

        lines(p[names(p) == grp1], y1,
            col = rgb(
                CM["red", grp1],
                CM["green", grp1],
                CM["blue", grp1],
                alpha * 255,
                maxColorValue = 255
            ),
            pch = 16,
            type = "p",
            cex = cex.points
        )
    
        lines(
            p[names(p) == grp2],
            y2,
            col = rgb(
                CM["red", grp2],
                CM["green", grp2],
                CM["blue", grp2],
                alpha * 255,
                maxColorValue = 255
            ),
            pch = 16,
            type = "p",
            cex = cex.points
        )

        # find points where to draw lines.
        # these points are in between the line points.
        # to use a vectorized solution we need to include a "temporary variable"
        p0 <- subset(p, names(p) == grp1 | names(p) == grp2)
        p1 <- c(0, p0)
        p2 <- c(p0, 0)
        p.middle.points <- ((p1 + p2) / 2)[-c(1, length(p1))]

        #add lines up to scores value (which is on the y-axis)
        for (i in 1:length(p.middle.points)){
            lines(
                c(p.middle.points[i], p.middle.points[i]),
                c(yrange[1], score[[1]][i]),
                col = "gray",
                type = "l",
                lwd = lwd
            )
        }

    }
}
)

#' @rdname permute
#' @param comparison Specify a comparison i.e.
#' ("grp1 vs grp2") and plot only that comparison.
#' @export
#' @importFrom graphics title

setMethod(
    "plot",
    c(
        "PermutationResults",
        "missing"
    ),
    function(
        x,
        y,
        comparison = "all",
        ...
    )
{
    scores.vec <- getData(x, "scores.vec")
    score.reals <- getData(x, "scores.real")

    if(comparison != "all") {
        if(
            !is.null(scores.vec[[comparison]]) &
            !is.null(score.reals[[comparison]])
        ) {
            scores.vec <- scores.vec[comparison]
            score.reals <- score.reals[comparison]
        } else {
            stop(
            "The specified comparison could not be found"
            )
        }
    }

    ##setup plot window
    steps <- length(scores.vec)
    setup <- n2mfrow(steps)
    oldparams <- par(mfrow = setup)

    cex.main <- 1
    cex.axis <- 1
    cex.lab <- 1
    lwd <- 1
    cex.hist <- 1
    abline.lwd <- 1
    
    ellipsis <- list(...)
    if("cex.main" %in% names(ellipsis)) {
        cex.main <- ellipsis[['cex.main']]
    }
    if("cex.axis" %in% names(ellipsis)) {
        cex.axis <- ellipsis[['cex.axis']]
    }
    if("cex.lab" %in% names(ellipsis)) {
        cex.lab <- ellipsis[['cex.lab']]
    }
    if("lwd" %in% names(ellipsis)) {
        lwd <- ellipsis[['lwd']]
    }
    if("cex.hist" %in% names(ellipsis)) {
        cex.hist <- ellipsis[['cex.hist']]
    }
    if("abline.lwd" %in% names(ellipsis)) {
        abline.lwd <- ellipsis[['abline.lwd']]
    }

    #plot histogram of score distribution
    for( yy in 1:steps) {
        score <- score.reals[yy]
        name <- names(score)
        range <- range(score, scores.vec[yy])
        if(cex.hist != 1) {opar <- par(lwd = cex.hist)}
        hist(
            c(scores.vec[[yy]]),
            xlim = range,
            main = '',
            xlab = "scores",
            cex.axis = cex.axis,
            cex.lab = cex.lab,
            lwd = lwd
        )
        if(cex.hist != 1) {par(opar)}
        title(main = name, cex.main = cex.main)
        
        #add line for the "real" data score
        abline(v = score.reals[[yy]], col = "red", lwd = abline.lwd)
    }
}
)

#' @rdname mlp
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics legend
#' @export

setMethod("plot",c("Mlp", "missing"), function(
    x,
    y,
    steps = "all",
    ...
){

    ellipsis <- list(...)
    oldparams <- par(no.readonly = TRUE)

    if("all" %in% steps){
        par(mfrow = c(3, 2))
        steps <- c(1, 2, 3, 4, 5, 6)
    }
    
    groups <- getData(x, "classes")
    
    ##set up color scheme for all groups
    CM <- .setColors(groups)
    
    if(1 %in% steps){
        #original plot
        plot <- .MlpPlotStep1(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(2 %in% steps){
        #plot with normalized data and line
        .MlpPlotStep2(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(3 %in% steps){
        #show the change from old points to new points on the line by dashing
        .MlpPlotStep3(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(4 %in% steps){
        #show the projected points in 3 dimensions
        .MlpPlotStep4(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(5 %in% steps){
        ##show projection into one dimension
        .MlpPlotStep5(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(6 %in% steps){
        #show the move to origin.
        .MlpPlotStep6(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    #reset mfrow
    if(length(steps) == 6){
        .legend(CM, oldparams, ellipsis)
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
.setColors <- function(
    groups
){
    colors <- colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(groups)))
    CM <- col2rgb(colors, alpha = FALSE)
    colnames(CM) <- unique(groups)
    return(CM)
}

#original plot
.MlpPlotStep1 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    col1 = rgb(
        CM["red", names(sx)[1]],
        CM["green", names(sx)[1]],
        CM["blue", names(sx)[1]],
        alpha * 255,
        maxColorValue = 255
    )
    
    col2 = rgb(
        CM["red", names(sx)[2]],
        CM["green", names(sx)[2]],
        CM["blue", names(sx)[2]],
        alpha * 255,
        maxColorValue = 255
    )

    plot(
        0,
        xlim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        ylim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
    )
    
    lines(sx[[1]], sy[[1]], col = col1, pch = 16, type = "p", cex = cex)
    lines(sx[[2]], sy[[2]], col = col2, pch = 16, type = "p", cex = cex)
}

#show how every point connects to the mean points
.MlpPlotStep2 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    #same points as in step1
    .MlpPlotStep1(x, groups, CM, ellipsis)
    
    #add mean Values
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    y1.mean <- mean(sy[[1]])
    y2.mean <- mean(sy[[2]])
    x1.mean <- mean(sx[[1]])
    x2.mean <- mean(sx[[2]])

    cex <- 1
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    lines(x1.mean, y1.mean, col = "black", pch = 16, type = "p", cex = cex)
    lines(x2.mean, y2.mean, col = "black", pch = 16, type = "p", cex = cex)
    
    #draw the lines grp1
    for(i in 1:length(sy[[1]])){
        lines(
            c(x1.mean, sx[[1]][i]),
            c(y1.mean, sy[[1]][i]),
            col = "grey",
            type = "l",
            lty = 2
        )
    }
    
    #draw the lines grp2
    for(i in 1:length(sy[[2]])){
        lines(
            c(x2.mean, sx[[2]][i]),
            c(y2.mean, sy[[2]][i]),
            col = "grey",
            type = "l",
            lty = 2
        )
    }
}

#show move to point where the mean line goes through origo
.MlpPlotStep3 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    #the first set of points
    p <- getData(x, "points.orig")
    po <- getData(x, "points.origo")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim = c(min(p,po), max(p,po)),
        ylim = c(min(p,po), max(p,po)),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
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
                col = "black",
                angle = 10,
                length = 0.1,
                lwd = 1
            )
        )
    }

    #the second set of points
    splt <- apply(po, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    col1 = rgb(
        CM["red", names(sx)[1]],
        CM["green", names(sx)[1]],
        CM["blue", names(sx)[1]],
        alpha * 255,
        maxColorValue = 255
    )
    
    col2 = rgb(
        CM["red", names(sx)[2]],
        CM["green", names(sx)[2]],
        CM["blue", names(sx)[2]],
        alpha * 255,
        maxColorValue = 255
    )

    lines(sx[[1]], sy[[1]], col = col1, pch = 16, type = "p", cex = cex)
    lines(sx[[2]], sy[[2]], col = col2, pch = 16, type = "p", cex = cex)

    #draw the mean line
    l <- getData(x, "line")
    lines(l[,1], l[,2], type = "l", pch = 16, cex = 0.2)
}


#show the move to one dimension (on the line)
.MlpPlotStep4 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    po <- getData(x, "points.origo")
    splt <- apply(po, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    plot(
        0,
        xlim = c(min(po), max(po)),
        ylim = c(min(po), max(po)),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
    )

    col1 = rgb(
        CM["red", names(sx)[1]],
        CM["green", names(sx)[1]],
        CM["blue", names(sx)[1]],
        alpha * 255,
        maxColorValue = 255
    )
    
    col2 = rgb(
        CM["red", names(sx)[2]],
        CM["green", names(sx)[2]],
        CM["blue", names(sx)[2]],
        alpha * 255,
        maxColorValue = 255
    )

    lines(sx[[1]], sy[[1]], col = col1, pch = 16, type = "p", cex = cex)
    lines(sx[[2]], sy[[2]], col = col2, pch = 16, type = "p", cex = cex)

    #draw the mean line
    l <- getData(x, "line")
    lines(l[,1], l[,2], type = "l", pch = 16, cex = 0.2)

    ##draw lines
    po <- getData(x, "points.origo")
    pd <- getData(x, "line")

    for(i in 1:nrow(po)){
        lines(
            c(pd[i,1], po[i,1]),
            c(pd[i,2], po[i,2]),
            col = "grey",
            type = "l"
        )
    }
}

#show the result of the projection onto the line
.MlpPlotStep5 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "line")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    col1 = rgb(
        CM["red", names(sx)[1]],
        CM["green", names(sx)[1]],
        CM["blue", names(sx)[1]],
        alpha * 255,
        maxColorValue = 255
    )
    
    col2 = rgb(
        CM["red", names(sx)[2]],
        CM["green", names(sx)[2]],
        CM["blue", names(sx)[2]],
        alpha * 255,
        maxColorValue = 255
    )

    plot(
        0,
        xlim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        ylim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
    )
    
    lines(sx[[1]], sy[[1]], col = col1, pch = 16, type = "p", cex = cex)
    lines(sx[[2]], sy[[2]], col = col2, pch = 16, type = "p", cex = cex)
}

#show how the points behave when there is only one dimension in play
.MlpPlotStep6 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "points.onedim")
    sx <- split(p, groups)

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    col1=rgb(
        CM["red", names(sx)[1]],
        CM["green", names(sx)[1]],
        CM["blue", names(sx)[1]],
        alpha * 255,
        maxColorValue = 255
    )
    
    col2=rgb(
        CM["red", names(sx)[2]],
        CM["green", names(sx)[2]],
        CM["blue", names(sx)[2]],
        alpha * 255,
        maxColorValue = 255
    )

    plot(
        0,
        xlim = c(min(unlist(sx)), max(unlist(sx))),
        ylim = c(-0.2, 1),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = '',
        yaxt = 'n'
    )
    
    lines(
        sx[[1]],
        rep(0, length(sx[[1]])),
        col = col1,
        pch = 16,
        type = "p",
        cex = cex
    )
    
    lines(
        sx[[2]],
        rep(0, length(sx[[2]])),
        col = col2,
        pch = 16,
        type = "p",
        cex = cex
    )
}

##plot Pcp projection with only 2 dims
.plotPcp2D <- function(
    x,
    steps,
    CM,
    ellipsis,
    ...
){
    
    groups <- getData(x, "classes")
    oldparams <- par(no.readonly = TRUE)

    if("all" %in% steps) {
        par(mfrow = c(3,2))
        steps <- c(1,2,3,4,5,6)
    }

    if(1 %in% steps){
        #original plot
        plot <- .Pcp2DPlotStep1(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(2 %in% steps){
        #plot.. line
        .Pcp2DPlotStep2(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(3 %in% steps){
        #show the change from old points to new points on the line by dashing
        .Pcp2DPlotStep3(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(4 %in% steps){
        #show the projected points in 3 dimensions
        .Pcp2DPlotStep4(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(5 %in% steps){
        ##show projection into one dimension
        .Pcp2DPlotStep5(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    if(6 %in% steps){
        #show the move to origin.
        .Pcp2DPlotStep6(x, groups, CM, ellipsis)
        if(length(steps) == 1) {.legend(CM, oldparams = NULL, ellipsis)}
    }
    
    #reset mfrow
    if(length(steps) == 6){
        .legend(CM, oldparams, ellipsis)
        par(oldparams)
    }
}

###############################################################################
#
# helpers as units
#
###############################################################################
.Pcp2DPlotStep1 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "points.orig")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]


    plot(
        0,
        xlim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        ylim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
    )
    
    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    for(i in 1:length(sx)) {
        lines(sx[[i]], sy[[i]],
            col = rgb(
                CM["red", names(sx)[i]],
                CM["green", names(sx)[i]],
                CM["blue", names(sx)[i]],
                alpha * 255,
                maxColorValue = 255
            ),
            pch = 16, type = "p", cex = cex)
    }
}

#add line to points
.Pcp2DPlotStep2 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    #same points as in step1
    .Pcp2DPlotStep1(x, groups, CM, ellipsis)
    
    #add mean Values
    l <- getData(x, "line")

    #draw line
    for(i in 1:(nrow(l) - 1)) {
        lines(c(l[i,1], l[i+1,1]), c(l[i,2], l[i+1,2]))
    }
}

.Pcp2DPlotStep3 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    #same points as in step1
    .Pcp2DPlotStep2(x, groups, CM, ellipsis)
    
    #add mean Values
    p <- getData(x, "points.orig")
    l <- getData(x, "line")

    #draw line from point to line point
    for(i in 1:(nrow(l) - 1)) {
        lines(c(l[i,1], p[i,1]), c(l[i,2], p[i,2]))
    }
}

#show the result of the projection onto the line
.Pcp2DPlotStep4 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    
    p <- getData(x, "line")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        ylim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
    )

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    for(i in 1:length(sx)){
        lines(sx[[i]], sy[[i]],
            col = rgb(
                CM["red", names(sx)[i]],
                CM["green", names(sx)[i]],
                CM["blue", names(sx)[i]],
                alpha * 255,
                maxColorValue = 255
            ),
            pch = 16, type = "p", cex = cex)
    }
}


#show the result of the projection onto the line
.Pcp2DPlotStep5 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    p <- getData(x, "line")
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]
    
    p2 <- getData(x,"points.onedim")
    p2 <- matrix(
        c(p2, rep(0,length(p2))),
        ncol = 2,
        dimnames = list(groups, NULL)
    )
    splt2 <- apply(p2, 2, function(x) split(x, groups))
    sx2 <- splt2[[1]]
    sy2 <- splt2[[2]]

    plot(
        0,
        xlim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        ylim = c(min(unlist(sx), unlist(sy)), max(unlist(sx), unlist(sy))),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = ''
    )

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    #all points
    for(i in 1:length(sx)){
        lines(c(sx[[i]], sx2[[i]]), c(sy[[i]], sy2[[i]]),
            col = rgb(
                CM["red", names(sx)[i]],
                CM["green", names(sx)[i]],
                CM["blue", names(sx)[i]],
                alpha * 255,
                maxColorValue = 255
            ),
            pch = 16,
            type = "p",
            cex = cex
        )
    }
    
    for(i in 1:nrow(p)){
        lines(c(p[i,1], p2[i,1]),
            c(p[i,2], p2[i,2]),
            col = "grey",
            type = "l"
        )
    }
}

#show how the points behave when there is only one dimension in play
.Pcp2DPlotStep6 <- function(
    x,
    groups,
    CM,
    ellipsis,
    ...
){
    p <- getData(x,"points.onedim")
    p <- matrix(
        c(p, rep(0, length(p))),
        ncol = 2,
        dimnames = list(groups, NULL)
    )
    splt <- apply(p, 2, function(x) split(x, groups))
    sx <- splt[[1]]
    sy <- splt[[2]]

    plot(
        0,
        xlim = c(min(unlist(sx)), max(unlist(sx))),
        ylim = c(-0.2,1),
        bty = 'n',
        pch = '',
        ylab = '',
        xlab = '',
        yaxt = 'n'
    )

    alpha <- 0.75
    cex <- 1
    if("alpha" %in% names(ellipsis)) {
        alpha <- ellipsis[['alpha']]
    }
    if("cex" %in% names(ellipsis)) {
        cex <- ellipsis[['cex']]
    }

    for(i in 1:length(sx)){
        lines(sx[[i]], sy[[i]],
            col = rgb(
                CM["red", names(sx)[i]],
                CM["green", names(sx)[i]],
                CM["blue", names(sx)[i]],
                alpha * 255,
                maxColorValue = 255
            ),
            pch = 16,
            type = "p",
            cex = cex
        )
    }
}
