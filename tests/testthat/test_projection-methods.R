
context("internal functions for pcp method")
#########################################################################
#
#test .normalizeMatrix function
#
#########################################################################

##run test
test_that("check that data is properly and normalized between 0 and 1", {
  
  ####TEST1####
  ##prepare normal input data
  mat <- matrix(c(seq(1, 11, by = 1), seq(1, 11, by = 1), seq(1, 11, by = 1)),
				ncol=3)

  #setup expected data
  expected <- matrix(c(seq(0, 1, by = 0.1), 
					   seq(0, 1, by = 0.1), seq(0, 1, by = 0.1)), ncol=3)
  
  ##run function
  mat.norm <- .normalizeMatrix(mat)
  
  ##test
  expect_true(all.equal(expected, mat.norm))
  
})

#########################################################################
#
#test .Curve function
#
#########################################################################


test_that("check that all ouput from princurve is ordered as expected", {
  
  ####TEST1####
  ##prepare normal input data
  mat.norm <- matrix(c(seq(0, 1, by = 0.1), 
					   seq(0, 1, by = 0.1), seq(0, 1, by = 0.1)), ncol=3)
  rownames(mat.norm) <- c(1:nrow(mat.norm))
  groups <- rownames(mat.norm)

  #setup expected data
  expected.mat.norm <- mat.norm
  expected.line <- mat.norm
  expected.vec.onedim <- as.numeric(as.matrix(dist(mat.norm))[,1])
  expected.distance <- 0.173205
  
  ##run function
  prCurve <- .Curve(mat.norm, groups)
  
  ##test
  expect_true(all.equal(expected.mat.norm, prCurve[[1]]))
  expect_true(all.equal(expected.line, prCurve[[2]], tolerance = 1e-7))
  expect_true(all.equal(expected.vec.onedim, 
						unname(prCurve[[3]]), tolerance = 1e-7))
  expect_true(all.equal(expected.distance, 
						unname((prCurve[[3]][3] - prCurve[[3]][2])), 
						tolerance = 1e-6))
  
})


#########################################################################
#
#test unequal number in each group in input
#
#########################################################################

test_that("check that  princurve is correct", {
  
  ####TEST1####
  ##prepare unequal groups input data
  mat <- matrix(c(seq(1, 11, by = 1), 
				  seq(1, 11, by = 1), seq(1, 11, by = 1)), ncol=3)
  g <- c("grp1", "grp2")
  groups <- c(rep(g[1], 5), rep(g[2], 6))
  #normalize matrix
  mat <- .normalizeMatrix(mat)
  #setup expected data
  expected.points.onedim <- seq(0, 1.7320505, by = (1.7320505 / 10))
  
  ##run function
  prCurve <- pcp(mat, groups)
  
  ##test
  expect_true(all.equal(as.numeric(prCurve@points.onedim), 
						as.numeric(expected.points.onedim), tolerance = 1e-6))
  
})

####TEST3####
##check for correspondance of points throughout all outputted data




context("internal functions for mlp method")

test_that(paste(
	"checking .projectMultidimensionalPointsOnMultidimensionalLine"), {

	#####################
	# Test 1
	#####################
	# test a projection on a line for four points in three dimensions
	 mat <- matrix(c( 2, 1, 1,
	  				1, 1, 2,
	  				1, 2, 1, 
	  				1, 2, 2), 
	  			 nrow=4, byrow=TRUE)
	 line <- c(1, 1, 1)
						  
	#prepare expected data
	e1 <- c(1.333333, 1.333333, 1.333333,
			1.333333, 1.333333, 1.333333,
			1.333333, 1.333333, 1.333333,
			1.666667, 1.666667, 1.666667)


	exp <- matrix(e1,ncol=3,  byrow=TRUE)
			
	#run tests
	res <- .projectMultidimensionalPointsOnMultidimensionalLine(mat, line)
    expect_equal(exp, res, tolerance=1e-6)

})


test_that(paste("checking .movePointsSoMeanLineGoesThroughOrigo2D"), {

	#####################
	# Test 1
	#####################
	# 
	mat <- matrix(c(2, 1, 
	  				1, 1, 
	  				3, 2, 
	  				2, 4), 
	  			 nrow=4, byrow=TRUE)
	groups <- rep(c("grp1","grp2"),each=2)

	meanMat <- .groupAndDimensionMean(mat, groups)
						  
	#prepare expected data
	exp <- matrix(c(2,1,3,2,3,3,4,6), ncol=2)
			
	#run tests
	res <- .movePointsSoMeanLineGoesThroughOrigo2D(mat, meanMat)
    expect_equal(exp, res, tolerance=1e-6)

})

test_that(paste("checking .distMeanPointsToLineLineTo2D3D"), {

	#####################
	# Test 1 - check 2D
	#####################
	# 
	 meanMat <- t(matrix(c( 1, 2, 
						  2, 3),
	  			 nrow=2, byrow=TRUE))
	 line <- c(1, 1)
						  
	#prepare expected data
	exp <- 0.7071068
			
	#run tests
	res <- .distMeanPointsToLineLineTo2D3D(line, meanMat)
    expect_equal(exp, res, tolerance=1e-6)

	#####################
	# Test 2 - check 3D
	#####################
	# 
	 meanMat <- t(matrix(c( 1, 2, 2, 
						  2, 3, 2),
	  			 nrow=3, byrow=TRUE))
	 line <- c(1, 1, 1)
						  
	#prepare expected data
	exp <- 1.414214
			
	#run tests
	res <- .distMeanPointsToLineLineTo2D3D(line, meanMat)
    expect_equal(exp, res, tolerance=1e-6)

})


test_that(paste("checking .vectorMatrixOrthogonalToMeanPointLine2D"), {

	#####################
	# Test 1 - check 2D
	#####################
	line <- c(1,2)
	dist <- 1.1
						  
	#prepare expected data
	exp <- matrix(c(0.9838699,-0.4919350,-0.9838699,0.4919350), ncol=2,
				  dimnames=list(NULL,c("right","left")))
			
	#run tests
	res <- .vectorMatrixOrthogonalToMeanPointLine2D(line, dist)
    expect_equal(exp, res, tolerance=1e-6)

	#plot confirmation
#	plot(-10,xlim=c(-2,4), ylim=c(-2,4))
#	abline(h=0)
#	abline(v=0)
#	arrows(x0=0,y0=0,x1=line[1],y1=line[2])
#	#right
#	arrows(x0=0,y0=0,x1=res[1,1],y1=res[2,1], col="green")
#	#left
#	arrows(x0=0,y0=0,x1=res[1,2],y1=res[2,2], col="blue")

})

test_that(paste("checking .axisIntercept2D"), {

	#####################
	# Test 1 - 
	#####################
	#prepare input data
#	# y +, x +, slope +
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y +, x +, slope -
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y +, x -, slope +
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y +, x -, slope -
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y -, x +, slope +
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y -, x +, slope -
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y -, x -, slope +
#	meanMat <- matrix(c(0,1,1,2),ncol=2)
#	# y -, x -, slope -
#	meanMat <- matrix(c(0,1,1,2),ncol=2)

#	#prepare expected data
#	exp <- c(xcept=-1,ycept=1)
#	#run tests
#	res <- .axisIntercept2D(meanMat)
#    expect_equal(exp, res, tolerance=1e-6)

	#####################
	# Test 2 - 
	#####################
	#prepare input data
	meanMat <- matrix(c(0,1,1,2),ncol=2)
	#prepare expected data
	exp <- c(xcept=-1,ycept=1)
	#run tests
	res <- .axisIntercept2D(meanMat)
    expect_equal(exp, res, tolerance=1e-6)

})

test_that(paste(
	"checking .reduceMultDimToOneDimAlongTheLine"), {
    #####################
    # Test 1 - check that group info is preserved
    #####################
    # reduce from 3 dim
    #mat <- matrix(c(
    #    2, 1, 1,
    #    1, 1, 2,
    #    3, 2, 1,
    #   	4, 3, 2,
    #   	1, 2, 1,
    #   	1, 2, 2), nrow=6, byrow=TRUE)
    #    groups <- c("grp1", "grp2", "grp1", "grp1","grp1","grp2")
    #rownames(mat) <- groups
    	
    #run tests
    #res <- .reduceMultDimToOneDimAlongTheLine(mat)
    #expect_equal(groups, names(res), tolerance=1e-6)
    
    #####################
    # Test 2 - check that it can handle points passing through all quadrants
    #####################
    cont <- c(-1,-1,1,1)
    mat <- matrix(cont, nrow=2, byrow=TRUE)
    	
    exp <- c(-1.414214, 1.414214)
    res <- .reduceMultDimToOneDimAlongTheLine(mat)
    expect_equal(exp, res, tolerance=1e-6)
})

