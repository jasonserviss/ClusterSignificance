context("Classifier methods")


##run test
test_that("check that the .codedMatrix function returns the correct output", {
  
  ####TEST1####
  ##prepare normal input data
  points <- c(
    0.00000000,
    0.07336109,
    0.18211577,
    0.24109924,
    0.49863309,
    0.53530597,
    0.55451192,
    0.67849177,
    1.04693221,
    1.09843308,
    1.20669106,
    1.22785326,
    1.56945941,
    1.58084130,
    1.59236455,
    1.59733175
  )
  
  names(points) <- 
	  c(rep("grp1", 4), rep("grp2", 4), rep("grp3", 4), rep("grp4", 4))

  sp <- sort(points,decreasing=FALSE, index.return=TRUE)
  order <- names(points)[sp$ix]
  allCombos <- combn(unique(order),2)

  #setup expected data
  cMat <- matrix(c(rep(c(rep(0,4), rep(1,4)), 8)), ncol=8)
  expected <- list(cMat, cMat, cMat, cMat, cMat, cMat)
  names(expected) <- c("grp1 vs grp2", "grp1 vs grp3", "grp1 vs grp4", "grp2 vs grp3", "grp2 vs grp4", "grp3 vs grp4")
  
  ##run function
  codedMat <- .codedMatrix(allCombos, order)
  
  ##test
  expect_true(all.equal(expected, codedMat))
  expect_equal(length(codedMat), ncol(allCombos))
  expect_equal((length(points)/length((unique(names(points)))))*2, 
			   nrow(codedMat[[1]]), ncol(codedMat[[1]]))
})




##run test
test_that("check that the .upperTriangle function returns the correct output",{
  
  ####TEST1####
  ##prepare normal input data
  cMat <- matrix(c(rep(c(rep(0,4), rep(1,4)), 8)), ncol=8)
  codedMat <- list(cMat, cMat, cMat, cMat, cMat, cMat)
  
  #setup expected data
  mat1 <- matrix(c(
  2, 2, 2, 2, 2, 2, 2, 2,
  0, 2, 2, 2, 2, 2, 2, 2,
  0, 0, 2, 2, 2, 2, 2, 2,
  0, 0, 0, 2, 2, 2, 2, 2,
  0, 0, 0, 0, 2, 2, 2, 2,
  0, 0, 0, 0, 1, 2, 2, 2,
  0, 0, 0, 0, 1, 1, 2, 2,
  0, 0, 0, 0, 1, 1, 1, 2
  ), ncol = 8)
  
  mat2 <- matrix(c(
  0, 0, 0, 0, 1, 1, 1, 1,
  2, 0, 0, 0, 1, 1, 1, 1,
  2, 2, 0, 0, 1, 1, 1, 1,
  2, 2, 2, 0, 1, 1, 1, 1,
  2, 2, 2, 2, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 1
  ), ncol = 8)
  
  expected <- list(list(mat1, mat1, mat1, mat1, mat1, mat1), 
				   list(mat2, mat2, mat2, mat2, mat2, mat2))
  
  ##run function
  sepCalc <- .upperTriangle(mat = codedMat)
  
  ##test
  expect_equal(expected[[1]], sepCalc[[1]])
  expect_equal(expected[[2]], sepCalc[[2]])
  expect_equal(length(sepCalc), 2)
  expect_equal(length(sepCalc[[1]]), 6)

})



##run test
test_that("check that the .TpFpFnTn function returns the correct output", {
  
  ####TEST1####
  ##prepare normal input data
  mat1 <- matrix(c(
  2, 2, 2, 2, 2, 2, 2, 2,
  0, 2, 2, 2, 2, 2, 2, 2,
  0, 0, 2, 2, 2, 2, 2, 2,
  0, 0, 0, 2, 2, 2, 2, 2,
  0, 0, 0, 0, 2, 2, 2, 2,
  0, 0, 0, 0, 1, 2, 2, 2,
  0, 0, 0, 0, 1, 1, 2, 2,
  0, 0, 0, 0, 1, 1, 1, 2
  ), ncol = 8)
  
  mat2 <- matrix(c(
  0, 0, 0, 0, 1, 1, 1, 1,
  2, 0, 0, 0, 1, 1, 1, 1,
  2, 2, 0, 0, 1, 1, 1, 1,
  2, 2, 2, 0, 1, 1, 1, 1,
  2, 2, 2, 2, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 1
  ), ncol = 8)
  
  sepCalc <- list(list(mat1, mat1, mat1, mat1, mat1, mat1), 
				  list(mat2, mat2, mat2, mat2, mat2, mat2))

  grp1.mat <- sepCalc[[1]]
  grp2.mat <- sepCalc[[2]]
  
  #setup expected data
  a <- c(0, 0, 0, 0, 1, 2, 3)
  b <- c(1, 2, 3, 4, 4, 4, 4)
  c <- c(4, 4, 4, 4, 3, 2, 1)
  d <- c(3, 2, 1, 0, 0, 0, 0)
  
  expected <- list(list(a,a,a,a,a,a), 
				   list(b,b,b,b,b,b), 
				   list(c,c,c,c,c,c), 
				   list(d,d,d,d,d,d))
  
  ##run function
  truth <- .TpFpFnTn(grp1.mat, grp2.mat)
  
  ##test
  expect_equal(expected, truth)
  
})



##run test
test_that("check that the .specificity function returns the correct output", {
  
  ####TEST1####
  ##prepare normal input data
  FP1 <- seq(9, 1, -1)
  TN1 <- seq(1, 9, 1)
  
  FP <- list(FP1,FP1,FP1)
  TN <- list(TN1, TN1, TN1)
  
  #setup expected data
  vec <- seq(0.1, 0.9, 0.1)
  expected <- list(vec, vec, vec)
  
  ##run function
  scores <- .specificity(TN, FP)
  
  ##test
  expect_equal(expected, scores)
  
})

##run test
test_that("check that the .sensitivity function returns the correct output", {
  
  ####TEST1####
  ##prepare normal input data
  TP1 <- seq(9, 1, -1)
  FN1 <- seq(1, 9, 1)
  
  TP <- list(TP1,TP1,TP1)
  FN <- list(FN1, FN1, FN1)
  
  #setup expected data
  vec <- seq(0.9, 0.1, -0.1)
  expected <- list(vec, vec, vec)
  
  ##run function
  scores <- .sensitivity(TP, FN)
  
  ##test
  expect_equal(expected, scores)
  
})

##run test
test_that("check that the .ROCdistance function returns the correct output", {
    
  ####TEST1####
  ##prepare normal input data
  
  sensitivity <- list(
    c(0,1),
    c(1,0)
  )
  specificity <- list(
    c(0,1),
    c(1,0)
  )
  #setup expected data
  vec <- c(sqrt(2), 0)
  expected <- list(vec, rev(vec))
    
  ##run function
  scores <- .ROCdistance(sensitivity,specificity)
    
  ##test
  expect_equal(expected, scores)
    
})

##run test
test_that("check that the .newScores function returns the correct output", {
    
 ####TEST1####
 ##prepare normal input data
 FP1 <- seq(9, 1, -1)
 TN1 <- seq(1, 9, 1)
 TP1 <- seq(9, 1, -1)
 FN1 <- seq(1, 9, 1)

 FP <- list(FP1,FP1,FP1)
 TN <- list(TN1, TN1, TN1)
 TP <- list(TP1,TP1,TP1)
 FN <- list(FN1, FN1, FN1)

 #setup expected data
 expected <- 5
    
 ##run function
 scores <- .newScores(TP,TN, FP, FN)
    
 ##test
 expect_equal(expected, which(scores[[1]] == max(scores[[1]])))
    
})

