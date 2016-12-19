context("Permutation methods")

##run test
test_that("check that the .scoreReal function returns the correct output", {
  
  ####TEST1####
  ##prepare normal input data

  mat <- matrix(c(
  1,   2,   3,   4,
  10,  20,  30,  40,
  100, 200, 300, 400,
  5,   6,   7,   8,
  50,  60,  70,  80,
  500, 600, 700, 800,
  9,   10,   11,   12,
  90,  100,  110,  120,
  900, 1000, 1100, 1200,
  13,   14,   15,   16,
  130,  140,  150,  160,
  1300, 1400, 1500, 1600
  ), ncol = 4)
  
  groups <- c(rep("grp1", 4), rep("grp2", 4), rep("grp3", 4))
  
  uniq.groups <- combn(unique(groups), 2)
  projm <- pcp
  
  #setup expected data
  a <- 1
  b <- 1
  c <- 1
  
  expected <- list(a, b, c)
  names(expected) <- c("grp1 vs grp2", "grp1 vs grp3", "grp2 vs grp3")
  
  ##run function
  scores.real <- .scoreReal(mat, groups, projm, df=5)
  
  ##test
  expect_true(all.equal(expected, scores.real))
})




##run test
test_that("check that the .scorePermats function returns the correct output", {
  
  ####TEST1####
  ##prepare normal input data
  
  mat <- matrix(c(
  1,  2,  3,  4,
  10, 20, 30, 40,
  5,  6,  7,  8,
  50, 60, 70, 80,
  9,  10,  11,  12,
  90, 100, 110, 120,
  13,  14,  15,  16,
  130, 140, 150, 160
  ), ncol = 4)
  
  groups <- c("grp1", "grp1", "grp1", "grp1", "grp2", "grp2", "grp2", "grp2")
  
  uniq.groups <- combn(unique(groups), 2)
  iterations <- 3
  
  permats <- list(list(mat, mat, mat))
  names(permats) <- "grp1 vs grp2"
  projm <- pcp
  
  #setup expected data
  expected <- list(c(1, 1, 1))
  
  names(expected) <-
    lapply(1:ncol(uniq.groups),
        function(x) paste(uniq.groups[, x], collapse = " vs "))
  
  expected <- expected[order(names(expected))]

  ##run function
  scores.vec <- .scorePermats(permats, groups, uniq.groups, mat, projm, df=5)
  
  ##test
  expect_true(all.equal(expected, scores.vec))
  expect_true(
	all.equal(
		max(classify(pcp(mat, groups))@scores[[1]]), scores.vec[[1]][[1]]
    ))
})



##run test
test_that("check that the .calculateP function returns the correct output", {
 
 ####TEST1####
 ##prepare normal input data
  scores.vec <- list(c(rep(20,1000)), c(rep(1,1000)))
  scores.real <- list(c(20), c(20))
  iterations <- 1000

 #setup expected data
  expected <- c(1, 0.001)
 
 ##run function
  p.value <- .calculateP(scores.vec, scores.real)
 
 ##test
  expect_true(all.equal(expected, p.value))
})


