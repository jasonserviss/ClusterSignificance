context("Plot methods")

#########################################################################
#
#test plot Pcp objects
#
#########################################################################



#########################################################################
#
#test .setColors
#
#########################################################################

##run test
test_that("check that .setColors outputs the correct ", {
  
  ####TEST1####
  ##prepare normal input data
  groups <- c("grp1", "grp2", "grp3", "grp4", "grp5")
  
  #setup expected data
  expected <- matrix(c(rep(0, (3*5))), ncol=5, 
					 dimnames=list(c("red", "green", "blue"), 
								   c("grp1", "grp2", "grp3", "grp4", "grp5")))
  
  ##run function
  CM <- .setColors(groups)
  
  ##test
  expect_identical(rownames(CM), rownames(expected))
  expect_identical(colnames(CM), colnames(expected))
  
})








