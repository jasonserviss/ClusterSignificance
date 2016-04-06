## generate demo matrices
## mlp matrix

set.seed(3)
points.amount <- 20
PCs <- 2

## Create the input matrix.
mlpMatrix <- matrix(
    c(
        sapply(1:PCs, function(xi)
            c(
                sample(
                    seq(
                        0.3,
                        1.0,
                        0.001
                    ),
                    replace=FALSE,
                    size=points.amount
                ),
                sample(
                    seq(
                        0.0,
                        0.7,
                        0.001
                    ),
                    replace=FALSE,
                    size=points.amount
                )
            )
        )
    ), ncol=PCs
)

groupNames <- c("grp1", "grp2")

## Create the group variable.
groups <- c(
    sapply(
        1:length(groupNames),
        function(ix, groupNames)
            rep(
                groupNames[ix],
                nrow(mlpMatrix)/length(groupNames)
            ),
        groupNames = groupNames
)
)

rownames(mlpMatrix) <- groups
colnames(mlpMatrix) <- paste("dim", 1:2, sep="")
save(mlpMatrix, file = "data/mlpMatrix.rda")

## pcp matrix
set.seed(3)
points.amount <- 20
PCs <- 4
groupNames <- c("grp1", "grp2", "grp3")

pcpMatrix <- matrix(
    c(
        sapply(1:PCs, function(xi)
            c(
                sample(
                    seq(
                        0.0,
                        1.0,
                        0.001
                    ),
                    replace=FALSE,
                    size=points.amount
                ),
                sample(
                    seq(
                        0.5,
                        1.5,
                        0.001
                    ),
                    replace=FALSE,
                    size=points.amount
                ),
                sample(
                    seq(
                        0.0,
                        1.0,
                        0.001
                    ),
                    replace=FALSE,
                    size=points.amount
                )
            )
        )
    ), ncol=PCs
)

groups <- c(
    sapply(
        1:length(groupNames),
        function(ix, groupNames)
            rep(
                groupNames[ix],
                nrow(pcpMatrix)/length(groupNames)
            ),
        groupNames = groupNames
    )
)

rownames(pcpMatrix) <- groups
colnames(pcpMatrix) <- paste("dim", 1:4, sep="")
save(pcpMatrix, file = "data/pcpMatrix.rda")
