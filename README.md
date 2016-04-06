
# ClusterSignificance
## Introduction
The ClusterSignificance package provides tools to assess if clusters, in
e.g. principal component analysis (PCA), have a separation different from
random or permuted data. This is accomplished in a 3 step process *projection*,
*classification*, and *permutation*. To be able to compare cluster
separations, we have to give them a score based on this separation. First, 
all data points in each cluster are projected onto a line (*projection*), after
which the seperation for two groups at a time is scored (*classification*).
Furthermore, to get a p-value for the separation we have to compare the
separation score for our real data to the separation score for permuted data 
(*permutation*).

The package includes 2 different methods for accomplishing the 3 steps
above, Mean line projection (**Mlp**) and Principal curve projection (**Pcp**).
Here we will first discuss the similaraties of the 3 steps independant
of which method is used. This is followed by an example of the Mlp and
Pcp methods and the unique features of each. Furthermore, we provide an
example where ClusterSignificance is used to examine the seperation between
2 clusters downsteam of a PCA. 

## Installation


## Quick Start
While we recommend reading the vignette, the instructions that follow will allow you to quickly get a feel for how ClusterSignificance works and what it is capable of.

```{r echo=TRUE, eval=TRUE, message=FALSE}
library(ClusterSignificance)
```

First, we create an example matrix and group argument to input into the
Mlp method. Please note that this projection method is limited to 2 groups and 2 dimensions per run. For more dimensions or groups refer to the vignette and the Pcp projection method.

```{r, echo=TRUE}
## Create example matrix.
set.seed(3)
points.amount <- 20 
PCs = 2

## Create the input matrix.
mat <- matrix(c(
    sapply(1:PCs, function(xi)
        c(
            sample(seq(0.3, 1.0, 0.001), replace=FALSE, size=points.amount),
            sample(seq(0.0, 0.7, 0.001), replace=FALSE, size=points.amount)
        )
    )), ncol=PCs
) 
```

```{r}
## Create the groups argument.
groupNames = c("grp1", "grp2") ## Maximum of 2 groups with the Mlp method.

## Create the group variable.
groups <- c(
    sapply(1:length(groupNames), function(ix, groupNames)
        rep(groupNames[ix], nrow(mat)/length(groupNames)), 
        groupNames = groupNames
    )
)
```
### Projection
We start by projecting the points into one dimension using the Mlp method. We are able to visualize each step in the projection by plotting the results as shown below. 

```{r, fig.align='center', fig.width=10, fig.height=8}
## Run Mlp and plot.
prj <- mlp(mat, groups)
plot(prj)
```

<img src="/inst/screenShots/MlpProjection.jpeg" alt="Projection plot">

### Classification
Now that the points are in one dimension, we can score each possible seperation and deduce the max seperation score. This is accomplished by the classsify command (again we can plot the results afterwards).

```{r classifyMlp, message=FALSE, fig.align='center', fig.width=8, fig.height=6}
## Classify and plot.
cl <- classify(prj)
plot(cl)
```

<img src="/inst/screenShots/MlpClassification.jpeg" alt="Classification plot">

### Permutation
Finally, as we have now determined the max seperation score, we can permute the data to examine how many max scores exceed that of our real max score and, thus, calculate a p-value for our seperation. Plotting the permutaion results show a histogram of the permuted max scores with the red line representing the real score.

```{r permuteMlp, message=FALSE, fig.align='center', message = FALSE}
## Set the seed and number of iterations.
set.seed(3)
iterations <- 100 

## Permute and plot.
pe <- permute(mat=mat, iterations=iterations, groups=groups, projmethod="mlp")
plot(pe)
```

<img src="/inst/screenShots/MlpPermutation.jpeg" alt="Permutation plot">


To calculate the p-value we use the follwing command.

```{r pValueMlp, echo=FALSE, eval=TRUE, message=FALSE}
## p-value
pvalue(pe)
```

## License
