
<a href="http://www.bioconductor.org/packages/devel/bioc/html/ClusterSignificance.html#since"><img border="0" src="http://www.bioconductor.org/shields/years-in-bioc/ClusterSignificance.svg" title="How long since the package was first in a develd Bioconductor version (or is it in devel only)."></a> <a href="http://bioconductor.org/packages/stats/bioc/ClusterSignificance.html"><img border="0" src="http://www.bioconductor.org/shields/downloads/ClusterSignificance.svg" title="Percentile (top 5/20/50% or 'available') of downloads over last 6 full months. Comparison is done across all package categories (software, annotation, experiment)."></a> <a href="https://support.bioconductor.org/t/ClusterSignificance/"><img border="0" src="http://www.bioconductor.org/shields/posts/ClusterSignificance.svg" title="Support site activity, last 6 months: tagged questions/avg. answers per question/avg. comments per question/accepted answers, or 0 if no tagged posts."></a> <a href="http://www.bioconductor.org/packages/devel/bioc/html/ClusterSignificance.html#svn_source"><img border="0" src="http://www.bioconductor.org/shields/commits/bioc/ClusterSignificance.svg" title="average Subversion commits (to the devel branch) per month for the last 6 months"></a>

Status: Travis CI [![Build Status](https://travis-ci.org/jasonserviss/ClusterSignificance.svg?branch=master)](https://travis-ci.org/jasonserviss/ClusterSignificance)

Bioc-release <a href="http://www.bioconductor.org/packages/release/bioc/html/ClusterSignificance.html#archives"><img border="0" src="http://www.bioconductor.org/shields/availability/release/ClusterSignificance.svg" title="Whether the package is available on all platforms; click for details."></a> <a href="http://bioconductor.org/checkResults/release/bioc-LATEST/ClusterSignificance/"><img border="0" src="http://www.bioconductor.org/shields/build/release/bioc/ClusterSignificance.svg" title="build results; click for full report"></a>

Bioc-devel <a href="http://www.bioconductor.org/packages/devel/bioc/html/ClusterSignificance.html#archives"><img border="0" src="http://www.bioconductor.org/shields/availability/devel/ClusterSignificance.svg" title="Whether the package is available on all platforms; click for details."></a> <a href="http://bioconductor.org/checkResults/devel/bioc-LATEST/ClusterSignificance/"><img border="0" src="http://www.bioconductor.org/shields/build/devel/bioc/ClusterSignificance.svg" title="build results; click for full report"></a>

# ClusterSignificance

The ClusterSignificance package is written in [R](https://cran.r-project.org) and can be found hosted at the [Bioconductor](https://www.bioconductor.org) repository via the links below.

* [release](https://master.bioconductor.org/packages/release/bioc/html/ClusterSignificance.html)
* [devel](https://bioconductor.org/packages/devel/bioc/html/ClusterSignificance.html)

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


## Installation
The release version of ClusterSignificance can be installed in R from 
[Bioconductor](https://www.bioconductor.org) as follows:

```{r}
source("https://bioconductor.org/biocLite.R")
biocLite("ClusterSignificance")
```
To install the development version use:

```{r}
install.packages("devtools")
library(devtools)
install_github("jasonserviss/ClusterSignificance")
```

## Quick Start
While we recommend reading the [vignette](https://bioconductor.org/packages/release/bioc/vignettes/ClusterSignificance/inst/doc/ClusterSignificance-vignette.html), the instructions that follow will allow you 
to quickly get a feel for how ClusterSignificance works and what it is capable of.

Here we utilize the example data included in the ClusterSignificance package 
for the Mlp method. Please note that this projection method is limited to 2 
groups and 2 dimensions per run. For more dimensions or groups refer to the 
vignette and the Pcp projection method.

### Projection
We start by projecting the points into one dimension using the Mlp method. We are able to visualize each step in the projection by plotting the results as shown below.

```{r projection}
library(ClusterSignificance)
data(mlpMatrix)
groups <- rownames(mlpMatrix)
prj <- mlp(mlpMatrix, groups)
plot(prj)
```
[<img src="http://i.imgur.com/KP5CPDW.jpg" alt="Projection Plot"/>](http://i.imgur.com/KP5CPDW.jpg)

### Classification
Now that the points are in one dimension, we can score each possible seperation and deduce the max seperation score. This is accomplished by the classify command (again we can plot the results afterwards). The vertical lines in the plot represent the seperation score for each possible seperation.

```{r classifyMlp}
## Classify and plot.
cl <- classify(prj)
plot(cl)
```

[<img src="http://i.imgur.com/SP3gDrT.jpg" alt="Projection Plot"/>](http://i.imgur.com/SP3gDrT.jpg)

### Permutation
Finally, as we have now determined the max seperation score, we can permute the data to examine how many permuted max scores exceed that of our real max score and, thus, calculate a p-value for our seperation. Plotting the permutaion results show a histogram of the permuted max scores with the red line representing the real score.

```{r permuteMlp}
## Set the seed and number of iterations.
set.seed(3)
iterations <- 100 

## Permute and plot.
pe <- permute(
	mat = mat,
	iter = iterations,
	groups = groups,
	projmethod = "mlp"
)
plot(pe)
```

[<img src="http://i.imgur.com/lzURCuo.jpg" alt="Projection Plot"/>](http://i.imgur.com/lzURCuo.jpg)


To calculate the p-value we use the follwing command.

```{r pValueMlp, echo=FALSE, eval=TRUE, message=FALSE}
pvalue(pe)
```

## Bug Reports and Issues

The Bioconductor support site for the ClusterSignificance package is located [here](https://support.bioconductor.org/t/ClusterSignificance/). Issues and bugs can be reported via Github at: [ClusterSignificance](https://github.com/jasonserviss/ClusterSignificance)

## Citation

```{r}
library(ClusterSignificance)
citation("ClusterSignificance")
```


## License
[GPL-3](https://www.r-project.org/Licenses/GPL-3)
