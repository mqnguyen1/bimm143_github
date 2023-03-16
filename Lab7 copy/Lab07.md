Lab07
================
Matthew

In this class we will explore clustering and dimensionality reduction
methods.

## K-means

Make up some input data where we know what the answer should be. rnorm
gives random data

``` r
tmp <- c(rnorm(30,-3),rnorm(30,3))
x <- cbind(x=tmp,y=rev(tmp))
head(x)
```

    ##              x        y
    ## [1,] -2.671587 3.110493
    ## [2,] -1.866266 1.680720
    ## [3,] -3.821791 1.631793
    ## [4,] -1.912199 3.481852
    ## [5,] -2.279989 2.589890
    ## [6,] -1.915278 3.419049

Quick plot of x to see the two groups at (-3,3) and (3,-3)

``` r
plot(x)
```

![](Lab07_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Use the ‘kmeans()’ function setting k to 2 and nstart=20

``` r
km <- kmeans(x,centers=2,nstart=20)
km
```

    ## K-means clustering with 2 clusters of sizes 30, 30
    ## 
    ## Cluster means:
    ##           x         y
    ## 1 -2.937295  3.046625
    ## 2  3.046625 -2.937295
    ## 
    ## Clustering vector:
    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    ## [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 43.87795 43.87795
    ##  (between_SS / total_SS =  92.4 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

Q How many points are in each cluster?

``` r
km$size
```

    ## [1] 30 30

Q. What ‘component’ of your result object details - cluster
assignment/membership - cluster center?

``` r
km$cluster
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    ## [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
km$centers
```

    ##           x         y
    ## 1 -2.937295  3.046625
    ## 2  3.046625 -2.937295

Q. Plot x colored by the kmean cluster assignment and add cluster as
blue points

``` r
plot(x,col=km$cluster)
points(km$centers,col="blue")
```

![](Lab07_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Play with kmeans and ask for different numbe of clusters

``` r
km <- kmeans(x,centers=4,nstart=20)
plot(x,col=km$cluster)
points(km$centers,col="blue")
```

![](Lab07_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Hierarchial Clustering

This is another very usefyl and widely employed clustering method which
as the advantage over kmeans in that it can help reveal the something of
the true grouping in your data

The ‘hclust()’ function wants a distance matrix as input. We can get
this from the ‘dist()’ function

``` r
d <- dist(x)
hc <- hclust(d)
hc
```

    ## 
    ## Call:
    ## hclust(d = d)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 60

There is a plot method for hclust results:

``` r
plot(hc)
abline(h=10,col="red")
```

![](Lab07_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

To get my cluster membership vector, I need to “cut” my tree to yield
sub-trees or branches with all the members of a given cluster residing
on the same cut branch. The function to do this is called ‘cutree()’

``` r
grps <- cutree(hc,h=10)
grps
```

    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    ## [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
plot(x,col=grps)
```

![](Lab07_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(hc)
```

![](Lab07_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

It is often helpful to use the ‘k=’ argument to cutree rather than the
‘h=’ height of cutting with ‘cutree()’. This will cut the tree to yield
the number of clusters you want

``` r
cutree(hc,k=4)
```

    ##  [1] 1 1 2 1 1 1 1 1 1 2 1 1 1 2 1 2 2 1 2 1 1 1 2 1 1 1 1 1 1 1 3 3 3 3 3 3 3 4
    ## [39] 3 3 3 4 3 4 4 3 4 3 3 3 4 3 3 3 3 3 3 4 3 3

# Principal Component Analysis (PCA)

The base R function for PCA is called ‘prcomp()’

## PCA of UK food data

Import the data

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
head(x)
```

    ##                X England Wales Scotland N.Ireland
    ## 1         Cheese     105   103      103        66
    ## 2  Carcass_meat      245   227      242       267
    ## 3    Other_meat      685   803      750       586
    ## 4           Fish     147   160      122        93
    ## 5 Fats_and_oils      193   235      184       209
    ## 6         Sugars     156   175      147       139

#How many rows and columns are in your new data frame named x? What R
functions could you use to answer this questions?

``` r
dim(x)
```

    ## [1] 17  5

There are 17 rows and 4 columns

There are actually 4 columns within this data so we have to change it to
give an output reading of 4 columns

``` r
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```

    ##                England Wales Scotland N.Ireland
    ## Cheese             105   103      103        66
    ## Carcass_meat       245   227      242       267
    ## Other_meat         685   803      750       586
    ## Fish               147   160      122        93
    ## Fats_and_oils      193   235      184       209
    ## Sugars             156   175      147       139

``` r
dim(x)
```

    ## [1] 17  4

``` r
x <- read.csv(url, row.names=1)
head(x)
```

    ##                England Wales Scotland N.Ireland
    ## Cheese             105   103      103        66
    ## Carcass_meat       245   227      242       267
    ## Other_meat         685   803      750       586
    ## Fish               147   160      122        93
    ## Fats_and_oils      193   235      184       209
    ## Sugars             156   175      147       139

# Q2. Which approach to solving the ‘row-names problem’ mentioned above do you prefer and why? Is one approach more robust than another under certain circumstances?

The second code since it is more simple and it is better to export the
data straight from the dataset/webiste

# Q3: Changing what optional argument in the above barplot() function results in the following plot?

``` r
barplot(as.matrix(x), beside=F, col=rainbow(nrow(x)))
```

![](Lab07_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> You would
change the ‘beside=T’ to ‘beside=F’

# Q5: Generating all pairwise plots may help somewhat. Can you make sense of the following code and resulting figure? What does it mean if a given point lies on the diagonal for a given plot?

``` r
pairs(x, col=rainbow(10), pch=16)
```

![](Lab07_files/figure-gfm/unnamed-chunk-19-1.png)<!-- --> It seems like
the graph would the comparison between the two countries that are either
on its horizontal or vertical axis. If it is on the line, the data is
similar between both of them. However, if the data leans more horizontal
or vertical from the diagonal axis then it looks like it is more related
towards that country it is leaning towards.

# Q6. What is the main differences between N. Ireland and the other countries of the UK in terms of this data-set?

The orange dot around the x=1100-1200 and the blue line at around x=700
(when looking at the graph of N.Ireland vs England)

# Use the prcomp() PCA function

``` r
pca <- prcomp( t(x) )
summary(pca)
```

    ## Importance of components:
    ##                             PC1      PC2      PC3       PC4
    ## Standard deviation     324.1502 212.7478 73.87622 4.189e-14
    ## Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    ## Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

# Q7. Complete the code below to generate a plot of PC1 vs PC2. The second line adds text labels over the data points.

``` r
# Plot PC1 vs PC2 ("PCA plot" or "Score plot")
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500),col=c("orange","red","blue", "darkgreen"))
text(pca$x[,1], pca$x[,2], colnames(x),col=c("orange","red","blue", "darkgreen"))
```

![](Lab07_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
v <- round( pca$sdev^2/sum(pca$sdev^2) * 100 )
v
```

    ## [1] 67 29  4  0

## or the second row here…

``` r
z <- summary(pca)
z$importance
```

    ##                              PC1       PC2      PC3          PC4
    ## Standard deviation     324.15019 212.74780 73.87622 4.188568e-14
    ## Proportion of Variance   0.67444   0.29052  0.03503 0.000000e+00
    ## Cumulative Proportion    0.67444   0.96497  1.00000 1.000000e+00

``` r
barplot(v, xlab="Principal Component", ylab="Percent Variation")
```

![](Lab07_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## Lets focus on PC1 as it accounts for \> 90% of variance

``` r
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,1], las=2 )
```

![](Lab07_files/figure-gfm/unnamed-chunk-25-1.png)<!-- --> #Q9: Generate
a similar ‘loadings plot’ for PC2. What two food groups feature
prominantely and what does PC2 maninly tell us about? Fresh potatoes and
soft drinks. PC2 tells us about our data’s variation that is with the
country dataset. From the data, we can see how the countries that skew
the two different foods are N. Ireland and Wales

``` r
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,2], las=2 )
```

![](Lab07_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
