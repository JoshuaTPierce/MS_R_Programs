Naive-Bayes Classifier
================
Joshua T Pierce
June 5, 2017

In this project we train a Naive-Bayes classifier to generate conditional probabilities and a model to classify discharge readiness using the e1071 package.

The dataset includes the binary target variable (discharge: S/A, S=discharged, A=not discharged), along with the categorical predictor variables.

Begin by importing the “e1071” package and library, specifying the CRAN mirror if necessary:

``` r
install.packages("e1071" , repos="https://cran.rstudio.com")
```

    ## package 'e1071' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\piercj01\AppData\Local\Temp\Rtmps3k0qM\downloaded_packages

``` r
library("e1071")
```

Import the dataset with the read.csv() function.

Note: in this case we want the data to be read in as factors. Since Bayes Theorem works with conditional probabilities involving discrete events, we can’t have continuous data. See the structure of the data using the str() function:

``` r
data <- read.csv("C:/Users/piercj01/Desktop/postop_data.csv" , header=TRUE, sep=",")

str(data)
```

    ## 'data.frame':    90 obs. of  8 variables:
    ##  $ core     : Factor w/ 3 levels "high","low","mid": 3 3 1 3 3 1 3 1 3 3 ...
    ##  $ surf     : Factor w/ 3 levels "high","low","mid": 2 1 2 2 3 2 2 3 1 2 ...
    ##  $ oxygen   : Factor w/ 2 levels "excellent","good": 1 1 1 2 1 2 1 1 2 1 ...
    ##  $ bp       : Factor w/ 3 levels "high","low","mid": 3 1 1 1 1 3 1 3 3 3 ...
    ##  $ surfstab : Factor w/ 2 levels "stable","unstable": 1 1 1 1 1 1 1 2 1 2 ...
    ##  $ corestab : Factor w/ 3 levels "mod-stable","stable",..: 2 2 2 3 2 2 2 3 2 2 ...
    ##  $ bpstab   : Factor w/ 3 levels "mod-stable","stable",..: 2 2 1 1 2 3 1 2 2 1 ...
    ##  $ discharge: Factor w/ 2 levels "A","S": 1 2 1 1 1 2 2 2 2 2 ...

Here is a quick rundown of the variables:

core = patient’s internal temperature (low/mid/high)

surf = patient’s surface temperature (low/mid/high)

oxygen = patient’s oxygen saturation (good/excellent)

bp = last measurement of blood pressure (low/mid/high)

surfstab = rating of surface temperature stability (stable/unstable)

corestab = rating of core temperature stability (stable/unstable)

bpstab = rating of blood pressure stability (stable/unstable)

discharge = target variable - discharged or not (S/A) where S=discharge, A=not discharge

**Therefore, we can see how each variable is categorical and nominal, so we don't need to do any data manipulation.**

Compute the Bayesian Probability of discharge from the training dataset \[p(yes) P(no)\] by normalizing the total number over the instances:

``` r
tprior <- table(data$discharge)

tprior <- tprior/sum(tprior)

tprior
```

    ## 
    ##         A         S 
    ## 0.7222222 0.2777778

Next, for each variable, compute the summaries that you need to create a Bayes model, e.g., **P(A|b), b={0, 1}.**

Count up the “0” and “1” counts per variable, then normalize over the total number of “no” and “yes” respectively to get the conditional probabilities for each variable:

``` r
corecount <- table(data[,c("discharge" , "core")])

corecount <- corecount/rowSums(corecount)

surfcount <- table(data[,c("discharge" , "surf")])

surfcount <- surfcount/rowSums(surfcount)

oxygencount <- table(data[,c("discharge" , "oxygen")])

oxygencount <- oxygencount/rowSums(oxygencount)

bpcount <- table(data[,c("discharge" , "bp")])

bpcount <- bpcount/rowSums(bpcount)

surfstabcount <- table(data[,c("discharge" , "surfstab")])

surfstabcount <- surfstabcount/rowSums(surfstabcount)

corestabcount <- table(data[,c("discharge" , "corestab")])

corestabcount <- corestabcount/rowSums(corestabcount)

bpstabcount <- table(data[,c("discharge" , "bpstab")])

bpstabcount <- bpstabcount/rowSums(bpstabcount)
```

Next, execute the Naive-Bayesian Classifier from the e1071 package.

The Naïve Bayes function computes the conditional a-posteriori probabilities of a categorical class variable given independent categorical predictor variables using Bayes' Theorem:

``` r
NBmodel <- naiveBayes(discharge ~.,data)

NBmodel
```

    ## 
    ## Naive Bayes Classifier for Discrete Predictors
    ## 
    ## Call:
    ## naiveBayes.default(x = X, y = Y, laplace = laplace)
    ## 
    ## A-priori probabilities:
    ## Y
    ##         A         S 
    ## 0.7222222 0.2777778 
    ## 
    ## Conditional probabilities:
    ##    core
    ## Y        high       low       mid
    ##   A 0.1538462 0.2000000 0.6461538
    ##   S 0.1200000 0.2400000 0.6400000
    ## 
    ##    surf
    ## Y        high       low       mid
    ##   A 0.2153846 0.2615385 0.5230769
    ##   S 0.1200000 0.3200000 0.5600000
    ## 
    ##    oxygen
    ## Y   excellent      good
    ##   A 0.4923077 0.5076923
    ##   S 0.4400000 0.5600000
    ## 
    ##    bp
    ## Y         high        low        mid
    ##   A 0.36923077 0.04615385 0.58461538
    ##   S 0.24000000 0.00000000 0.76000000
    ## 
    ##    surfstab
    ## Y      stable  unstable
    ##   A 0.4923077 0.5076923
    ##   S 0.5200000 0.4800000
    ## 
    ##    corestab
    ## Y   mod-stable     stable   unstable
    ##   A 0.01538462 0.93846154 0.04615385
    ##   S 0.00000000 0.88000000 0.12000000
    ## 
    ##    bpstab
    ## Y   mod-stable    stable  unstable
    ##   A  0.2615385 0.4769231 0.2615385
    ##   S  0.1600000 0.6000000 0.2400000

By calling the model we can see a nice summary of the conditional probabilities of discharge for each level of each variable.
