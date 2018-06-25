Association Rules Mining with the Apriori Algorithm
================
Joshua T Pierce
June 5, 2017

Step 1: Install arules and arulesViz packages and libraries:

``` r
install.packages('arules' , repos="http://cran.rstudio.com")

install.packages('arulesViz', repos="http://cran.rstudio.com" )

library('arules')
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library('arulesViz')
```

    ## Loading required package: grid

Step 2: Read in the Groceries dataset - a default dataset loaded in R Studio - by calling the data() function:

``` r
data(Groceries)
```

Step 3: Apply the apriori algorithm to the transactional data by calling the apriori() function from the arules package. Assign the output to the variable “rules.”

The algorithm scans the transactional data to search for frequent items as single-item sets, then iterates to find 2-item, 3-item, … , n-item sets containing the frequent items.

Note: the apriori() function takes the arguments support (sup=), which defines the threshold for frequency, and confidence level (conf=), which defines the threshold for listing the association rules by a minimum level of occurence (more on this later).

``` r
rules <- apriori(Groceries, parameter=list(sup=0.001 , conf=0.5))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.5    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.06s].
    ## writing ... [5668 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

Step 4: Investigate the number of rules and inspect the first 10 rules:

``` r
rules
```

    ## set of 5668 rules

``` r
inspect(head(rules))
```

    ##     lhs                    rhs          support     confidence lift    
    ## [1] {honey}             => {whole milk} 0.001118454 0.7333333  2.870009
    ## [2] {tidbits}           => {rolls/buns} 0.001220132 0.5217391  2.836542
    ## [3] {cocoa drinks}      => {whole milk} 0.001321810 0.5909091  2.312611
    ## [4] {pudding powder}    => {whole milk} 0.001321810 0.5652174  2.212062
    ## [5] {cooking chocolate} => {whole milk} 0.001321810 0.5200000  2.035097
    ## [6] {cereals}           => {whole milk} 0.003660397 0.6428571  2.515917

At this point, we have 5668 association rules. The inspect() function shows what the output of the association rules looks like. These are conditional (if-then) rules which state that if the first {set} occurs, then the second {set} is likely to occur at least as often as the confidence level specifies.

Notice the other parameter:lift.

Lift is a number used to assess the strength of a conditional rule. The lift of a conditional rule equals equals the support of a multi-item set divided by the sum of the support of the single-item members of the multi-item set.

Therefore, a larger lift indicates a larger ratio of multi-item set occurence to individual item set occurence, which indicates a stronger, more likely association between items.

The final step is to extract the best rules - the association rules that will give us the strongest likelihood of occuring in practice.

First, select the rules with a confidence threshold of 0.8 using the quality() function from the arules package, assigned to the variable bestRules.

Then, we can sort the bestRules according to lift, and extract the top 5 rules by assigning the output to the variable finalRules:

``` r
bestRules <- rules[quality(rules) $ confidence > 0.8]

finalRules <- head(sort(bestRules , by="lift") , 5)

inspect(finalRules)
```

    ##     lhs                        rhs                   support confidence      lift
    ## [1] {liquor,                                                                     
    ##      red/blush wine}        => {bottled beer}    0.001931876  0.9047619 11.235269
    ## [2] {citrus fruit,                                                               
    ##      other vegetables,                                                           
    ##      soda,                                                                       
    ##      fruit/vegetable juice} => {root vegetables} 0.001016777  0.9090909  8.340400
    ## [3] {tropical fruit,                                                             
    ##      other vegetables,                                                           
    ##      whole milk,                                                                 
    ##      yogurt,                                                                     
    ##      oil}                   => {root vegetables} 0.001016777  0.9090909  8.340400
    ## [4] {citrus fruit,                                                               
    ##      grapes,                                                                     
    ##      fruit/vegetable juice} => {tropical fruit}  0.001118454  0.8461538  8.063879
    ## [5] {other vegetables,                                                           
    ##      whole milk,                                                                 
    ##      yogurt,                                                                     
    ##      rice}                  => {root vegetables} 0.001321810  0.8666667  7.951182
