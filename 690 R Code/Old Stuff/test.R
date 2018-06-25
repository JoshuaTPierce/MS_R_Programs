```{r, eval = F, echo = T, results='hide'} 
#Install all packages:
install.packages("ridittools", repos = "https://cran.rstudio.com")
install.packages("Amelia", repos = "https://cran.rstudio.com")
install.packages("ggvis", repos = "https://cran.rstudio.com")
install.packages("lattice", repos = "https://cran.rstudio.com")
install.packages("ggplot2", repos = "https://cran.rstudio.com")
install.packages("e1071", repos = "https://cran.rstudio.com")
install.packages("caret", repos = "https://cran.rstudio.com")
install.packages("FactoMineR", repos = "https://cran.rstudio.com")
install.packages("ROCR", repos = "https://cran.rstduio.com")
install.packages("arules", repos = "https://cran.rstudio.com")
```

```{r, warning = FALSE}
#Load packages into working memory:
library("ridittools")
library("Amelia")
library("ggvis")
library("lattice")
library("ggplot2")
library("e1071")
library("caret")
library("FactoMineR")
library("ROCR")
library("arules")
```

Part 1: Data Import and Transformation

```{r}
#import dataset into workspace
mammMasses <- read.csv("c:/users/Joshu/documents/datasets/mammMasses.csv")
str(mammMasses)

#reorder columns so age is first
mammMasses <- mammMasses[c(6,2,1,3,4,5)]
str(mammMasses)
```
Create and Transform Dot Products for Each Variable combination

```{r}
#Create a Copy of mammMasses
mammMassesDot = mammMasses

#Like above, create a new vector for each individual variable.
dot1 <- mammMassesDot[2]
dot2 <- mammMassesDot[3]
dot3 <- mammMassesDot[4]
dot4 <- mammMassesDot[5]
dot5 <- mammMassesDot[6]

#create a derived variable product for each of 10 possible variable combinations:
# 1*2, 1*3, 1*4, 1*5, 2*3, 2*4, 2*5, 3*4, 3*5, 4*5
#new variable (dotp1) = matrix1 * matrix2

dotp1 <- dot1 * dot2
dotp2 <- dot1 * dot3
dotp3 <- dot1 * dot4
dotp4 <- dot1 * dot5
dotp5 <- dot2 * dot3
dotp6 <- dot2 * dot4
dotp7 <- dot2 * dot5
dotp8 <- dot3 * dot4
dotp9 <- dot3 * dot5
dotp10 <- dot4 * dot5

#Column bind discretized dot products back into mammMassesDot:
mammMassesDot = cbind.data.frame(dotp1, dotp2, dotp3, dotp4, dotp5, dotp6, dotp7, dotp8, dotp9, dotp10)

#Rename mammMassesDot Columns to reflect the two variables from which the new variables are derived
colnames(mammMassesDot) <- c("age_birads", "age_shape", "age_margin", "age_density", "birads_shape", "birads_margin", "birads_density", "shape_margin", "shape_density", "margin_density")

#Append mammMassesRidit and mammMassesDot onto mammMasses
mammMasses2 = cbind(mammMasses, mammMassesDot)
str(mammMasses2)

#Discretize Age and Dot Product Variables
mammMasses2$Age <- discretize(mammMasses2$Age, breaks = 7)
mammMasses2$age_birads <- discretize(mammMasses2$age_birads, breaks = 5)
mammMasses2$age_shape <- discretize(mammMasses2$age_shape, breaks = 5)
mammMasses2$age_margin <- discretize(mammMasses2$age_margin, breaks = 5)
mammMasses2$age_density <- discretize(mammMasses2$age_density, breaks = 5)
mammMasses2$birads_shape <- discretize(mammMasses2$birads_shape, breaks = 5)
mammMasses2$birads_margin <- discretize(mammMasses2$birads_margin, breaks = 5)
mammMasses2$birads_density <- discretize(mammMasses2$birads_density, breaks = 3)
mammMasses2$shape_margin <- discretize(mammMasses2$shape_margin, breaks = 3)
mammMasses2$shape_density <- discretize(mammMasses2$shape_density, breaks = 3)
mammMasses2$margin_density <- discretize(mammMasses2$margin_density, breaks = 3)

str(mammMasses2)

#Using ggvis, make a histogram for each variable.
#With this version of the dataset, the intervals of discretization are visualized in the histograms:
mammMasses2 %>% ggvis(~Class) %>% layer_bars()
mammMasses2 %>% ggvis(~Age) %>% layer_bars()
mammMasses2 %>% ggvis(~BI_RADS) %>% layer_bars()
mammMasses2 %>% ggvis(~Shape) %>% layer_bars()
mammMasses2 %>% ggvis(~Density) %>% layer_bars()
mammMasses2 %>% ggvis(~age_birads) %>% layer_bars()
mammMasses2 %>% ggvis(~age_shape) %>% layer_bars()
mammMasses2 %>% ggvis(~age_margin) %>% layer_bars()
mammMasses2 %>% ggvis(~age_density) %>% layer_bars()
mammMasses2 %>% ggvis(~birads_shape) %>% layer_bars()
mammMasses2 %>% ggvis(~birads_margin) %>% layer_bars()
mammMasses2 %>% ggvis(~birads_density) %>% layer_bars()
mammMasses2 %>% ggvis(~shape_margin) %>% layer_bars()
mammMasses2 %>% ggvis(~shape_density) %>% layer_bars()
mammMasses2 %>% ggvis(~margin_density) %>% layer_bars()

#For modeling, I want these to be in integers. 
#Coerce each column to integer and save as mammMassesFinal
mammMassesFinal = mammMasses2

mammMassesFinal$Age <- as.integer(mammMassesFinal$Age)
mammMassesFinal$age_birads <- as.integer(mammMassesFinal$age_birads)
mammMassesFinal$age_shape <- as.integer(mammMassesFinal$age_shape)
mammMassesFinal$age_margin <- as.integer(mammMassesFinal$age_margin)
mammMassesFinal$age_density <- as.integer(mammMassesFinal$age_density)
mammMassesFinal$birads_shape <- as.integer(mammMassesFinal$birads_shape)
mammMassesFinal$birads_margin <- as.integer(mammMassesFinal$birads_margin)
mammMassesFinal$birads_density <- as.integer(mammMassesFinal$birads_density)
mammMassesFinal$shape_margin <- as.integer(mammMassesFinal$shape_margin)
mammMassesFinal$shape_density <- as.integer(mammMassesFinal$shape_density)
mammMassesFinal$margin_density <- as.integer(mammMassesFinal$margin_density)
```

Next: NB modeling

Next: NB Analysis and Visualization

Next: Hypothesis Confirmation/Rejection

Next: RIDIT Transformation

Next: Principal Component Analysis

Next: PRIDIT Scoring

Next: PRIDIT Visualization

Next: Future Steps?