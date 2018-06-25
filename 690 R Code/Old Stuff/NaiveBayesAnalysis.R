```{r, eval = F, echo = T, results='hide'} 
#Install all packages:
install.packages("ridittools", repos = "https://cran.rstudio.com")
install.packages("psych", repos = "https://cran.rstudio.com")
install.packages("Amelia", repos = "https://cran.rstudio.com")
install.packages("ggvis", repos = "https://cran.rstudio.com")
install.packages("lattice", repos = "https://cran.rstudio.com")
install.packages("ggplot2", repos = "https://cran.rstudio.com")
install.packages("e1071", repos = "https://cran.rstudio.com")
install.packages("caret", repos = "https://cran.rstudio.com")
install.packages("FactoMineR", repos = "https://cran.rstudio.com")
install.packages("ROCR", repos = "https://cran.rstduio.com")
install.packages("klaR", repos = "https://cran.rstudio.com")
install.packages("arules", repos = "https://cran.rstudio.com")
```

```{r, warning = FALSE}
#Load packages into working memory:
library("ridittools")
library("psych")
library("Amelia")
library("ggvis")
library("lattice")
library("ggplot2")
library("e1071")
library("caret")
library("FactoMineR")
library("ROCR")
library("klaR")
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

```{r}
#subset mammMasses variables 2.5 into "mammMassesRidit"
mammMassesRidit = mammMasses[,3:6] 
str(mammMassesRidit)

#perform RIDIT-Transformation on mammMassesRidit
#Transform Each Variable into a Vector for RIDIT-ization
#Ridit-ize Each Variable
rid1 <- mammMassesRidit[1]
rid1 <- toridit(rid1)

rid2 <- mammMassesRidit[2]
rid2 <- toridit(rid2)

rid3 <- mammMassesRidit[3]
rid3 <- toridit(rid3)

rid4 <- mammMassesRidit[4]
rid4 <- toridit(rid4)

#Re-define mammMassesRidit 
mammMassesRidit <- cbind(rid1, rid2, rid3, rid4)

#Rename the Columns so it can be appended to original dataset
colnames(mammMassesRidit) <- c("BI_RADS_RIDIT", "Shape_RIDIT", "Margin_RIDIT", "Density_RIDIT")
str(mammMassesRidit)

```

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

#Discretize the Dot Products, call str() to view frequency bounds 
dotp1 <- as.numeric(unlist(dotp1)) 
dotp1 <- discretize(dotp1, breaks = 5)
str(dotp1)

dotp2 <- as.numeric(unlist(dotp2)) 
dotp2 <- discretize(dotp2, breaks = 5)
dotp2 <- as.integer(dotp2)

dotp3 <- as.numeric(unlist(dotp3)) 
dotp3 <- discretize(dotp3, breaks = 5)
dotp3 <- as.integer(dotp3)

dotp4 <- as.numeric(unlist(dotp4)) 
dotp4 <- discretize(dotp4, breaks = 5)
dotp4 <- as.integer(dotp4)

dotp5 <- as.numeric(unlist(dotp5)) 
dotp5 <- discretize(dotp5, breaks = 5)
dotp5 <- as.integer(dotp5)

dotp6 <- as.numeric(unlist(dotp6)) 
dotp6 <- discretize(dotp6, breaks = 5)
dotp6 <- as.integer(dotp6)

dotp7 <- as.numeric(unlist(dotp7)) 
dotp7 <- discretize(dotp7, breaks = 3)
dotp7 <- as.integer(dotp7)

dotp8 <- as.numeric(unlist(dotp8)) 
dotp8 <- discretize(dotp8, breaks = 3)
dotp8 <- as.integer(dotp8)

dotp9 <- as.numeric(unlist(dotp9)) 
dotp9 <- discretize(dotp9, breaks = 3)
dotp9 <- as.integer(dotp9)

dotp10 <- as.numeric(unlist(dotp10)) 
dotp10 <- discretize(dotp10, breaks = 3)
dotp10 <- as.integer(dotp10)

#Coerce dot products into integers
dotp1 <- as.integer(dotp1)
dotp2 <- as.integer(dotp2)
dotp3 <- as.integer(dotp3)
dotp4 <- as.integer(dotp4)
dotp5 <- as.integer(dotp5)
dotp6 <- as.integer(dotp6)
dotp7 <- as.integer(dotp7)
dotp8 <- as.integer(dotp8)
dotp9 <- as.integer(dotp9)
dotp10 <- as.integer(dotp10)

#Column bind discretized dot products back into mammMassesDot:
mammMassesDot = cbind.data.frame(dotp1, dotp2, dotp3, dotp4, dotp5, dotp6, dotp7, dotp8, dotp9, dotp10)

#Rename mammMassesDot Columns to reflect the two variables from which the new variables are derived
colnames(mammMassesDot) <- c("age_birads", "age_shape", "age_margin", "age_density", "birads_shape", "birads_margin", "birads_density", "shape_margin", "shape_density", "margin_density")


#Append mammMassesRidit and mammMassesDot onto mammMasses
mammMasses2 = cbind(mammMasses, mammMassesDot)
str(mammMasses2)

#Export mammMasses2 so we have it on hand:
write.csv(mammMasses2, file = "mammMasses2.csv")

```

Part 2: Data Exploration and Visualization

```{r}
#Use Amelia MissMap Tool to Check for Missing Data
#Use "Psych" Package Descibe Function for Summary Stats
describe(mammMasses2)

missmap(mammMasses2)

```

```{r}
#Create ggvis//ggplot2 plots for outlier visualization
mammMasses2 %>% ggvis(~Class) %>% layer_bars()
mammMasses2 %>% ggvis(~Age) %>% layer_densities()
mammMasses2 %>% ggvis(~BI_RADS) %>% layer_bars()
mammMasses2 %>% ggvis(~Shape) %>% layer_bars()
mammMasses2 %>% ggvis(~Margin) %>% layer_bars()
mammMasses2 %>% ggvis(~Density) %>% layer_bars()

```
Part 3: Modeling

```{r}
#Naive-Bayes Modeling
#Call the NaiveBayes() Function on MammMasses2
NBModel <- naiveBayes(Class ~., mammMasses2)
NBModel

#Make a Predictive NB Model
NBPredictions <- predict(NBModel, newdata = mammMasses2)
NBPredictions
#Save the model as an RDS object
#With a new dataset, the RDS model can be re-loaded and used 
#in a new predict() function:
saveRDS(NBModel, file = "initialNaiveBayesModel.rds")
#to restore: readRDS(file = "initialNaiveBayesModel.rds")
```

```{r}
#Perform Principal Component Analysis
#Perform Simple PCA/PRIDIT Scoring
myRidit <- mammMassesRidit
myRiditPCA <- princomp(myRidit)
summary(myRiditPCA)
#Will visualize the Output in the next step

#Save the PRIDIT model as an RDS object.
saveRDS(myRiditPCA, file = "myRiditPCA.rds")
#to restore: readRDS(file = "myRiditPCA.rds")

```

Part 4: Visualization and Analysis

```{r}
#Create tools to assess the accuracy of the NB model
#Confusion Matrix:


#ROC/AUC With Plots:


```

```{r, eval = F}
#Visualize the PRIDIT model
#screeVis = screeplot(mammMassesRidit)
screeplot(myRiditPCA)

#FactoMineR PCA Dimensionality Plots
res.pca <- PCA(mammMassesRidit, scale.unit = TRUE, ncp = 5, graph = T)
mammMasses2$Class <- as.factor(mammMasses2$Class)
res.pca2 <- PCA(mammMassesRidit, scale.unit = TRUE, ncp = 5, quali.sup = 21, graph = TRUE)
plotellipses(res.pca2, 21)

#FactoMineR DimDesc() Function with Summary Statistics
dimdesc(res.pca2, axes = c(1,2))

```
Final Step: Investigate False Positives for Trends