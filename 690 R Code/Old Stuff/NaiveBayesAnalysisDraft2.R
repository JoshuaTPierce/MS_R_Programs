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
install.packages("mlr", repos = "https://cran.rstudio.com")
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
library("mlr")
```

#Part 1: Data Import and Transformation

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

#For modeling, I want these to be in integers (testing below) 
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

str(mammMassesFinal)
write.csv(mammMassesFinal, file = "mammMassesFinal.csv")
```

#Part 2: Naive-Bayes Modeling, Analysis, and Visualization

```{r}
#Call the NaiveBayes() Function on MammMasses2
NBModel <- naiveBayes(Class ~., mammMassesFinal)
NBModel

#Create a Classification Task for the model
require(mlr)
task <- makeClassifTask(data = mammMassesFinal, target = "Class")

#initialize the NB Classifier
selected_model <- makeLearner("classif.naiveBayes")

#Train the Model:
NBPred <- train(selected_model, task)
NBPred

#Apply Predictive model to mammMassesFinal without passing on the target variable
predictions_mlr <- as.data.frame(predict(NBPred, newdata = mammMassesFinal[2:16]))

#Create a Confusion Matrix as First Test of Accuracy
require(caret)
table1 <- table(predictions_mlr[,1], mammMassesFinal$Class)
table1
table2 <- prop.table(table1)
table2

totalSuccessRate = ((0.41375 + 0.416164) * 100)
totalSuccessRate
totalErrorRate = 100 - totalSuccessRate
totalErrorRate

#Save the model as an RDS object
#With a new dataset, the RDS model can be re-loaded and used 
#in a new predict() function:
saveRDS(NBPred, file = "initialNaiveBayesModel.rds")
#to restore: readRDS(file = "initialNaiveBayesModel.rds")
```
Total reduction of class error: 1%.

#Part 3: RIDIT Transformation and Principal Component Analysis

```{r}
#Create subset of mammMassesFinal for RIDIT-Transformation
mammMassesRidit <- mammMassesFinal[2:16] #Leave off the target variable

#RIDIT-ize Each Variable:
rid1 <- mammMassesRidit[1]
rid1 <- toridit(rid1)

rid2 <- mammMassesRidit[2]
rid2 <- toridit(rid2)

rid3 <- mammMassesRidit[3]
rid3 <- toridit(rid3)

rid4 <- mammMassesRidit[4]
rid4 <- toridit(rid4)

rid5 <- mammMassesRidit[5]
rid5 <- toridit(rid5)

rid6 <- mammMassesRidit[6]
rid6 <- toridit(rid6)

rid7 <- mammMassesRidit[7]
rid7 <- toridit(rid7)

rid8 <- mammMassesRidit[8]
rid8 <- toridit(rid8)

rid9 <- mammMassesRidit[9]
rid9 <- toridit(rid9)

rid10 <- mammMassesRidit[10]
rid10 <- toridit(rid10)

rid11 <- mammMassesRidit[11]
rid11 <- toridit(rid11)

rid12 <- mammMassesRidit[12]
rid12 <- toridit(rid12)

rid13 <- mammMassesRidit[13]
rid13 <- toridit(rid13)

rid14 <- mammMassesRidit[14]
rid14 <- toridit(rid14)

rid15 <- mammMassesRidit[15]
rid15 <- toridit(rid15)

```


```{r}
#Remake mammMassesRidit by Column Binding all Ridit-ized Variables
mammMassesRidit <- cbind(rid1, rid2, rid3, rid4, rid5, rid6, rid7, rid8, rid9, rid10, rid11, rid12, rid13, rid14, rid15)
colnames(mammMassesRidit) <- c("Age_RIDIT","BI_RADS_RIDIT", "Shape_RIDIT", "Margin_RIDIT", "Density_RIDIT", "age_birads_RIDIT", "age_shape_RIDIT", "age_margin_RIDIT", "age_density_RIDIT", "birads_shape_RIDIT", "birads_margin_RIDIT", "birads_density_RIDIT", "shape_margin_RIDIT", "shape_density_RIDIT", "margin_density_RIDIT")
```


```{r}
#Discretize the RIDITs:
mammMassesRidit$Age_RIDIT <- as.integer(discretize(mammMassesRidit$Age_RIDIT, breaks = 5))
mammMassesRidit$BI_RADS_RIDIT <- as.integer(discretize(mammMassesRidit$BI_RADS_RIDIT, breaks = 5))
mammMassesRidit$Shape_RIDIT <- as.integer(discretize(mammMassesRidit$Shape_RIDIT, breaks = 5))
mammMassesRidit$Margin_RIDIT <- as.integer(discretize(mammMassesRidit$Margin_RIDIT, breaks = 5))
mammMassesRidit$Density_RIDIT <- as.integer(discretize(mammMassesRidit$Density_RIDIT, breaks = 5))
mammMassesRidit$age_birads_RIDIT <- as.integer(discretize(mammMassesRidit$age_birads_RIDIT, breaks = 5))
mammMassesRidit$age_shape_RIDIT <- as.integer(discretize(mammMassesRidit$age_shape_RIDIT, breaks = 5))
mammMassesRidit$age_margin_RIDIT <- as.integer(discretize(mammMassesRidit$age_margin_RIDIT, breaks = 5))
mammMassesRidit$age_density_RIDIT <- as.integer(discretize(mammMassesRidit$age_density_RIDIT, breaks = 5))
mammMassesRidit$birads_shape_RIDIT <- as.integer(discretize(mammMassesRidit$birads_shape_RIDIT, breaks = 5))
mammMassesRidit$birads_margin_RIDIT <- as.integer(discretize(mammMassesRidit$birads_margin_RIDIT, breaks = 5))
mammMassesRidit$birads_density_RIDIT <- as.integer(discretize(mammMassesRidit$birads_density_RIDIT, breaks = 5))
mammMassesRidit$shape_margin_RIDIT <- as.integer(discretize(mammMassesRidit$shape_margin_RIDIT, breaks = 5))
mammMassesRidit$shape_density_RIDIT <- as.integer(discretize(mammMassesRidit$shape_density_RIDIT, breaks = 5))
mammMassesRidit$margin_density_RIDIT <- as.integer(discretize(mammMassesRidit$margin_density_RIDIT, breaks = 5))

```

```{r}
#Column bind mammMassesRidit to mammMassesFinal as mammMassesFinal2
#Note: think of better dataset names for last time
mammMassesFinal2 <- cbind(mammMassesFinal, mammMassesRidit)
str(mammMassesFinal2)

```
Next, recreate the NB classifier for mammMassesFinal2
[Same procedure as above, not going to comment it out]

```{r}
NBModel2 <- naiveBayes(Class ~., mammMassesFinal2)
NBModel2
require(MLR)
task2 <- makeClassifTask(data = mammMassesFinal2, target = "Class")
selected_model <- makeLearner("classif.naiveBayes")
NBPred2 <- train(selected_model, task2) 
predictions_mlr2 <- as.data.frame(predict(NBPred2, newdata = mammMassesFinal2[,2:31]))
require(caret)
table3 <- table(predictions_mlr2[,1], mammMassesFinal2$Class)
table3
table4 <- prop.table(table3)
table4

totalSuccessRate2 = ((0.41375 + 0.412154) * 100)
totalSuccessRate2
totalErrorRate2 = 100 - totalSuccessRate
totalErrorRate2
```

Principal Component Analysis

```{r}
#Perform Simple PCA
require(FactoMineR)
require(ggplot2)

pca1 <- PCA(mammMassesFinal, graph = T)
```

```{r}
#More Visualizations
pca1
pca1$eig
dimdesc(pca1)
```


```{r}
#Part 4: Investigating the False Positives and False Negatives
#Not Entirely Sure How to Proceed
#Help/Suggestions?

```
