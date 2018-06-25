################################################################
# Joshua T. Pierce
# DAT690 Capstone in Data Analytics
# Programming Revision 1
# Written "ChunkWise" for R Studio Markdown
# Outputs to rpubs.com/jpierce072 
#################################################################

Install Packages

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
install.packages("Matrix", repos = "https://cran.rstudio.com")
install.packages("psych", repos = "https://cran.rstudio.com")
install.packages("dplyr", repos = "https://cran.rstudio.com")
```
Load Packages Into Memory

```{r, warning = FALSE}
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
library("Matrix")
library("psych")
library("dplyr")
```
Load Dataset Into Working Memory

```{r}
mammMasses <- read.csv("c:/users/Joshu/documents/datasets/mammMasses.csv")
str(mammMasses)
```
Reorder Columns so Age is First and Class is Last

```{r}
mammMasses <- mammMasses[c(2,1,3,4,5,6)]
str(mammMasses)
```
Create Dot Products

```{r}
#Create copy of mammMasses, "cross"
cross = mammMasses #see str(mammMasses) above
```
```{r}
#Make Each Column Into a Unique Vector
dot1 <- cross[,1]
dot2 <- cross[,2]
dot3 <- cross[,3]
dot4 <- cross[,4]
dot5 <- cross[,5]

str(dot1)
str(dot2)
str(dot3)
str(dot4)
str(dot5)
```
```{r}
#Create Cross Product Variables for All Variable Combinations
dotp1 <- tcrossprod(dot1, dot2)
dotp2 <- tcrossprod(dot1, dot3)
dotp3 <- tcrossprod(dot1, dot4)
dotp4 <- tcrossprod(dot1, dot5)
dotp5 <- tcrossprod(dot2, dot3)
dotp6 <- tcrossprod(dot2, dot4)
dotp7 <- tcrossprod(dot2, dot5)
dotp8 <- tcrossprod(dot3, dot4)
dotp9 <- tcrossprod(dot3, dot5)
dotp10 <- tcrossprod(dot4, dot5)

dotp1 <- diag(dotp1)
dotp2 <- diag(dotp2)
dotp3 <- diag(dotp3)
dotp4 <- diag(dotp4)
dotp5 <- diag(dotp5)
dotp6 <- diag(dotp6)
dotp7 <- diag(dotp7)
dotp8 <- diag(dotp8)
dotp9 <- diag(dotp9)
dotp10 <- diag(dotp10)

str(dotp1)
str(dotp2)
str(dotp3)
str(dotp4)
str(dotp5)
str(dotp6)
str(dotp7)
str(dotp8)
str(dotp9)
str(dotp10)
```
```{r}
#Bind New Variables Into a Dataset, "cross2"
cross2 <- as.data.frame(cbind(dotp1, dotp2, dotp3, dotp4, dotp5, dotp6, dotp7, dotp8, dotp9, dotp10))
```
```{r}
#Rename Columns to Reflect Cross Products
colnames(cross2) <- c("age_birads", "age_shape", "age_margin", "age_density", "birads_shape", "birads_margin", "birads_density", "shape_margin", "shape_density", "margin_density")
str(cross2)
```
```{r}
#Cbind cross2 onto mammMasses as mammMasses2
mammMasses2 <- cbind(mammMasses, cross2)
#Reorder
mammMasses2 <- mammMasses2[c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,6)]
str(mammMasses2)

#Export mammMasses2
write.csv(mammMasses2, "mammMasses2.csv")

```
Summary Statistics

```{r}
describe(mammMasses2)
```

Create Tabular Summaries for Original Variables

```{r}
table(mammMasses2$BI_RADS)
table(mammMasses2$Shape)
table(mammMasses2$Margin)
table(mammMasses2$Density)
table(mammMasses2$Class) #target variable
```

Visualize Cross Product New Variables

```{r}
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
```

Perform NB Classifications

```{r}
#Call the NaiveBayes() Function on MammMasses2
NBModel <- naiveBayes(Class ~., mammMasses2)
NBModel

#Create a Classification Task for the model
require(mlr)
task <- makeClassifTask(data = mammMasses2, target = "Class")

#initialize the NB Classifier
selected_model <- makeLearner("classif.naiveBayes")

#Train the Model:
NBPred <- train(selected_model, task)
NBPred

#Apply Predictive model to mammMassesFinal without passing on the target variable
predictions_mlr <- as.data.frame(predict(NBPred, newdata = mammMasses2[1:15]))

#Create a Confusion Matrix as First Test of Accuracy
require(caret)
table1 <- table(predictions_mlr[,1], mammMasses2$Class)
table1
table2 <- prop.table(table1)
table2

#Save the model as an RDS object
#With a new dataset, the RDS model can be re-loaded and used 
#in a new predict() function:
saveRDS(NBPred, file = "initialNaiveBayesModel.rds")
#to restore: readRDS(file = "initialNaiveBayesModel.rds")
######################################################################
# Assumes new data will be of the same type.
# Will need to write function to convert...I'm not great with loops in R.
# Will also need to generate new variables.
# Here's pseudo-R-code: 
# newobs <- read.csv(newObservation)
# reorder so age is first, class is last
# for(variable) in (newobs){
#   if is.numeric(variable) = FALSE
#      newobs$variable <- is.numeric(newobs$variable)
#create new variables for each column [see: chunk 6]
#create derived variables [see: chunk 7]
#bind derived variables into new set [see: chunk 8]
#rename columns [see: chunk 9]
#load saved RDS model back into memory
#call RDS model on new observation
#get class value
#######################################################################
```
Principal Component Analysis 1

```{r}
require(FactoMineR)
pca1 <- PCA(mammMasses2, graph = T)
pca1
pca1$eig
dimdesc(pca1)
```
Alternative PCA Method
Get Loadings of Best Principal Components

```{r}
model <- princomp(~.,mammMasses2[1:829,1:15], na.action = na.omit)
model$loadings
```

Create mammMasses3

```{r}
predictions_mlr[,1]
Preds <- as.integer(predictions_mlr[,1])
Preds <- ifelse(Preds == 2, 1, 0) #evaluates if preds = 2 and returns a boolean. T = re-evaluates to 1. F = 0.
#therefore, all 2s evaluate to 1s and all 1s evaluate to 0
Preds
table(Preds)
mammMasses3 <- cbind(mammMasses2, Preds)
str(mammMasses3)
```
Create errorTable
A subset of mammMasses3 where Class != Preds

```{r}
#copy mammMasses3
errorTable <- mammMasses3
#filter out correct predictions, reassign to errortable
errortable <- errorTable %>% filter(errorTable$Class != errorTable$Preds)
#get str
str(errorTable)

describe(errorTable)
```
############################################################################
# tcrossprod + diag() wasn't more successful than the last run
# ridit-ize all variables from mammMasses2
# try linear models
#############################################################################

Ridit Transformation

```{r}
require(ridittools)

rid1 <- mammMasses2[,1]
rid2 <- mammMasses2[,2]
rid3 <- mammMasses2[,3]
rid4 <- mammMasses2[,4]
rid5 <- mammMasses2[,5]
rid6 <- mammMasses2[,6]
rid7 <- mammMasses2[,7]
rid8 <- mammMasses2[,8]
rid9 <- mammMasses2[,9]
rid10 <- mammMasses2[,10]
rid11 <- mammMasses2[,11]
rid12 <- mammMasses2[,12]
rid13 <- mammMasses2[,13]
rid14 <- mammMasses2[,14]
rid15 <- mammMasses2[,15]
ridClass <- mammMasses2[,16]

rid1 <- as.data.frame(toridit(rid1))
rid2 <- as.data.frame(toridit(rid2))
rid3 <- as.data.frame(toridit(rid3))
rid4 <- as.data.frame(toridit(rid4))
rid5 <- as.data.frame(toridit(rid5))
rid6 <- as.data.frame(toridit(rid6))
rid7 <- as.data.frame(toridit(rid7))
rid8 <- as.data.frame(toridit(rid8))
rid9 <- as.data.frame(toridit(rid9))
rid10 <- as.data.frame(toridit(rid10))
rid11 <- as.data.frame(toridit(rid11))
rid12 <- as.data.frame(toridit(rid12))
rid13 <- as.data.frame(toridit(rid13))
rid14 <- as.data.frame(toridit(rid14))
rid15 <- as.data.frame(toridit(rid15))
ridClass <- as.data.frame(ridClass)
```

Bind into mammMassesRidit and Rename Columns

```{r}
mammMassesRidit <- cbind(rid1, rid2, rid3, rid4, rid5, rid6, rid7, rid8, rid9, rid10, rid11, rid12, rid13, rid14, rid15, ridClass)

colnames(mammMassesRidit) <- c("Age_RIDIT","BI_RADS_RIDIT", "Shape_RIDIT", "Margin_RIDIT", "Density_RIDIT", "age_birads_RIDIT", "age_shape_RIDIT", "age_margin_RIDIT", "age_density_RIDIT", "birads_shape_RIDIT", "birads_margin_RIDIT", "birads_density_RIDIT", "shape_margin_RIDIT", "shape_density_RIDIT", "margin_density_RIDIT", "Class")

str(mammMassesRidit)
```
PCA for mammMassesRidit

```{r}
require(FactoMineR)
pca2 <- PCA(mammMassesRidit, graph = T)
pca2
pca2$eig
dimdesc(pca2)
```
Alternative PCA Method
Get Loadings of Best Principal Components

```{r}
model2 <- princomp(~.,mammMassesRidit[1:829,1:15], na.action = na.omit)
model2$loadings
```

########################################################################
#After analysis in RapidMiner and Rattle, this was very ineffective.
#I think that 17% from Milestone 2 is about as good as this dataset will
# get it terms of accuracy. 
#Will roll this code back to the PCA and start to focus on
# visualization and other finalizing features.
#########################################################################