#Missing value and outliers are different, performed to clean the data
#Missing value are treated with replacing them with mean and median
#Outliers, we take distribution consider 2nd stddev, values out of these replace them with mean or median
#If outlier is too large remove the outlier
#If data highly skewed then mean is polluted go for the median technique
#Read about non parametric test
#proxy data


View(iris_mis)
d=data.frame(SL=iris_mis$SepalLengthCm, SW=iris_mis$SepalWidthCm, PL=iris_mis$PetalLengthCm,
             PW=iris_mis$PetalWidthCm, Species=iris_mis$Species)
View(d)
library(mice) #multivariant imputation by chained equations imputation
md.pattern(d) #gives the missing data pattern
miceiris=mice(d, m=5, maxit = 50, method = 'pmm')
#predictive mean matching, max iterations, m is no of time
#mice stands for multivariate imputation by chained equations.
#syntax is
#mice(data, number of datasets to be generated, number of iterations to be performed, method of predicting the missing values)
#Note: methods used 1. pmm-predictive mean mactching- used to predict numerical variables
#2. logreg-used for binary variable imputation
#3. polyreg- used is bayesin polytomous regression- for more than 2 classification

summary(miceiris) #imputed data
View(miceiris)
miceiris$imp
#shows generated data set

#now we need to order data
miceiris1=complete(miceiris, 1)
# 1 represents first dataset among the predicted 5 datasets
View(miceiris1)
#export the data to excel for regression
write.csv(miceiris1, "imputed data using mice.csv")
#mice uses serial method which is time consuming and are chances of missing categorical data hence we use other algorithms
#mice can't find categorical data

library(Amelia)
#mulitiple imputation of incomplete multivaiate data
ameliairis=amelia(d, m=5, parallel = "multicore", noms = "Species")
# m is number of imputed dataset
#parallel - type of parallel operation to be used
#noms is a vector no indicating columns in the data that are nominal variables
ameliairis$imputations[[1]]
#asign a new dataframe then export 
#homework
#amelia does parallel computations, and can automatically identify regression data and classification data
#noms is keeping nominal variables
write.csv(ameliairis, "imputed_data_using_amelia.csv")
View(ameliairis)
#mean and covariance is applied to find the missing values in amelia
#em algorithm - expectation maximisation it uses. basically probability
#works on OOB model

#missforest
library(missForest)
missiris=missForest(d)
missiris
missiris$ximp
missiris$OOBerror
#NRMSE is error in predicting numerical missing values(Normalised mean square error),it refers to residuals
#PFC is proportion of falsely classified, error in predicting categorical variable True negative and false positive ratio

#missforest generated out of an algorithm called random forest(is a prediction algorithm)
#non parametric imputaion method coz we don't sepecify m, method etc parameters
#works on OOB model - out of the bag (work on error estimation not accuracy)

#Hmisc
library(Hmisc)
hmisciris= aregImpute(~ SL+SW+PL+PW+Species, data = d, n.impute = 5)
hmisciris
#hmisc's problems is that it assumes all the variables as numerical and linear in relationships
#hmisc using aregimpute() it is a self built package
#hmisc uses Fisher's optimum scoring method for classification problem and simple linear regression for numerical variables


#mi called multiple imputation with diagnostics
library(mi)
mi_iris=mi(d, seed=335)
summary(mi_iris)
#mi uses the method of PMM
#automatically switch between continuous and categorical
#true is no of  missing and false is no of non missing values

