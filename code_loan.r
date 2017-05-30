
#Set the path of the working directory
setwd("C:/Users/Krishna/R proj_loan")
# read the files
> train = read.csv("train_loan.csv")
> test = read.csv("test_loan.csv")
> #check dimensions
> dim(train)
[1] 614  12
> dim(test)
[1] 367  11
> #check the variables and their type in train set
> str(train)
'data.frame':	614 obs. of  12 variables:
 $ Gender           : Factor w/ 3 levels "","Female","Male": 3 3 3 3 3 3 3 3 3 3 ...
 $ Married          : Factor w/ 3 levels "","No","Yes": 2 3 3 3 2 3 3 3 3 3 ...
 $ Dependents       : Factor w/ 5 levels "","0","1","2",..: 2 3 2 2 2 4 2 5 4 3 ...
 $ Education        : Factor w/ 2 levels "Graduate","Not Graduate": 1 1 1 2 1 1 2 1 1 1 ...
 $ Self_Employed    : Factor w/ 3 levels "","No","Yes": 2 2 3 2 2 3 2 2 2 2 ...
 $ ApplicantIncome  : int  5849 4583 3000 2583 6000 5417 2333 3036 4006 12841 ...
 $ CoapplicantIncome: num  0 1508 0 2358 0 ...
 $ LoanAmount       : int  NA 128 66 120 141 267 95 158 168 349 ...
 $ Loan_Amount_Term : int  360 360 360 360 360 360 360 360 360 360 ...
 $ Credit_History   : int  1 1 1 1 1 1 1 0 1 1 ...
 $ Property_Area    : Factor w/ 3 levels "Rural","Semiurban",..: 3 1 3 3 3 3 3 2 3 2 ...
 $ Loan_Status      : Factor w/ 2 levels "N","Y": 2 1 2 2 2 2 2 1 2 1 ...
> # find the missing values
> table(is.na(train))

FALSE  TRUE 
 7282    86 
> table(is.na(test))

FALSE  TRUE 
 3997    40 
> # Variables in which values are missing
> colSums(is.na(train))
           Gender           Married        Dependents         Education     Self_Employed 
                0                 0                 0                 0                 0 
  ApplicantIncome CoapplicantIncome        LoanAmount  Loan_Amount_Term    Credit_History 
                0                 0                22                14                50 
    Property_Area       Loan_Status 
                0                 0 
> colSums(is.na(test))
           Gender           Married        Dependents         Education     Self_Employed 
                0                 0                 0                 0                 0 
  ApplicantIncome CoapplicantIncome        LoanAmount  Loan_Amount_Term    Credit_History 
                0                 0                 5                 6                29 
    Property_Area 
                0 
> #Distribution Analyses
> # Histogram of ApplicantIncome

> hist(train$Credit_History)

https://github.com/Krishnadhruv/Loan-Status-Prediction-Model-in-R/blob/master/Credit_History_hist.jpeg

> hist(train$LoanAmount)

https://github.com/Krishnadhruv/Loan-Status-Prediction-Model-in-R/blob/master/LoanAmount_hist.jpeg

> hist(train$ApplicantIncome)

https://github.com/Krishnadhruv/Loan-Status-Prediction-Model-in-R/blob/master/ApplicantIncome%20_hist.jpeg

> #Due to extreme values in ApplicantIncome and LoanAmount..data mugging is required
> #now...gender vs applicant income
> library(ggplot2)
> ggplot(train, aes(x= Gender, y = ApplicantIncome)) + geom_point(size = 2.5, color="navy") + xlab("Gender") + ylab("ApplicantIncome") + ggtitle("Gender Disparity Vs ApplicantIncome")

https://github.com/Krishnadhruv/Loan-Status-Prediction-Model-in-R/blob/master/gender%20vs%20applicant%20income.jpeg
> #applicant_income vs Loan_Status
> ggplot(train, aes(x= ApplicantIncome, y = Loan_Status)) + geom_point(size = 2.5, color="navy") + xlab("ApplicantIncome") + ylab("Loan_Status") + ggtitle("Applicant Income Vs Loan Status")

https://github.com/Krishnadhruv/Loan-Status-Prediction-Model-in-R/blob/master/applicant%20income%20vs%20loan%20status.jpeg
> #eliminating the missing values using median

> test$Loan_Status <- 1
> combi <- rbind(train, test)

> combi$Credit_History[is.na(combi$Credit_History)] <- median(combi$Credit_History, na.rm = TRUE)
> combi$LoanAmount[is.na(combi$LoanAmount)] <- median(combi$LoanAmount, na.rm = TRUE)
> combi$Loan_Amount_Term[is.na(combi$Loan_Amount_Term)] <- median(combi$Loan_Amount_Term, na.rm = TRUE)
> table(is.na(combi$Credit_History))

FALSE 
  981 

> table(is.na(combi$LoanAmount))

FALSE 
  981 

> table(is.na(combi$Loan_Amount_Term))

FALSE 
  981 
  
#Finding the probability of loan_status( Y- Yes  N- No)
> table(train$Loan_Status)/nrow(train)

        N         Y 
0.3127036 0.6872964 

# Dividing the sample into two parts: developent and validation
sample.ind <- sample(2, 
+                      nrow(train),
+                      replace = T,
+                      prob = c(0.6,0.4))
> train.dev <- train[sample.ind==1,]
> train.val <- train[sample.ind==2,]

#Finding the probability of loan_status in Development sample
> table(train.dev$Loan_Status)/nrow(train.dev)

        N         Y 
0.3150685 0.6849315 

#Finding the probability of loan_status in Validtion Sample
> table(train.val$Loan_Status)/nrow(train.val)

        N         Y 
0.3092369 0.6907631 

> varNames <- names(train.dev)

# Exclude ID or Response variable
> varNames <- varNames[!varNames %in% c("Loan_Status")]

# add + sign between exploratory variables
> varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
> rf.form <- as.formula(paste("Loan_Status", varNames1, sep = " ~ "))


#Building Random Forest using R

#Now, we have a sample data and formula for building Random Forest model. Let’s build 500 decision trees using Random Forest.

>library(randomForest)
randomForest 4.6-12
Type rfNews() to see new features/changes/bug fixes.

Attaching package: ‘randomForest’

The following object is masked from ‘package:ggplot2’:

    margin

> train.rf <- randomForest(rf.form,
+                               train.dev,
+                               ntree=500,
+                               importance=T, na.action = na.exclude)


> plot(train.rf)

# 500 decision trees or a forest has been built using the Random Forest algorithm based learning. We can plot the error rate across decision trees. The plot seems to indicate that after 50 decision trees, there is no reduction in error rate.

https://github.com/Krishnadhruv/Loan-Status-Prediction-Model-in-R/blob/master/plot.jpeg

#Predict Response Variable(Loan_Status value using Random Forest)

> train.dev$predicted.response <- predict(train.rf ,train.dev)

#confusionMatrix function from caret package can be used for creating confusion matrix based on actual response variable and predicted value.

> library(e1071)
> library(caret)
Loading required package: lattice

> confusionMatrix(data=train.dev$predicted.response,
+                 reference=train.dev$Loan_Status,
+                 positive='Y')
Confusion Matrix and Statistics

          Reference
Prediction   N   Y
         N  98   0
         Y   1 214
                                          
      
 Accuracy : 0.9968          
                 
#It has accuracy of 99.68%, which is fantastic. Now we can predict response for the validation sample and calculate model accuracy for the sample.


	
> train.val$predicted.response <- predict(train.rf ,train.val)
> confusionMatrix(data=train.val$predicted.response,
+                 reference=train.val$Loan_Status,
+                 positive='Y')
Confusion Matrix and Statistics

          Reference
Prediction   N   Y
         N  30   9
         Y  34 143
                                         
               Accuracy : 0.8009         
                 95% CI : (0.7414, 0.852)
    No Information Rate : 0.7037         
    P-Value [Acc > NIR] : 0.0007909      
                                         
                  Kappa : 0.4618         
 Mcnemar's Test P-Value : 0.0002522      
                                         
            Sensitivity : 0.9408         
            Specificity : 0.4688         
         Pos Pred Value : 0.8079         
         Neg Pred Value : 0.7692         
             Prevalence : 0.7037         
         Detection Rate : 0.6620         
   Detection Prevalence : 0.8194         
      Balanced Accuracy : 0.7048         
                                         
       'Positive' Class : Y              
                                         
#Accuracy level has dropped to 80% but still significantly higher.