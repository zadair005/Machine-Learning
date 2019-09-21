##R Script for Week 2 Assignment

library(readr)
View(processed_cleveland)
pc <- processed_cleveland
pc
# A tibble: 303 x 14
#X1    X2    X3    X4    X5    X6    X7    X8    X9   X10   X11 X12   X13  
#<dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <chr>
# 1    63     1     1   145   233     1     2   150     0   2.3     3 0.0   6.0  
# 2    67     1     4   160   286     0     2   108     1   1.5     2 3.0   3.0  
# 3    67     1     4   120   229     0     2   129     1   2.6     2 2.0   7.0  
# 4    37     1     3   130   250     0     0   187     0   3.5     3 0.0   3.0  
# 5    41     0     2   130   204     0     2   172     0   1.4     1 0.0   3.0  
# 6    56     1     2   120   236     0     0   178     0   0.8     1 0.0   3.0  
# 7    62     0     4   140   268     0     2   160     0   3.6     3 2.0   3.0  
# 8    57     0     4   120   354     0     0   163     1   0.6     1 0.0   3.0  
# 9    63     1     4   130   254     0     2   147     0   1.4     2 1.0   7.0  
# 10    53     1     4   140   203     1     2   155     1   3.1     3 0.0   7.0  
# ... with 293 more rows, and 1 more variable: X14 <int>
colnames(pc)
# [1] "X1"  "X2"  "X3"  "X4"  "X5"  "X6"  "X7"  "X8"  "X9"  "X10" "X11" "X12"
# [13] "X13" "X14"
names(pc)[names(pc) == "X1"] <- "Age"
names(pc)[names(pc) == "X2"] <- "Heart Disease"
names(pc)[names(pc) == "X3"] <- "Level"
names(pc)[names(pc) == "X2"] <- "Sex"
names(pc)[names(pc) == "X3"] <- "cp"
names(pc)[names(pc) == "X4"] <- "trestbps"
names(pc)[names(pc) == "X5"] <- "chol"
names(pc)[names(pc) == "X6"] <- "fbs"
names(pc)[names(pc) == "X7"] <- "restecg"
names(pc)[names(pc) == "X8"] <- "thalach"
names(pc)[names(pc) == "X9"] <- "exang"
names(pc)[names(pc) == "X10"] <- "oldpeak"
names(pc)[names(pc) == "X11"] <- "slope"
names(pc)[names(pc) == "X12"] <- "ca"
names(pc)[names(pc) == "X13"] <- "thal"
names(pc)[names(pc) == "X14"] <- "num"
pc
# A tibble: 303 x 14
# Age `Heart Disease` Level trestbps  chol   fbs restecg thalach exang
# <dbl>           <dbl> <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>
# 1    63               1     1      145   233     1       2     150     0
# 2    67               1     4      160   286     0       2     108     1
# 3    67               1     4      120   229     0       2     129     1
# 4    37               1     3      130   250     0       0     187     0
# 5    41               0     2      130   204     0       2     172     0
# 6    56               1     2      120   236     0       0     178     0
# 7    62               0     4      140   268     0       2     160     0
# 8    57               0     4      120   354     0       0     163     1
# 9    63               1     4      130   254     0       2     147     0
# 10    53               1     4      140   203     1       2     155     1
# ... with 293 more rows, and 5 more variables: oldpeak <dbl>, slope <dbl>,
#   ca <chr>, thal <chr>, num <int>
head(pc)
# A tibble: 6 x 14
# Age `Heart Disease` Level trestbps  chol   fbs restecg thalach exang oldpeak
# <dbl>           <dbl> <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl>
# 1    63               1     1      145   233     1       2     150     0     2.3
# 2    67               1     4      160   286     0       2     108     1     1.5
# 3    67               1     4      120   229     0       2     129     1     2.6
# 4    37               1     3      130   250     0       0     187     0     3.5
# 5    41               0     2      130   204     0       2     172     0     1.4
# 6    56               1     2      120   236     0       0     178     0     0.8
# ... with 4 more variables: slope <dbl>, ca <chr>, thal <chr>, num <int>
str(pc)
# Classes 'tbl_df', 'tbl' and 'data.frame':	303 obs. of  14 variables:
#  $ Age          : num  63 67 67 37 41 56 62 57 63 53 ...
# $ Heart Disease: num  1 1 1 1 0 1 0 0 1 1 ...
# $ Level        : num  1 4 4 3 2 2 4 4 4 4 ...
# $ trestbps     : num  145 160 120 130 130 120 140 120 130 140 ...
# $ chol         : num  233 286 229 250 204 236 268 354 254 203 ...
# $ fbs          : num  1 0 0 0 0 0 0 0 0 1 ...
# $ restecg      : num  2 2 2 0 2 0 2 0 2 2 ...
# $ thalach      : num  150 108 129 187 172 178 160 163 147 155 ...
# $ exang        : num  0 1 1 0 0 0 0 1 0 1 ...
# $ oldpeak      : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
# $ slope        : num  3 2 2 3 1 1 3 1 2 3 ...
# $ ca           : chr  "0.0" "3.0" "2.0" "0.0" ...
# $ thal         : chr  "6.0" "3.0" "7.0" "3.0" ...
# $ num          : int  0 2 1 0 0 0 3 0 2 1 ...
# - attr(*, "spec")=List of 2
# ..$ cols   :List of 14
# .. ..$ X1 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X2 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X3 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X4 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X5 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X6 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X7 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X8 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X9 : list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X10: list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X11: list()
# .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
# .. ..$ X12: list()
# .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
# .. ..$ X13: list()
# .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
# .. ..$ X14: list()
# .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
# ..$ default: list()
# .. ..- attr(*, "class")= chr  "collector_guess" "collector"
# ..- attr(*, "class")= chr "col_spec"
summary(pc)
# Age        Heart Disease        Level          trestbps    
# Min.   :29.00   Min.   :0.0000   Min.   :1.000   Min.   : 94.0  
# 1st Qu.:48.00   1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:120.0  
# Median :56.00   Median :1.0000   Median :3.000   Median :130.0  
# Mean   :54.44   Mean   :0.6799   Mean   :3.158   Mean   :131.7  
# 3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:140.0  
# Max.   :77.00   Max.   :1.0000   Max.   :4.000   Max.   :200.0  
# chol            fbs            restecg          thalach     
# Min.   :126.0   Min.   :0.0000   Min.   :0.0000   Min.   : 71.0  
# 1st Qu.:211.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:133.5  
# Median :241.0   Median :0.0000   Median :1.0000   Median :153.0  
# Mean   :246.7   Mean   :0.1485   Mean   :0.9901   Mean   :149.6  
# 3rd Qu.:275.0   3rd Qu.:0.0000   3rd Qu.:2.0000   3rd Qu.:166.0  
# Max.   :564.0   Max.   :1.0000   Max.   :2.0000   Max.   :202.0  
# exang           oldpeak         slope            ca           
# Min.   :0.0000   Min.   :0.00   Min.   :1.000   Length:303        
# 1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:1.000   Class :character  
# Median :0.0000   Median :0.80   Median :2.000   Mode  :character  
# Mean   :0.3267   Mean   :1.04   Mean   :1.601                     
# 3rd Qu.:1.0000   3rd Qu.:1.60   3rd Qu.:2.000                     
# Max.   :1.0000   Max.   :6.20   Max.   :3.000                     
# thal                num        
# Length:303         Min.   :0.0000  
# Class :character   1st Qu.:0.0000  
# Mode  :character   Median :0.0000  
# Mean   :0.9373  
# 3rd Qu.:2.0000  
# Max.   :4.0000  
plot(pc)
# Warning messages:
#   1: In data.matrix(x) : NAs introduced by coercion
# 2: In data.matrix(x) : NAs introduced by coercion
plot(pc$Age, y=pc$num)
# Warning messages:
#   1: In doTryCatch(return(expr), name, parentenv, handler) :
#   display list redraw incomplete
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state
# 3: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state
# 4: In doTryCatch(return(expr), name, parentenv, handler) :
#  display list redraw incomplete
# 5: In doTryCatch(return(expr), name, parentenv, handler) :
#  invalid graphics state
# 6: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state
plot(pc$fbs, y=pc$num)
plot(pc$fbs, y=pc$thalach)
pc[complete.cases(pc),]
# A tibble: 303 x 14
# Age `Heart Disease` Level trestbps  chol   fbs restecg thalach exang
# <dbl>           <dbl> <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>
#   1    63               1     1      145   233     1       2     150     0
# 2    67               1     4      160   286     0       2     108     1
# 3    67               1     4      120   229     0       2     129     1
# 4    37               1     3      130   250     0       0     187     0
# 5    41               0     2      130   204     0       2     172     0
# 6    56               1     2      120   236     0       0     178     0
# 7    62               0     4      140   268     0       2     160     0
# 8    57               0     4      120   354     0       0     163     1
# 9    63               1     4      130   254     0       2     147     0
# 10    53               1     4      140   203     1       2     155     1
# ... with 293 more rows, and 5 more variables: oldpeak <dbl>, slope <dbl>,
#   ca <chr>, thal <chr>, num <int>
pc[!complete.cases(pc),]
# A tibble: 0 x 14
# ... with 14 variables: Age <dbl>, `Heart Disease` <dbl>, Level <dbl>,
#   trestbps <dbl>, chol <dbl>, fbs <dbl>, restecg <dbl>, thalach <dbl>,
#   exang <dbl>, oldpeak <dbl>, slope <dbl>, ca <chr>, thal <chr>, num <int>
normalize <- function(pc) {
     return((pc - min(pc)) / (max(pc) - min(pc)))
 }
pc_n <- as.data.frame(lapply[1:14], normalize)
#Error in lapply[1:14] : object of type 'closure' is not subsettable
pc_n <- as.data.frame(lapply[pc[1:14], normalize))
Error: unexpected ')' in "pc_n <- as.data.frame(lapply[pc[1:14], normalize)"
pc_n <- as.data.frame(lapply[pc[2:9], normalize))
#Error: unexpected ')' in "pc_n <- as.data.frame(lapply[pc[2:9], normalize)"
pc_n <- as.data.frame(lapply(pc[2:9], normalize))
pc_n <- as.data.frame(lapply(pc[1:14], normalize))
#Error in pc - min(pc) : non-numeric argument to binary operator
#Called from: FUN(X[[i]], ...)
#Browse[1]> summary(pc_n$Heart.Disease)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  1.0000  0.6799  1.0000  1.0000 
#Browse[1]> summary(pc_n$Level)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.6667  0.6667  0.7195  1.0000  1.0000 
#Browse[1]> summary(pc_n$chol)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.1941  0.2626  0.2756  0.3402  1.0000 
#Browse[1]> summary(pc_n$fbs)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.1485  0.0000  1.0000 
#Browse[1]> 
pc_train <- pc_n[1:212,]
pc_test <- pc_n[213:303,]
pc_train_labels <- pc_n[1:212, 1]
pc_test_labels <- pc_n[213:303, 1]
install.packages("class")
#Installing package into 'C:/Users/zadai/OneDrive/Documents/R/win-library/3.5'
#(as 'lib' is unspecified)
#trying URL 'https://cran.rstudio.com/bin/windows/contrib/3.5/class_7.3-15.zip'
#Content type 'application/zip' length 106363 bytes (103 KB)
#downloaded 103 KB

#package 'class' successfully unpacked and MD5 sums checked

#The downloaded binary packages are in
#C:\Users\zadai\AppData\Local\Temp\RtmpovnxZ1\downloaded_packages
library(class)
#Warning message:
#  package 'class' was built under R version 3.5.3 
pc_test_pred <- knn(train = pc_train, test=pc_test, cl=pc_train_labels, k=1)
pc_test_pred
#[1] 1 0 1 1 0 0 0 1 0 0 0 1 0 0 1 0 1 1 0 0 1 0 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1
#[38] 1 1 1 1 0 1 0 0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 1 1 1
#[75] 0 1 1 1 1 0 1 1 0 1 1 0 1 1 1 0 1
#Levels: 0 1
#plot(pc_test_pred)
install.packages("gmodels")
#Installing package into 'C:/Users/zadai/OneDrive/Documents/R/win-library/3.5'
#(as 'lib' is unspecified)
#trying URL 'https://cran.rstudio.com/bin/windows/contrib/3.5/gmodels_2.18.1.zip'
#Content type 'application/zip' length 113581 bytes (110 KB)
#downloaded 110 KB

#package 'gmodels' successfully unpacked and MD5 sums checked

#The downloaded binary packages are in
#C:\Users\zadai\AppData\Local\Temp\RtmpovnxZ1\downloaded_packages
library(gmodels)
#Warning message:
#  package 'gmodels' was built under R version 3.5.3 
CrossTable(x=pc_test_labels, y=pc_test_pred, prop.chisq = FALSE)


#Cell Contents
#|-------------------------|
#  |                       N |
#  |           N / Row Total |
#  |           N / Col Total |
#  |         N / Table Total |
#  |-------------------------|
  
  
#  Total Observations in Table:  91 


#| pc_test_pred 
#pc_test_labels |         0 |         1 | Row Total | 
#---------------|-----------|-----------|-----------|
#  0 |        35 |         0 |        35 | 
#    |     1.000 |     0.000 |     0.385 | 
#    |     1.000 |     0.000 |           | 
#    |     0.385 |     0.000 |           | 
# ---------------|-----------|-----------|-----------|
#  1 |         0 |        56 |        56 | 
#    |     0.000 |     1.000 |     0.615 | 
#    |     0.000 |     1.000 |           | 
#    |     0.000 |     0.615 |           | 
# ---------------|-----------|-----------|-----------|
#Column Total |        35 |        56 |        91 | 
#  |     0.385 |     0.615 |           | 
#  ---------------|-----------|-----------|-----------|
  
  
pc_test_pred <- knn(train = pc_train, test = pc_test, cl= pc_train_labels, k=10)
pc_test_pred
#[1] 1 0 1 1 0 0 0 1 0 0 0 1 0 0 1 0 1 1 0 0 1 0 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1
#[38] 1 1 1 1 0 1 0 0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 1 1 1
#[75] 0 1 1 1 1 0 1 1 0 1 1 0 1 1 1 0 1
#Levels: 0 1
CrossTable(x=pc_test_labels, y=pc_test_pred, prop.chisq = FALSE)


#Cell Contents
#|-------------------------|
#  |                       N |
#  |           N / Row Total |
#  |           N / Col Total |
#  |         N / Table Total |
#  |-------------------------|
  
  
#  Total Observations in Table:  91 


#| pc_test_pred 
#pc_test_labels |         0 |         1 | Row Total | 
#  ---------------|-----------|-----------|-----------|
#  0 |        35 |         0 |        35 | 
#  |     1.000 |     0.000 |     0.385 | 
#  |     1.000 |     0.000 |           | 
#  |     0.385 |     0.000 |           | 
#  ---------------|-----------|-----------|-----------|
#  1 |         0 |        56 |        56 | 
#  |     0.000 |     1.000 |     0.615 | 
#  |     0.000 |     1.000 |           | 
#  |     0.000 |     0.615 |           | 
#  ---------------|-----------|-----------|-----------|
#  Column Total |        35 |        56 |        91 | 
#  |     0.385 |     0.615 |           | 
#  ---------------|-----------|-----------|-----------|
  