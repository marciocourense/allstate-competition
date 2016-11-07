# allstate-competition

The following is a collection of self analysis, methods from R cookbooks from O'Reilly.
Special credits to Kaggle member dmi3kno for strong inspiration and very active presence in the Kaggle's community.
In [1]:
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")
train <- read.csv("../input/train.csv", stringsAsFactors = T)
test <- read.csv("../input/test.csv", stringsAsFactors = T)
# Any results you write to the current directory are saved as output.
In [2]:
library(lattice)
library(Metrics)
library(Rtsne)
library(gridExtra)
library(corrplot)
library(caret)
library(ggplot2)
library(e1071)
library(GGally)
library(dyplr)
library(SparkR)
Error in library(dyplr): there is no package called ‘dyplr’
Traceback:

1. library(dyplr)
2. stop(txt, domain = NA)
In [3]:
##################################################

#Preparing our training dataset for mutilation


ID = 'id'
TARGET = 'loss'

train_ids <- train[ID]
test_ids <- test[ID]
y_train <- train[TARGET]
loss <- train$loss

#train[,c(ID)] <- NULL  #
#test[,c(ID)] <- NULL           #

catVars <- paste0("cat", seq(1, 116))
contVars <- paste0("cont", seq(1, 14))
targetVar <- "loss"

###################################################
Let's have a quicklook at our dataset
In [4]:
dim(train)
str(train)
# summary(train) #hashtaged for presentation reasons
188318 132
'data.frame':	188318 obs. of  132 variables:
 $ id    : int  1 2 5 10 11 13 14 20 23 24 ...
 $ cat1  : Factor w/ 2 levels "A","B": 1 1 1 2 1 1 1 1 1 1 ...
 $ cat2  : Factor w/ 2 levels "A","B": 2 2 2 2 2 2 1 2 2 2 ...
 $ cat3  : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 2 1 ...
 $ cat4  : Factor w/ 2 levels "A","B": 2 1 1 2 2 1 1 2 2 1 ...
 $ cat5  : Factor w/ 2 levels "A","B": 1 1 2 1 1 1 2 1 2 2 ...
 $ cat6  : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 2 ...
 $ cat7  : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat8  : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat9  : Factor w/ 2 levels "A","B": 2 2 2 2 2 2 1 2 2 2 ...
 $ cat10 : Factor w/ 2 levels "A","B": 1 2 2 1 2 1 1 1 2 1 ...
 $ cat11 : Factor w/ 2 levels "A","B": 2 1 2 1 1 1 1 1 2 1 ...
 $ cat12 : Factor w/ 2 levels "A","B": 1 1 2 1 2 1 1 1 2 1 ...
 $ cat13 : Factor w/ 2 levels "A","B": 1 1 2 1 1 1 1 1 2 1 ...
 $ cat14 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat15 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat16 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 2 1 ...
 $ cat17 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat18 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat19 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat20 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat21 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat22 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat23 : Factor w/ 2 levels "A","B": 2 1 1 2 2 1 1 2 2 1 ...
 $ cat24 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat25 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat26 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat27 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat28 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 2 1 1 ...
 $ cat29 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat30 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat31 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat32 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 2 1 1 ...
 $ cat33 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat34 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat35 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat36 : Factor w/ 2 levels "A","B": 1 1 2 1 1 1 2 1 2 2 ...
 $ cat37 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat38 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 2 2 ...
 $ cat39 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 2 ...
 $ cat40 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 2 ...
 $ cat41 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 2 1 1 1 ...
 $ cat42 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 2 ...
 $ cat43 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat44 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat45 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat46 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat47 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat48 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat49 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat50 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 2 ...
 $ cat51 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat52 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 2 ...
 $ cat53 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat54 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat55 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat56 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat57 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat58 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat59 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat60 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat61 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat62 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat63 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat64 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat65 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat66 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat67 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat68 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat69 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat70 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat71 : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat72 : Factor w/ 2 levels "A","B": 1 1 1 1 2 2 1 1 2 1 ...
 $ cat73 : Factor w/ 3 levels "A","B","C": 1 1 1 2 1 1 1 1 1 1 ...
 $ cat74 : Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat75 : Factor w/ 3 levels "A","B","C": 2 1 1 1 1 1 1 1 1 1 ...
 $ cat76 : Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 1 1 1 1 ...
 $ cat77 : Factor w/ 4 levels "A","B","C","D": 4 4 4 4 4 4 4 4 4 4 ...
 $ cat78 : Factor w/ 4 levels "A","B","C","D": 2 2 2 2 2 2 2 2 2 2 ...
 $ cat79 : Factor w/ 4 levels "A","B","C","D": 2 2 2 2 4 4 2 4 4 2 ...
 $ cat80 : Factor w/ 4 levels "A","B","C","D": 4 4 2 4 2 2 4 2 2 2 ...
 $ cat81 : Factor w/ 4 levels "A","B","C","D": 4 4 4 4 4 4 4 4 2 2 ...
 $ cat82 : Factor w/ 4 levels "A","B","C","D": 2 1 2 4 2 2 2 1 2 2 ...
 $ cat83 : Factor w/ 4 levels "A","B","C","D": 4 2 4 2 2 2 4 2 2 2 ...
 $ cat84 : Factor w/ 4 levels "A","B","C","D": 3 3 3 3 3 3 3 3 3 3 ...
 $ cat85 : Factor w/ 4 levels "A","B","C","D": 2 2 2 2 2 2 2 2 2 2 ...
 $ cat86 : Factor w/ 4 levels "A","B","C","D": 4 4 2 4 2 2 2 4 4 4 ...
 $ cat87 : Factor w/ 4 levels "A","B","C","D": 2 2 2 2 3 2 2 3 4 3 ...
 $ cat88 : Factor w/ 4 levels "A","B","D","E": 1 1 1 1 1 1 1 3 1 3 ...
 $ cat89 : Factor w/ 8 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ cat90 : Factor w/ 7 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 2 1 ...
 $ cat91 : Factor w/ 8 levels "A","B","C","D",..: 1 1 1 1 2 1 1 1 1 2 ...
 $ cat92 : Factor w/ 7 levels "A","B","C","D",..: 1 1 1 1 6 1 1 1 1 6 ...
 $ cat93 : Factor w/ 5 levels "A","B","C","D",..: 4 4 4 4 4 4 4 3 4 4 ...
 $ cat94 : Factor w/ 7 levels "A","B","C","D",..: 2 4 4 4 2 4 4 2 3 2 ...
 $ cat95 : Factor w/ 5 levels "A","B","C","D",..: 3 3 3 3 4 4 4 3 3 3 ...
 $ cat96 : Factor w/ 8 levels "A","B","C","D",..: 5 5 5 5 5 5 5 5 5 5 ...
 $ cat97 : Factor w/ 7 levels "A","B","C","D",..: 1 5 5 5 5 3 3 1 3 1 ...
 $ cat98 : Factor w/ 5 levels "A","B","C","D",..: 3 4 1 4 1 1 1 3 4 3 ...
  [list output truncated]
132 variables 116 categorical variables 14 continuous variables
no descriptive labels, and the values were mostly altered for data protection reasons
In [5]:
#first and foremost let's check our target collumn
boxplot(train$loss)
#it appears to have several outliers, let's dig deeper
hist(train$loss)


In [6]:
#when loss > 40000 the dots appear to be very scarce and sparce
In [7]:
dim(train[(which(train$loss > 40000)),])
#obs. above 40000 treshold appears to behave differentely
# the two most extreme values are much more distant than the rest (outliers)
23 132
In [8]:
#############################################
###### Analysis: continuos variables ########
#############################################
In [9]:
summary(train[contVars])
train_cont <- train[contVars]
train_cont <- cbind(train_cont,loss)

#qucik dirty correlation analysis on the target variable
#simple linear model, searching for obvious correlations
lm.fit.continuous = lm(loss ~., data =train_cont)
summary(lm.fit.continuous)
     cont1              cont2              cont3              cont4       
 Min.   :0.000016   Min.   :0.001149   Min.   :0.002634   Min.   :0.1769  
 1st Qu.:0.346090   1st Qu.:0.358319   1st Qu.:0.336963   1st Qu.:0.3274  
 Median :0.475784   Median :0.555782   Median :0.527991   Median :0.4529  
 Mean   :0.493861   Mean   :0.507188   Mean   :0.498918   Mean   :0.4918  
 3rd Qu.:0.623912   3rd Qu.:0.681761   3rd Qu.:0.634224   3rd Qu.:0.6521  
 Max.   :0.984975   Max.   :0.862654   Max.   :0.944251   Max.   :0.9543  
     cont5            cont6             cont7            cont8       
 Min.   :0.2811   Min.   :0.01268   Min.   :0.0695   Min.   :0.2369  
 1st Qu.:0.2811   1st Qu.:0.33610   1st Qu.:0.3502   1st Qu.:0.3128  
 Median :0.4223   Median :0.44094   Median :0.4383   Median :0.4411  
 Mean   :0.4874   Mean   :0.49094   Mean   :0.4850   Mean   :0.4864  
 3rd Qu.:0.6433   3rd Qu.:0.65502   3rd Qu.:0.5910   3rd Qu.:0.6236  
 Max.   :0.9837   Max.   :0.99716   Max.   :1.0000   Max.   :0.9802  
     cont9             cont10           cont11            cont12       
 Min.   :0.00008   Min.   :0.0000   Min.   :0.03532   Min.   :0.03623  
 1st Qu.:0.35897   1st Qu.:0.3646   1st Qu.:0.31096   1st Qu.:0.31166  
 Median :0.44145   Median :0.4612   Median :0.45720   Median :0.46229  
 Mean   :0.48551   Mean   :0.4981   Mean   :0.49351   Mean   :0.49315  
 3rd Qu.:0.56682   3rd Qu.:0.6146   3rd Qu.:0.67892   3rd Qu.:0.67576  
 Max.   :0.99540   Max.   :0.9950   Max.   :0.99874   Max.   :0.99848  
     cont13             cont14      
 Min.   :0.000228   Min.   :0.1797  
 1st Qu.:0.315758   1st Qu.:0.2946  
 Median :0.363547   Median :0.4074  
 Mean   :0.493138   Mean   :0.4957  
 3rd Qu.:0.689974   3rd Qu.:0.7246  
 Max.   :0.988494   Max.   :0.8448  
Call:
lm(formula = loss ~ ., data = train_cont)

Residuals:
   Min     1Q Median     3Q    Max 
 -4719  -1780   -786    918 116156 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1477.32      49.88  29.617  < 2e-16 ***
cont1       -2137.11     123.84 -17.257  < 2e-16 ***
cont2        1762.24      39.20  44.960  < 2e-16 ***
cont3         -56.58      52.30  -1.082 0.279310    
cont4        -301.17      43.96  -6.851 7.34e-12 ***
cont5          30.03      34.34   0.874 0.381867    
cont6        -557.23     150.52  -3.702 0.000214 ***
cont7        1707.96      79.55  21.471  < 2e-16 ***
cont8         298.61      47.64   6.268 3.67e-10 ***
cont9        2049.38     119.12  17.204  < 2e-16 ***
cont10       -250.02      93.48  -2.675 0.007483 ** 
cont11      -1732.35     305.14  -5.677 1.37e-08 ***
cont12       2724.39     310.10   8.786  < 2e-16 ***
cont13       -631.30      80.82  -7.811 5.69e-15 ***
cont14        273.63      29.66   9.225  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2845 on 188303 degrees of freedom
Multiple R-squared:  0.04015,	Adjusted R-squared:  0.04007 
F-statistic: 562.5 on 14 and 188303 DF,  p-value: < 2.2e-16
In [10]:
#searching for possible correltations between the predictors
library(car)
vif(lm.fit.continuous)
correlations <- cor(train[,contVars])
corrplot(correlations, method="square", order="hclust")

corContPred <- cor(train[,contVars])
head(round(correlations, 2))
cont1
12.5605784388474
cont2
1.53424540206079
cont3
2.59868059741203
cont4
2.00669706483496
cont5
1.19873337790071
cont6
22.2068468351509
cont7
4.68707848340825
cont8
2.09862717279925
cont9
10.8924503974449
cont10
7.02290480135091
cont11
95.2748453394669
cont12
98.106897416824
cont13
6.87885997094247
cont14
1.01308647870913
cont1	cont2	cont3	cont4	cont5	cont6	cont7	cont8	cont9	cont10	cont11	cont12	cont13	cont14
cont1	1.00	-0.09	-0.45	0.37	-0.03	0.76	0.37	0.36	0.93	0.81	0.60	0.61	0.53	0.06
cont2	-0.09	1.00	0.46	0.04	0.19	0.02	0.05	0.14	-0.03	0.06	0.12	0.11	0.02	-0.05
cont3	-0.45	0.46	1.00	-0.34	0.09	-0.35	0.10	-0.19	-0.42	-0.33	0.03	0.01	-0.42	-0.04
cont4	0.37	0.04	-0.34	1.00	0.16	0.22	-0.12	0.53	0.33	0.28	0.12	0.13	0.18	0.02
cont5	-0.03	0.19	0.09	0.16	1.00	-0.15	-0.25	0.01	-0.09	-0.06	-0.15	-0.15	-0.08	-0.02
cont6	0.76	0.02	-0.35	0.22	-0.15	1.00	0.66	0.44	0.80	0.88	0.77	0.79	0.82	0.04

In [11]:
# scatter plot each numeric/continuous predictor against target variable using featurePlot from caret
#it appears to exist some positive correlation with variables cont7, cont11 and cont12, but more investigation is needed
theme <- trellis.par.get()
theme$plot.symbol$col = rgb(.2, .2, .2, .4)
theme$plot.symbol$pch = 16
theme$plot.line$col = rgb(1, 0, 0, .7)
theme$plot.line$lwd <- 2
trellis.par.set(theme)

library(caret)
featurePlot(x = train[,contVars], 
            y = train$loss, 
            plot = "scatter", 
            layout = c(4, 4))

In [12]:
#cont2 appears to be some sort of categorical value



#cont 2 appears to have very distinctive intervals, perhaps it as certain ordinal feature
#much more clear distintion , that cont variables were some sort of categorical patterns, more speciffically cont2,4,5,8 

#the bar-code / stratified pattern is definetely there let's have a closer look
In [13]:
### Helper functions to run quick analysis (credits to: dmi3kno from Kaggle)
doPlots <- function(data_in, fun, ii, lab, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i, lab=lab)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotBox <- function(data_in, i, lab) {
  data <- data.frame(x=data_in[[i]], y=lab)
  p <- ggplot(data=data, aes(x=x, y=y)) +geom_boxplot()+ xlab(colnames(data_in)[i]) + theme_light() + 
    ylab("log(loss)") + theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

plotScatter <- function(data_in, i, lab){
  data <- data.frame(x=data_in[[i]], y = lab)
  p <- ggplot(data= data, aes(x = x, y=y)) + geom_point(size=1, alpha=0.3)+ geom_smooth(method = lm) +
    xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], lab, use = 'complete.obs'), 2)))+
    ylab("log(loss)") + theme_light()
  return(suppressWarnings(p))
} 

plotDen <- function(data_in, i, lab){
  data <- data.frame(x=data_in[[i]], y=lab)
  p <- ggplot(data= data) + geom_density(aes(x = x), size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n','Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) +
    theme_light() 
  return(p)
}
In [14]:
#call the functions above individually, for quick plots

train_contVars <- train[,contVars]
#train_contVars <- cbind(train_contVars,loss)

#let's have a closer look, by calling the helper functions
doPlots(train_contVars, fun = plotScatter, ii =1:14, lab=log(train$loss), ncol = 3)
doPlots(train_contVars, fun = plotScatter, ii =2:2, lab=log(train$loss), ncol = 3)
#GOLD! so what is the purpose of all this?
#in case a variable is an ordinal variable (eg.: id), perhaps it should be excluded

#hypothesis: in some cases dates should be excluded from a prediction
#(assuming tehre no exotic events or a explicit correlation, e.g: insurance terms coming to an end, near the 30th day)


#additional analysis
doPlots(train_contVars,fun = plotDen,ii =1:6,lab=log(train$loss),ncol = 3)


doPlots(train_contVars, fun = plotDen, ii =1:6, lab=log(train$loss), ncol = 3)
doPlots(train_contVars, fun = plotDen, ii =7:14, lab=log(train$loss), ncol = 3)
doPlots(train_contVars, fun = plotScatter, ii =1:6, lab=log(train$loss), ncol = 3)
doPlots(train_contVars, fun = plotScatter, ii =7:14, lab=log(train$loss), ncol = 3)







In [15]:
################################################
####### Analysis: categorical variables ########
################################################
In [16]:
#Normal distribution analysis


train_cat <- train[catVars]
summary(train_cat)
doPlots(train_cat, fun = plotBox, ii =1:12, lab=log(train$loss), ncol = 3)
doPlots(train_cat, fun = plotBox, ii =100:116, lab=log(train$loss), ncol = 3) #some  visualizations problems
 cat1       cat2       cat3       cat4       cat5       cat6       cat7      
 A:141550   A:106721   A:177993   A:128395   A:123737   A:131693   A:183744  
 B: 46768   B: 81597   B: 10325   B: 59923   B: 64581   B: 56625   B:  4574  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat8       cat9       cat10      cat11      cat12      cat13      cat14     
 A:177274   A:113122   A:160213   A:168186   A:159825   A:168851   A:186041  
 B: 11044   B: 75196   B: 28105   B: 20132   B: 28493   B: 19467   B:  2277  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat15      cat16      cat17      cat18      cat19      cat20      cat21     
 A:188284   A:181843   A:187009   A:187331   A:186510   A:188114   A:187905  
 B:    34   B:  6475   B:  1309   B:   987   B:  1808   B:   204   B:   413  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat22      cat23      cat24      cat25      cat26      cat27      cat28     
 A:188275   A:157445   A:181977   A:169969   A:177119   A:168250   A:180938  
 B:    43   B: 30873   B:  6341   B: 18349   B: 11199   B: 20068   B:  7380  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat29      cat30      cat31      cat32      cat33      cat34      cat35     
 A:184593   A:184760   A:182980   A:187107   A:187361   A:187734   A:188105  
 B:  3725   B:  3558   B:  5338   B:  1211   B:   957   B:   584   B:   213  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat36      cat37      cat38      cat39      cat40      cat41      cat42     
 A:156313   A:165729   A:169323   A:183393   A:180119   A:181177   A:186623  
 B: 32005   B: 22589   B: 18995   B:  4925   B:  8199   B:  7141   B:  1695  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat43      cat44      cat45      cat46      cat47      cat48      cat49     
 A:184110   A:172716   A:183991   A:187436   A:187617   A:188049   A:179127  
 B:  4208   B: 15602   B:  4327   B:   882   B:   701   B:   269   B:  9191  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat50      cat51      cat52      cat53      cat54      cat55      cat56     
 A:137611   A:187071   A:179505   A:172949   A:183762   A:188173   A:188136  
 B: 50707   B:  1247   B:  8813   B: 15369   B:  4556   B:   145   B:   182  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat57      cat58      cat59      cat60      cat61      cat62      cat63     
 A:185296   A:188079   A:188018   A:187872   A:187596   A:188273   A:188239  
 B:  3022   B:   239   B:   300   B:   446   B:   722   B:    45   B:    79  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat64      cat65      cat66      cat67      cat68      cat69      cat70     
 A:188271   A:186056   A:179982   A:187626   A:188176   A:188011   A:188295  
 B:    47   B:  2262   B:  8336   B:   692   B:   142   B:   307   B:    23  
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             
 cat71      cat72      cat73      cat74      cat75      cat76      cat77     
 A:178646   A:118322   A:154275   A:184731   A:154307   A:181347   A:    49  
 B:  9672   B: 69996   B: 34017   B:  3561   B: 34010   B:  6183   B:   358  
                       C:    26   C:    26   C:     1   C:   788   C:   408  
                                                                   D:187503  
                                                                             
                                                                             
                                                                             
 cat78      cat79      cat80      cat81      cat82      cat83      cat84     
 A:   788   A:  7064   A:   783   A:   788   A: 19322   A: 26038   A: 29450  
 B:186526   B:152929   B: 46538   B: 24132   B:147536   B:141534   B:   431  
 C:   645   C:  1668   C:  3492   C:  9013   C:  2655   C:  4958   C:154939  
 D:   359   D: 26657   D:137505   D:154385   D: 18805   D: 15788   D:  3498  
                                                                             
                                                                             
                                                                             
 cat85      cat86      cat87      cat88          cat89        cat90     
 A:   788   A:  1589   A:   788   A:168926   A      :183744   A:177993  
 B:186005   B:103852   B:166992   B:     7   B      :  4312   B:  9515  
 C:  1011   C: 10290   C:  8819   D: 19302   C      :   220   C:   728  
 D:   514   D: 72587   D: 11719   E:    83   D      :    33   D:    70  
                                             E      :     5   E:     6  
                                             I      :     2   F:     4  
                                             (Other):     2   G:     2  
     cat91        cat92      cat93      cat94      cat95         cat96       
 A      :111028   A:124689   A:   432   A:   738   A: 3736   E      :174360  
 B      : 42630   B:   628   B:  1133   B: 51710   B:  109   D      :  7922  
 G      : 26734   C:    62   C: 35788   C: 13623   C:87531   B      :  2957  
 C      :  6400   D:    11   D:150237   D:121642   D:79525   G      :  2665  
 D      :  1149   F:     1   E:   728   E:    91   E:17417   F      :   343  
 E      :   254   H: 62901              F:   494             A      :    35  
 (Other):   123   I:    26              G:    20             (Other):    36  
 cat97     cat98          cat99           cat100          cat101      
 A:41970   A:105492   P      :79455   F      :42970   A      :106721  
 B:   34   B:   542   T      :72591   I      :39933   D      : 17171  
 C:78127   C: 21485   R      :10290   L      :19961   C      : 16971  
 D: 3779   D: 50557   D      : 8844   K      :13817   G      : 10944  
 E:47450   E: 10242   S      : 7045   G      :12935   F      : 10139  
 F:  213              N      : 2894   J      :12027   J      :  7259  
 G:16745              (Other): 7199   (Other):46675   (Other): 19113  
     cat102           cat103           cat104          cat105     
 A      :177274   A      :123737   E      :42925   E      :76493  
 B      :  5155   B      : 33342   G      :40660   F      :62892  
 C      :  4929   C      : 16508   D      :27611   G      :20613  
 E      :   482   D      :  7806   F      :19228   D      :12172  
 D      :   449   E      :  4473   H      :17187   H      :11258  
 G      :    15   F      :  1528   K      :14297   I      : 2941  
 (Other):    14   (Other):   924   (Other):26410   (Other): 1949  
     cat106          cat107          cat108          cat109      
 G      :47165   F      :47310   B      :65512   BI     :152918  
 H      :37713   G      :28560   K      :42435   AB     : 21933  
 F      :36143   H      :23461   G      :21421   BU     :  3142  
 I      :21433   J      :22405   D      :19160   K      :  2999  
 J      :18281   K      :20236   F      :10242   G      :  1353  
 E      :13000   I      :20066   A      : 9299   BQ     :  1067  
 (Other):14583   (Other):26280   (Other):20249   (Other):  4906  
     cat110          cat111           cat112          cat113     
 CL     :25305   A      :128395   E      :25148   BM     :26191  
 EG     :24654   C      : 32401   AH     :18639   AE     :22030  
 CS     :24592   E      : 14682   AS     :17669   L      :13058  
 EB     :21396   G      :  7039   J      :16222   AX     :12661  
 CO     :17495   I      :  3578   AF     : 9368   Y      :11374  
 BT     :16365   K      :  1353   AN     : 9138   K      : 7738  
 (Other):58511   (Other):   870   (Other):92134   (Other):95266  
     cat114           cat115          cat116      
 A      :131693   K      :43866   HK     : 21061  
 C      : 16793   O      :26813   DJ     : 20244  
 E      : 16475   J      :23895   CK     : 10162  
 J      :  8199   N      :22438   DP     :  9202  
 F      :  7905   P      :21538   GS     :  8736  
 N      :  2455   L      :16125   CR     :  6862  
 (Other):  4798   (Other):33643   (Other):112051  


In [17]:
#####################################
######## Analysis: Variances ########
#####################################
In [18]:
# check for features variance
zero.var <- nearZeroVar(train, saveMetrics=TRUE)
zero.var
head(zero.var[zero.var$nzv == TRUE,])
freqRatio	percentUnique	zeroVar	nzv
id	1.000000	1.000000e+02	FALSE	FALSE
cat1	3.026642	1.062033e-03	FALSE	FALSE
cat2	1.307903	1.062033e-03	FALSE	FALSE
cat3	17.239031	1.062033e-03	FALSE	FALSE
cat4	2.142666	1.062033e-03	FALSE	FALSE
cat5	1.915997	1.062033e-03	FALSE	FALSE
cat6	2.325704	1.062033e-03	FALSE	FALSE
cat7	40.171404	1.062033e-03	FALSE	TRUE
cat8	16.051612	1.062033e-03	FALSE	FALSE
cat9	1.504362	1.062033e-03	FALSE	FALSE
cat10	5.700516	1.062033e-03	FALSE	FALSE
cat11	8.354163	1.062033e-03	FALSE	FALSE
cat12	5.609272	1.062033e-03	FALSE	FALSE
cat13	8.673704	1.062033e-03	FALSE	FALSE
cat14	81.704436	1.062033e-03	FALSE	TRUE
cat15	5537.764706	1.062033e-03	FALSE	TRUE
cat16	28.083861	1.062033e-03	FALSE	TRUE
cat17	142.864018	1.062033e-03	FALSE	TRUE
cat18	189.798379	1.062033e-03	FALSE	TRUE
cat19	103.158186	1.062033e-03	FALSE	TRUE
cat20	922.127451	1.062033e-03	FALSE	TRUE
cat21	454.975787	1.062033e-03	FALSE	TRUE
cat22	4378.488372	1.062033e-03	FALSE	TRUE
cat23	5.099764	1.062033e-03	FALSE	FALSE
cat24	28.698470	1.062033e-03	FALSE	TRUE
cat25	9.263121	1.062033e-03	FALSE	FALSE
cat26	15.815609	1.062033e-03	FALSE	FALSE
cat27	8.383994	1.062033e-03	FALSE	FALSE
cat28	24.517344	1.062033e-03	FALSE	TRUE
cat29	49.555168	1.062033e-03	FALSE	TRUE
⋮	⋮	⋮	⋮	⋮
103	34.388749	0.004779150	FALSE	TRUE
104	3.711145	0.006903217	FALSE	FALSE
105	1.055706	0.009027284	FALSE	FALSE
106	1.216260	0.010620334	FALSE	FALSE
107	1.250630	0.009027284	FALSE	FALSE
108	1.656513	0.010620334	FALSE	FALSE
109	1.543820	0.005841184	FALSE	FALSE
110	6.972051	0.044605402	FALSE	FALSE
111	1.026405	0.069563186	FALSE	FALSE
112	3.962686	0.008496267	FALSE	FALSE
113	1.349214	0.027081851	FALSE	FALSE
114	1.188879	0.032392018	FALSE	FALSE
115	7.842137	0.010089317	FALSE	FALSE
116	1.635997	0.012213384	FALSE	FALSE
117	1.040358	0.173111439	FALSE	FALSE
118	1.799816	0.343567795	FALSE	FALSE
119	1.038881	0.017523551	FALSE	FALSE
120	1.796296	0.040357268	FALSE	FALSE
121	1.233676	0.059473869	FALSE	FALSE
122	5.763152	0.074873353	FALSE	FALSE
123	1.232417	1.366305929	FALSE	FALSE
124	1.126523	2.990685967	FALSE	FALSE
125	1.204029	0.106734354	FALSE	FALSE
126	1.277609	0.184262790	FALSE	FALSE
127	1.010121	0.092396903	FALSE	FALSE
128	1.128857	0.173111439	FALSE	FALSE
129	1.027522	0.174173473	FALSE	FALSE
130	1.690553	0.187448890	FALSE	FALSE
131	1.017241	9.951252668	FALSE	FALSE
132	4.000000	84.019052879	FALSE	FALSE

freqRatio	percentUnique	zeroVar	nzv
cat7	40.17140	0.001062033	FALSE	TRUE
cat14	81.70444	0.001062033	FALSE	TRUE
cat15	5537.76471	0.001062033	FALSE	TRUE
cat16	28.08386	0.001062033	FALSE	TRUE
cat17	142.86402	0.001062033	FALSE	TRUE
cat18	189.79838	0.001062033	FALSE	TRUE
In [19]:
#to be continued...
#next, the predictive model
