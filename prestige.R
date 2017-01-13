## Load the libraries
library(car)
library(ggplot2)
library(data.table)
library(corrplot)
library(kknn)
library(stats)

##Load the data frame Prestige and convert it into datatable
data("Prestige")
Prestige <-data.table(Prestige)

##Exploratory Analysis

## Explore the data within the dataset
?Prestige #R documentation about the dataset
head(Prestige) #first 6 rows of the dataset
tail(Prestige) #last 6 rows of the dataset
names(Prestige) #print the variables of the data set
sd(Prestige$education) #standard deviation of education
sd(Prestige$income) #standard deviation of income
sd(Prestige$prestige) #standard deviation of prestige
range(Prestige$prestige) #range of prestige
summary(Prestige) #summarize the dataset
str(Prestige) #structure of the dataset


#Histogram of Prestige
ggplot(Prestige, aes(prestige)) + geom_histogram(aes(y =..density..),
col="blue", fill="green", alpha = 0.2) + labs(title="Histogram for Prestige") + labs(x="Prestige", y="Count")+
geom_density(col=2)

#Histogram of income
ggplot(Prestige, aes(income)) + geom_histogram(aes(y =..density..),breaks=seq(0, 26000, by = 500),
col="blue", fill="green", alpha = 0.2) + labs(title="Histogram for Income") + labs(x="Income", y="Count") + geom_density(col=2)

# Subset the data to capture only income and education.
newdata <- Prestige[,list(income,education)]
summary(newdata)

# Create a plot of the subset data.
ggplot(newdata, aes(x=education, y=income)) + geom_point()+ labs(title="Relationship between Income and Education",x="Income", y="Education")

##bivariabte regression between education and income
education_regression = lm(income ~ education, data = newdata)
summary(education_regression)

newdata[,predicted_regression:= predict(education_regression)]
ggplot(newdata, aes(x=newdata$education)) + geom_point(aes(y=newdata$income)) + geom_line(aes(y=newdata$predicted_regression)) + labs(title="Education by Income",
                                                                                                                                      x="Education",
                                                                                                                                      y="Income")
#Scatter plot of Prestige with income and type showing the trendline with standard error
ggplot(Prestige, aes(x=income, y=education)) + geom_point(aes(colour=type), alpha=0.5) + geom_smooth(method=lm, se=T) + labs(title="Income by Education and Type",
x="Income",
y="Education")

#plot residual vs fitted
plot(education_regression, pch=16, which=1)

#Subset the data to capture only income and prestige
newdata2 <- Prestige[,list(income,prestige)]
summary(newdata2)

##bivariabte regression between income and prestige
prestige_regression = lm(prestige ~ income, data = newdata2)
summary(prestige_regression)

newdata2[,predicted_regression2:= predict(prestige_regression)]
ggplot(Prestige, aes(x=newdata2$income)) + geom_point(aes(y=newdata2$prestige)) + geom_line(aes(y=newdata2$predicted_regression2))

#plot residual vs fitted
plot(prestige_regression, pch=16, which=1)

#subset the data to capture income, education, women and prestige.
multidata <- Prestige[,list(income,education,women,prestige)]
summary(multidata)

##Scatterplot of income education women and prestige
plot(multidata, pch=16, col="blue", main="Matrix Scatterplot of Income, Education, Women and Prestige")

##multivariabte regression between income, prestige, women and education
multi_regression = lm(income ~ education + prestige + women, data=multidata)
summary(multi_regression)

# Plot a correlation graph
newdatacor = cor(multidata[1:4])
corrplot(newdatacor, method = "number")

multi_regression2 = lm(income ~ prestige + women, data=multidata)
summary(multi_regression2)

# Plot model residuals.
plot(multi_regression2, pch=16, which=1)

# fit a model excluding the variable education,  log the income variable.
multi_regression3 = lm(log(income) ~ prestige + women, data=multidata)
summary(multi_regression3)

# Plot model residuals.
plot(multi_regression3, pch=16, which=1)


#Data Analysis for table of variance
l1 <- lm(prestige ~ income + education, data = Prestige)
summary(l1)

l2 <- lm(prestige ~ income + education + type, data = Prestige)
summary(l2)

anova(l1, l2)

any(is.na(Prestige$type))##remove NAs

l3 <- update(l1, subset = !is.na(type))
summary(l3)

anova(l3,l2)

mod1 <- lm(prestige ~ income, data=Prestige)
summary(mod1)
ls(mod1) # objects in mod1

#Scatter plot of Prestige with education and women
ggplot(Prestige, aes(x=education, y=prestige)) + geom_point(aes(colour=women)) + labs(title="Education by Prestige and Women",
                                                                                      x="Education",
                                                                                      y="Prestige")

#Scatter plot of Prestige with education and type showing the trendline with standard error
ggplot(Prestige, aes(x=education, y=prestige)) + geom_point(aes(colour=type)) + geom_smooth(method=lm, se=T) + labs(title="Education by Prestige and Type",
                                                                                                                    x="Education",
                                                                                                                    y="Prestige")

#Scatter plot of Prestige with income and type showing the trendline with standard error
ggplot(Prestige, aes(x=income, y=prestige)) + geom_point(aes(colour=type), alpha=0.5) + geom_smooth(method=lm, se=T) + labs(title="Income by Prestige and Type",
                                                                                                                            x="Income",
                                                                                                                            y="Prestige")

##Knn analysis

set.seed(250)
new_order <- sample(nrow(Prestige))
Prestige <- Prestige[new_order]

## split this shuffled dataset in two: a training set with about 90% of the data, 
## and a test set with about 10%
train <- Prestige[1:92, list(income, women, type)] #training set
test <- Prestige[93:102, list(income, women, type)] #testing set

## Run the k-NN algorithm, with k=3 to start with.
knn_output1 <- kknn(type~., train, test, k=3)
test[, predicted_type:=knn_output1$fitted.values] #add the output to the testing test
#Plot predicted vs dark type for knn, k=3
ggplot(test, aes(x=women, y=income)) +
  geom_point(aes(color=type), size=5, alpha=0.3) +
  geom_point(aes(color=predicted_type)) +
  geom_point(data=train, aes(color=type), alpha=0.4, shape=2) +
  labs(title="Predicted (dark) vs Real (opaque) Type, k=3")

## test a few other values of k, using for loop.
set.seed(250)
k_values <- c(5, 7, 9) #list of k values

for (this_k in k_values){
  
  ## run k-nn
  knn_output2 <- kknn(type~., train, test, k=this_k)
  
  ## predict new values
  test[, predicted_type:=knn_output2$fitted.values]
  
  ## predict results
  plot <- ggplot(test, aes(x=women, y=income)) +
    geom_point(aes(color=type), size=5, alpha=0.3) +
    geom_point(aes(color=predicted_type)) +
    geom_point(data=train, aes(color=type), alpha=0.4, shape=2) +
    labs(title=paste0("Predicted (dark) vs Real (opaque) type, k=", this_k))
  
  print(plot)
  
  ## Print out how many times it mispredicts
  number_wrong = nrow(test[type!=predicted_type])
  pct_wrong = number_wrong/nrow(test)
  print(paste0("With k=", this_k, " k-NN makes ", number_wrong, " incorrect predictions, ", pct_wrong, " percent."))
  
}

## subset the dataset:
for_kmeans <- Prestige[, list(women, income)]

# Check for the optimal number of clusters given the data using elbow method

mydata <- for_kmeans
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


##k-means clustering analysis
set.seed(250)

output <- kmeans(for_kmeans, centers=4)
Prestige[, kmean_type:=output$cluster]

##plotting the predicted clusters with k=4
ggplot(Prestige, aes(x=women, y=income, color=factor(kmean_type))) +
  geom_point() +
  labs(title="Predicted Clusters, K=4")
