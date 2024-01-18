# install.packages()
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("car")
install.packages("caret")
install.packages("visdat")
install.packages("pheatmap")
install.packages("kernlab")

# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(tidyverse)   # For data transformation and visualization
library(corrplot)
library(car)
library(caret)
library(visdat)
library(pheatmap)
library(kernlab)

# Loading the Dataset
# Load the E-commerce shipping dataset
Ecommerce_data <- read.csv("Train.csv")

# Exploratory Data Analysis (EDA)
summary(Ecommerce_data)  # Summary statistics of the dataset
str(Ecommerce_data)  # Structure of the dataset (variable types)
colSums(is.na(Ecommerce_data))  # Check for missing values in each column
# Checking for null values in dataset
sum(is.null(Ecommerce_data))
vis_miss(Ecommerce_data)

#Column 1 (ID) is just index number of rows and we don't need it.
Ecommerce_data <- Ecommerce_data[, -1]

#Some columns are not in the right structure. Change the structure of these columns into factor.

col.factor <- c("Warehouse_block", "Mode_of_Shipment", "Product_importance", "Gender", "Reached.on.Time_Y.N")
Ecommerce_data[col.factor] <- lapply(Ecommerce_data[col.factor], factor)

# lets look at the summary of the data and see changes made

summary(Ecommerce_data)

# Create a summary table of delivery time categories
delivery_summary <- table(Ecommerce_data$Reached.on.Time_Y.N)

# Create a pie chart
pie_chart <- ggplot(data = as.data.frame(delivery_summary), aes(x = "", y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Distribution of Delivery Time",
       fill = "Delivery Time",
       x = NULL,
       y = NULL)

# Display the pie chart
print(pie_chart)

# Bar plots for each categorical variable

#Warehouse with highest calls Customer care calls 
ware<-aggregate(Ecommerce_data$Customer_care_calls~Ecommerce_data$Warehouse_block,FUN=sum)
View(ware)

#Male vs Female retained customers 
rtn <- table(Ecommerce_data$Gender)
View(rtn)
# Create a vector of colors
bar_colors <- c("red", "skyblue1")
# Create the bar plot with colors
barplot(rtn, ylab = "Retaining", main = "Male vs Female retained customers", col = bar_colors)
text(rtn, paste(rtn, 0, sep = ""), cex = 1, pos = 1)

rtn1<-xtabs(~Prior_purchases+Gender,data=Ecommerce_data)
barplot(rtn1)

#Shipment mode With most customers calls
shp <- aggregate(Ecommerce_data$Customer_care_calls ~ Ecommerce_data$Mode_of_Shipment, FUN = sum)

# Create a color vector for the bars
bar_colors <- c("skyblue", "lightgreen", "lightcoral", "lightgoldenrodyellow", "lightsteelblue")
# Create the bar plot with colors
barplot(shp$`Ecommerce_data$Customer_care_calls`,
        names.arg = shp$`Ecommerce_data$Mode_of_Shipment`,
        col = bar_colors,
        main = "Shipment mode With most customers calls",
        xlab = "Mode", ylab = "Calls")

#Who Purchased Imp products
imp<-table(Ecommerce_data$Product_importance,Ecommerce_data$Gender)
imp

barplot(imp,beside = TRUE, 
        col = c("Red", "skyblue1","skyblue3"), 
        main = "Who Purchased Imp products", 
        width=c(2,3),
        legend.text = rownames(imp),
        args.legend = list(cex=1.5,x = "topleft"))

# Which shipment reached on time
tme<-Ecommerce_data[,c(3,11)]
tme
tme<-table(tme)
barplot(tme,beside = TRUE,
        main = "Which shipment reached on time",
        col = c("cyan2","cornsilk3","darkcyan"),
        legend.text = rownames(tme),
        args.legend = list(cex=0.5,x = "topleft"))



ggplot(Ecommerce_data, aes(Cost_of_the_Product, fill= Reached.on.Time_Y.N)) +
  geom_density(alpha = 0.5)

# Convert categorical columns to numeric
Ecommerce <- Ecommerce_data %>%
  mutate(
    Warehouse_block = as.numeric(Warehouse_block),
    Mode_of_Shipment = as.numeric(Mode_of_Shipment),
    Product_importance = as.numeric(Product_importance),
    Gender = as.numeric(Gender)
  )

# Convert non-numeric columns to numeric 
Ecommerce_numeric <- as.data.frame(lapply(Ecommerce, as.numeric))

# Calculate Kendall's correlation
corr1 <- cor(Ecommerce_numeric, method = "kendall")

corrplot(corr1,method = "circle",
         type = "lower",outline = T,
         addgrid.col = "darkgray",order="hclust",
         mar = c(0,0,0,4),addrect = 4,
         rect.col = "black", rect.lwd = 5,
         cl.pos = "b", tl.col = "red",
         tl.cex =0.5, cl.cex = 0.5)

# Identifying Main Causes of Late Shipment
round(prop.table(table(Ecommerce_data$Reached.on.Time_Y.N, Ecommerce_data$Mode_of_Shipment), margin = 2)*100, 2)
round(prop.table(table(Ecommerce_data$Reached.on.Time_Y.N, Ecommerce_data$Warehouse_block), margin = 2)*100, 2)
round(prop.table(table(Ecommerce_data$Reached.on.Time_Y.N, Ecommerce_data$Prior_purchases), margin = 2)*100, 2)
round(prop.table(table(Ecommerce_data$Reached.on.Time_Y.N, Ecommerce_data$Product_importance), margin = 2)*100, 2)

p1 <-preProcess(Ecommerce[,c(1:11)],
                method=c("center","scale"))
Ecommerce <- predict(p1, Ecommerce[,c(1:11)])

str(Ecommerce)
# Install and load the caret package
# install.packages("caret")
# library(caret)

set.seed(77)
# Shuffle the rows of the data frame
Ecommerce_shuffled <- Ecommerce %>%
  sample_n(size = nrow(Ecommerce),
           replace = FALSE)

#create a data partition to split data into 80% for training and 20% for testing
 
intrain <- createDataPartition(y = Ecommerce_shuffled$Reached.on.Time_Y.N, p= 0.8, list = FALSE)
training <- Ecommerce_shuffled[intrain,]
testing <- Ecommerce_shuffled[-intrain,]

dim(training)
dim (testing)

#set up the train control method. this computes all the computational overhead prior to training
trctrl <- trainControl(method= "repeatedcv",
                       number = 10,
                       repeats = 5)

#train the model with the train() method
#install.packages("kernlab")
#library(kernlab)
set.seed(7)
svm_Linear <- train(Reached.on.Time_Y.N~.,data = training,
                    method ="svmLinear",trControl = trctrl,
                    preProcess = c("center", "scale"),tuneLength =10)

#check the result of the train() method svm_Linear
test_pred <-predict(svm_Linear, newdata = testing)
test_pred

# using the confusion matrix
confusionMatrix(table(test_pred,
                      testing$Reached.on.Time_Y.N))

#create a grid
grid <-expand.grid(C=c(0, 0.01, 0.05, 0.1, 0.25, 0.5,0.75, 1, 1.25, 1.5, 1.75, 2.5))

#retrain your model with the grid to know the best value of c
svm_Linear_Grid <- train(Reached.on.Time_Y.N~.,data = training,
                         method = "svmLinear", trControl = trctrl,
                         preProcess =c("center", "scale"), tuneGrid = grid,
                         tuneLength = 10)

svm_Linear_Grid

#plot the SVM
plot(svm_Linear_Grid)

test_pred_Grid <-predict(svm_Linear_Grid, newdata = testing)
test_pred_Grid

confusionMatrix(table(test_pred_Grid,testing$Reached.on.Time_Y.N))

#Compare SVM result to KNN

#train the KNN model
set.seed(7)
fit.knn <- train(Reached.on.Time_Y.N~., data=training,
                 method="knn", preProcess = c("center", "scale"),
                 trControl=trainControl())
#train the SVM model
set.seed(7)
fit.svm <-train(Reached.on.Time_Y.N~., data=training,
                method="svmLinear",preProcess =c("center", "scale"),
                trControl=trainControl())

#Compare algorithms

comp <- resamples(list(SVM=fit.svm, KNN=fit.knn))
summary(comp)

