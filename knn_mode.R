library(class)
library(dplyr)
library(caret)

#df <- read.csv('creditcardFraud.csv')
col_names<- names(df)
dim(df)

round(prop.table(table(df$class)),3)
ncols <- dim(df)[2]-1
col_var <- rep(0, ncols)

for (i in 1:ncols) {
  value <- round(var(df[col_names[i]]),3)
  col_var[i]<- value
}
print(col_var)
standardized.X= scale(df [,-31])
nrow<- dim(df)[1]
test=1:round(nrow*0.3)
train.X= standardized.X[-test ,]
test.X= standardized.X[test ,]

train.Y= factor(class)[-test]
test.Y= factor(class)[test]
set.seed(1)
knn1.pred <- knn(train.X, test.X,train.Y,k=1)
knn3.pred <- knn(train.X, test.X,train.Y,k=3)
knn5.pred <- knn(train.X, test.X,train.Y,k=5)

result1<- confusionMatrix(knn1.pred, test.Y)
result3<- confusionMatrix(knn3.pred, test.Y)
result5<- confusionMatrix(knn5.pred, test.Y)

print(result1)
print(result3)
print(result5)
