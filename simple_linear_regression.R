## Simple Linear Regression

# Importing the data set
data <- read.csv("Book1.csv")

# Splittiing the data into train and test
split <- sample.split(data$Scores, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Simple Linear Regression to the training data set
train_fit <- lm(Scores ~ Hours,
         data = train)

# Finding the coefficient
coef(train_fit)

# Model summary
summary(train_fit)

# Predicting from the test dataset
test_pred <- predict(train_fit, newdata = test)

# Visualizing
ggplot() + 
  geom_point(aes(x = train$Hours,y = train$Scores), colour = 'red') +
  geom_line(aes(x = train$Hours,
                y = predict(train_fit, newdata = train)), colour = 'blue') +
  ggtitle('Scores vs Hours (Training set)') +
  xlab('Hours') +
  ylab('Scores')


ggplot() +
  geom_point(aes(x = test$Hours, y = test$Scores),colour = 'red') +
  geom_line(aes(x = train$Hours,y = predict(train_fit, newdata = train)),
            colour = 'blue') +
  ggtitle('Scores vs Hours (Test set)') +
  xlab('Hours') +
  ylab('Scores')

## Hence, we can see that as the number of study hours increases, scores increases
## Also, from the model summary we saw that "Hours" is the significant variable in 
# predicting scores because its p-value is less than 0.05. i.e the model is 
# statistically significant

## Overall, the model has a good predictor variable

## What will be the predicted score if the student studies 9.25hrs per day?
pred_score <- data.frame(Hours = 9.25)
predict(train_fit,pred_score)

# Hence, we can see the score as 92.87, if a student studies for 9.25 hours a day