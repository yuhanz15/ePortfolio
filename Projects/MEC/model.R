library(tidyverse)
library(caret)
library(keras3)
library(tensorflow)
library(reticulate)
library(glmnet)
library(nnet)
library(corrplot)
library(ggcorrplot)
library(ModelMetrics)
library(modelsummary)
library(stargazer)
library(ggplot2)

########## lasso
set.seed(123)
temp <- read.csv("MEC.csv")
emo <- temp %>%
  count(Emotion)
temp <- temp %>%
  select(-c(X, X.1, X.2, X.3, id))
unique(temp$Emotion)

train_i <- sample(1:nrow(temp), size=0.8*nrow(temp))
train <- temp[train_i, ]
test <- temp[-train_i, ]

X <- train %>% 
  select(-c(valence_mean, arousal_mean, valence_std, arousal_std,
            Radius, Emotion, Angle))
X <- as.matrix(X)
Y_arousal <- as.matrix(train$valence_mean)
Y_valence <- as.matrix(train$arousal_mean)

test_X <- test %>%
  select(-c(valence_mean, arousal_mean, valence_std, arousal_std,
            Radius, Emotion, Angle))
test_X <- as.matrix(test_X)
test_Y_a <- as.matrix(test$arousal_mean)
test_Y_v <- as.matrix(test$valence_mean)

model_a <- glmnet(X, Y_arousal, alpha=1)
cv_lasso_a <- cv.glmnet(X, Y_arousal, alpha=1)
lambda_a <- cv_lasso_a$lambda.min
lasso_a <- glmnet(X, Y_arousal, newx=test_X, lambda=lambda_a)

model_v <- glmnet(X, Y_valence, alpha=1)
cv_lasso_v <- cv.glmnet(X, Y_valence, alpha=1)
lambda_v <- cv_lasso_v$lambda.min
lasso_v <- glmnet(X, Y_valence, alpha=1, lambda=lambda_v)

v_pred <- predict(lasso_v, newx=test_X, s="lambda.min")
a_pred <- predict(lasso_a, newx=test_X, s="lambda.min")
emo_test <- test[,"Emotion"]
Angle <- atan2(a_pred, v_pred) *180/pi
Angle[Angle < 0] <- Angle[Angle < 0] + 360

coef_min <- as.matrix(coef(cv_lasso_a))
coef_df <- data.frame(
  Variable=rownames(coef_min),
  Coefficient = coef_min[,1]
)
coef_df <- coef_df[coef_df$Coefficient != 0, ]
stargazer(coef_df, summary=FALSE, rownames=FALSE)



# predict quadrant
q_test <- as.factor(case_when(
  test$valence_mean >= 0 & test$arousal_mean >= 0 ~ 1,
  test$valence_mean >= 0 & test$arousal_mean < 0 ~ 4,
  test$valence_mean < 0 & test$arousal_mean >= 0 ~ 2,
  test$valence_mean < 0 & test$arousal_mean < 0 ~ 3
))
q_pred <- as.factor(case_when(
  v_pred >= 0 & a_pred >= 0 ~ 1,
  v_pred >= 0 & a_pred < 0 ~ 4,
  v_pred < 0 & a_pred >= 0 ~ 2,
  v_pred < 0 & a_pred < 0 ~ 3
))
table(test=q_test, pred=q_pred)
sum(q_test == q_pred)/nrow(test)

## emotion

emo_pred <- case_when(
    30 > Angle | Angle == 360 ~ "Excited",
    30 <= Angle & 60 > Angle ~ "Pleased", 
    60 <= Angle & 90 > Angle ~ "Happy",
    90 <= Angle & 120 > Angle ~ "Annoying",
    120 <= Angle & 150 > Angle ~ "Nervous",
    150 <= Angle & 180 > Angle ~ "Angry",
    180 <= Angle & 210 > Angle ~ "Sad",
    210 <= Angle & 240 > Angle ~ "Sleepy", 
    240 <= Angle & 270 > Angle ~ "Bored",
    270 <= Angle & 300 > Angle ~ "Relaxed",
    300 <= Angle & 330 > Angle ~ "Calm",
    330 <= Angle & 360 > Angle ~ "Peaceful")
table(test=emo_test, pred=emo_pred)
sum(as.factor(emo_test)==as.factor(emo_pred))/length(emo_test)
ggplot(data.frame(v_pred, a_pred, emo_pred), aes(x=v_pred, y=a_pred, color=emo_pred))+
  geom_point()
summary(as.factor(temp$Emotion))

####### multinom
data <- train %>%
  select(-c(arousal_mean, valence_mean, arousal_std, valence_std))
model <- multinom(Emotion ~ ., data)
test_data <- test %>%
  select(-c(arousal_mean, valence_mean, arousal_std, valence_std))
pred <- predict(model, newdata=test_data, type="class")
sum(pred==test_data[,"Emotion"])/nrow(test)
table(test=test_data$Emotion, pred=pred)

# multinom with quadrant
train <- train %>%
  mutate(Q = as.factor(case_when(
    valence_mean >= 0 & arousal_mean >= 0 ~ 1,
    valence_mean >= 0 & arousal_mean < 0 ~ 4,
    valence_mean < 0 & arousal_mean >= 0 ~ 2,
    valence_mean < 0 & arousal_mean < 0 ~ 3
  )))
test <- test %>%
  mutate(Q = as.factor(case_when(
    valence_mean >= 0 & arousal_mean >= 0 ~ 1,
    valence_mean >= 0 & arousal_mean < 0 ~ 4,
    valence_mean < 0 & arousal_mean >= 0 ~ 2,
    valence_mean < 0 & arousal_mean < 0 ~ 3
  )))
data <- train %>%
  select(-c(arousal_mean, valence_mean, arousal_std, valence_std, Emotion))
model <- multinom(Q ~ ., data)
test_data <- test %>%
  select(-c(arousal_mean, valence_mean, arousal_std, valence_std, Emotion))
pred <- predict(model, newdata=test_data, type="class")
sum(pred==test_data$Q)/nrow(test_data)
table(test=test_data$Q, pred=pred)



























