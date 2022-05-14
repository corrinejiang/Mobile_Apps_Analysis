###### Final Project Code ######
gamestore = read.csv("/Users/SandyChen/Desktop/MGSC661/Google_Play_Store.csv")

# ------------ DATA PRE-PROCESSING ------------
gamestore$Category = as.factor(gamestore$Category)
gamestore$Type = as.factor(gamestore$Type)
gamestore$Content.Rating = as.factor(gamestore$Content.Rating)
gamestore$Installs <- as.numeric(gsub('\\+|,', '', gamestore$Installs))
gamestore <- na.omit(gamestore)
attach(gamestore)
table(Category)
# ------------ DATA EXPLORATION ------------
library(corrplot)
library(ggplot2)
m=cor(gamestore[,c(3,4,5,11,14)])
corrplot(m, method="circle",col=colorRampPalette(c("orange","white","tomato1"))(200))
ggplot(data=gamestore,aes(x=Category)) + geom_bar(fill="tomato1")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(data=gamestore,aes(x=Rating)) + geom_bar(fill="tomato1")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(data=gamestore,aes(x=Type)) + geom_bar(fill="tomato1")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(gamestore) +
  geom_bar(aes(x=Category,y=Reviews),fill="tomato1",stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(gamestore,aes(x=Category),las=2)+ geom_bar(fill = "tomato1")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(gamestore, aes(x=Category, y=Rating),las=2) + geom_point(color = "tomato1")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(gamestore, aes(x=Reviews, y=Rating))+geom_point(color = "tomato1")
ggplot(gamestore, aes(x=Size, y=Rating))+geom_point(color = "tomato1")
ggplot(gamestore, aes(x=price, y=Rating))+geom_point(color = "tomato1")
ggplot(gamestore, aes(x=Content.Rating))+geom_bar(fill = "tomato1")
game_var = gamestore[,c(3,4,5,11,14)]
pca=prcomp(game_var, scale=TRUE) 
pca
library(ggfortify)
autoplot(pca, data = game_var, loadings = TRUE, col=ifelse(game_var$Rating >=4,"tomato1","tan1"), loadings.label = TRUE )

# ------------ MODEL FITTING ------------
## Model 1: Random Forest ------------
library(randomForest)
myforest=randomForest(Rating~Category+Reviews+Type+Content.Rating+Installs+price+Size, ntree=1000, data=gamestore, importance=TRUE, na.action = na.omit,do.trace=100)
myforest
importance(myforest)
varImpPlot(myforest,bg = 'tomato1',cex = 1,main = 'Feature Importance')

## Model 2: Boosting ------------
library(gbm)
boosted=gbm(Rating~Category+Size+Reviews+Type+Content.Rating+Installs+price,
            data=gamestore,distribution="gaussian",n.trees=1000, interaction.depth=4)
summary(boosted,las = 2)

predicted_score=predict(boosted, newdata=gamestore, n.trees=1000) 
mean((predicted_score-Rating)^2) #MSE calculation

## Cross Validation for boosting model (Validation Set Test)
library(caTools)
### cross validation for the boosting model:
sample = sample.split(gamestore$Rating, SplitRatio = 0.9)
train = subset(gamestore, sample == TRUE)
test = subset(gamestore, sample == FALSE)
fit = gbm(Rating~Category+Size+Reviews+Type+Content.Rating+Installs+price,
          data=train,distribution="gaussian",n.trees=1000, interaction.depth=4)
test$pred = predict(fit, test)
test$res = (test$Rating-test$pred)
test$res_sq = (test$res)^2
mean(test$res_sq)
### cross validation for the random forest model:
fit2 = randomForest(Rating~Category+Reviews+Type+Content.Rating+Installs+price+Size, ntree=300, data=train, importance=TRUE, na.action = na.omit,do.trace=100)
test$pred2 = predict(fit2, test)
test$res2 = (test$Rating-test$pred2)
test$res_sq2 = (test$res2)^2
mean(test$res_sq2)

# ------------ MODEL VISUALIZATION ------------
# Visualize marginal effects of variables in the boosted model
plot.gbm(boosted, i.var=3) # Review
plot.gbm(boosted, i.var=2) # size
plot.gbm(boosted, i.var=6) # install
plot.gbm(boosted, i.var=7) # price

# ------------ FINAL MODEL ------------
final=randomForest(Rating~Category+Size+Reviews+Type+Content.Rating+Installs+price, ntree=300, data=gamestore, importance=TRUE, na.action = na.omit,do.trace=100)
summary(final)

# Thank you for grading :) ------------------------------------
