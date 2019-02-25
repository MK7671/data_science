require(ISLR)

names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)
cor(Smarket[,-9]) #Pairwise correlation

####Logistic regression####
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit) # or summary(glm.fit)$coef

glm.probs = predict(glm.fit, type="response")
glm.probs[1:5]
glm.pred = ifelse(glm.probs > 0.5, 'Up', 'Down')
# instead of using ifelse, we can do glm.pred[glm.probs > 0.5] = "Up"
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)


# Make training and test set
train = Year < 2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=Smarket[!train, ], type="response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)


# Fit smaller model
glm.fit = glm(Direction~Lag1+Lag2, 
              data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)



#### LDA ####
library(MASS) #lda function is part of MASS library

# ida is function for LDA model
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket, Year==2005)
lda.pred = predict(lda.fit, Smarket.2005)
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

# Applying 50% threshold to the posterior probabilities allows us to 
# recreate the predictions contained in lda.pred$class
sum(lda.pred$posterior[,1] >= .5)


#### Quadratic Discriminant Analysis ####
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=Year < 2005)
qda.fit
qda.pred = predict(qda.fit, Smarket.2005)
table(qda.pred$class, Smarket.2005$Direction)
mean(qda.pred$class==Smarket.2005$Direction)
     

#### K-Nearest Neighbors ####
library(class)
?knn
attach(Smarket) #search()
Xlag = cbind(Lag1, Lag2)
train=Year < 2005
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])
