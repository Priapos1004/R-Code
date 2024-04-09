library(tidyverse)

bank = read.csv("data/Banknotes.csv")

par(mfrow=c(2,3)) # Boxplots arrangement
par(mar=c(2,2,2,2)) # sets outer margins
for (i in 2:ncol(bank)){
  boxplot(bank[,i] ~ Group,data=bank, main=names(bank)[i])
}

# model to predict features by group
bank.manova <- manova(cbind(Length,HeightL,HeightR,DistLB,DistUB,LengthD) ~ Group,data=bank)

summary(bank.manova, test="Hotelling-Lawley")

summary.aov(bank.manova)

features <- bank %>% subset(select = -Group)
pol.pca <- prcomp(features) # based on svd
head(pol.pca$x)

par(mfrow=c(1,1))
plot(pol.pca, type="line")

biplot(pol.pca, xlabs=substring(bank$Group,1,1))
abline(h=0)
abline(v=0)

### LDA ###
library(MASS)

bank.lda <- lda(Group ~ .,data=bank) # Using formula interface: Group on all the others
bank.lda

plot(bank.lda) # histogram

plot(bank.lda, type="density") # smooth distribution

discriminant_scores <- as.matrix(features) %*% bank.lda$scaling # discriminant scores
discriminant_scores

predict(bank.lda, head(features)) # posterior probabilities + allocated group

pred.gr <- c("Predict Fake","Predict Genuine")
table(bank$Group, factor(predict(bank.lda)$class,labels=pred.gr))

### leave-one-out cross validation

library(caret)

control <- trainControl(method = "LOOCV")
bank.lda.cv <- train(Group ~ ., data = bank, method = "lda", trControl = control)

pred.gr <- c("Predict Fake","Predict Genuine")
table(bank$Group, factor(predict(bank.lda.cv),labels=pred.gr))

### correlation

corr_dscores <- cor(cbind(features, discriminant_scores))
corr_dscores

new <- list(Length=214.70, HeightL=131.12, HeightR=131.01, DistLB=10.63, DistUB=10.99, LengthD=140.13)
new <- as.data.frame(new) # Create data.frame ...
names(new) <- names(bank[,2:7]) # ... with the same variable names

predict(bank.lda, new)
