house_train <- read.csv("~/R Work/house_prices_train.csv", header = TRUE, stringsAsFactors = FALSE)
house_test <-read.csv("~/R Work/house_prices_test.csv", header = TRUE, stringsAsFactors = FALSE)

# #Some basic eda on train
# str(house_train)
# 
# #First, let's see how the dependent variable is distributed
# hist(log(house_train$SalePrice))
# 
# aggregate(SalePrice ~ MSSubClass, house_train, mean)
#There is some variation with MsSubClass

#Check how many NAs
# sum(is.na(house_train))
# house_train$LotFrontage[is.na(house_train$LotFrontage)] <- mean(na.omit(house_train$LotFrontage))
# sum(is.numeric(house_train))

#Find all the numeric columns
nums <- unlist(lapply(house_train, is.numeric))
house_train_num <- house_train[,nums]
for(i in 1:length(house_train_num))
{
  for(j in 1:nrow(house_train_num))
  {
    if(is.na(house_train_num[j,i]))
    {
      house_train_num[j,i] <- mean(na.omit(house_train_num[,i]))
    }
  }
}
house_train_not_num <- house_train[,!nums]
#Checking how many columns have NULLs
colSums(is.na(house_train_not_num))[colSums(is.na(house_train_not_num)) > 0]

#Now, let's treat the NA's: everything with too many NAs can't be considered
house_train_not_num$Alley[is.na(house_train_not_num$Alley)] <- 'None'
house_train_not_num$PoolQC[is.na(house_train_not_num$PoolQC)] <- 'None'
house_train_not_num$Fence[is.na(house_train_not_num$Fence)] <- 'None'
house_train_not_num$MiscFeature[is.na(house_train_not_num$MiscFeature)] <- 'None'

#The columns with less number of NA's can be replaced with the mode in that column
house_train_not_num$MasVnrType[is.na(house_train_not_num$MasVnrType)] <- mode(na.omit(house_train_not_num$MasVnrType))
house_train_not_num$BsmtQual[is.na(house_train_not_num$BsmtQual)] <- 'None'
house_train_not_num$BsmtCond[is.na(house_train_not_num$BsmtCond)] <- 'None'
house_train_not_num$BsmtExposure[is.na(house_train_not_num$BsmtExposure)] <- 'No basement'
house_train_not_num$BsmtFinType1[is.na(house_train_not_num$BsmtFinType1)] <- 'None'
house_train_not_num$BsmtFinType2[is.na(house_train_not_num$BsmtFinType2)] <- 'None'
house_train_not_num$Electrical[is.na(house_train_not_num$Electrical)] <- mode(na.omit(house_train_not_num$Electrical))
house_train_not_num$GarageType[is.na(house_train_not_num$GarageType)] <- 'None'
house_train_not_num$GarageFinish[is.na(house_train_not_num$GarageFinish)] <- 'None'
house_train_not_num$GarageQual[is.na(house_train_not_num$GarageQual)] <- 'None'
house_train_not_num$GarageCond[is.na(house_train_not_num$GarageCond)] <- 'None'

house_train_not_num$FireplaceQu[is.na(house_train_not_num$FireplaceQu)] <- 'None'

house_train <- cbind(house_train_num, house_train_not_num)

# house_train_dummy <- model.matrix(~., data = house_train, contrasts.arg = lapply(house_train[,39:77], contrasts, contrasts=FALSE))
.libPaths("C://Users/apoorva.bhide/Documents/R library")
library(caret)
house_train_dmy <- dummyVars(" ~ .", data = house_train)
house_train_dmy2 <- data.frame(predict(house_train_dmy, newdata = house_train))

#Now, let's do a PCA and get a scree plot for this
res.pca <- prcomp(house_train_dmy2, scale = TRUE)
library(factoextra)
fviz_eig(res.pca)

#Have to do the same thing for test set
nums_2 <- unlist(lapply(house_test, is.numeric))
house_test_num <- house_test[,nums]
for(i in 1:length(house_test_num))
{
  for(j in 1:nrow(house_test_num))
  {
    if(is.na(house_test_num[j,i]))
    {
      house_test_num[j,i] <- mean(na.omit(house_test_num[,i]))
    }
  }
}
house_test_not_num <- house_test[,!nums]
#Checking how many columns have NULLs
colSums(is.na(house_test_not_num))[colSums(is.na(house_test_not_num)) > 0]

#Now, let's treat the NA's: everything with too many NAs can't be considered
house_test_not_num$Alley[is.na(house_test_not_num$Alley)] <- 'None'
house_test_not_num$MSZoning[is.na(house_test_not_num$MSZoning)] <- mode(na.omit(house_test_not_num$MSZoning))
house_test_not_num$MasVnrType[is.na(house_test_not_num$MasVnrType)] <- mode(na.omit(house_test_not_num$MasVnrType))
house_test_not_num$Utilities[is.na(house_test_not_num$Utilities)] <- mode(na.omit(house_test_not_num$Utilities))
house_test_not_num$BsmtQual[is.na(house_test_not_num$BsmtQual)] <- 'None'
house_test_not_num$MiscFeature[is.na(house_test_not_num$MiscFeature)] <- 'None'
house_test_not_num$Fence[is.na(house_test_not_num$Fence)] <- 'None'
house_test_not_num$FireplaceQu[is.na(house_test_not_num$FireplaceQu)] <- 'None'
house_test_not_num$PoolQC[is.na(house_test_not_num$PoolQC)] <- 'None'
house_test_not_num$GarageType[is.na(house_test_not_num$GarageType)] <- 'None'
house_test_not_num$GarageFinish[is.na(house_test_not_num$GarageFinish)] <- 'None'
house_test_not_num$GarageQual[is.na(house_test_not_num$GarageQual)] <- 'None'
house_test_not_num$GarageCond[is.na(house_test_not_num$GarageCond)] <- 'None'
house_test_not_num$BsmtCond[is.na(house_test_not_num$BsmtCond)] <- 'None'
house_test_not_num$BsmtExposure[is.na(house_test_not_num$BsmtExposure)] <- 'None'
house_test_not_num$BsmtFinType1[is.na(house_test_not_num$BsmtFinType1)] <- 'None'
house_test_not_num$BsmtFinType2[is.na(house_test_not_num$BsmtFinType2)] <- 'None'
house_test_not_num$Exterior1st[is.na(house_test_not_num$Exterior1st)] <- mode(na.omit(house_test_not_num$Exterior1st))
house_test_not_num$Exterior2nd[is.na(house_test_not_num$Exterior2nd)] <- mode(na.omit(house_test_not_num$Exterior2nd))
house_test_not_num$KitchenQual[is.na(house_test_not_num$KitchenQual)] <- mode(na.omit(house_test_not_num$KitchenQual))
house_test_not_num$Functional[is.na(house_test_not_num$Functional)] <- mode(na.omit(house_test_not_num$Functional))
house_test_not_num$SaleType[is.na(house_test_not_num$SaleType)] <- mode(na.omit(house_test_not_num$SaleType))

#Let's create a very basic, baseline, base submission
basic_linear <- lm(log(SalePrice) ~ GrLivArea, house_train)
summary(basic_linear)
basic_submission <- cbind(house_test$Id, predict(basic_linear, house_test))
colnames(basic_submission) <- c("Id", "SalePrice")
basic_submission[,2] <- exp(basic_submission[,2])
write.csv(basic_submission, "~/R Work/basic_sub.csv")
#Score(Error) was 0.29619, rank was 4627, 332 ranks from the bottom. Long way to the top :(

#Let's try a basic PCA model, just to see if it performs better than this baseline lin.reg.
base_pca <- prcomp(house_train_dmy2, scale = TRUE, center = TRUE)
scores_base <- base_pca$x
mod_base_pca <- lm(log(house_train_dmy2$SalePrice) ~ scores_base[,1:4])
summary(mod_base_pca)
basic_pca_submission <- cbind(house_test$Id, predict(mod_base_pca, house_test))
colnames(basic_pca_submission) <- c("Id", "SalePrice")
basic_pca_submission <- basic_pca_submission[-1460,]
basic_pca_submission[,2] <- exp(basic_pca_submission[,2])
write.csv(basic_pca_submission, "~/R Work/basic_pca.csv")
#Well, that bombed. The error rate was 0.55. Let's go back to the linear model, use forward selection.

#Let's use the same basic linear model, but with 10-fold cross-validation.
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
basic_linear_cv <- train(log(SalePrice) ~ GrLivArea, data = house_train_dmy2, method = "lm",
               trControl = train.control)
print(basic_linear_cv)
basic_submission_cv <- cbind(house_test$Id, predict(basic_linear_cv, house_test))
colnames(basic_submission_cv) <- c("Id", "SalePrice")
basic_submission_cv[,2] <- exp(basic_submission_cv[,2])
write.csv(basic_submission_cv, "~/R Work/basic_cv_sub.csv")
#Rank is now 4630. Great.
