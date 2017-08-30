# "Wrapper Feature Selection"


#loading the data only from the first approach
training=read.csv("training_1st_approach.csv", stringsAsFactors = F)
training=training[,-1]

p = training[, 80]#target variable
X_train=training[,-79:-81]#independent features

#Feature Selection wrapper method

#installing a a wrapper package from github
#devtools::install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org"

#calling FeatureSelection package
library(FeatureSelection)
library(xgboost)
library(glmnet)
library(ranger)
library(graphics)
library(ggplot2)
# Glmnet is a package that fits a generalized linear model via penalized maximum likelihood. The regularization path is computed for the lasso or elasticnet penalty at a grid of values for the regularization parameter lambda. It fits linear, logistic, multinomial, poisson, and Cox regression models. 
params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 5, parallel = TRUE)

#Xgboost stands for “Extreme Gradient Boosting” and is a fast implementation of the well known boosted trees. The tree ensemble model of xgboost is a set of classification and regression trees and the main purpose is to define an objective function and optimize it. Xgboost does an additive training and controls model complexity by regularization.

params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.001, "subsample" = 0.75, "max_depth" = 5, "colsample_bytree" = 0.75, "nthread" = 6),
                       nrounds = 1000, print.every.n = 250, maximize = FALSE)

#ranger is a fast implementation of random forest, particularly suited for high-dimensional data. Both random forest and boosted trees are tree ensembles, the only difference is that a random forest trains a number of trees and then these trees are averaged, whereas in boosting the learning of the next tree (N+1) depends on the previous tree (N).
params_ranger = list(dependent.variable.name = 'y', probability = FALSE, num.trees = 1000, verbose = TRUE, mtry = 5, min.node.size = 10, num.threads = 6, classification = FALSE, importance = 'permutation')

params_features = list(keep_number_feat = NULL, union = TRUE)

feat = wrapper_feat_select(X = X_train, y = p, params_glmnet = params_glmnet, params_xgboost = params_xgboost,params_ranger = params_ranger, xgb_sort = 'Gain', CV_folds = 5, stratified_regr = FALSE,  scale_coefs_glmnet = FALSE, cores_glmnet = 5, params_features = params_features, verbose = TRUE)

#structure of the wrapper feature selection
str(feat)
```


```{r}
#ploting the features per model
params_barplot = list(keep_features = 30, horiz = TRUE, cex.names = 1.0)
```


```{r}
barplot_feat_select(feat, params_barplot, xgb_sort = 'Cover')
```

```{r}
#The *func_correlation * function can be used here to return the predictors that are highly correlated with the response

dat = data.frame(p = p, X_train)

cor_feat = func_correlation(dat, target = 'p', correlation_thresh = 0.1, use_obs = 'complete.obs', correlation_method = 'pearson')
#the most highly correlated predictors with target variable:STOPPING_TIME

(cor_feat)
```

```{r}
out_lst = lapply(feat$all_feat, function(x) which(rownames(cor_feat) %in% x[1:100, 1]))

#structure of our results
#It turns out that predictors highly correlated with the response appear in all methods as the out_lst object shows (taking into account the top 83 selected features)
str(out_lst)

```

```{r}
#checking for multicollinearity in lasso
cor_lasso = func_correlation(X_train[, feat$all_feat$`glmnet-lasso`[, 1]], target = NULL, correlation_thresh = 0.60, use_obs = 'complete.obs', correlation_method = 'pearson')


dim(cor_lasso$out_df)[1]  # number of correlated pairs of predictors, lasso
head(cor_lasso$out_df,10)
```


```{r}
#checking for multicollinearity in xgboost
cor_xgb = func_correlation(X_train[, feat$all_feat$xgboost[, 1][1:100]], target = NULL, correlation_thresh = 0.6, use_obs = 'complete.obs', correlation_method = 'pearson')



dim(cor_xgb$out_df)[1]  # there is no multicollinearity in xgboost predictors
```


```{r}
cor_rf = func_correlation(X_train[, feat$all_feat$ranger[, 1][1:100]], target = NULL, correlation_thresh = 0.1,use_obs = 'complete.obs', correlation_method = 'pearson')

dim(cor_rf$out_df)[1]  # there are no correlated pairs of predictors in ranger
```


