-   [Random Forest](#random-forest)
-   [Bagged Tree Ensemble](#bagged-tree-ensemble)
-   [Boosted Tree Ensemble](#boosted-tree-ensemble)
-   [Un-penalised Logistic Regression with hand-crafted Feature
    Engineering](#un-penalised-logistic-regression-with-hand-crafted-feature-engineering)
-   [Conclusion](#conclusion)

1.  Comparing the multiple learning algorithms employed using the
    estimated test error as a measure of success, we shortlist the
    following 3 to predict the dependent variable for the test set.
    1.  Unpenalised Logistic Regression with hand-crafted feature
        engineering (est. test set error = 0.1681)
    2.  Random Forest (est. test set error = 0.1684)
    3.  Bagged Tree ensemble (est. test set error = 0.193)
    4.  Boosted Tree ensemble (estimated test set error = 0.2)

<!-- -->

    rm(list = ls()); gc()

    ##           used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 1629153 49.8    2617084 79.9  2617084 79.9
    ## Vcells 1939132 14.8    8388608 64.0  4008887 30.6

    train = readRDS("../RDA/train.Rda")
    test = readRDS("../RDA/test.Rda")
    m = nrow(train)
    p = ncol(train)-1

Random Forest
-------------

1.  We use the *train-validate* strategy using the train dataset to
    decide on the optimal number of trees to grow for the ensemble.
2.  We build a final ensemble using the entire train dataset and for
    optimal number of trees decided by the earlier step.

<!-- -->

    m = nrow(train)
    dat = rbind.data.frame(train, test)

    trees.n = c(100, 500, 2500, 12500)
    leasterr = 1; bestn = 500
    for (t in trees.n) {
        fit = randomForest(Survived~., data = train, ntree = t, keep.forest = FALSE)
        err = 1-(fit$confusion[1,1]+fit$confusion[2,2])/m
        if (err < leasterr) {
            leasterr = err
            bestn = t
        }
    }

    #   Important to keep.forest = TRUE so predictions can be made
    fit = randomForest(Survived~., data = train, ntree = bestn, keep.forest = TRUE) 
    pred.rf = predict(fit, newdata = test, type = "response")
    pred.rf = as.numeric(levels(pred.rf))[pred.rf]

    out = cbind(PassengerId = rownames(test), Survived = pred.rf)
    write.csv(out, "../data/out.rf.csv", quote = FALSE, row.names = FALSE)

Bagged Tree Ensemble
--------------------

1.  We use the *train-validate* strategy using the train dataset to
    decide on the optimal number of trees to grow for the ensemble.
2.  We build a final ensemble using the entire train dataset and for
    optimal number of trees decided by the earlier step.

<!-- -->

    p = ncol(train)-1

    trees.n = c(100, 500, 2500, 12500)
    leasterr = 1; bestn = 500
    for (t in trees.n) {
        fit = randomForest(Survived~., data = train, ntree = t, mtry = p, keep.forest = FALSE)
        err = 1-(fit$confusion[1,1]+fit$confusion[2,2])/m
        if (err < leasterr) {
            leasterr = err
            bestn = t
        }
    }

    #   Important to keep.forest = TRUE so predictions can be made
    fit = randomForest(Survived~., data = train, ntree = bestn, mtry = p, keep.forest = TRUE) 
    pred.bag = predict(fit, newdata = test, type = "response")
    pred.bag = as.numeric(levels(pred.bag))[pred.bag]

    out = cbind(PassengerId = rownames(test), Survived = pred.bag)
    write.csv(out, "../data/out.bag.csv", quote = FALSE, row.names = FALSE)

Boosted Tree Ensemble
---------------------

1.  The `gbm()` function requires that the dependent/response variable
    be numerical. For the train dataset we need to convert `Survived`
    from `<fct>` to `<int>`.
2.  We use the *train-validate* strategy using the train dataset to
    optimise 3 parameters - number of trees (`n.trees`), depth of each
    tree (`interaction.depth`) and learning rate (`shrinkage`).
3.  The optimal value for `n.trees`, in turn, depends on
    `interaction.depth` and `shrinkage` and for a fit, i.e. specific
    values of `interaction.depth` and `shrinkage`, can be obtained by
    setting the parameter `cv.folds > 1`, and then recovering the best
    value through `gbm.perf()` function.
4.  For the other two parameters we use grid search, setting *accuracy*
    as the measure to optimise to.
5.  The `predict.gbm()` function returns the *logit* values for examples
    and need to be converted to *Probability(Y = 1|x)* by the
    appropriate transformation

<!-- -->

    train$Survived = as.numeric(levels(train$Survived))[train$Survived]

    depth = 1:4
    alpha = c(0.1, 0.01, 0.001)

    est.test.err = 0;
    for (k in 1:5) {
      set.seed(k)
      indx = sample(1:m, 0.8*m)
      
      bestd = 1; bestalpha = 1; bestcutoff = 0; besttrees.n = 1000; leasterr = 1
      for (d in depth) {
        for (a in alpha) {
            fit = gbm(Survived~., data = train[indx,], distribution = "bernoulli", cv.folds = 5,
                            n.trees = 5000, interaction.depth = d, shrinkage = a, 
                            class.stratify.cv = TRUE, keep.data = FALSE)
            trees.n = gbm.perf(fit, plot.it = FALSE, method = "cv")
            
            pred = predict(fit, newdata = train[indx,], n.trees = trees.n)
            prob = exp(pred)/(1+exp(pred))
            
            pred = ROCR::prediction(prob, train$Survived[indx])         #   Using the ROCR library
            perf = performance(pred, measure = "acc")                   #   to decide the best threshold probability
            max.acc.indx = perf@y.values %>% unlist() %>% which.max()   #   to classify an example as '1'
            cutoff = perf@x.values %>% unlist() %>% .[max.acc.indx]
            
            pred = predict(fit, newdata = train[-indx,], n.trees = trees.n)
            prob = exp(pred)/(1+exp(pred))
            pred = ifelse(prob < cutoff, 0, 1)
            err = mean(pred != train$Survived[-indx])
            if (err < leasterr) {
                leasterr = err
                bestd = d 
                bestalpha = a
                bestcutoff = cutoff
                besttrees.n = trees.n
          }
        }
      }
    }  
      
    fit = gbm(Survived~., data = train[indx,], distribution = "bernoulli", n.trees = besttrees.n, 
              interaction.depth = bestd, shrinkage = bestalpha, keep.data = FALSE)
      
    pred = predict(fit, newdata = test, n.trees = besttrees.n)
    prob = exp(pred)/(1+exp(pred))
            
    pred.boost = ifelse(prob < bestcutoff, 0, 1)

    out = cbind(PassengerId = rownames(test), Survived = pred.boost)
    write.csv(out, "../data/out.boost.csv", quote = FALSE, row.names = FALSE)

    train$Survived = factor(train$Survived)

Un-penalised Logistic Regression with hand-crafted Feature Engineering
----------------------------------------------------------------------

1.  We use the best model built previously.

<!-- -->

    fit = readRDS("../RDA/best.logit.Rda")   #   Loading the best logistic model previously built

    prob = predict(fit, newdata = train, type = "response")
    pred = ROCR::prediction(prob, train$Survived)         
    perf = performance(pred, measure = "acc")                   
    max.acc.indx = perf@y.values %>% unlist() %>% which.max()   
    cutoff = perf@x.values %>% unlist() %>% .[max.acc.indx]

    prob = predict(fit, newdata = test, type = "response")
    pred.logit = ifelse(prob < cutoff, 0, 1)

    out = cbind(PassengerId = rownames(test), Survived = pred.logit)
    write.csv(out, "../data/out.logit.feat.engg.csv", quote = FALSE, row.names = FALSE)

Conclusion
----------

1.  We used four models to make predictions for the test dataset. The
    results, from the Kaggle evaluation, are as follows:

**Model Est. Test Error Act. Test Error** Un-penalised logistic
regression  
with hand-crafted feature engg. 0.1681 0.2440

Random Forest ensemble 0.1684 0.2249

Bagged Trees ensemble 0.193 0.2440

Boosted Trees ensemble 0.2 0.2201

1.  Expectedly, for all the 4 models, the actual test error was greater
    than the estimated test error.

2.  Of the four, the Boosted Trees ensemble model performed the best and
    more importantly delivered the least difference between the actual
    and estimated test error.
