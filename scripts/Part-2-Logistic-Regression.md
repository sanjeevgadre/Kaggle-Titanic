-   [Logistic Regression with hand-crafted Feature
    Engineering](#logistic-regression-with-hand-crafted-feature-engineering)

1.  We use the train and test subsets processed earlier.
2.  We use the *train-validate-test* strategy to choose the optimal
    parameters for the different models and then estimate test error for
    a learning algorithm based on such a model

<!-- -->

    train = readRDS("../RDA/train.Rda")
    m = nrow(train)

Logistic Regression with hand-crafted Feature Engineering
---------------------------------------------------------

1.  We use the *train-test* strategy to estimate the test error.
2.  At each pass we will use the p-values of the included features to
    decide on additional features to be included or engineered.
3.  We begin with an unpenalised logistic regression that includes all
    features

<!-- -->

    f1 = paste("Survived", "~", ".", sep = "")
    F = as.formula(f1)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx])
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.186"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6952  -0.6026  -0.4076   0.6148   2.4938  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  4.328766   0.495394   8.738  < 2e-16 ***
    ## Pclass2     -1.057640   0.305214  -3.465  0.00053 ***
    ## Pclass3     -2.350519   0.312693  -7.517 5.60e-14 ***
    ## Sexmale     -2.698870   0.201850 -13.371  < 2e-16 ***
    ## Age         -0.043214   0.008233  -5.249 1.53e-07 ***
    ## SibSp       -0.343676   0.110323  -3.115  0.00184 ** 
    ## Parch       -0.088697   0.120128  -0.738  0.46030    
    ## Fare         0.001970   0.002454   0.803  0.42224    
    ## EmbarkedQ   -0.146866   0.384275  -0.382  0.70232    
    ## EmbarkedS   -0.414524   0.240379  -1.724  0.08462 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  780.63  on 881  degrees of freedom
    ## AIC: 800.63
    ## 
    ## Number of Fisher Scoring iterations: 5

1.  The p-values for features indicate that `Parch` and `Fare` are not
    significant to the model. However we do not drop these features at
    this point.
2.  We now add engineered features. From the EDA we remember that
    `Survived` was co-related to `Pclass`, `Sex` and `Embarked`. We add
    interaction terms including these features to our model.

<!-- -->

    f2 = paste(f1, "+Pclass:Sex+Pclass:Embarked+Sex:Embarked", sep = "")
    F = as.formula(f2)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1843"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1484  -0.6165  -0.4245   0.3730   2.5803  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        5.547643   0.851273   6.517 7.18e-11 ***
    ## Pclass2           -0.827825   1.130678  -0.732 0.464078    
    ## Pclass3           -3.854898   0.794937  -4.849 1.24e-06 ***
    ## Sexmale           -4.048807   0.710899  -5.695 1.23e-08 ***
    ## Age               -0.048690   0.009117  -5.341 9.26e-08 ***
    ## SibSp             -0.293331   0.112522  -2.607 0.009137 ** 
    ## Parch             -0.011057   0.129056  -0.086 0.931726    
    ## Fare               0.001187   0.002494   0.476 0.634180    
    ## EmbarkedQ         -0.525939   3.753068  -0.140 0.888553    
    ## EmbarkedS         -0.365611   0.649159  -0.563 0.573295    
    ## Pclass2:Sexmale   -0.576145   0.840869  -0.685 0.493232    
    ## Pclass3:Sexmale    2.348804   0.697187   3.369 0.000754 ***
    ## Pclass2:EmbarkedQ  1.342344   5.544150   0.242 0.808688    
    ## Pclass3:EmbarkedQ  0.958971   3.746369   0.256 0.797971    
    ## Pclass2:EmbarkedS -0.272196   0.887195  -0.307 0.758991    
    ## Pclass3:EmbarkedS -0.452676   0.541778  -0.836 0.403415    
    ## Sexmale:EmbarkedQ -1.579089   0.891687  -1.771 0.076577 .  
    ## Sexmale:EmbarkedS  0.256758   0.568990   0.451 0.651807    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  740.75  on 873  degrees of freedom
    ## AIC: 776.75
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error has decreased.
2.  From the summary we infer:
    1.  the interaction term `Pclass:Sex` is significant but
        `Pclass:Embarked` isn't. We make the necessary changes and rerun
        the model.
    2.  the term `Embarked` is not significant. However, since we are
        retaining the interaction term `Sex:Embarked`, we retain the
        `Embarked` term as well. We also remember from the EDA that the
        test subset had a noticable difference for `Embarked` from the
        train subset and that is another reason to retain the term.

<!-- -->

    f3 = paste(f1, "+Pclass:Sex+Sex:Embarked", sep = "")
    F = as.formula(f3)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1776"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0963  -0.6241  -0.4232   0.3710   2.5799  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        5.819690   0.802527   7.252 4.12e-13 ***
    ## Pclass2           -0.977815   0.775646  -1.261 0.207436    
    ## Pclass3           -4.173134   0.695938  -5.996 2.02e-09 ***
    ## Sexmale           -4.140246   0.718383  -5.763 8.25e-09 ***
    ## Age               -0.048884   0.009094  -5.375 7.64e-08 ***
    ## SibSp             -0.294856   0.112044  -2.632 0.008498 ** 
    ## Parch             -0.007238   0.128482  -0.056 0.955074    
    ## Fare               0.000799   0.002452   0.326 0.744557    
    ## EmbarkedQ          0.484001   0.577215   0.839 0.401744    
    ## EmbarkedS         -0.748341   0.463720  -1.614 0.106575    
    ## Pclass2:Sexmale   -0.626989   0.827716  -0.757 0.448755    
    ## Pclass3:Sexmale    2.320626   0.691491   3.356 0.000791 ***
    ## Sexmale:EmbarkedQ -1.482915   0.880416  -1.684 0.092117 .  
    ## Sexmale:EmbarkedS  0.404610   0.542543   0.746 0.455809    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  741.57  on 877  degrees of freedom
    ## AIC: 769.57
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error has reduced.
2.  We add a third order interaction terms for the three variables
    `Pclass`, `Sex`, `Embarked`.
3.  We also add interaction terms for `Pclass` and `Age` & `Sex` and
    `Age` as these features are stratified differently.

<!-- -->

    f4 = paste(f3, "+Pclass:Sex:Embarked+Pclass:Age+Sex:Age", sep = "")
    F = as.formula(f4)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1793"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8178  -0.6435  -0.4034   0.3449   3.2673  
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)                  3.403e+00  1.348e+00   2.524   0.0116 *
    ## Pclass2                      1.353e+01  5.414e+02   0.025   0.9801  
    ## Pclass3                     -2.148e+00  1.379e+00  -1.557   0.1194  
    ## Sexmale                     -2.498e+00  1.294e+00  -1.930   0.0536 .
    ## Age                          9.845e-03  2.356e-02   0.418   0.6761  
    ## SibSp                       -2.976e-01  1.157e-01  -2.572   0.0101 *
    ## Parch                       -7.718e-02  1.298e-01  -0.595   0.5521  
    ## Fare                         1.927e-03  2.538e-03   0.759   0.4477  
    ## EmbarkedQ                    1.196e+01  1.455e+03   0.008   0.9934  
    ## EmbarkedS                   -5.259e-01  1.248e+00  -0.421   0.6735  
    ## Pclass2:Sexmale             -1.315e+01  5.414e+02  -0.024   0.9806  
    ## Pclass3:Sexmale              1.813e+00  1.259e+00   1.441   0.1497  
    ## Sexmale:EmbarkedQ           -2.647e+01  2.058e+03  -0.013   0.9897  
    ## Sexmale:EmbarkedS            4.640e-01  1.314e+00   0.353   0.7241  
    ## Pclass2:Age                 -6.705e-02  2.634e-02  -2.545   0.0109 *
    ## Pclass3:Age                 -3.527e-02  2.223e-02  -1.587   0.1125  
    ## Sexmale:Age                 -4.494e-02  2.082e-02  -2.159   0.0309 *
    ## Pclass2:Sexfemale:EmbarkedQ -1.169e+01  1.863e+03  -0.006   0.9950  
    ## Pclass3:Sexfemale:EmbarkedQ -1.161e+01  1.455e+03  -0.008   0.9936  
    ## Pclass2:Sexmale:EmbarkedQ    3.448e+00  2.058e+03   0.002   0.9987  
    ## Pclass3:Sexmale:EmbarkedQ    1.331e+01  1.455e+03   0.009   0.9927  
    ## Pclass2:Sexfemale:EmbarkedS -1.204e+01  5.414e+02  -0.022   0.9823  
    ## Pclass3:Sexfemale:EmbarkedS -2.935e-01  1.350e+00  -0.217   0.8279  
    ## Pclass2:Sexmale:EmbarkedS   -2.476e-01  1.042e+00  -0.238   0.8121  
    ## Pclass3:Sexmale:EmbarkedS   -5.856e-01  5.901e-01  -0.992   0.3210  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  729.73  on 866  degrees of freedom
    ## AIC: 779.73
    ## 
    ## Number of Fisher Scoring iterations: 14

1.  The estimated test error has increased marginally.
2.  Of the interaction terms added in the last pass, the third order
    term has no significance to the quality of fit but the other two do.
    We drop the third order term and refit the model

<!-- -->

    f5 = paste(f3, "+Pclass:Age+Sex:Age", sep = "")
    F = as.formula(f5)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1793"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8904  -0.6496  -0.4064   0.3411   3.2775  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        3.700208   1.051283   3.520 0.000432 ***
    ## Pclass2            1.599543   1.205807   1.327 0.184663    
    ## Pclass3           -2.440122   0.983626  -2.481 0.013111 *  
    ## Sexmale           -2.576483   0.983433  -2.620 0.008796 ** 
    ## Age                0.008400   0.023400   0.359 0.719606    
    ## SibSp             -0.296899   0.115168  -2.578 0.009938 ** 
    ## Parch             -0.073335   0.129321  -0.567 0.570662    
    ## Fare               0.001451   0.002497   0.581 0.561208    
    ## EmbarkedQ          0.370141   0.577849   0.641 0.521815    
    ## EmbarkedS         -0.812516   0.466766  -1.741 0.081730 .  
    ## Pclass2:Sexmale   -1.372051   0.888299  -1.545 0.122447    
    ## Pclass3:Sexmale    1.634816   0.730396   2.238 0.025204 *  
    ## Sexmale:EmbarkedQ -1.374360   0.882530  -1.557 0.119400    
    ## Sexmale:EmbarkedS  0.463418   0.545787   0.849 0.395836    
    ## Pclass2:Age       -0.067653   0.026283  -2.574 0.010053 *  
    ## Pclass3:Age       -0.034213   0.022088  -1.549 0.121395    
    ## Sexmale:Age       -0.043708   0.020656  -2.116 0.034343 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  731.46  on 874  degrees of freedom
    ## AIC: 765.46
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error remained same.
2.  The interaction term `Sex:Embarked` doesn't seem to be significant
    to the model. So we drop it.

<!-- -->

    f6 = paste(f5, "-Sex:Embarked", sep = "")
    F = as.formula(f6)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.177"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8268  -0.6459  -0.3961   0.3313   3.2999  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      3.505514   1.030658   3.401 0.000671 ***
    ## Pclass2          1.549650   1.205476   1.286 0.198615    
    ## Pclass3         -2.254129   0.987319  -2.283 0.022426 *  
    ## Sexmale         -2.276328   0.932214  -2.442 0.014612 *  
    ## Age              0.008613   0.023450   0.367 0.713406    
    ## SibSp           -0.343198   0.115054  -2.983 0.002855 ** 
    ## Parch           -0.087594   0.128541  -0.681 0.495588    
    ## Fare             0.001417   0.002508   0.565 0.572043    
    ## EmbarkedQ       -0.057374   0.364354  -0.157 0.874877    
    ## EmbarkedS       -0.465047   0.244461  -1.902 0.057127 .  
    ## Pclass2:Sexmale -1.257527   0.877335  -1.433 0.151758    
    ## Pclass3:Sexmale  1.439719   0.717355   2.007 0.044751 *  
    ## Pclass2:Age     -0.069247   0.026353  -2.628 0.008596 ** 
    ## Pclass3:Age     -0.037415   0.022291  -1.679 0.093247 .  
    ## Sexmale:Age     -0.044387   0.020686  -2.146 0.031889 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  738.23  on 876  degrees of freedom
    ## AIC: 768.23
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error has decreased slightly but all the features
    in the model except `Fare` and `Parch` are significant to the model.
    Moreover, `Fare` is a function of `Pclass` and `Embarked`, two terms
    that are already part of the model. So we will drop `Fare` from the
    model.
2.  Since `Pclass`,`Sex` and `Age` are important predictor of survival,
    we add interaction terms for `Parch` with these 3 features to
    investigate the impact on the model quality.

<!-- -->

    f7 = paste(f6, "-Fare+Pclass:Parch+Sex:Parch+Age:Parch", sep = "")
    F = as.formula(f7)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1742"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.0061  -0.6083  -0.4092   0.3816   3.0239  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      4.126274   1.197282   3.446 0.000568 ***
    ## Pclass2         -0.138624   1.317920  -0.105 0.916230    
    ## Pclass3         -3.133865   1.103352  -2.840 0.004507 ** 
    ## Sexmale         -3.089593   1.056902  -2.923 0.003464 ** 
    ## Age              0.007623   0.025972   0.294 0.769123    
    ## SibSp           -0.423206   0.123440  -3.428 0.000607 ***
    ## Parch           -0.019905   0.548504  -0.036 0.971052    
    ## EmbarkedQ       -0.065980   0.369364  -0.179 0.858227    
    ## EmbarkedS       -0.489600   0.243830  -2.008 0.044648 *  
    ## Pclass2:Sexmale -0.597631   0.900402  -0.664 0.506858    
    ## Pclass3:Sexmale  1.801168   0.773207   2.329 0.019834 *  
    ## Pclass2:Age     -0.042845   0.027775  -1.543 0.122932    
    ## Pclass3:Age     -0.018223   0.022716  -0.802 0.422437    
    ## Sexmale:Age     -0.035304   0.021550  -1.638 0.101382    
    ## Pclass2:Parch    1.172500   0.525644   2.231 0.025708 *  
    ## Pclass3:Parch    0.300611   0.410013   0.733 0.463453    
    ## Sexmale:Parch    0.548447   0.303154   1.809 0.070429 .  
    ## Age:Parch       -0.019400   0.010725  -1.809 0.070477 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.7  on 890  degrees of freedom
    ## Residual deviance:  724.7  on 873  degrees of freedom
    ## AIC: 760.7
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error is reduced to its minimum so far.
2.  In the latest model the interaction terms `Pclass:Age` and `Sex:Age`
    are not signifiant to the model. We drop these interaction terms to
    investigate the impact on the quality.

<!-- -->

    f8 = paste(f7, "-Pclass:Age-Sex:Age", sep = "")
    F = as.formula(f8)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1742"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8928  -0.5815  -0.4327   0.4001   2.5658  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      5.74640    0.84802   6.776 1.23e-11 ***
    ## Pclass2         -1.86837    0.81929  -2.280 0.022579 *  
    ## Pclass3         -4.25973    0.73860  -5.767 8.05e-09 ***
    ## Sexmale         -4.48114    0.69831  -6.417 1.39e-10 ***
    ## Age             -0.03375    0.01135  -2.974 0.002939 ** 
    ## SibSp           -0.42688    0.12348  -3.457 0.000546 ***
    ## Parch           -0.28115    0.52326  -0.537 0.591061    
    ## EmbarkedQ       -0.04640    0.37151  -0.125 0.900615    
    ## EmbarkedS       -0.45196    0.24344  -1.857 0.063372 .  
    ## Pclass2:Sexmale -0.10593    0.85462  -0.124 0.901354    
    ## Pclass3:Sexmale  2.36213    0.71803   3.290 0.001003 ** 
    ## Pclass2:Parch    1.42247    0.50336   2.826 0.004714 ** 
    ## Pclass3:Parch    0.47042    0.38968   1.207 0.227361    
    ## Sexmale:Parch    0.69764    0.27806   2.509 0.012110 *  
    ## Age:Parch       -0.01727    0.01041  -1.659 0.097168 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  729.45  on 876  degrees of freedom
    ## AIC: 759.45
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error remains same.
2.  We now add interaction terms for `SibSp` with `Pclass`, `Age` and
    `Sex` to investigate the impact on the quality of the model.
3.  We also add power terms for the two quantitative features `SibSp`
    and `Age`.

<!-- -->

    f9 = paste(f8, "+SibSp:Pclass+SibSp:Age+SibSp:Sex+I(SibSp^2)+I(Age^2)", sep = "")
    F = as.formula(f9)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1793"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1017  -0.6045  -0.4095   0.3982   2.4742  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      6.5642597  1.0410440   6.305 2.87e-10 ***
    ## Pclass2         -1.5840326  0.8447890  -1.875  0.06078 .  
    ## Pclass3         -4.0857637  0.7602865  -5.374 7.70e-08 ***
    ## Sexmale         -4.3271937  0.7025077  -6.160 7.29e-10 ***
    ## Age             -0.1033785  0.0353683  -2.923  0.00347 ** 
    ## SibSp           -0.4378252  0.7241740  -0.605  0.54545    
    ## Parch           -0.2952004  0.6096519  -0.484  0.62824    
    ## EmbarkedQ       -0.0129651  0.3796922  -0.034  0.97276    
    ## EmbarkedS       -0.4208806  0.2469006  -1.705  0.08826 .  
    ## I(SibSp^2)      -0.0774679  0.1038817  -0.746  0.45583    
    ## I(Age^2)         0.0009070  0.0004288   2.115  0.03440 *  
    ## Pclass2:Sexmale -0.3219406  0.8623245  -0.373  0.70890    
    ## Pclass3:Sexmale  2.2992829  0.7257737   3.168  0.00153 ** 
    ## Pclass2:Parch    1.3314917  0.5502294   2.420  0.01553 *  
    ## Pclass3:Parch    0.6056864  0.4196325   1.443  0.14892    
    ## Sexmale:Parch    0.5554554  0.3239911   1.714  0.08645 .  
    ## Age:Parch       -0.0187260  0.0123391  -1.518  0.12911    
    ## Pclass2:SibSp   -0.2616484  0.5589825  -0.468  0.63973    
    ## Pclass3:SibSp   -0.2644045  0.4592267  -0.576  0.56478    
    ## Age:SibSp        0.0212084  0.0140863   1.506  0.13217    
    ## Sexmale:SibSp    0.1052850  0.2753772   0.382  0.70222    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  716.16  on 870  degrees of freedom
    ## AIC: 758.16
    ## 
    ## Number of Fisher Scoring iterations: 7

1.  The estimated test error has increased marginally.
2.  The three interaction terms added are not significant to the model.
3.  Of the power terms only the `Age^2` term is significant to the
    model.
4.  We drop the terms from the previous pass that are not relevant and
    add multiple power terms for `Age` to investigate impact on the
    quality of the model.

<!-- -->

    f10 = paste(f8, "+poly(Age, 5)", sep = "")
    F = as.formula(f10)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1776"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1495  -0.5496  -0.4571   0.3934   2.7488  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      6.022514   0.863491   6.975 3.07e-12 ***
    ## Pclass2         -1.800247   0.820103  -2.195 0.028153 *  
    ## Pclass3         -4.125590   0.741011  -5.568 2.58e-08 ***
    ## Sexmale         -4.546267   0.696873  -6.524 6.85e-11 ***
    ## Age             -0.041462   0.011858  -3.497 0.000471 ***
    ## SibSp           -0.453020   0.131956  -3.433 0.000597 ***
    ## Parch           -0.641328   0.539909  -1.188 0.234895    
    ## EmbarkedQ       -0.044784   0.369977  -0.121 0.903655    
    ## EmbarkedS       -0.495692   0.247520  -2.003 0.045217 *  
    ## poly(Age, 5)1          NA         NA      NA       NA    
    ## poly(Age, 5)2    5.496836   3.529816   1.557 0.119409    
    ## poly(Age, 5)3   -5.135607   3.170752  -1.620 0.105301    
    ## poly(Age, 5)4    7.975666   2.926561   2.725 0.006425 ** 
    ## poly(Age, 5)5    0.698231   2.867195   0.244 0.807599    
    ## Pclass2:Sexmale -0.124258   0.856597  -0.145 0.884664    
    ## Pclass3:Sexmale  2.375211   0.718090   3.308 0.000941 ***
    ## Pclass2:Parch    1.142313   0.504491   2.264 0.023556 *  
    ## Pclass3:Parch    0.318726   0.387617   0.822 0.410923    
    ## Sexmale:Parch    0.639470   0.279400   2.289 0.022095 *  
    ## Age:Parch       -0.002434   0.011225  -0.217 0.828349    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  717.05  on 872  degrees of freedom
    ## AIC: 755.05
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error has slightly improved.
2.  More importantly only the 4th order power term for `Age` seems
    significant to the model.
3.  We make the necessary changes and refit the model.

<!-- -->

    f11 = paste(f8, "+I(Age^4)", sep = "")
    F = as.formula(f11)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1776"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9107  -0.6037  -0.4233   0.3984   2.4892  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      6.116e+00  9.107e-01   6.715 1.88e-11 ***
    ## Pclass2         -1.868e+00  8.189e-01  -2.281 0.022570 *  
    ## Pclass3         -4.333e+00  7.410e-01  -5.847 4.99e-09 ***
    ## Sexmale         -4.489e+00  6.985e-01  -6.427 1.30e-10 ***
    ## Age             -4.822e-02  1.708e-02  -2.823 0.004756 ** 
    ## SibSp           -4.341e-01  1.242e-01  -3.495 0.000473 ***
    ## Parch           -4.390e-01  5.396e-01  -0.813 0.415936    
    ## EmbarkedQ       -4.061e-02  3.722e-01  -0.109 0.913130    
    ## EmbarkedS       -4.354e-01  2.444e-01  -1.782 0.074789 .  
    ## I(Age^4)         5.051e-08  4.345e-08   1.163 0.245016    
    ## Pclass2:Sexmale -1.317e-01  8.561e-01  -0.154 0.877741    
    ## Pclass3:Sexmale  2.395e+00  7.183e-01   3.334 0.000856 ***
    ## Pclass2:Parch    1.409e+00  5.019e-01   2.808 0.004980 ** 
    ## Pclass3:Parch    4.988e-01  3.870e-01   1.289 0.197368    
    ## Sexmale:Parch    6.765e-01  2.783e-01   2.431 0.015061 *  
    ## Age:Parch       -1.262e-02  1.094e-02  -1.153 0.248763    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  728.19  on 875  degrees of freedom
    ## AIC: 760.19
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  The estimated test error is unchanged. More importantly, all terms
    except `Age:Parch` in the model are significant to the quality of
    the model.
2.  We drop the non-significant term and rebuild the model.

<!-- -->

    f12 = paste(f11, "-Age:Parch", sep = "")
    F = as.formula(f12)

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        logit.fit = glm(F, data = train[indx,], family = "binomial") 
        prob = predict(logit.fit, newdata = train[indx,], type = "response") 
        pred = ROCR::prediction(prob, train$Survived[indx]) 
        perf = performance(pred, measure = "acc") 
        bestaccindx = perf@y.values %>% unlist() %>% which.max() 
        bestcutoff = perf@x.values %>% unlist() %>% .[bestaccindx] 
        
        prob = predict(logit.fit, newdata = train[-indx,], type = "response") 
        pred = ifelse(prob < bestcutoff, 0, 1) 
        test.err = mean(pred != train$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using this logistic fit is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using this logistic fit is 0.1681"

    logit.fit = glm(F, data = train, family = "binomial")

    summary(logit.fit)

    ## 
    ## Call:
    ## glm(formula = F, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8037  -0.6160  -0.4218   0.3929   2.5484  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      6.537e+00  8.409e-01   7.774 7.61e-15 ***
    ## Pclass2         -1.926e+00  8.134e-01  -2.367 0.017910 *  
    ## Pclass3         -4.459e+00  7.302e-01  -6.106 1.02e-09 ***
    ## Sexmale         -4.486e+00  6.941e-01  -6.463 1.03e-10 ***
    ## Age             -6.128e-02  1.306e-02  -4.693 2.69e-06 ***
    ## SibSp           -4.004e-01  1.188e-01  -3.370 0.000752 ***
    ## Parch           -8.844e-01  3.692e-01  -2.396 0.016589 *  
    ## EmbarkedQ       -5.740e-02  3.724e-01  -0.154 0.877507    
    ## EmbarkedS       -4.159e-01  2.435e-01  -1.708 0.087718 .  
    ## I(Age^4)         7.001e-08  3.963e-08   1.767 0.077262 .  
    ## Pclass2:Sexmale -1.523e-01  8.543e-01  -0.178 0.858533    
    ## Pclass3:Sexmale  2.404e+00  7.141e-01   3.367 0.000761 ***
    ## Pclass2:Parch    1.440e+00  4.895e-01   2.941 0.003277 ** 
    ## Pclass3:Parch    5.929e-01  3.572e-01   1.660 0.096927 .  
    ## Sexmale:Parch    6.828e-01  2.655e-01   2.571 0.010133 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1186.66  on 890  degrees of freedom
    ## Residual deviance:  729.61  on 876  degrees of freedom
    ## AIC: 759.61
    ## 
    ## Number of Fisher Scoring iterations: 6

1.  We now have a model with the lowest estimated test error and with
    all terms significant to the model quality.
2.  We save this model for future reference.

<!-- -->

    saveRDS(logit.fit, "../RDA/best.logit.Rda")
