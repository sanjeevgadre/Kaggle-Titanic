-   [Support Vector Machine](#support-vector-machine)
    -   [I. Linear Kernel](#i.-linear-kernel)
    -   [II. Radial Kernel](#ii.-radial-kernel)
    -   [III. Polynomial Kernel](#iii.-polynomial-kernel)
-   [Neural Networks](#neural-networks)

1.  We use the train and test subsets processed earlier.
2.  We use the *train-validate-test* strategy to choose the optimal
    parameters for the different models and then estimate test error for
    a learning algorithm based on such a model

<!-- -->

    train = readRDS("../RDA/train.Rda")
    m = nrow(train)

Support Vector Machine
----------------------

We implement a SVM with different kernels - linear, radial and
polynomial

#### I. Linear Kernel

    cost = c(0.01, 0.1, 1, 10, 100, 1000)

    est.test.err = 0; 
    for (k in 1:5) {
      set.seed(k)
      indx = split(sample(1:m, m), f = c(rep("train", 6), rep("cval", 2), rep("test", 2)))
      
      #     Fitting an svm to train subset and using using the cval subset to determine the optimal cost for the SVM model using accuracy as the measure
      bestcost = 1; leasterr = 1;
      for (c in cost) {
            svm.fit = svm(Survived~., data = train[indx$train,], kernel = "linear", cost = c, 
                          scale = TRUE, fitted = FALSE)
            pred = predict(svm.fit, newdata = train[indx$cval,])
            err = mean(pred != train$Survived[indx$cval])
            if (err < leasterr) {
                leasterr = err
                bestcost = c 
            }
        }
      svm.fit = svm(Survived~., data = train[indx$train,], kernel = "linear", cost = bestcost,  
                    scale = TRUE, fitted = FALSE)
      pred = predict(svm.fit, newdata = train[indx$test,])
      test.err = mean(pred != train$Survived[indx$test])
      est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using an SVM with a linear kernel is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using an SVM with a linear kernel is 0.2258"

### II. Radial Kernel

    cost = c(0.01, 0.1, 1, 10, 100, 1000)
    gamma = c(0.25, 0.125, 0.5, 1, 2)

    est.test.err = 0; 
    for (k in 1:5) {
      set.seed(k)
      indx = split(sample(1:m, m), f = c(rep("train", 6), rep("cval", 2), rep("test", 2)))
      
      # Fitting an svm to train subset and using using the cval subset to determine the optimal cost and gamma for the SVM model using accuracy as the measure
      bestcost = 1; bestgamma = 0.05; leasterr = 1;
      for (c in cost) {
        for (g in gamma) {
            svm.fit = svm(Survived~., data = train[indx$train,], kernel = "radial", cost = c, gamma = g,
                          scale = TRUE, fitted = FALSE)
            pred = predict(svm.fit, newdata = train[indx$cval,])
            err = mean(pred != train$Survived[indx$cval])
            if (err < leasterr) {
                leasterr = err
                bestcost = c 
                bestgamma = g
          }
        }
      }
      svm.fit = svm(Survived~., data = train[indx$train,], kernel = "radial", cost = bestcost, gamma = bestgamma, 
                    scale = TRUE, fitted = FALSE)
      pred = predict(svm.fit, newdata = train[indx$test,])
      test.err = mean(pred != train$Survived[indx$test])
      est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using an SVM with a radial kernel is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using an SVM with a radial kernel is 0.2135"

### III. Polynomial Kernel

    cost = c(0.01, 0.1, 1, 10, 100)
    degree = 2:4
    coeff0 = seq(0, 1, length.out = 5)
    gamma = c(0.025, 0.05, 0.125, 0.25, 0.5)

    est.test.err = 0; 
    for (k in 1:5) {
        set.seed(k)
        indx = split(sample(1:m, m), f = c(rep("train", 6), rep("cval", 2), rep("test", 2)))
        # Fitting an svm to train subset and using using the cval subset to determine the optimal degree, cost and gamma for the SVM model using accuracy as the measure
        bestcost = 1; bestdegree = 3; bestcoeff0 = 0; leasterr = 1; bestgamma = 0.125
        for (c in cost) {
            for (d in degree) {
                for (a in coeff0) {
                    for (g in gamma) {
                        svm.fit = svm(
                            Survived ~ ., data = train[indx$train,], kernel = "polynomial",
                            cost = c, degree = d, coef0 = a, gamma = g,
                            scale = TRUE, fitted = FALSE)
                        pred = predict(svm.fit, newdata = train[indx$cval,])
                        err = mean(pred != train$Survived[indx$cval])
                        if (err < leasterr) {
                            leasterr = err; bestcost = c
                            bestdegree = d; bestcoeff0 = a; bestgamma = g
                        }
                    }
                }
            }
        }
        svm.fit = svm(Survived ~ ., data = train[indx$train,], kernel = "polynomial",
            cost = bestcost, degree = bestdegree, coef0 = bestcoeff0, gamma = bestgamma,
            scale = TRUE, fitted = FALSE)
        pred = predict(svm.fit, newdata = train[indx$test,])
        test.err = mean(pred != train$Survived[indx$test])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error using an SVM with a polynomial kernel is", 
                round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error using an SVM with a polynomial kernel is 0.2225"

Overall, the SVM as a learning algorithm did not meaningfully reduce the
estimated test error with the SVM with a *polynomial* kernel delivering
the least estimated test error.

Neural Networks
---------------

We implement a neural network to gauge the algorighm's effectiveness in
addressing the problem. Specifically we build four different neural
networks: 1. A neural network with 1 hidden layer and as many neurons as
features. 2. A neural network with 2 hidden layers and in each layer as
many neurons as features. 3. A neural network with 1 hidden layer and
1.5 times as many neurons as features. 4. A neural network with 2 hidden
layers and in each layer 1.5 times as many neurons as features.

Comparing the results of these 4 neural network models will hopefully
unearth the direction in which we need to optimise the structure of the
neural network.

    p = ncol(train)-1
    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        x = model.matrix(Survived~., data = train)[,-1]     #   Convert to a model matrix neuralnet() will only                                                                 accept numerical inputs for features
        
        
        #   We normalise the data but only using the train subset examples. This is to avoid test subset from participating in training the model.
        mu = scale(x[indx,]) %>% attr("scaled:center")
        sdev = scale(x[indx,]) %>% attr("scaled:scale")
        x = scale(x, center = mu, scale = sdev) %>% as.data.frame()
        x$Survived = train$Survived
        
        set.seed(k)
        nn.fit = neuralnet(Survived~., data = x[indx,], hidden = p, threshold = 0.02, linear.output = FALSE)
        
        pred = predict(nn.fit, newdata = x[-indx,], all.units = FALSE)
        pred = apply(pred, 1, which.max)
        pred = factor(pred, labels = c(0,1))
        test.err = mean(pred != x$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error for a neural network with 1 hidden layer and 10 neurons in the hidden layer is", round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error for a neural network with 1 hidden layer and 10 neurons in the hidden layer is 0.2303"

1.  The estimated test error for a neural network with 1 hidden layer
    and 10 neurons in the hidden layer forms the baseline for the
    performance measure.
2.  We now fit a neural network with 2 hidden layers and 10 neurons per
    hidden layer.

<!-- -->

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        x = model.matrix(Survived~., data = train)[,-1]

        mu = scale(x[indx,]) %>% attr("scaled:center")
        sdev = scale(x[indx,]) %>% attr("scaled:scale")
        x = scale(x, center = mu, scale = sdev) %>% as.data.frame()
        x$Survived = train$Survived
        p = ncol(x)-1
        
        set.seed(k)
        nn.fit = neuralnet(Survived~., data = x[indx,], hidden = c(p,p), threshold = 0.12, linear.output = FALSE)
        
        pred = predict(nn.fit, newdata = x[-indx,], all.units = FALSE)
        pred = apply(pred, 1, which.max)
        pred = factor(pred, labels = c(0,1))
        test.err = mean(pred != x$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error for a neural network with 2 hidden layers and 10 neurons per hidden layer is", round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error for a neural network with 2 hidden layers and 10 neurons per hidden layer is 0.223"

1.  There is a reasonable increase in the estimated test error over the
    baseline.
2.  We now fit a neural network with just one hidden layer but greater
    number of neurons.

<!-- -->

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        x = model.matrix(Survived~., data = train)[,-1]
        mu = scale(x[indx,]) %>% attr("scaled:center")
        sdev = scale(x[indx,]) %>% attr("scaled:scale")
        x = scale(x, center = mu, scale = sdev) %>% as.data.frame()
        x$Survived = train$Survived
        
        set.seed(k)
        nn.fit = neuralnet(Survived~., data = x[indx,], hidden = 1.5*p, threshold = 0.03, linear.output = FALSE)
        
        pred = predict(nn.fit, newdata = x[-indx,], all.units = FALSE)
        pred = apply(pred, 1, which.max)
        pred = factor(pred, labels = c(0,1))
        test.err = mean(pred != x$Survived[-indx])
        est.test.err = test.err + est.test.err
    }
    print(paste("The estimated test error for a neural network with 1 hidden layer and 15 neurons per hidden layer is", round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error for a neural network with 1 hidden layer and 15 neurons per hidden layer is 0.2185"

1.  There is a reasonable increase in the estimated test error over the
    baseline.
2.  We now fit a neural network with just 2 hidden layers with 15
    neurons per hidden layer.

<!-- -->

    est.test.err = 0
    for (k in 1:5) {
        set.seed(k)
        indx = sample(1:m, 0.6*m)
        x = model.matrix(Survived~., data = train)[,-1]
        mu = scale(x[indx,]) %>% attr("scaled:center")
        sdev = scale(x[indx,]) %>% attr("scaled:scale")
        x = scale(x, center = mu, scale = sdev) %>% as.data.frame()
        x$Survived = train$Survived
        
        set.seed(k)
        nn.fit = neuralnet(Survived~., data = x[indx,], hidden = c(1.5*p, 1.5*p), threshold = 0.05)
        
        pred = predict(nn.fit, newdata = x[-indx,], all.units = FALSE)
        pred = apply(pred, 1, which.max)
        pred = factor(pred, labels = c(0,1))
        test.err = mean(pred != x$Survived[-indx])
        est.test.err = test.err + est.test.err
    }

    print(paste("The estimated test error for a neural network with 2 hidden layer and 15 neurons per hidden layer is", round(est.test.err/5, digits = 4), sep = " "))

    ## [1] "The estimated test error for a neural network with 2 hidden layer and 15 neurons per hidden layer is 0.2549"

1.  The estimated test error increased significantly over the baseline
    value.

Overall, the neural network learning model doesn't seem appropriate for
this particular problem.
