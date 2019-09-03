# The *Titanic - Predict the Survivor* Challenge on www.kaggle.com

##  Introduction
The Titanic disaster is part of the popular culture, spawning any number of books, movies and music; it also presents an excellent case study for applying machine learning algorithm to derive insights, both causal and inferential.

In this project, I have focussed on applying multiple machine learning algorithm to the given data-set with a goal of reviewing their relative efficacy in predicting the survivors of the Titanic disaster. I have applied 6 different algorithms - 3 parametric and 3 tree-based. The algorithms applied are:
1.  Un-penalised Logistic Regression with hand-crafted feature engineering
2.  SVM with different kernels - linear, radial and polynomial
3.  Neural Network with different architectures - number of hidden layers, number of neurons in each layer
4.  Random Forest Ensemble
5.  Bagged Trees Ensemble
6.  Boosted Trees Ensemble

The project is implemented in R.
    
##  The Challenge
The reasons that this makes an excellent case study especially for new practitioners on machine learning:
1.  **Domain knowledge** - To successfully address a machine learning problem, contextual familiarity is important. The ML practitioner must have reasonable knowledge of the domain from which the problem emerges. Since the Titanic disaster is part of the popular culture, almost everyone has contextual familiarity and can easily build hypotheses to validate.
2.  **Moderate data size** - This challenge's data-set is moderate in size - both the number of examples as well as the *features* of the example. This helps the new practitoner as s/he is not burdened with additional data engineering challenges of managing resources to deal with large data-sets, allowing her/him to focus on implementing the machine learning algorithms.

The goal of the challenge is to learn from the *training* data-set and predict the survivors in the *test* data-set. The accuracy of the predictions is the measure of success.

##  My approach
The focus of my approach is to understand the relative efficacy of the different learning algorithms. Equally importantly, I was not focussed on *winning* the challenge but on using this as an opportunity to learn how a challenge may be addressed in a real-life situation.

The first part of my solution focusses on data wrangling 
1.  Performing exploratory data analysis on the train data-set to get better sense of the data and form preliminary hypotheses.
2.  Making changes to the data type of the features for both train and test data-sets to better reflect the *information* contained in them.
3.  Imputing missing values to features in both the train and test data-sets.
4.  Drawing *learning curves* to better inform the choice of the learning algorithms to use and the ranges of parameter values to test for in each of the chosen learning algorithm.

When applying the various learning algorithms I use the *train-validate-test* strategy to choose the optimal values of different parameters relevant to the specific learning algorithm and to estimate the likely test error.

The second part of my solution applies the un-penalised logistic regression algorithm. I make multiple passes, at each pass choosing to add or delete features (both original and new hand-crafted-from-original features) with a goal to improve the estimated test set prediction accuracy.

The third part of my solution applies other parametric learning alogrithms - SVM with different kernels and Neural Networks with different architectures.

The fourth part of my solution applies tress based learning algorithms - random forest ensemble, bagged tree ensemble and boosted trees ensemble.

For each of the learning algorithm I estimated the test set prediction accuracy and in the final fifth part I use the top 4 performing algorithms - logistic regression with hand-crafted feature engineering, random forest ensemble, bagged trees ensemble and boosted trees ensemble -  to make predictions for the test data-set. We use Kaggle's evaluation engine to get final verified results of the performance of these 4 algorithms.

##  The Directory Structure
The entire project is available either as a single markdown document `Titanic-Experiments-with-ML-algorithms.md` in the root directory or split into 5 sections, paralleling the discussion above, in the `/scripts` directory. The data, both input and output is in the `/data` directory. Finally the `/RDA` directory contains the RData created and used by the code in both the single document version and the 5-sections version.
