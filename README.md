# Automatic chord recognition using Machine Learning Algorithms

Implementing an automatic musical chord recognition template-based using machine learning algorithms in R.
The ML methods analyzed were:
- BagCART
- C50
- DecisionTree
- Multinomial
- Random Forest
- SVM
- GBM
- Naive Bayes
- FDA
- PDA
- Nnet
- KNN
- MDA
- QDA
- LDA

It was proposed "Mel Frequency Cepstral Coefficients, Zero Crossing Rate", CSH, Spectral Centroid, Spectral Flux, DOMF as features of the WAV file to train and classify the chords into the track.

The file aprendizajeFeatures.R includes the main program with the templates build and function calls. The file ./tools/SupervisedLearning.R has the calls for each algorithm selected.

There is a post-processing step to clean up the final result using Markovian chains.
Results and applications can be reviewed in the paper 'A template-based algorithm by geometric means for the automatic and efficient recognition of music
chords'  https://dx.doi.org/10.1007/s12065-022-00771-6 where I am co-author.

