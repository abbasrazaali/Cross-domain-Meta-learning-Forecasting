#####################################################################################################################
#
# Name: MetaLearning.R
#
# Description: Meta-level Learning Algorithms
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
#
#####################################################################################################################

# libraries
#library(caret);

#################
# Meta-Learning #
#################

MetaLearning = function(MK, target, MLLAccuracy, Method, mlTechniques, Analysis, MLLSMAPE)
{
  # data pre-processing
#   preproc = preProcess(MK[, -dim(MK)[2]], method = c("center", "scale"));
#   MK_preproc = predict(preproc, MK[, -dim(MK)[2]]);
  
  if (length(grep('NN', mlTechniques)) == 1) {    								
    ###################
    # neural networks #
    ###################
    tune = expand.grid(.size = c(10, 15, 20, 25, 30, 35, 40), .decay = c(0.01));
    fitControl = trainControl(classProbs = TRUE); #, summaryFunction = customSummary);

    model = train(y = as.factor(target), x = data.frame(MK), method = 'nnet', maxit = 1000, trControl = fitControl, tuneGrid = tune, linout = TRUE, trace = FALSE);
    
    model$results = model$results[order(-model$results$Accuracy), ];
    MLLAccuracy[1] = model$results$Accuracy[1] * 100;
    
    print('Meta-learning');
    print(paste("Neural Networks Accuracy: ", MLLAccuracy[1], sep = ''));
    print(model);
    print(model$bestTune);
    MLLNNModel = model;

    #MLLSMAPE[1] = model$results$stats3
    #MLLSMAPE[1] = performance(as.numeric(model$pred$pred[(model$pred$size == model$results$size[1])]), Method[, 1], length(Method[, 1]), divisor = 1);
    #Method[, 3] = as.numeric(model$pred$pred[(model$pred$size == model$results$size[1])]);
    
    MLLModel = model;
    MLLNNModel = model;
  } 
  
  if (length(grep('DT', mlTechniques)) == 1) {    												
    ##################
    # Decision trees #
    ##################
    tune = expand.grid(.trials = c(1:100), .model = c("tree"), .winnow = c(FALSE));
    fitControl = trainControl(classProbs = TRUE); #, summaryFunction = customSummary);
    
    model = train(y = as.factor(target), x = data.frame(MK), method = 'C5.0', trControl = fitControl, tuneLength = 8, tuneGrid = tune);
    
    model$results = model$results[order(-model$results$Accuracy), ];
    MLLAccuracy[2] = model$results$Accuracy[1] * 100;

    print(paste("Decision Trees Accuracy: ", MLLAccuracy[2], sep = ''));
    print(model);
    print(model$bestTune);
    MLLDTModel = model;
    
#     MLLSMAPE[2] = performance(as.numeric(model$pred$pred[(model$pred$trials == model$results$trials[1])]), Method[, 1], length(Method[, 1]), divisor = 1);
    
    if(MLLAccuracy[1] < MLLAccuracy[2]) {
#       Method[, 3] = as.numeric(model$pred$pred[(model$pred$trials == model$results$trials[1])]);
      MLLModel = model;
    }
  } 
  
  if (length(grep('SVM', mlTechniques)) == 1) {													
    ###########################
    # support vector machines #
    ###########################
    
    tune = expand.grid(.sigma = c(0.05, 0.01, 0.1), .C = c(30, 35, 40, 45, 50, 55, 60, 65, 70));
    
    fitControl = trainControl(classProbs = TRUE); #, summaryFunction=customSummary);
    
    #set.seed(825);
    model = train(y = as.factor(target), x = data.frame(MK), method = 'svmRadial', trControl = fitControl, tuneGrid = tune);    
    model$results = model$results[order(-model$results$Accuracy), ];
    MLLAccuracy[3] = model$results$Accuracy[1] * 100;
    
    print(paste("Support Vector Machines Accuracy: ", MLLAccuracy[3], sep = ''));
    print(model);
    print(model$bestTune);
    MLLSVMModel = model;
    
#     MLLSMAPE[3] = performance(as.numeric(model$pred$pred[(model$pred$sigma == model$results$sigma[1]) & (model$pred$C == model$results$C[1])]), Method[, 1], length(Method[, 1]), divisor = 1);
    
    if(MLLAccuracy[1] < MLLAccuracy[3] & MLLAccuracy[2] < MLLAccuracy[3]) {
#       Method[, 3] = as.numeric(model$pred$pred[(model$pred$sigma == model$results$sigma[1]) & (model$pred$C == model$results$C[1])]);
      MLLModel = model;
    }
  }
  
  return(list(MLLAccuracy = MLLAccuracy, Method = Method, MLLModel = MLLModel, MLLNNModel = MLLNNModel, MLLDTModel = MLLDTModel, MLLSVMModel = MLLSVMModel)); #, MLLSMAPE = MLLSMAPE));
}

# Custom Summary Function 
customSummary = function (data, lev = NULL, model = NULL) 
{
  stats1 = postResample(data$pred, data$obs);
  stats2 = accuracy(data$pred, data$obs);
  stats3 = ((sum(abs(data[, "obs"] - data[, "pred"]) / ((data[, "obs"] + data[, "pred"])))) * 100) / length(data[, "obs"]);
  
  names(stats3) = 'SMAPE';
  
  return(c(stats1, stats2, stats3));
}
