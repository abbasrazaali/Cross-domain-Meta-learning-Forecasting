#####################################################################################################################
#
# Name: MetaLearning_Scoring.R
#
# Description: Meta-level Learning Algorithms - scoring
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
#
#####################################################################################################################

# libraries

#################
# Meta-Learning #
#################

MetaLearningScoring = function(MetaModel, MetaFeaturesNNGC_A, MetaFeaturesNNGC_B, MetaFeaturesNNGC_C, MetaFeaturesNNGC_D, MetaFeaturesNNGC_E, MetaFeaturesNNGC_F, MethodNNGC_A, MethodNNGC_B, MethodNNGC_C, MethodNNGC_D, MethodNNGC_E, MethodNNGC_F, index, ScoringGC_NN)
{
  MethodNNGC[, index] = rbind(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_A, type = "raw")),
                          as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_B, type = "raw")),
                          as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_C, type = "raw")),
                          as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_D, type = "raw")),
                          as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_E, type = "raw")),
                          as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_F, type = "raw")));
  ScoringGC_NN[1, index] = performanceAcc(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_B, type = "raw")), MethodNNGC_A[, 1], horizon = length(MethodNNGC_A[, 1])); 
  ScoringGC_NN[2, index] = performanceAcc(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_B, type = "raw")), MethodNNGC_B[, 1], horizon = length(MethodNNGC_B[, 1])); 
  ScoringGC_NN[3, index] = performanceAcc(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_C, type = "raw")), MethodNNGC_C[, 1], horizon = length(MethodNNGC_C[, 1])); 
  ScoringGC_NN[4, index] = performanceAcc(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_D, type = "raw")), MethodNNGC_D[, 1], horizon = length(MethodNNGC_D[, 1])); 
  ScoringGC_NN[5, index] = performanceAcc(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_E, type = "raw")), MethodNNGC_E[, 1], horizon = length(MethodNNGC_E[, 1])); 
  ScoringGC_NN[6, index] = performanceAcc(as.numeric(predict(MetaModel, newdata = MetaFeaturesNNGC_F, type = "raw")), MethodNNGC_F[, 1], horizon = length(MethodNNGC_F[, 1])); 
  
  return(list(ScoringGC_NN = ScoringGC_NN));
}
