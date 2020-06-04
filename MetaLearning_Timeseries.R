#####################################################################################################################
#
# Name: MetaLearning_Timeseries.R
#
# Description: Meta-learning for time-series forecasting and forecast combination
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali
#
#####################################################################################################################

##########
# Globals
##########

gc();
rm(list = ls());
while(!is.null(dev.list())) dev.off();

#setwd('C:/Users/IBM_ADMIN/Dropbox/PhD/TimeSeries/');
setwd('E:/Dropbox/PhD/TimeSeries/');
#setwd('C:/Users/Administrator/Dropbox/PhD/TimeSeries/');

# source files and libraries
source("Implementation/GlobalFunctions.R");
source("Implementation/TimeseriesAlgorithms.R");
source("Implementation/MetaFeaturing.R");
source("Implementation/Preprocessing.R");
source("Implementation/MetaLearning.R");
source("Implementation/MetaLearning_Scoring.R");
source("Implementation/MLLAnalysis.R");

#clearScreen();

#######################
# reading time-series #
#######################

Analysis = 'both';

MLLExecute = T; MLLTest = T; analysisExecute = T;
baseLearningLoad = T; MFLoad = T; applyPreprocess = T; fsAlgo = F; MLLLoad = T;
testBaseLearningLoad = T; testMFLoad = T; MLLNNGCLoad = T; applyPreprocessScoring = T; MLLScoringLoad = F;
ClustAnalysis = T; TreeAnalysis = F; ScoringAnalysis = T;

VIThreshold = 'all';
ScoringModel = 'NN';

forcastingHorizonNN3 = 18; forcastingHorizonNN5 = 56;

methods = c('MA', 'SES', 'Arima', 'Structural', 'NN');

NoOfTimeSeries = 111;
NoOfTimeSeriesMethods = length(methods);
NoOfMetafeatures = 25;
NoOfMLLAlgorithms = 3;

NoOfDatasetsTest = 6;
NoOfTimeSeriesTest = 11;

SMAPENN3 = matrix(rep(0, NoOfTimeSeries * NoOfTimeSeriesMethods), ncol = NoOfTimeSeries);
SMAPENN5 = matrix(rep(0, NoOfTimeSeries * NoOfTimeSeriesMethods), ncol = NoOfTimeSeries);

SMAPENNGC_A = matrix(rep(0, NoOfTimeSeriesTest * NoOfTimeSeriesMethods), ncol = NoOfTimeSeriesTest);
SMAPENNGC_B = matrix(rep(0, NoOfTimeSeriesTest * NoOfTimeSeriesMethods), ncol = NoOfTimeSeriesTest);
SMAPENNGC_C = matrix(rep(0, NoOfTimeSeriesTest * NoOfTimeSeriesMethods), ncol = NoOfTimeSeriesTest);
SMAPENNGC_D = matrix(rep(0, NoOfTimeSeriesTest * NoOfTimeSeriesMethods), ncol = NoOfTimeSeriesTest);
SMAPENNGC_E = matrix(rep(0, NoOfTimeSeriesTest * NoOfTimeSeriesMethods), ncol = NoOfTimeSeriesTest);
SMAPENNGC_F = matrix(rep(0, NoOfTimeSeriesTest * NoOfTimeSeriesMethods), ncol = NoOfTimeSeriesTest);

performanceNN3 = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);
performanceNN5 = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);

PerformanceNNGC_A = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);
PerformanceNNGC_B = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);
PerformanceNNGC_C = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);
PerformanceNNGC_D = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);
PerformanceNNGC_E = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);
PerformanceNNGC_F = matrix(rep(0, NoOfTimeSeriesMethods * 2), ncol = NoOfTimeSeriesMethods);

SMAPESNNGC_Min_NN3 = rep(0, NoOfTimeSeriesTest * NoOfDatasetsTest);
SMAPESNNGC_Min_NN5 = rep(0, NoOfTimeSeriesTest * NoOfDatasetsTest);
SMAPESNNGC_Min_Combined = rep(0, NoOfTimeSeriesTest * NoOfDatasetsTest);
MLLSMAPESNNGC_Min = rep(0, NoOfTimeSeriesTest * NoOfDatasetsTest);
MLLSMAPES_NN_NNGC_Min = rep(0, NoOfTimeSeriesTest * NoOfDatasetsTest);

AccuracyNN3 = rep(0, NoOfTimeSeries);
AccuracyNN5 = rep(0, NoOfTimeSeries);

MLLSMAPENN3 = rep(0, 3);
MLLSMAPENN5 = rep(0, 3);
MLLSMAPECombined = rep(0, 3);
MLLSMAPEGC = rep(0, 3);
MLLSMAPENNGC = rep(0, 3);

MLL_GCAccuracy = rep(0, 3);
MLL_NNGCAccuracy = rep(0, 3);

MethodNN3 = matrix(rep(0, NoOfTimeSeries * 3), ncol = 3);
MethodNN5 = matrix(rep(0, NoOfTimeSeries * 3), ncol = 3);
MethodCombined = matrix(rep(0, NoOfTimeSeries * 3 * 2), ncol = 3);

MethodNNGC_A = matrix(rep(0, NoOfTimeSeriesTest * 3), ncol = 3);
MethodNNGC_B = matrix(rep(0, NoOfTimeSeriesTest * 3), ncol = 3);
MethodNNGC_C = matrix(rep(0, NoOfTimeSeriesTest * 3), ncol = 3);
MethodNNGC_D = matrix(rep(0, NoOfTimeSeriesTest * 3), ncol = 3);
MethodNNGC_E = matrix(rep(0, NoOfTimeSeriesTest * 3), ncol = 3);
MethodNNGC_F = matrix(rep(0, NoOfTimeSeriesTest * 3), ncol = 3);

MethodNNGC = matrix(rep(0, NoOfTimeSeriesTest * 3 * NoOfDatasetsTest), ncol = 3);

BestMethodNNGC = rep(0, NoOfDatasetsTest); 

AccuracyNNGC_A = rep(0, NoOfTimeSeriesTest);
AccuracyNNGC_B = rep(0, NoOfTimeSeriesTest);
AccuracyNNGC_C = rep(0, NoOfTimeSeriesTest);
AccuracyNNGC_D = rep(0, NoOfTimeSeriesTest);
AccuracyNNGC_E = rep(0, NoOfTimeSeriesTest);
AccuracyNNGC_F = rep(0, NoOfTimeSeriesTest);

MetaFeaturesNN3 = matrix(rep(0.0, NoOfTimeSeries * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNN5 = matrix(rep(0.0, NoOfTimeSeries * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_A_NN3 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_B_NN3 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_C_NN3 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_D_NN3 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_E_NN3 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_F_NN3 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);

MetaFeaturesNNGC_A_NN5 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_B_NN5 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_C_NN5 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_D_NN5 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_E_NN5 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_F_NN5 = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);

MetaFeaturesNNGC_A = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_B = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_C = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_D = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_E = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);
MetaFeaturesNNGC_F = matrix(rep(0.0, NoOfTimeSeriesTest * NoOfMetafeatures), ncol = NoOfMetafeatures);

MLLAccuracyNN3 = rep(0, NoOfMLLAlgorithms);
MLLAccuracyNN5 = rep(0, NoOfMLLAlgorithms);
MLLAccuracyCombined = rep(0, NoOfMLLAlgorithms);

ScoringGC_NN = matrix(rep(0, NoOfDatasetsTest * 3), ncol = 3);

MLLModelNN3 = NULL; MLLModelNN3_NN = NULL; MLLModelNN3_DT = NULL; MLLModelNN3_SVM = NULL;
MLLModelNN5 = NULL; MLLModelNN5_NN = NULL; MLLModelNN5_DT = NULL; MLLModelNN5_SVM = NULL;
MLLModelCombined = NULL; MLLModelCombined_NN = NULL; MLLModelCombined_DT = NULL; MLLModelCombined_SVM = NULL;
MKNNGC = NULL; MKNNGC_NN = NULL; MKNNGC_DT = NULL; MKNNGC_SVM = NULL; 
MLLModelNN_NNGC_Model = NULL; MLLModelNN_NNGC_Model_NN = NULL; MLLModelNN_NNGC_Model_DT = NULL; MLLModelNN_NNGC_Model_SVM = NULL;

includeSetNN3 = NULL;
includeSetNN5 = NULL;
includeSetCombined = NULL;
clustNN3 = clustNN5 = clustCombined = clustNN3_NNGC = clustNN5_NNGC = clustCombined_NNGC = NULL;
clust_k = 20;
preprocNN3 = preprocNN5 = preprocNNCombined = NULL; 

##########################
# Meta-learning training #
##########################

if(MLLExecute == T) {
  
  #############
  # Load Data #
  #############
  datasetNN3 = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NN/NN3.csv", sep = ''), header = TRUE);
  datasetNN5 = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NN/NN5.csv", sep = ''), header = TRUE);
  
  if(baseLearningLoad == F) {
    
    #####################
    # Accuracy Measures #
    #####################
    if(Analysis != 'NN5') {
      MAPES = TimeSeries(SMAPENN3, dataset = datasetNN3, seriesNN = 'NN3', Method = MethodNN3, Accuracy = AccuracyNN3, NoOfTimeSeries = NoOfTimeSeries, header = 3, forcastingHorizon = forcastingHorizonNN3, Lag = 12, NNLayers = 12);
      SMAPENN3 = MAPES$SMAPE;
      AccuracyNN3 = MAPES$Accuracy;
      MethodNN3 = MAPES$Method;
      
      # compute SMAPE and standard deviation
      performanceNN3 = SMAPE(SMAPENN3, performanceNN3, series = NoOfTimeSeries);
      
      save(AccuracyNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3Accuracy", sep = ''));
      save(MethodNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3Method", sep = ''));
      save(SMAPENN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3SMAPE", sep = ''));
      save(performanceNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3Performances", sep = ''));
    }
    
    if(Analysis != 'NN3') {
      MAPES = TimeSeries(SMAPENN5, dataset = datasetNN5, seriesNN = 'NN5', Method = MethodNN5, Accuracy = AccuracyNN5, NoOfTimeSeries = NoOfTimeSeries, header = 2, forcastingHorizon = forcastingHorizonNN5, Lag = 12, NNLayers = 12);
      SMAPENN5 = MAPES$SMAPE;
      AccuracyNN5 = MAPES$Accuracy;
      MethodNN5 = MAPES$Method;
      
      # compute SMAPE and standard deviation
      performanceNN5 = SMAPE(SMAPENN5, performanceNN5, series = NoOfTimeSeries);
      
      save(AccuracyNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5Accuracy", sep = ''));
      save(MethodNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5Method", sep = ''));
      save(SMAPENN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5SMAPE", sep = ''));
      save(performanceNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5Performances", sep = ''));
    }
  } else {
    if(Analysis != 'NN5') {
      AccuracyNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Accuracy", sep = '')));
      MethodNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Method", sep = '')));
      SMAPENN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3SMAPE", sep = '')));
      performanceNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Performances", sep = '')));
    }

    if(Analysis != 'NN3') {
      AccuracyNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Accuracy", sep = '')));
      MethodNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Method", sep = '')));
      SMAPENN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5SMAPE", sep = '')));
      performanceNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Performances", sep = '')));
    }
  }
  
  #########################
  # Compute Meta-features #
  #########################
  
  if(MFLoad == F) {
    if(Analysis != 'NN5') {
      features = MetaFeatures(MetaFeaturesNN3, datasetNN3, SMAPENN3, forcastingHorizonNN = 18, TS = 'NN3', header = 3, NoOfTimeSeries = NoOfTimeSeries);
      MetaFeaturesNN3 = features$MetaFeaturesNN;
      
      save(MetaFeaturesNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3MFs", sep = ''));
    }
    
    if(Analysis != 'NN3') {
      features = MetaFeatures(MetaFeaturesNN5, datasetNN5, SMAPENN5, forcastingHorizonNN = 56, TS = 'NN5', header = 2, NoOfTimeSeries = NoOfTimeSeries);
      MetaFeaturesNN5 = features$MetaFeaturesNN;
      
      save(MetaFeaturesNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5MFs", sep = ''));
    }
  } else {
    if(Analysis != 'NN5') {
      #MetaFeaturesNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3MFs", sep = '')));
      MetaFeaturesNN3 = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/MetaFeatures/NN3_MetaFeatures.csv", sep = ''), header = TRUE);
    }  
    
    if(Analysis != 'NN3') {
      #MetaFeaturesNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5MFs", sep = '')));
      MetaFeaturesNN5 = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/MetaFeatures/NN5_MetaFeatures.csv", sep = ''), header = TRUE);
    }
  }

  ################################
  # Meta-knowledge Preprocessing #
  ################################

  if(applyPreprocess == T) {
    if(Analysis != 'NN5') {
      MKNN3 = data.frame(cbind(MetaFeaturesNN3, MethodNN3[, 1]));
      names(MKNN3)[dim(MKNN3)[2]] = 'Method';

      includeSetNN3 = VarImportance(MKNN3, VIThreshold, fsAlgo);

      MKNN3 = data.frame(cbind(MetaFeaturesNN3[, na.omit(match(includeSetNN3$vars, colnames(MetaFeaturesNN3)))], MethodNN3[, 1]));
      names(MKNN3)[dim(MKNN3)[2]] = 'Method';
      
      save(includeSetNN3, file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN3", "_", VIThreshold, sep = '')); 
    } 
    
    if(Analysis != 'NN3') {
      MKNN5 = data.frame(cbind(MetaFeaturesNN5, MethodNN5[, 1]));
      names(MKNN5)[dim(MKNN5)[2]] = 'Method';
      
      includeSetNN5 = VarImportance(MKNN5, VIThreshold, fsAlgo);
      
      MKNN5 = data.frame(cbind(MetaFeaturesNN5[, na.omit(match(includeSetNN5$vars, colnames(MetaFeaturesNN5)))], MethodNN5[, 1]));
      names(MKNN5)[dim(MKNN5)[2]] = 'Method';
      
      save(includeSetNN5, file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN5", "_", VIThreshold, sep = '')); 
    }
    
    if(Analysis == 'both') {
      MKNN3_C = data.frame(cbind(MetaFeaturesNN3, MethodNN3[, 1]));
      MKNN5_C = data.frame(cbind(MetaFeaturesNN5, MethodNN5[, 1]));
      names(MKNN3_C)[dim(MKNN3_C)[2]] = 'Method';
      names(MKNN5_C)[dim(MKNN5_C)[2]] = 'Method';
      
      MKCombined = data.frame(rbind(MKNN3_C, MKNN5_C));

      includeSetCombined = VarImportance(MKCombined, VIThreshold, fsAlgo);
      
      MKNN3_C = cbind(MetaFeaturesNN3[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN3)))], MethodNN3[, 1]);
      MKNN5_C = cbind(MetaFeaturesNN5[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN5)))], MethodNN5[, 1]);
      names(MKNN3_C)[dim(MKNN3_C)[2]] = 'Method';
      names(MKNN5_C)[dim(MKNN5_C)[2]] = 'Method';
      
      MKCombined = data.frame(rbind(MKNN3_C, MKNN5_C));
            
      save(includeSetCombined, file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessCombined", "_", VIThreshold, sep = ''));
    } 
  }
#   } else {
#     if(Analysis != 'NN5') {
#       includeSetNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN3", "_", VIThreshold, sep = ''))); 
#       MKNN3 = data.frame(cbind(MetaFeaturesNN3[, na.omit(match(includeSetNN3$vars, colnames(MetaFeaturesNN3)))], MethodNN3[, 1]));
#     } 
#     
#     if(Analysis != 'NN3') {
#       includeSetNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN5", "_", VIThreshold, sep = ''))); 
#       MKNN5 = data.frame(cbind(MetaFeaturesNN5[, na.omit(match(includeSetNN5$vars, colnames(MetaFeaturesNN5)))], MethodNN5[, 1]));
#     }
#     
#     if(Analysis == 'both') {
#       includeSetCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessCombined", "_", VIThreshold, sep = '')));
#       MKNN3_C = cbind(MetaFeaturesNN3[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN3)))], MethodNN3[, 1]);
#       MKNN5_C = cbind(MetaFeaturesNN5[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN5)))], MethodNN5[, 1]);
#       names(MKNN3_C)[dim(MKNN3_C)[2]] = 'Method';
#       names(MKNN5_C)[dim(MKNN5_C)[2]] = 'Method';
#       
#       MKCombined = data.frame(rbind(MKNN3_C, MKNN5_C));
#     }      
#   }

  #################
  # Meta-learning #
  #################

  mlTechniques = c('NN', 'DT', 'SVM');
  
  if(MLLLoad == F) {
    if(Analysis != 'NN5') {
      print('NN3');
      
      preprocNN3 = preProcess(MKNN3[, -dim(MKNN3)[2]], method = c("center", "scale"));
      MKNN3_preproc = predict(preprocNN3, MKNN3[, -dim(MKNN3)[2]]);

      accuracy = MetaLearning(MK = MKNN3_preproc, target = MKNN3[, dim(MKNN3)[2]], MLLAccuracy = MLLAccuracyNN3, Method = MethodNN3, mlTechniques = mlTechniques, Analysis = 'NN3', MLLSMAPE = MLLSMAPENN3);
      MLLAccuracyNN3 = accuracy$MLLAccuracy;
      MethodNN3 = accuracy$Method;
      MLLModelNN3 = accuracy$MLLModel;
      MLLModelNN3_NN = accuracy$MLLNNModel;
      MLLModelNN3_DT = accuracy$MLLDTModel;
      MLLModelNN3_SVM = accuracy$MLLSVMModel;
      MLLSMAPENN3 = accuracy$MLLSMAPE;
      
      save(MethodNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3Method", "_", VIThreshold, sep = ''));
      save(MLLAccuracyNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3MLLAccuracy", "_", VIThreshold, sep = ''));
      save(MLLModelNN3, file = paste(getwd(), "/Implementation/Objects/NN/NN3Model", "_", VIThreshold, sep = ''));
      save(MLLModelNN3_NN, file = paste(getwd(), "/Implementation/Objects/NN/NN3Model_NN", "_", VIThreshold, sep = ''));
      save(MLLModelNN3_DT, file = paste(getwd(), "/Implementation/Objects/NN/NN3Model_DT", "_", VIThreshold, sep = ''));
      save(MLLModelNN3_SVM, file = paste(getwd(), "/Implementation/Objects/NN/NN3Model_SVM", "_", VIThreshold, sep = ''));
      save(MLLSMAPENN3, file = paste(getwd(), "/Implementation/Objects/NN/MLLSMAPENN3", "_", VIThreshold, sep = ''));
      save(preprocNN3, file = paste(getwd(), "/Implementation/Objects/NN/preprocNN3", "_", VIThreshold, sep = ''));
    }
    
    if(Analysis != 'NN3') {
      print('NN5');
      
      preprocNN5 = preProcess(MKNN5[, -dim(MKNN5)[2]], method = c("center", "scale"));
      MKNN5_preproc = predict(preprocNN5, MKNN5[, -dim(MKNN5)[2]]);
      
      accuracy = MetaLearning(MK = MKNN5_preproc, target = MKNN5[, dim(MKNN5)[2]], MLLAccuracy = MLLAccuracyNN5, Method = MethodNN5, mlTechniques = mlTechniques, Analysis = 'NN5', MLLSMAPE = MLLSMAPENN5);
      MLLAccuracyNN5 = accuracy$MLLAccuracy;
      MethodNN5 = accuracy$Method;
      MLLModelNN5 = accuracy$MLLModel;
      MLLModelNN5_NN = accuracy$MLLNNModel;
      MLLModelNN5_DT = accuracy$MLLDTModel;
      MLLModelNN5_SVM = accuracy$MLLSVMModel;
      MLLSMAPENN5 = accuracy$MLLSMAPE;
      
      save(MethodNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5Method", "_", VIThreshold, sep = ''));
      save(MLLAccuracyNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5MLLAccuracy", "_", VIThreshold, sep = ''));
      save(MLLModelNN5, file = paste(getwd(), "/Implementation/Objects/NN/NN5Model", "_", VIThreshold, sep = ''));
      save(MLLModelNN5_NN, file = paste(getwd(), "/Implementation/Objects/NN/NN5Model_NN", "_", VIThreshold, sep = ''));
      save(MLLModelNN5_DT, file = paste(getwd(), "/Implementation/Objects/NN/NN5Model_DT", "_", VIThreshold, sep = ''));
      save(MLLModelNN5_SVM, file = paste(getwd(), "/Implementation/Objects/NN/NN5Model_SVM", "_", VIThreshold, sep = ''));
      save(MLLSMAPENN5, file = paste(getwd(), "/Implementation/Objects/NN/MLLSMAPENN5", "_", VIThreshold, sep = ''));
      save(preprocNN5, file = paste(getwd(), "/Implementation/Objects/NN/preprocNN5", "_", VIThreshold, sep = ''));
    }
    
    if(Analysis == 'both') {
      print('Combined NN3 and NN5');
      MethodCombined = rbind(MethodNN3, MethodNN5);
      
      preprocNNCombined = preProcess(MKCombined[, -dim(MKCombined)[2]], method = c("center", "scale"));
      MKNNCombined_preproc = predict(preprocNNCombined, MKCombined[, -dim(MKCombined)[2]]);
      
      accuracy = MetaLearning(MK = MKNNCombined_preproc, target = MKCombined[, dim(MKCombined)[2]], MLLAccuracy = MLLAccuracyCombined, Method = MethodCombined, mlTechniques = mlTechniques, Analysis = 'both', MLLSMAPE = MLLSMAPECombined);
      MLLAccuracyCombined = accuracy$MLLAccuracy;
      MethodCombined = accuracy$Method;
      MLLModelCombined = accuracy$MLLModel;
      MLLModelCombined_NN = accuracy$MLLNNModel;
      MLLModelCombined_DT = accuracy$MLLDTModel;
      MLLModelCombined_SVM = accuracy$MLLSVMModel;
      MLLSMAPECombined = accuracy$MLLSMAPE;
      
      save(MethodCombined, file = paste(getwd(), "/Implementation/Objects/NN/CombinedMethod", "_", VIThreshold, sep = ''));
      save(MLLAccuracyCombined, file = paste(getwd(), "/Implementation/Objects/NN/CombinedMLLAccuracy", "_", VIThreshold, sep = ''));
      save(MLLModelCombined, file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel", "_", VIThreshold, sep = ''));
      save(MLLModelCombined_NN, file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel_NN", "_", VIThreshold, sep = ''));
      save(MLLModelCombined_DT, file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel_DT", "_", VIThreshold, sep = ''));
      save(MLLModelCombined_SVM, file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel_SVM", "_", VIThreshold, sep = ''));
      save(MLLSMAPECombined, file = paste(getwd(), "/Implementation/Objects/NN/MLLSMAPECombined", "_", VIThreshold, sep = ''));
      save(preprocNNCombined, file = paste(getwd(), "/Implementation/Objects/NN/preprocNNCombined", "_", VIThreshold, sep = ''));
    }
  } else {
    if(Analysis != 'NN5') {
      MethodNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Method", "_", VIThreshold, sep = ''))); 
      MLLAccuracyNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3MLLAccuracy", "_", VIThreshold, sep = '')));
      MLLModelNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Model", "_", VIThreshold, sep = '')));
      MLLModelNN3_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Model_NN", "_", VIThreshold, sep = '')));
      MLLModelNN3_DT = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Model_DT", "_", VIThreshold, sep = '')));
      MLLModelNN3_SVM = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Model_SVM", "_", VIThreshold, sep = '')));
      MLLSMAPENN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/MLLSMAPENN3", "_", VIThreshold, sep = '')));
      preprocNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/preprocNN3", "_", VIThreshold, sep = '')));
    }

    if(Analysis != 'NN3') {
      MethodNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Method", "_", VIThreshold, sep = '')));
      MLLAccuracyNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5MLLAccuracy", "_", VIThreshold, sep = '')));
      MLLModelNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Model", "_", VIThreshold, sep = '')));
      MLLModelNN5_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Model_NN", "_", VIThreshold, sep = '')));
      MLLModelNN5_DT = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Model_DT", "_", VIThreshold, sep = '')));
      MLLModelNN5_SVM = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Model_SVM", "_", VIThreshold, sep = '')));
      MLLSMAPENN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/MLLSMAPENN5", "_", VIThreshold, sep = '')));
      preprocNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/preprocNN5", "_", VIThreshold, sep = '')));
    }

    if(Analysis != 'NN3' && Analysis != 'NN5') {      
      MethodCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedMethod", "_", VIThreshold, sep = '')));
      MLLAccuracyCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedMLLAccuracy", "_", VIThreshold, sep = '')));
      MLLModelCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel", "_", VIThreshold, sep = '')));
      MLLModelCombined_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel_NN", "_", VIThreshold, sep = '')));
      MLLModelCombined_DT = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel_DT", "_", VIThreshold, sep = '')));
      MLLModelCombined_SVM = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel_SVM", "_", VIThreshold, sep = '')));
      MLLSMAPECombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/MLLSMAPECombined", "_", VIThreshold, sep = '')));  
      preprocNNCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/preprocNNCombined", "_", VIThreshold, sep = '')));
    }
  }

  ############
  # Accuracy #
  ############
  
  print("Forecasting Accuracy:")
  print(performanceNN3);
  print(performanceNN5);

  print("Meta-learning Accuracy:")
  print(MLLAccuracyNN3);
  print(MLLAccuracyNN5);
  print(MLLAccuracyCombined);
}

#########################
# Meta-learning testing #
#########################

if(MLLTest == T) {
  #############
  # Load Data #
  #############
  
  datasetNN_GC_A = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NNGC/NNGC_A.csv", sep = ''), header = FALSE);
  datasetNN_GC_B = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NNGC/NNGC_B.csv", sep = ''), header = FALSE);
  datasetNN_GC_C = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NNGC/NNGC_C.csv", sep = ''), header = FALSE);
  datasetNN_GC_D = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NNGC/NNGC_D.csv", sep = ''), header = FALSE);
  datasetNN_GC_E = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NNGC/NNGC_E.csv", sep = ''), header = FALSE);
  datasetNN_GC_F = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/Datasets/NNGC/NNGC_F.csv", sep = ''), header = FALSE);
  
  if(testBaseLearningLoad == F) {
    #####################
    # Accuracy Measures #
    #####################
    
    MAPES = TimeSeries(SMAPENNGC_A, dataset = datasetNN_GC_A, seriesNN = 'NNGC_A', Method = MethodNNGC_A, Accuracy = AccuracyNNGC_A, NoOfTimeSeries = NoOfTimeSeriesTest, Lag = 1, smaN = 15, NNLayers = 7);
    MethodNNGC_A = MAPES$Method;
    AccuracyNNGC_A = MAPES$Accuracy;
    SMAPENNGC_A = MAPES$SMAPE;
    PerformanceNNGC_A = SMAPE(SMAPENNGC_A, PerformanceNNGC_A, series = NoOfTimeSeriesTest);
    save(MethodNNGC_A, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Method", sep = ''));
    save(SMAPENNGC_A, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_SMAPE", sep = ''));
    save(AccuracyNNGC_A, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Accuracy", sep = ''));
    save(PerformanceNNGC_A, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Performance", sep = ''));
    
    MAPES = TimeSeries(SMAPENNGC_B, dataset = datasetNN_GC_B, seriesNN = 'NNGC_B', Method = MethodNNGC_B, Accuracy = AccuracyNNGC_B, NoOfTimeSeries = NoOfTimeSeriesTest, Lag = 4, smaN = 15, NNLayers = 7);
    MethodNNGC_B = MAPES$Method;
    SMAPENNGC_B = MAPES$SMAPE;
    PerformanceNNGC_B = SMAPE(SMAPENNGC_B, PerformanceNNGC_B, series = NoOfTimeSeriesTest);
    save(MethodNNGC_B, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Method", sep = ''));
    save(SMAPENNGC_B, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_SMAPE", sep = ''));
    save(AccuracyNNGC_B, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Accuracy", sep = ''));
    save(PerformanceNNGC_B, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Performance", sep = ''));
    
    MAPES = TimeSeries(SMAPENNGC_C, dataset = datasetNN_GC_C, seriesNN = 'NNGC_C', Method = MethodNNGC_C, Accuracy = AccuracyNNGC_C, NoOfTimeSeries = NoOfTimeSeriesTest, Lag = 12, NNLayers = 9);
    MethodNNGC_C = MAPES$Method;
    AccuracyNNGC_C = MAPES$Accuracy;
    SMAPENNGC_C = MAPES$SMAPE;
    PerformanceNNGC_C = SMAPE(SMAPENNGC_C, PerformanceNNGC_C, series = NoOfTimeSeriesTest);
    save(MethodNNGC_C, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Method", sep = ''));
    save(SMAPENNGC_C, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_SMAPE", sep = ''));
    save(AccuracyNNGC_C, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Accuracy", sep = ''));
    save(PerformanceNNGC_C, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Performance", sep = ''));
    
    MAPES = TimeSeries(SMAPENNGC_D, dataset = datasetNN_GC_D, seriesNN = 'NNGC_D', Method = MethodNNGC_D, Accuracy = AccuracyNNGC_D, NoOfTimeSeries = NoOfTimeSeriesTest, Lag = 7, NNLayers = 9);
    MethodNNGC_D = MAPES$Method;
    AccuracyNNGC_D = MAPES$Accuracy;
    SMAPENNGC_D = MAPES$SMAPE;
    PerformanceNNGC_D = SMAPE(SMAPENNGC_D, PerformanceNNGC_D, series = NoOfTimeSeriesTest);
    save(MethodNNGC_D, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Method", sep = ''));
    save(SMAPENNGC_D, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_SMAPE", sep = ''));
    save(AccuracyNNGC_D, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Accuracy", sep = ''));
    save(PerformanceNNGC_D, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Performance", sep = ''));
    
    MAPES = TimeSeries(SMAPENNGC_E, dataset = datasetNN_GC_E, seriesNN = 'NNGC_E', Method = MethodNNGC_E, Accuracy = AccuracyNNGC_E, NoOfTimeSeries = NoOfTimeSeriesTest, Lag = 7, NNLayers = 11);
    MethodNNGC_E = MAPES$Method;
    AccuracyNNGC_E = MAPES$Accuracy;
    SMAPENNGC_E = MAPES$SMAPE;
    PerformanceNNGC_E = SMAPE(SMAPENNGC_E, PerformanceNNGC_E, series = NoOfTimeSeriesTest);
    save(MethodNNGC_E, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Method", sep = ''));
    save(SMAPENNGC_E, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_SMAPE", sep = ''));
    save(AccuracyNNGC_E, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Accuracy", sep = ''));
    save(PerformanceNNGC_E, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Performance", sep = ''));
    
    MAPES = TimeSeries(SMAPENNGC_F, dataset = datasetNN_GC_F, seriesNN = 'NNGC_F', Method = MethodNNGC_F, Accuracy = AccuracyNNGC_F, NoOfTimeSeries = NoOfTimeSeriesTest, Lag = 7, NNLayers = 12);
    MethodNNGC_F = MAPES$Method;
    AccuracyNNGC_F = MAPES$Accuracy;
    SMAPENNGC_F = MAPES$SMAPE;
    PerformanceNNGC_F = SMAPE(SMAPENNGC_F, PerformanceNNGC_F, series = NoOfTimeSeriesTest);
    save(MethodNNGC_F, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Method", sep = ''));    
    save(SMAPENNGC_F, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_SMAPE", sep = ''));  
    save(AccuracyNNGC_F, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Accuracy", sep = ''));  
    save(PerformanceNNGC_F, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Performance", sep = ''));  
  } else {
    MethodNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Method", sep = '')));
    MethodNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Method", sep = '')));
    MethodNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Method", sep = '')));
    MethodNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Method", sep = '')));
    MethodNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Method", sep = '')));
    MethodNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Method", sep = '')));
    
    SMAPENNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_SMAPE", sep = '')));
    SMAPENNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_SMAPE", sep = '')));
    SMAPENNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_SMAPE", sep = '')));
    SMAPENNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_SMAPE", sep = '')));
    SMAPENNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_SMAPE", sep = '')));
    SMAPENNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_SMAPE", sep = '')));
    
    AccuracyNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Accuracy", sep = '')));
    AccuracyNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Accuracy", sep = '')));
    AccuracyNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Accuracy", sep = '')));
    AccuracyNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Accuracy", sep = '')));
    AccuracyNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Accuracy", sep = '')));
    AccuracyNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Accuracy", sep = '')));
    
    PerformanceNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Performance", sep = '')));
    PerformanceNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Performance", sep = '')));
    PerformanceNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Performance", sep = '')));
    PerformanceNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Performance", sep = '')));
    PerformanceNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Performance", sep = '')));
    PerformanceNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Performance", sep = '')));
  }
  
  #########################
  # Compute Meta-features #
  #########################
  
  if(testMFLoad == F) {
    features = MetaFeatures(MetaFeaturesNNGC_A, datasetNN_GC_A, SMAPENNGC_A, forcastingHorizonNN = 0, TS = 'NNGC_A', header = 0, NoOfTimeSeries = NoOfTimeSeriesTest);
    MetaFeaturesNNGC_A_NN3 = MetaFeaturesNNGC_A_NN5 = MetaFeaturesNNGC_A = features$MetaFeaturesNN;
    
    features = MetaFeatures(MetaFeaturesNNGC_B, datasetNN_GC_B, SMAPENNGC_B, forcastingHorizonNN = 4, TS = 'NNGC_B', header = 0, NoOfTimeSeries = NoOfTimeSeriesTest);
    MetaFeaturesNNGC_B_NN3 = MetaFeaturesNNGC_B_NN5 = MetaFeaturesNNGC_B = features$MetaFeaturesNN;
    
    features = MetaFeatures(MetaFeaturesNNGC_C, datasetNN_GC_C, SMAPENNGC_C, forcastingHorizonNN = 12, TS = 'NNGC_C', header = 0, NoOfTimeSeries = NoOfTimeSeriesTest);
    MetaFeaturesNNGC_C_NN3 = MetaFeaturesNNGC_C_NN5 = MetaFeaturesNNGC_C = features$MetaFeaturesNN;
    
    features = MetaFeatures(MetaFeaturesNNGC_D, datasetNN_GC_D, SMAPENNGC_D, forcastingHorizonNN = 52, TS = 'NNGC_D', header = 0, NoOfTimeSeries = NoOfTimeSeriesTest);
    MetaFeaturesNNGC_D_NN3 = MetaFeaturesNNGC_D_NN5 = MetaFeaturesNNGC_D = features$MetaFeaturesNN;
    
    features = MetaFeatures(MetaFeaturesNNGC_E, datasetNN_GC_E, SMAPENNGC_E, forcastingHorizonNN = 7, TS = 'NNGC_E', header = 0, NoOfTimeSeries = NoOfTimeSeriesTest);
    MetaFeaturesNNGC_E_NN3 = MetaFeaturesNNGC_E_NN5 = MetaFeaturesNNGC_E = features$MetaFeaturesNN;
    
    features = MetaFeatures(MetaFeaturesNNGC_F, datasetNN_GC_F, SMAPENNGC_F, forcastingHorizonNN = 24, TS = 'NNGC_F', header = 0, NoOfTimeSeries = NoOfTimeSeriesTest);
    MetaFeaturesNNGC_F_NN3 = MetaFeaturesNNGC_F_NN5 = MetaFeaturesNNGC_F = features$MetaFeaturesNN;
    
    save(MetaFeaturesNNGC_A, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_MFs", sep = ''));
    save(MetaFeaturesNNGC_B, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_MFs", sep = ''));
    save(MetaFeaturesNNGC_C, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_MFs", sep = ''));
    save(MetaFeaturesNNGC_D, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_MFs", sep = ''));
    save(MetaFeaturesNNGC_E, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_MFs", sep = ''));
    save(MetaFeaturesNNGC_F, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_MFs", sep = ''));
  } else {
    #MetaFeaturesNNGC_A_NN3 = MetaFeaturesNNGC_A_NN5 = MetaFeaturesNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_MFs", sep = '')));
    #MetaFeaturesNNGC_B_NN3 = MetaFeaturesNNGC_B_NN5 = MetaFeaturesNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_MFs", sep = '')));
    #MetaFeaturesNNGC_C_NN3 = MetaFeaturesNNGC_C_NN5 = MetaFeaturesNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_MFs", sep = '')));
    #MetaFeaturesNNGC_D_NN3 = MetaFeaturesNNGC_D_NN5 = MetaFeaturesNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_MFs", sep = '')));
    #MetaFeaturesNNGC_E_NN3 = MetaFeaturesNNGC_E_NN5 = MetaFeaturesNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_MFs", sep = '')));
    #MetaFeaturesNNGC_F_NN3 = MetaFeaturesNNGC_F_NN5 = MetaFeaturesNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_MFs", sep = '')));
    MetaFeaturesNNGC = LoadData(storageMedium = "csv", filePath = paste(getwd(), "/Implementation/MetaFeatures/NNGC_MetaFeatures.csv", sep = ''), header = TRUE);
    MetaFeaturesNNGC_A_NN3 = MetaFeaturesNNGC_A_NN5 = MetaFeaturesNNGC_A = MetaFeaturesNNGC[1:11, ];
    MetaFeaturesNNGC_B_NN3 = MetaFeaturesNNGC_B_NN5 = MetaFeaturesNNGC_B = MetaFeaturesNNGC[12:22, ];
    MetaFeaturesNNGC_C_NN3 = MetaFeaturesNNGC_C_NN5 = MetaFeaturesNNGC_C = MetaFeaturesNNGC[23:33, ];
    MetaFeaturesNNGC_D_NN3 = MetaFeaturesNNGC_D_NN5 = MetaFeaturesNNGC_D = MetaFeaturesNNGC[34:44, ];
    MetaFeaturesNNGC_E_NN3 = MetaFeaturesNNGC_E_NN5 = MetaFeaturesNNGC_E = MetaFeaturesNNGC[45:55, ];
    MetaFeaturesNNGC_F_NN3 = MetaFeaturesNNGC_F_NN5 = MetaFeaturesNNGC_F = MetaFeaturesNNGC[56:66, ];
  } 
  
  ##########################
  # Meta-learning on NN GC #
  ##########################
  
  includeSetNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN3", "_", VIThreshold, sep = '')));
  includeSetNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN5", "_", VIThreshold, sep = '')));
  includeSetCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessCombined", "_", VIThreshold, sep = '')));
  
  MLL_GC = rep(0, 2);
  if(MLLNNGCLoad == F) {
    # NNGC
    # combine 6 NN GC Metafeautre datasets and Methods
    MetaFeaturesNNGC = rbind(MetaFeaturesNNGC_A, MetaFeaturesNNGC_B, MetaFeaturesNNGC_C, MetaFeaturesNNGC_D, MetaFeaturesNNGC_E, MetaFeaturesNNGC_F);
    MethodNNGC_Base = cbind(t(MethodNNGC_A[, 1]), t(MethodNNGC_B[, 1]), t(MethodNNGC_C[, 1]), t(MethodNNGC_D[, 1]), t(MethodNNGC_E[, 1]), t(MethodNNGC_F[, 1]));
    MethodNNGCAll = rbind(MethodNNGC_A, MethodNNGC_B, MethodNNGC_C, MethodNNGC_D, MethodNNGC_E, MethodNNGC_F);
    MKNNGC = data.frame(cbind(MetaFeaturesNNGC, t(MethodNNGC_Base)));
    names(MKNNGC)[dim(MKNNGC)[2]] = 'Method'
    MKNNGC_VI = VarImportance(MKNNGC, VIThreshold, fsAlgo);
    MKNNGC = data.frame(cbind(MetaFeaturesNNGC[, na.omit(match(MKNNGC_VI$vars, colnames(MetaFeaturesNNGC)))], t(MethodNNGC_Base)));
    
    # Meta-learning NN GC 
    mlTechniques = c('NN', 'DT', 'SVM');
    MLLAccuracyNNGC = rep(0, NoOfMLLAlgorithms);
    accuracy = MetaLearning(MK = MKNNGC, MLLAccuracy = MLLAccuracyNNGC, Method = MethodNNGCAll, mlTechniques = mlTechniques, Analysis = 'NNGC', MLLSMAPE = MLLSMAPEGC);
    MLL_GCAccuracy = accuracy$MLLAccuracy;
    MLL_GC[1] = max(accuracy$MLLAccuracy);
    MLLMethodNNGC = accuracy$Method;
    MLLModelNNGC = accuracy$MLLModel;
    MLLModelNNGC_NN = accuracy$MLLNNModel;
    print(MLLModelNNGC_NN)
    MLLModelNNGC_DT = accuracy$MLLDTModel;
    MLLModelNNGC_SVM = accuracy$MLLSVMModel;
    MLLSMAPEGC = accuracy$MLLSMAPE;
    
    # Meta-learning NN + NN GC 
    MKALL = data.frame(rbind(cbind(MetaFeaturesNN3[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN3)))], MethodNN3[, 1]), cbind(MetaFeaturesNN5[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN5)))], MethodNN5[, 1]), cbind(MetaFeaturesNNGC[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNNGC)))], t(MethodNNGC_Base))));
    accuracy = MetaLearning(MK = MKALL, MLLAccuracy = MLLAccuracyNNGC, Method = rbind(MethodNN3, MethodNN5, MethodNNGCAll), mlTechniques = mlTechniques, Analysis, MLLSMAPE = MLLSMAPENNGC);
    MLL_NNGCAccuracy = accuracy$MLLAccuracy;
    MLL_GC[2] = max(accuracy$MLLAccuracy);
    MLLMethod_NN_NNGC_Methods = accuracy$Method;
    MLLModelNN_NNGC_Model = accuracy$MLLModel;
    MLLModelNN_NNGC_Model_NN = accuracy$MLLNNModel;
    MLLModelNN_NNGC_Model_DT = accuracy$MLLDTModel;
    MLLModelNN_NNGC_Model_SVM = accuracy$MLLSVMModel;
    MLLSMAPENNGC = accuracy$MLLSMAPE;
    
    save(MKNNGC_VI, file = paste(getwd(), "/Implementation/Objects/NN/MKNNGC_VI", "_", VIThreshold, sep = ''));
    save(MLL_GCAccuracy, file = paste(getwd(), "/Implementation/Objects/NNGC/MLL_GCAccuracy", "_", VIThreshold, sep = ''));
    save(MLLMethodNNGC, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLMethodNNGC", "_", VIThreshold, sep = ''));
    save(MLLModelNNGC, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC", "_", VIThreshold, sep = ''));
    save(MLLModelNNGC_NN, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC_NN", "_", VIThreshold, sep = ''));
    save(MLLModelNNGC_DT, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC_DT", "_", VIThreshold, sep = ''));
    save(MLLModelNNGC_SVM, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC_SVM", "_", VIThreshold, sep = ''));
    save(MLLSMAPEGC, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLSMAPEGC", "_", VIThreshold, sep = ''));
    
    save(MLL_NNGCAccuracy, file = paste(getwd(), "/Implementation/Objects/NNGC/MLL_NNGCAccuracy", "_", VIThreshold, sep = ''));
    save(MLLMethod_NN_NNGC_Methods, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLMethod_NN_NNGC_Methods", "_", VIThreshold, sep = ''));
    save(MLLModelNN_NNGC_Model, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model", "_", VIThreshold, sep = ''));
    save(MLLModelNN_NNGC_Model_NN, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model_NN", "_", VIThreshold, sep = ''));
    save(MLLModelNN_NNGC_Model_DT, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model_DT", "_", VIThreshold, sep = ''));
    save(MLLModelNN_NNGC_Model_SVM, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model_SVM", "_", VIThreshold, sep = ''));
    save(MLL_GC, file = paste(getwd(), "/Implementation/Objects/NNGC/MLL_GC", "_", VIThreshold, sep = ''));
    save(MLLSMAPENNGC, file = paste(getwd(), "/Implementation/Objects/NNGC/MLLSMAPENNGC", "_", VIThreshold, sep = ''));
  } else {
    MKNNGC_VI = get(load(file = paste(getwd(), "/Implementation/Objects/NN/MKNNGC_VI", "_", VIThreshold, sep = '')));
    MLL_GCAccuracy = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLL_GCAccuracy", "_", VIThreshold, sep = '')));
    MLLMethodNNGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLMethodNNGC", "_", VIThreshold, sep = '')));
    MLLModelNNGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC", "_", VIThreshold, sep = '')));
    MLLModelNNGC_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC_NN", "_", VIThreshold, sep = '')));
    MLLModelNNGC_DT = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC_DT", "_", VIThreshold, sep = '')));
    MLLModelNNGC_SVM = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNNGC_SVM", "_", VIThreshold, sep = '')));
    MLLSMAPEGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLSMAPEGC", "_", VIThreshold, sep = '')));
    
    MLL_NNGCAccuracy = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLL_NNGCAccuracy", "_", VIThreshold, sep = '')));
    MLLMethod_NN_NNGC_Methods = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLMethod_NN_NNGC_Methods", "_", VIThreshold, sep = '')));
    MLLModelNN_NNGC_Model = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model", "_", VIThreshold, sep = '')));
    MLLModelNN_NNGC_Model_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model_NN", "_", VIThreshold, sep = '')));
    MLLModelNN_NNGC_Model_DT = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model_DT", "_", VIThreshold, sep = '')));
    MLLModelNN_NNGC_Model_SVM = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model_SVM", "_", VIThreshold, sep = '')));
    MLL_GC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLL_GC", "_", VIThreshold, sep = '')));
    MLLSMAPENNGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLSMAPENNGC", "_", VIThreshold, sep = '')));
  }
  
  ################################
  # Meta-knowledge Preprocessing #
  ################################
  
  if(applyPreprocessScoring == T) {
    MetaFeaturesNNGC_A_NN3 = data.frame(MetaFeaturesNNGC_A_NN3[, includeSetNN3$vars]);     # eliminate near to zero variables
    MetaFeaturesNNGC_B_NN3 = data.frame(MetaFeaturesNNGC_B_NN3[, includeSetNN3$vars]);
    MetaFeaturesNNGC_C_NN3 = data.frame(MetaFeaturesNNGC_C_NN3[, includeSetNN3$vars]);
    MetaFeaturesNNGC_D_NN3 = data.frame(MetaFeaturesNNGC_D_NN3[, includeSetNN3$vars]);
    MetaFeaturesNNGC_E_NN3 = data.frame(MetaFeaturesNNGC_E_NN3[, includeSetNN3$vars]);
    MetaFeaturesNNGC_F_NN3 = data.frame(MetaFeaturesNNGC_F_NN3[, includeSetNN3$vars]);
    
    MetaFeaturesNNGC_A_NN5 = data.frame(MetaFeaturesNNGC_A_NN5[, includeSetNN5$vars]);     # eliminate near to zero variables
    MetaFeaturesNNGC_B_NN5 = data.frame(MetaFeaturesNNGC_B_NN5[, includeSetNN5$vars]);
    MetaFeaturesNNGC_C_NN5 = data.frame(MetaFeaturesNNGC_C_NN5[, includeSetNN5$vars]);
    MetaFeaturesNNGC_D_NN5 = data.frame(MetaFeaturesNNGC_D_NN5[, includeSetNN5$vars]);
    MetaFeaturesNNGC_E_NN5 = data.frame(MetaFeaturesNNGC_E_NN5[, includeSetNN5$vars]);
    MetaFeaturesNNGC_F_NN5 = data.frame(MetaFeaturesNNGC_F_NN5[, includeSetNN5$vars]);
    
    MetaFeaturesNNGC_A = data.frame(MetaFeaturesNNGC_A[, includeSetCombined$vars]);     # eliminate near to zero variables
    MetaFeaturesNNGC_B = data.frame(MetaFeaturesNNGC_B[, includeSetCombined$vars]);
    MetaFeaturesNNGC_C = data.frame(MetaFeaturesNNGC_C[, includeSetCombined$vars]);
    MetaFeaturesNNGC_D = data.frame(MetaFeaturesNNGC_D[, includeSetCombined$vars]);
    MetaFeaturesNNGC_E = data.frame(MetaFeaturesNNGC_E[, includeSetCombined$vars]);
    MetaFeaturesNNGC_F = data.frame(MetaFeaturesNNGC_F[, includeSetCombined$vars]);
  }
  
  #########################
  # Meta-learning Scoring #
  #########################
  
  if(MLLScoringLoad == F) {
    # NN3
    MKGCNN3_preproc_A = predict(preprocNN3, MetaFeaturesNNGC_A_NN3);
    MKGCNN3_preproc_B = predict(preprocNN3, MetaFeaturesNNGC_B_NN3);
    MKGCNN3_preproc_C = predict(preprocNN3, MetaFeaturesNNGC_C_NN3);
    MKGCNN3_preproc_D = predict(preprocNN3, MetaFeaturesNNGC_D_NN3);
    MKGCNN3_preproc_E = predict(preprocNN3, MetaFeaturesNNGC_E_NN3);
    MKGCNN3_preproc_F = predict(preprocNN3, MetaFeaturesNNGC_F_NN3);
    
    MLLModelNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Model", '_', ScoringModel, "_", VIThreshold, sep = '')));
    MethodNNGC[, 1] = rbind(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_A, type = "raw")),
                            as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_B, type = "raw")),
                            as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_C, type = "raw")),
                            as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_D, type = "raw")),
                            as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_E, type = "raw")),
                            as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_F, type = "raw")));
    ScoringGC_NN[1, 1] = performanceAcc(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_A, type = "raw")), MethodNNGC_A[, 1], horizon = length(MethodNNGC_A[, 1])); 
    ScoringGC_NN[2, 1] = performanceAcc(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_B, type = "raw")), MethodNNGC_B[, 1], horizon = length(MethodNNGC_B[, 1])); 
    ScoringGC_NN[3, 1] = performanceAcc(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_C, type = "raw")), MethodNNGC_C[, 1], horizon = length(MethodNNGC_C[, 1])); 
    ScoringGC_NN[4, 1] = performanceAcc(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_D, type = "raw")), MethodNNGC_D[, 1], horizon = length(MethodNNGC_D[, 1])); 
    ScoringGC_NN[5, 1] = performanceAcc(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_E, type = "raw")), MethodNNGC_E[, 1], horizon = length(MethodNNGC_E[, 1])); 
    ScoringGC_NN[6, 1] = performanceAcc(as.numeric(predict(MLLModelNN3, newdata = MKGCNN3_preproc_F, type = "raw")), MethodNNGC_F[, 1], horizon = length(MethodNNGC_F[, 1])); 
    
    # NN5
    MKGCNN5_preproc_A = predict(preprocNN5, MetaFeaturesNNGC_A_NN5);
    MKGCNN5_preproc_B = predict(preprocNN5, MetaFeaturesNNGC_B_NN5);
    MKGCNN5_preproc_C = predict(preprocNN5, MetaFeaturesNNGC_C_NN5);
    MKGCNN5_preproc_D = predict(preprocNN5, MetaFeaturesNNGC_D_NN5);
    MKGCNN5_preproc_E = predict(preprocNN5, MetaFeaturesNNGC_E_NN5);
    MKGCNN5_preproc_F = predict(preprocNN5, MetaFeaturesNNGC_F_NN5);
    
    MLLModelNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Model", '_', ScoringModel, "_", VIThreshold, sep = '')));
    MethodNNGC[, 2] = rbind(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_A, type = "raw")),
                            as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_B, type = "raw")),
                            as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_C, type = "raw")),
                            as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_D, type = "raw")),
                            as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_E, type = "raw")),
                            as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_F, type = "raw")));
    ScoringGC_NN[1, 2] = performanceAcc(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_A, type = "raw")), MethodNNGC_A[, 1], horizon = length(MethodNNGC_A[, 1])); 
    ScoringGC_NN[2, 2] = performanceAcc(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_B, type = "raw")), MethodNNGC_B[, 1], horizon = length(MethodNNGC_B[, 1])); 
    ScoringGC_NN[3, 2] = performanceAcc(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_C, type = "raw")), MethodNNGC_C[, 1], horizon = length(MethodNNGC_C[, 1])); 
    ScoringGC_NN[4, 2] = performanceAcc(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_D, type = "raw")), MethodNNGC_D[, 1], horizon = length(MethodNNGC_D[, 1])); 
    ScoringGC_NN[5, 2] = performanceAcc(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_E, type = "raw")), MethodNNGC_E[, 1], horizon = length(MethodNNGC_E[, 1])); 
    ScoringGC_NN[6, 2] = performanceAcc(as.numeric(predict(MLLModelNN5, newdata = MKGCNN5_preproc_F, type = "raw")), MethodNNGC_F[, 1], horizon = length(MethodNNGC_F[, 1])); 
    
    # Combined
    MKGCNN_preproc_A = predict(preprocNNCombined, MetaFeaturesNNGC_A);
    MKGCNN_preproc_B = predict(preprocNNCombined, MetaFeaturesNNGC_B);
    MKGCNN_preproc_C = predict(preprocNNCombined, MetaFeaturesNNGC_C);
    MKGCNN_preproc_D = predict(preprocNNCombined, MetaFeaturesNNGC_D);
    MKGCNN_preproc_E = predict(preprocNNCombined, MetaFeaturesNNGC_E);
    MKGCNN_preproc_F = predict(preprocNNCombined, MetaFeaturesNNGC_F);

    MLLModelCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel", '_', ScoringModel, "_", VIThreshold, sep = '')));
    MethodNNGC[, 3] = rbind(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_A, type = "raw")),
                            as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_B, type = "raw")),
                            as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_C, type = "raw")),
                            as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_D, type = "raw")),
                            as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_E, type = "raw")),
                            as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_F, type = "raw")));
    ScoringGC_NN[1, 3] = performanceAcc(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_A, type = "raw")), MethodNNGC_A[, 1], horizon = length(MethodNNGC_A[, 1])); 
    ScoringGC_NN[2, 3] = performanceAcc(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_B, type = "raw")), MethodNNGC_B[, 1], horizon = length(MethodNNGC_B[, 1])); 
    ScoringGC_NN[3, 3] = performanceAcc(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_C, type = "raw")), MethodNNGC_C[, 1], horizon = length(MethodNNGC_C[, 1])); 
    ScoringGC_NN[4, 3] = performanceAcc(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_D, type = "raw")), MethodNNGC_D[, 1], horizon = length(MethodNNGC_D[, 1])); 
    ScoringGC_NN[5, 3] = performanceAcc(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_E, type = "raw")), MethodNNGC_E[, 1], horizon = length(MethodNNGC_E[, 1])); 
    ScoringGC_NN[6, 3] = performanceAcc(as.numeric(predict(MLLModelCombined, newdata = MKGCNN_preproc_F, type = "raw")), MethodNNGC_F[, 1], horizon = length(MethodNNGC_F[, 1])); 

    #MLLModelNN_NNGC_Model_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MLLModelNN_NNGC_Model", '_', ScoringModel, "_", VIThreshold, sep = '')));
    #MLLMethod_NN_NNGC = as.numeric(predict(MLLModelNN_NNGC_Model, newdata = rbind(MetaFeaturesNNGC_A, MetaFeaturesNNGC_B, MetaFeaturesNNGC_C, MetaFeaturesNNGC_D, MetaFeaturesNNGC_E, MetaFeaturesNNGC_F), type = "raw"));

    # meta-learning scoring
    if(as.numeric(which(ScoringGC_NN[1, ] == min(ScoringGC_NN[1, ]))[1]) == 1) {
      score = as.numeric(predict(MLLModelNN3, newdata = MetaFeaturesNNGC_A_NN3, type = "raw"));
      MethodNNGC_A[, 3] = score;
      BestMethodNNGC[1] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else if(as.numeric(which(ScoringGC_NN[1, ] == min(ScoringGC_NN[1, ]))[1]) == 2) {
      score = as.numeric(predict(MLLModelNN5, newdata = MetaFeaturesNNGC_A_NN5, type = "raw"));
      MethodNNGC_A[, 3] = score;
      BestMethodNNGC[1] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else {
      score = as.numeric(predict(MLLModelCombined, newdata = MetaFeaturesNNGC_A, type = "raw"));
      MethodNNGC_A[, 3] = score;
      BestMethodNNGC[1] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    }
    
    if(as.numeric(which(ScoringGC_NN[2, ] == min(ScoringGC_NN[2, ]))[1]) == 1) {
      score = as.numeric(predict(MLLModelNN3, newdata = MetaFeaturesNNGC_B_NN3, type = "raw"));
      MethodNNGC_B[, 3] = score;
      BestMethodNNGC[2] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else if(as.numeric(which(ScoringGC_NN[2, ] == min(ScoringGC_NN[2, ]))[1]) == 2) {
      score = as.numeric(predict(MLLModelNN5, newdata = MetaFeaturesNNGC_B_NN5, type = "raw"));
      MethodNNGC_B[, 3] = score;
      BestMethodNNGC[2] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else {
      score = as.numeric(predict(MLLModelCombined, newdata = MetaFeaturesNNGC_B, type = "raw"));
      MethodNNGC_B[, 3] = score;
      BestMethodNNGC[2] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    }
    
    if(as.numeric(which(ScoringGC_NN[3, ] == min(ScoringGC_NN[3, ]))[1]) == 1) {
      score = as.numeric(predict(MLLModelNN3, newdata = MetaFeaturesNNGC_C_NN3, type = "raw"));
      MethodNNGC_C[, 3] = score;
      BestMethodNNGC[3] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else if(as.numeric(which(ScoringGC_NN[3, ] == min(ScoringGC_NN[3, ]))[1]) == 2) {
      score = as.numeric(predict(MLLModelNN5, newdata = MetaFeaturesNNGC_C_NN5, type = "raw"));
      MethodNNGC_C[, 3] = score;
      BestMethodNNGC[3] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else {
      score = as.numeric(predict(MLLModelCombined, newdata = MetaFeaturesNNGC_C, type = "raw"));
      MethodNNGC_C[, 3] = score;
      BestMethodNNGC[3] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    }
    
    if(as.numeric(which(ScoringGC_NN[4, ] == min(ScoringGC_NN[4, ]))[1]) == 1) {
      score = as.numeric(predict(MLLModelNN3, newdata = MetaFeaturesNNGC_D_NN3, type = "raw"));
      MethodNNGC_D[, 3] = score;
      BestMethodNNGC[4] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else if(as.numeric(which(ScoringGC_NN[4, ] == min(ScoringGC_NN[4, ]))[1]) == 2) {
      score = as.numeric(predict(MLLModelNN5, newdata = MetaFeaturesNNGC_D_NN5, type = "raw"));
      MethodNNGC_D[, 3] = score;
      BestMethodNNGC[4] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else {
      score = as.numeric(predict(MLLModelCombined, newdata = MetaFeaturesNNGC_D, type = "raw"));
      MethodNNGC_D[, 3] = score;
      BestMethodNNGC[4] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    }
    
    if(as.numeric(which(ScoringGC_NN[5, ] == min(ScoringGC_NN[5, ]))[1]) == 1) {
      score = as.numeric(predict(MLLModelNN3, newdata = MetaFeaturesNNGC_E_NN3, type = "raw"));
      MethodNNGC_E[, 3] = score;
      BestMethodNNGC[5] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else if(as.numeric(which(ScoringGC_NN[5, ] == min(ScoringGC_NN[5, ]))[1]) == 2) {
      score = as.numeric(predict(MLLModelNN5, newdata = MetaFeaturesNNGC_E_NN5, type = "raw"));
      MethodNNGC_E[, 3] = score;
      BestMethodNNGC[5] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else {
      score = as.numeric(predict(MLLModelCombined, newdata = MetaFeaturesNNGC_E, type = "raw"));
      MethodNNGC_E[, 3] = score;
      BestMethodNNGC[5] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    }
    
    if(as.numeric(which(ScoringGC_NN[6, ] == min(ScoringGC_NN[6, ]))[1]) == 1) {
      score = as.numeric(predict(MLLModelNN3, newdata = MetaFeaturesNNGC_F_NN3, type = "raw"));
      MethodNNGC_F[, 3] = score
      BestMethodNNGC[6] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else if(as.numeric(which(ScoringGC_NN[6, ] == min(ScoringGC_NN[6, ]))[1]) == 2) {
      score = as.numeric(predict(MLLModelNN5, newdata = MetaFeaturesNNGC_F_NN5, type = "raw"));
      MethodNNGC_F[, 3] = score;
      BestMethodNNGC[6] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    } else {
      score = as.numeric(predict(MLLModelCombined, newdata = MetaFeaturesNNGC_F, type = "raw"));
      MethodNNGC_F[, 3] = score;
      BestMethodNNGC[6] = count(score)$x[which(count(score)$freq == max(count(score)$freq))];
    }
    
    # evaluation matrix
    #     Baseline = cbind(PerformanceNNGC_A[1, ], PerformanceNNGC_B[1, ], PerformanceNNGC_C[1, ], PerformanceNNGC_D[1, ], PerformanceNNGC_E[1, ], PerformanceNNGC_F[1, ]);
    #     rownames(Baseline) = methods;
    #     
    #     colnames(ScoringGC_NN) = c('MLL (NN3) -> GC', 'MLL (NN5) -> GC', 'MLL (NN3 + NN5) -> GC');
    #     ScoringNNGC = rbind(Baseline, t(ScoringGC_NN));
    #     
    #     ScoringNNGC = cbind(ScoringNNGC, rowMeans(ScoringNNGC));
    #     colnames(ScoringNNGC) = c('NNGC-A', 'NNGC-B', 'NNGC-C', 'NNGC-D', 'NNGC-E', 'NNGC-F', 'Average');
    #     
    #     ScoringNNGC = rbind(ScoringNNGC, MLL_GC);
    
    #######################
    # Evaluation Matrices #
    #######################
    
    # finding series-wise SMAPES matrix
    SMAPESNNGC = rbind(t(SMAPENNGC_A), t(SMAPENNGC_B), t(SMAPENNGC_C), t(SMAPENNGC_D), t(SMAPENNGC_E), t(SMAPENNGC_F));
    MethodNNGC_Base = cbind(t(MethodNNGC_A[, 1]), t(MethodNNGC_B[, 1]), t(MethodNNGC_C[, 1]), t(MethodNNGC_D[, 1]), t(MethodNNGC_E[, 1]), t(MethodNNGC_F[, 1]));
    SMAPES_Min = apply(SMAPESNNGC, 1, min, na.rm = TRUE);
    
    for(i in 1:(NoOfTimeSeriesTest * NoOfDatasetsTest)) {
      SMAPESNNGC_Min_NN3[i] = SMAPESNNGC[i, MethodNNGC[i, 1]];
      SMAPESNNGC_Min_NN5[i] = SMAPESNNGC[i, MethodNNGC[i, 2]];
      SMAPESNNGC_Min_Combined[i] = SMAPESNNGC[i, MethodNNGC[i, 3]];
#       MLLSMAPESNNGC_Min[i] = SMAPESNNGC[i, MLLMethodNNGC[i, 3]];
#       MLLSMAPES_NN_NNGC_Min[i] = SMAPESNNGC[i, MLLMethod_NN_NNGC];
    }
    
    SMAPESNNGC_Base = cbind(SMAPESNNGC, t(MethodNNGC_Base), SMAPES_Min, rowMeans(SMAPESNNGC));
    SMAPESNNGC_Base = rbind(SMAPESNNGC_Base, as.vector(colMeans(SMAPESNNGC_Base)));
    colnames(SMAPESNNGC_Base) = c(methods, 'Baseline Best Method', 'Minimum SMAPE', 'Average');
    SMAPESNNGC_Base = round(SMAPESNNGC_Base, 2);
    
    SMAPESNNGC_MLL = cbind(MethodNNGC[, 1], SMAPESNNGC_Min_NN3, MethodNNGC[, 2], SMAPESNNGC_Min_NN5, MethodNNGC[, 3], SMAPESNNGC_Min_Combined);
                           #, MLLMethodNNGC[, 3], MLLSMAPESNNGC_Min, MLLMethod_NN_NNGC, MLLSMAPES_NN_NNGC_Min);
    SMAPESNNGC_MLL = round(rbind(SMAPESNNGC_MLL, colMeans(SMAPESNNGC_MLL)), 2);
    
    colnames(SMAPESNNGC_MLL) = c('MLL(NN3)', 'MLL(NN3) SMAPE', 'MLL(NN5)', 'MLL(NN5) SMAPE', 'MLL(NN3+NN5)', 'MLL(NN3+NN5) SMAPE');
                          #, 'MLL(GC)', 'MLL(GC) SMAPE'), 'MLL(NN3+NN5+NNGC)', 'MLL(NN3+NN5+NNGC) SMAPE');
    
    # evaluation matrix - finding overall SMAPES matrix 
    Baseline = cbind(PerformanceNNGC_A[1, ], PerformanceNNGC_A[2, ], PerformanceNNGC_B[1, ], PerformanceNNGC_B[2, ], PerformanceNNGC_C[1, ], PerformanceNNGC_C[2, ], PerformanceNNGC_D[1, ], PerformanceNNGC_D[2, ], PerformanceNNGC_E[1, ], PerformanceNNGC_E[2, ], PerformanceNNGC_F[1, ], PerformanceNNGC_F[2, ]);
    
    ScoringNNGC = rbind(Baseline, cbind(mean(SMAPES_Min[c(1:11)]), sd(SMAPES_Min[c(1:11)]), mean(SMAPES_Min[c(12:22)]), sd(SMAPES_Min[c(12:22)]), mean(SMAPES_Min[c(23:33)]), sd(SMAPES_Min[c(23:33)]), 
                                        mean(SMAPES_Min[c(34:44)]), sd(SMAPES_Min[c(34:44)]), mean(SMAPES_Min[c(45:55)]), sd(SMAPES_Min[c(45:55)]), mean(SMAPES_Min[c(56:66)]), sd(SMAPES_Min[c(56:66)])));
    
    ScoringNNGC = rbind(ScoringNNGC, cbind(mean(SMAPESNNGC_Min_NN3[c(1:11)]), sd(SMAPESNNGC_Min_NN3[c(1:11)]), mean(SMAPESNNGC_Min_NN3[c(12:22)]), sd(SMAPESNNGC_Min_NN3[c(12:22)]), mean(SMAPESNNGC_Min_NN3[c(23:33)]), sd(SMAPESNNGC_Min_NN3[c(23:33)]), 
                                           mean(SMAPESNNGC_Min_NN3[c(34:44)]), sd(SMAPESNNGC_Min_NN3[c(34:44)]), mean(SMAPESNNGC_Min_NN3[c(45:55)]), sd(SMAPESNNGC_Min_NN3[c(45:55)]), mean(SMAPESNNGC_Min_NN3[c(56:66)]), sd(SMAPESNNGC_Min_NN3[c(56:66)])));
    
    ScoringNNGC = rbind(ScoringNNGC, cbind(mean(SMAPESNNGC_Min_NN5[c(1:11)]), sd(SMAPESNNGC_Min_NN5[c(1:11)]), mean(SMAPESNNGC_Min_NN5[c(12:22)]), sd(SMAPESNNGC_Min_NN5[c(12:22)]), mean(SMAPESNNGC_Min_NN5[c(23:33)]), sd(SMAPESNNGC_Min_NN5[c(23:33)]), 
                                           mean(SMAPESNNGC_Min_NN5[c(34:44)]), sd(SMAPESNNGC_Min_NN5[c(34:44)]), mean(SMAPESNNGC_Min_NN5[c(45:55)]), sd(SMAPESNNGC_Min_NN5[c(45:55)]), mean(SMAPESNNGC_Min_NN5[c(56:66)]), sd(SMAPESNNGC_Min_NN5[c(56:66)])));
    
    ScoringNNGC = rbind(ScoringNNGC, cbind(mean(SMAPESNNGC_Min_Combined[c(1:11)]), sd(SMAPESNNGC_Min_Combined[c(1:11)]), mean(SMAPESNNGC_Min_Combined[c(12:22)]), sd(SMAPESNNGC_Min_Combined[c(12:22)]), mean(SMAPESNNGC_Min_Combined[c(23:33)]), sd(SMAPESNNGC_Min_Combined[c(23:33)]), 
                                           mean(SMAPESNNGC_Min_Combined[c(34:44)]), sd(SMAPESNNGC_Min_Combined[c(34:44)]), mean(SMAPESNNGC_Min_Combined[c(45:55)]), sd(SMAPESNNGC_Min_Combined[c(45:55)]), mean(SMAPESNNGC_Min_Combined[c(56:66)]), sd(SMAPESNNGC_Min_Combined[c(56:66)])));
    
    #ScoringNNGC = rbind(ScoringNNGC, cbind(mean(MLLSMAPESNNGC_Min[c(1:11)]), sd(MLLSMAPESNNGC_Min[c(1:11)]), mean(MLLSMAPESNNGC_Min[c(12:22)]), sd(MLLSMAPESNNGC_Min[c(12:22)]), mean(MLLSMAPESNNGC_Min[c(23:33)]), sd(MLLSMAPESNNGC_Min[c(23:33)]), 
    #                                       mean(MLLSMAPESNNGC_Min[c(34:44)]), sd(MLLSMAPESNNGC_Min[c(34:44)]), mean(MLLSMAPESNNGC_Min[c(45:55)]), sd(MLLSMAPESNNGC_Min[c(45:55)]), mean(MLLSMAPESNNGC_Min[c(56:66)]), sd(MLLSMAPESNNGC_Min[c(56:66)])));
    
    #ScoringNNGC = rbind(ScoringNNGC, cbind(mean(MLLSMAPES_NN_NNGC_Min[c(1:11)]), sd(MLLSMAPES_NN_NNGC_Min[c(1:11)]), mean(MLLSMAPES_NN_NNGC_Min[c(12:22)]), sd(MLLSMAPES_NN_NNGC_Min[c(12:22)]), mean(MLLSMAPES_NN_NNGC_Min[c(23:33)]), sd(MLLSMAPES_NN_NNGC_Min[c(23:33)]), 
    #                                       mean(MLLSMAPES_NN_NNGC_Min[c(34:44)]), sd(MLLSMAPES_NN_NNGC_Min[c(34:44)]), mean(MLLSMAPES_NN_NNGC_Min[c(45:55)]), sd(MLLSMAPES_NN_NNGC_Min[c(45:55)]), mean(MLLSMAPES_NN_NNGC_Min[c(56:66)]), sd(MLLSMAPES_NN_NNGC_Min[c(56:66)])));
    
    ScoringNNGC = cbind(ScoringNNGC, rowMeans(ScoringNNGC));
   
    colnames(ScoringNNGC) = c('NNGC-A SMAPE', 'NNGC-A SD', 'NNGC-B SMAPE', 'NNGC-B SD', 'NNGC-C SMAPE', 'NNGC-C SD', 'NNGC-D SMAPE', 'NNGC-D SD', 'NNGC-E SMAPE', 'NNGC-E SD', 'NNGC-F SMAPE', 'NNGC-F SD', 'Average');
    rownames(ScoringNNGC) = c(methods, 'Base Learning (Best Possible)', 'MLL (NN3)->GC', 'MLL (NN5)->GC', 'MLL (NN3+NN5)->GC');
                          #, 'MLL (NNGC)', 'MLL (NN3+NN5+NNGC)->GC');
    
    ScoringNNGC = round(ScoringNNGC, 2);
    
    write.csv(SMAPESNNGC_Base, file = paste(getwd(), '/Implementation/Analysis/NN_GC_SMAPES_Base', '_', ScoringModel, "_", VIThreshold, '.csv', sep = ''), col.names = TRUE, sep = ",");
    write.csv(SMAPESNNGC_MLL, file = paste(getwd(), '/Implementation/Analysis/NN_GC_SMAPES_MLL', '_', ScoringModel, "_", VIThreshold, '.csv', sep = ''), col.names = TRUE, sep = ",");
    write.csv(ScoringNNGC, file = paste(getwd(), '/Implementation/Analysis/NN_GC_Scoring', '_', ScoringModel, "_", VIThreshold, '.csv', sep = ''), col.names = TRUE, sep = ",");
    save(SMAPESNNGC_Base, file = paste(getwd(), "/Implementation/Objects/NNGC/NN_GC_SMAPES_Base", "_", VIThreshold, sep = ''));
    save(SMAPESNNGC_MLL, file = paste(getwd(), "/Implementation/Objects/NNGC/NN_GC_SMAPES_MLL", "_", VIThreshold, sep = ''));
    save(ScoringNNGC, file = paste(getwd(), "/Implementation/Objects/NNGC/NN_GC_Scoring", "_", VIThreshold, sep = ''));
    
    save(MethodNNGC_A, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Method", sep = ''));
    save(MethodNNGC_B, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Method", sep = ''));
    save(MethodNNGC_C, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Method", sep = ''));
    save(MethodNNGC_D, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Method", sep = ''));
    save(MethodNNGC_E, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Method", sep = ''));
    save(MethodNNGC_F, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Method", sep = ''));
    
    save(SMAPESNNGC_Min_NN3, file = paste(getwd(), "/Implementation/Objects/NNGC/SMAPESNNGC_Min_NN3", sep = ''));
    save(SMAPESNNGC_Min_NN5, file = paste(getwd(), "/Implementation/Objects/NNGC/SMAPESNNGC_Min_NN5", sep = ''));
    save(SMAPESNNGC_Min_Combined, file = paste(getwd(), "/Implementation/Objects/NNGC/SMAPESNNGC_Min_Combined", sep = ''));
    
    save(ScoringGC_NN, file = paste(getwd(), "/Implementation/Objects/NNGC/ScoringGC_NN", sep = ''));
    save(BestMethodNNGC, file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_Best_Method", sep = ''));
    save(MethodNNGC, file = paste(getwd(), "/Implementation/Objects/NNGC/MethodNNGC", sep = ''));
  } else {
    SMAPESNNGC_Base = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NN_GC_SMAPES_Base", "_", VIThreshold, sep = '')));
    SMAPESNNGC_MLL = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NN_GC_SMAPES_MLL", "_", VIThreshold, sep = '')));
    ScoringNNGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NN_GC_Scoring", "_", VIThreshold, sep = '')));
    
    MethodNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Method", sep = '')));
    MethodNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Method", sep = '')));
    MethodNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Method", sep = '')));
    MethodNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Method", sep = '')));
    MethodNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Method", sep = '')));
    MethodNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Method", sep = '')));
    
    SMAPESNNGC_Min_NN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/SMAPESNNGC_Min_NN3", sep = '')));
    SMAPESNNGC_Min_NN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/SMAPESNNGC_Min_NN5", sep = '')));
    SMAPESNNGC_Min_Combined = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/SMAPESNNGC_Min_Combined", sep = '')));
    
    ScoringGC_NN = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/ScoringGC_NN", sep = '')));
    BestMethodNNGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_Best_Method", sep = '')));
    MethodNNGC = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/MethodNNGC", sep = '')));
  }
  
  # scoring
  #print('MLL Scoring');
  #print(SMAPESNNGC_Base);
  #print(SMAPESNNGC_MLL);
  print(ScoringNNGC);
  #print(BestMethodNNGC);
}

############
# Analysis #
############

if(analysisExecute == T) {
  
  # Load data
#   if(Analysis != 'NN5') {
#     AccuracyNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Accuracy", sep = '')));
#     MethodNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Method", sep = '')));
#     SMAPENN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3SMAPE", sep = '')));
#     performanceNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3Performances", sep = '')));
#     MetaFeaturesNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3MFs", sep = '')));
#     MLLAccuracyNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3MLLAccuracy", "_", VIThreshold, sep = '')));
#   }
#   
#   if(Analysis != 'NN3') {
#     AccuracyNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Accuracy", sep = '')));
#     MethodNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Method", sep = '')));
#     SMAPENN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5SMAPE", sep = '')));
#     performanceNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5Performances", sep = '')));
#     MetaFeaturesNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5MFs", sep = ''))); 
#     MLLAccuracyNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5MLLAccuracy", "_", VIThreshold, sep = '')));
#   }
#   
#   if(Analysis == 'both') {      
#     MethodCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedMethod", "_", VIThreshold, sep = '')));
#     MLLAccuracyCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedMLLAccuracy", "_", VIThreshold, sep = '')));
#     MLLModelCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/CombinedModel", "_", VIThreshold, sep = '')));
#   }
  
  SMAPECombined = t(rbind(t(SMAPENN3), t(SMAPENN5)));
  
  print('NN3 Confusion matrix');
  if(Analysis != 'NN5') {
    BLLAnalysis(MethodNN3, series = 'NN3', methods = methods, filename = 'NN', multiPlot = TRUE, store = TRUE, devOff = FALSE, horz = 3, vert = 1, width = 4, height = 7, plotPerformances = T);
    MLLAnalysis(MethodNN3, SMAPENN3, series = 'NN3', filename = 'NN', methods = methods, store = TRUE, devOff = TRUE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);
  }
  
  print('NN5 Confusion matrix');
  if(Analysis != 'NN3') {
    BLLAnalysis(MethodNN5, series = 'NN5', methods = methods, store = FALSE, devOff = FALSE, plotPerformances = T);
    MLLAnalysis(MethodNN5, SMAPENN5, series = 'NN5', filename = 'NN', methods = methods, store = TRUE, devOff = TRUE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);
  }
  
  print('Combined Confusion matrix');
  if(Analysis == 'both') {
    BLLAnalysis(MethodCombined, series = 'NN3 and NN5 Combined', methods = methods, store = FALSE, devOff = TRUE, plotPerformances = T);
    MLLAnalysis(MethodCombined, SMAPECombined, series = 'Combined', filename = 'NN', methods = methods, store = TRUE, devOff = TRUE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
  }
  
  # Tree Analysis
  if(TreeAnalysis == T) {
    TreePlot(MLLModelNN3_DT, title = 'NN3 Meta-model', filename = 'NN3Tree');
    TreePlot(MLLModelNN5_DT, title = 'NN5 Meta-model', filename = 'NN5Tree');
    TreePlot(MLLModelCombined_DT, title = 'NN3 + NN5 Combined Meta-model', filename = 'NN3_NN5Tree');
    TreePlot(MKNNGC_DT, title = 'NNGC Combined Meta-model', filename = 'NNGCTree');
    TreePlot(MLLModelNN_NNGC_Model_DT, title = 'NN3 + NN5 + NNGC Meta-model', filename = 'NN3_NN5_NNGCTree');
  }
  
  # cluster Analysis
  if(ClustAnalysis == T) {  
    includeSetNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN3", "_", VIThreshold, sep = '')));
    includeSetNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessNN5", "_", VIThreshold, sep = '')));
    includeSetCombined = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcessCombined", "_", VIThreshold, sep = '')));
    
    SMAPESNNGC = rbind(t(SMAPENNGC_A), t(SMAPENNGC_B), t(SMAPENNGC_C), t(SMAPENNGC_D), t(SMAPENNGC_E), t(SMAPENNGC_F));
    SMAPES_Min = apply(SMAPESNNGC, 1, min, na.rm = TRUE);
    
    if(Analysis != 'NN5') {
      MFNNGC_A = MetaFeaturesNNGC_A_NN3;  rownames(MFNNGC_A) = paste('GCA-', index(MFNNGC_A), ':', round(SMAPESNNGC_Min_NN3[1:11], 1), '_', MethodNNGC[1:11, 1], '|', round(AccuracyNNGC_A, 1), '_', MethodNNGC_A[, 1], sep = '');
      MFNNGC_B = MetaFeaturesNNGC_B_NN3;  rownames(MFNNGC_B) = paste('GCB-', index(MFNNGC_B), ':', round(SMAPESNNGC_Min_NN3[12:22], 1), '_', MethodNNGC[12:22, 1], '|', round(AccuracyNNGC_B, 1), '_', MethodNNGC_B[, 1], sep = '');
      MFNNGC_C = MetaFeaturesNNGC_C_NN3;  rownames(MFNNGC_C) = paste('GCC-', index(MFNNGC_C), ':', round(SMAPESNNGC_Min_NN3[23:33], 1), '_', MethodNNGC[23:33, 1], '|', round(AccuracyNNGC_C, 1), '_', MethodNNGC_C[, 1], sep = '');
      MFNNGC_D = MetaFeaturesNNGC_D_NN3;  rownames(MFNNGC_D) = paste('GCD-', index(MFNNGC_D), ':', round(SMAPESNNGC_Min_NN3[34:44], 1), '_', MethodNNGC[34:44, 1], '|', round(AccuracyNNGC_D, 1), '_', MethodNNGC_D[, 1], sep = '');
      MFNNGC_E = MetaFeaturesNNGC_E_NN3;  rownames(MFNNGC_E) = paste('GCE-', index(MFNNGC_E), ':', round(SMAPESNNGC_Min_NN3[45:55], 1), '_', MethodNNGC[45:55, 1], '|', round(AccuracyNNGC_E, 1), '_', MethodNNGC_E[, 1], sep = '');
      MFNNGC_F = MetaFeaturesNNGC_F_NN3;  rownames(MFNNGC_F) = paste('GCF-', index(MFNNGC_F), ':', round(SMAPESNNGC_Min_NN3[56:66], 1), '_', MethodNNGC[56:66, 1], '|', round(AccuracyNNGC_F, 1), '_', MethodNNGC_F[, 1], sep = '');
      
      MFNN3 = MetaFeaturesNN3[, includeSetNN3$vars]; rownames(MFNN3) = paste('NN3-', index(MFNN3), ':', round(AccuracyNN3, 2), '_', MethodNN3[, 1], sep = '');
      
      clustNN3 = ClusterAnalysis(MFNN3, title = 'Dendrogram of NN3 Meta-features', filename = 'NN3Clusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);
      clustNN3_NNGC = ClusterAnalysis(rbind(MFNN3, MFNNGC_A, MFNNGC_B, MFNNGC_C, MFNNGC_D, MFNNGC_E, MFNNGC_F), title = 'Dendrogram of NN3 and NNGC Meta-features', filename = 'NN3NNGCClusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);
    }
    
    if(Analysis != 'NN3') {
      MFNNGC_A = MetaFeaturesNNGC_A_NN5;  rownames(MFNNGC_A) = paste('GCA-', index(MFNNGC_A), ':', round(SMAPESNNGC_Min_NN5[1:11], 1), '_', MethodNNGC[1:11, 2], '|', round(AccuracyNNGC_A, 1), '_', MethodNNGC_A[, 1], sep = '');
      MFNNGC_B = MetaFeaturesNNGC_B_NN5;  rownames(MFNNGC_B) = paste('GCB-', index(MFNNGC_B), ':', round(SMAPESNNGC_Min_NN5[12:22], 1), '_', MethodNNGC[12:22, 2], '|', round(AccuracyNNGC_B, 1), '_', MethodNNGC_B[, 1], sep = '');
      MFNNGC_C = MetaFeaturesNNGC_C_NN5;  rownames(MFNNGC_C) = paste('GCC-', index(MFNNGC_C), ':', round(SMAPESNNGC_Min_NN5[23:33], 1), '_', MethodNNGC[23:33, 2], '|', round(AccuracyNNGC_C, 1), '_', MethodNNGC_C[, 1], sep = '');
      MFNNGC_D = MetaFeaturesNNGC_D_NN5;  rownames(MFNNGC_D) = paste('GCD-', index(MFNNGC_D), ':', round(SMAPESNNGC_Min_NN5[34:44], 1), '_', MethodNNGC[34:44, 2], '|', round(AccuracyNNGC_D, 1), '_', MethodNNGC_D[, 1], sep = '');
      MFNNGC_E = MetaFeaturesNNGC_E_NN5;  rownames(MFNNGC_E) = paste('GCE-', index(MFNNGC_E), ':', round(SMAPESNNGC_Min_NN5[45:55], 1), '_', MethodNNGC[45:55, 2], '|', round(AccuracyNNGC_E, 1), '_', MethodNNGC_E[, 1], sep = '');
      MFNNGC_F = MetaFeaturesNNGC_F_NN5;  rownames(MFNNGC_F) = paste('GCF-', index(MFNNGC_F), ':', round(SMAPESNNGC_Min_NN5[56:66], 1), '_', MethodNNGC[56:66, 2], '|', round(AccuracyNNGC_F, 1), '_', MethodNNGC_F[, 1], sep = '');
      
      MFNN5 = MetaFeaturesNN5[, includeSetNN5$vars]; rownames(MFNN5) = paste('NN5-', index(MFNN5), ':', round(AccuracyNN5, 2), '_', MethodNN5[, 1], sep = '');   
      
      clustNN5 = ClusterAnalysis(MFNN5, title = 'Dendrogram of NN5 Meta-features', filename = 'NN5Clusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);
      clustNN5_NNGC = ClusterAnalysis(rbind(MFNN5, MFNNGC_A, MFNNGC_B, MFNNGC_C, MFNNGC_D, MFNNGC_E, MFNNGC_F), title = 'Dendrogram of NN5 and NNGC Meta-features', filename = 'NN5NNGCClusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);    
    }
    
    if(Analysis != 'NN3' && Analysis != 'NN5') {
      MFNNGC_A = MetaFeaturesNNGC_A;  rownames(MFNNGC_A) = paste('GCA-', index(MFNNGC_A), ':', round(SMAPESNNGC_Min_Combined[1:11], 1), '_', MethodNNGC[1:11, 3], '|', round(AccuracyNNGC_A, 1), '_', MethodNNGC_A[, 1], sep = '');
      MFNNGC_B = MetaFeaturesNNGC_B;  rownames(MFNNGC_B) = paste('GCB-', index(MFNNGC_B), ':', round(SMAPESNNGC_Min_Combined[12:22], 1), '_', MethodNNGC[12:22, 3], '|', round(AccuracyNNGC_B, 1), '_', MethodNNGC_B[, 1], sep = '');
      MFNNGC_C = MetaFeaturesNNGC_C;  rownames(MFNNGC_C) = paste('GCC-', index(MFNNGC_C), ':', round(SMAPESNNGC_Min_Combined[23:33], 1), '_', MethodNNGC[23:33, 3], '|', round(AccuracyNNGC_C, 1), '_', MethodNNGC_C[, 1], sep = '');
      MFNNGC_D = MetaFeaturesNNGC_D;  rownames(MFNNGC_D) = paste('GCD-', index(MFNNGC_D), ':', round(SMAPESNNGC_Min_Combined[34:44], 1), '_', MethodNNGC[34:44, 3], '|', round(AccuracyNNGC_D, 1), '_', MethodNNGC_D[, 1], sep = '');
      MFNNGC_E = MetaFeaturesNNGC_E;  rownames(MFNNGC_E) = paste('GCE-', index(MFNNGC_E), ':', round(SMAPESNNGC_Min_Combined[45:55], 1), '_', MethodNNGC[45:55, 3], '|', round(AccuracyNNGC_E, 1), '_', MethodNNGC_E[, 1], sep = '');
      MFNNGC_F = MetaFeaturesNNGC_F;  rownames(MFNNGC_F) = paste('GCF-', index(MFNNGC_F), ':', round(SMAPESNNGC_Min_Combined[56:66], 1), '_', MethodNNGC[56:66, 3], '|', round(AccuracyNNGC_F, 1), '_', MethodNNGC_F[, 1], sep = '');
      
      MFNN3 = MetaFeaturesNN3[, includeSetCombined$vars]; rownames(MFNN3) = paste('NN3-', index(MFNN3), ':', round(AccuracyNN3, 2), '_', MethodNN3[, 1], sep = '');   
      MFNN5 = MetaFeaturesNN5[, includeSetCombined$vars]; rownames(MFNN5) = paste('NN5-', index(MFNN5), ':', round(AccuracyNN5, 2), '_', MethodNN5[, 1], sep = '');   
      
      clustCombined = ClusterAnalysis(rbind(MetaFeaturesNN3[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN3)))], MetaFeaturesNN5[, na.omit(match(includeSetCombined$vars, colnames(MetaFeaturesNN5)))]), title = 'Dendrogram of NN3 and NN5 Meta-features', filename = 'NN3_5Clusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);
      clustCombined_NNGC = ClusterAnalysis(rbind(MFNN3, MFNN5, MFNNGC_A, MFNNGC_B, MFNNGC_C, MFNNGC_D, MFNNGC_E, MFNNGC_F), title = 'Cluster Dendrogram of NN and NNGC Meta-features', filename = 'NNNNGCClusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);    
    }    
    
    ClusterAnalysis(rbind(MFNNGC_A, MFNNGC_B, MFNNGC_C, MFNNGC_D, MFNNGC_E, MFNNGC_F), ScoringNNGC, title = 'Dendrogram of NNGC Meta-features', filename = 'NNGCClusters', methods = 'euclidean', link = 'complete', series = Analysis, k = clust_k);
  }
  
  # MLL scoring
  if(ScoringAnalysis == T) {
    # load data
    MethodNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_Method", sep = '')));
    MethodNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_Method", sep = '')));
    MethodNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_Method", sep = '')));
    MethodNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_Method", sep = '')));
    MethodNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_Method", sep = '')));
    MethodNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_Method", sep = '')));
    
    BLLAnalysis(MethodNNGC_A, series = 'NN GC - A', methods = methods, filename = 'NNGC', multiPlot = TRUE, store = TRUE, devOff = FALSE, width = 7, height = 7, plotPerformances = T);
    BLLAnalysis(MethodNNGC_B, series = 'NN GC - B', methods = methods, store = FALSE, devOff = FALSE, plotPerformances = T);
    BLLAnalysis(MethodNNGC_C, series = 'NN GC - C', methods = methods, store = FALSE, devOff = FALSE, plotPerformances = T);
    BLLAnalysis(MethodNNGC_D, series = 'NN GC - D', methods = methods, store = FALSE, devOff = FALSE, plotPerformances = T);
    BLLAnalysis(MethodNNGC_E, series = 'NN GC - E', methods = methods, store = FALSE, devOff = FALSE, plotPerformances = T);
    BLLAnalysis(MethodNNGC_F, series = 'NN GC - F', methods = methods, store = FALSE, devOff = TRUE, plotPerformances = T);
    
    print('Scoring Confusion matrix: ');
    print('NN GC A - Confusion matrix');
    
    MLLAnalysis(MethodNNGC_A, SMAPENNGC_A, series = 'NN GC - A', methods = methods, filename = 'NNGCMatrix', store = TRUE, devOff = FALSE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
    
    print('NN GC B - Confusion matrix');
    MLLAnalysis(MethodNNGC_B, SMAPENNGC_B, series = 'NN GC - B', methods = methods, store = FALSE, devOff = FALSE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
    
    print('NN GC C - Confusion matrix');
    MLLAnalysis(MethodNNGC_C, SMAPENNGC_C, series = 'NN GC - C', methods = methods, store = FALSE, devOff = FALSE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
    
    print('NN GC D - Confusion matrix');
    MLLAnalysis(MethodNNGC_D, SMAPENNGC_D, series = 'NN GC - D', methods = methods, store = FALSE, devOff = FALSE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
    
    print('NN GC E - Confusion matrix');
    MLLAnalysis(MethodNNGC_E, SMAPENNGC_E, series = 'NN GC - E', methods = methods, store = FALSE, devOff = FALSE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
    
    print('NN GC F - Confusion matrix');
    MLLAnalysis(MethodNNGC_F, SMAPENNGC_F, series = 'NN GC - F', methods = methods, store = FALSE, devOff = TRUE, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = F);    
  }
}

# memory clearance
# gc();
# rm(list = ls());