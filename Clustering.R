LoadLibrary('pvclust'); library('pvclust');

setwd('C:/Users/IBM_ADMIN/Dropbox/PhD/TimeSeries/');

excludeSet = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NNPreProcess", sep = '')));
excludeSet = MKPreprocess(MetaFeaturesNN5);

MFNN3 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN3MFs", sep = '')));
MFNN5 = get(load(file = paste(getwd(), "/Implementation/Objects/NN/NN5MFs", sep = '')));
MetaFeaturesNNGC_A = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_A_MFs", sep = '')));
MetaFeaturesNNGC_B = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_B_MFs", sep = '')));
MetaFeaturesNNGC_C = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_C_MFs", sep = '')));
MetaFeaturesNNGC_D = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_D_MFs", sep = '')));
MetaFeaturesNNGC_E = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_E_MFs", sep = '')));
MetaFeaturesNNGC_F = get(load(file = paste(getwd(), "/Implementation/Objects/NNGC/NNGC_F_MFs", sep = '')));

ClusterAnalysis = function(MF, title, filename, width = 22, height = 12)
{
  MF = scale(MF);      # standardize variables
  #, width = width, height = height
  pdf(paste("Report/Figures/", filename, '.pdf', sep = ''));
  
  fit = pvclust(MF, method.hclust = "average", method.dist = "euclidean");   # dendogram with p values
  plot(fit, main = title);       
  pvrect(fit, alpha = 0.95);
  
  dev.off();
}

MFNN3 = MFNN3[, -excludeSet];
colnames(MFNN3) = paste('NN3_', colnames(MFNN3), sep = '');
MFNN5 = MFNN5[, -excludeSet];
colnames(MFNN5) = paste('NN5_', colnames(MFNN5), sep = '');
MetaFeaturesNNGC_A = rbind(MetaFeaturesNNGC_A[, -excludeSet], matrix(rep(0.0, (NoOfTimeSeries - 11) * NoOfMetafeatures), ncol = NoOfMetafeatures)[, -excludeSet]);
colnames(MetaFeaturesNNGC_A) = paste('GCA_', colnames(MetaFeaturesNNGC_A), sep = '');
MetaFeaturesNNGC_B = rbind(MetaFeaturesNNGC_B[, -excludeSet], matrix(rep(0.0, (NoOfTimeSeries - 11) * NoOfMetafeatures), ncol = NoOfMetafeatures)[, -excludeSet]);
colnames(MetaFeaturesNNGC_B) = paste('GCB_', colnames(MetaFeaturesNNGC_B), sep = '');
MetaFeaturesNNGC_C = rbind(MetaFeaturesNNGC_C[, -excludeSet], matrix(rep(0.0, (NoOfTimeSeries - 11) * NoOfMetafeatures), ncol = NoOfMetafeatures)[, -excludeSet]);
colnames(MetaFeaturesNNGC_C) = paste('GCC_', colnames(MetaFeaturesNNGC_C), sep = '');
MetaFeaturesNNGC_D = rbind(MetaFeaturesNNGC_D[, -excludeSet], matrix(rep(0.0, (NoOfTimeSeries - 11) * NoOfMetafeatures), ncol = NoOfMetafeatures)[, -excludeSet]);
colnames(MetaFeaturesNNGC_D) = paste('GCD_', colnames(MetaFeaturesNNGC_D), sep = '');
MetaFeaturesNNGC_E = rbind(MetaFeaturesNNGC_E[, -excludeSet], matrix(rep(0.0, (NoOfTimeSeries - 11) * NoOfMetafeatures), ncol = NoOfMetafeatures)[, -excludeSet]);
colnames(MetaFeaturesNNGC_E) = paste('GCE_', colnames(MetaFeaturesNNGC_E), sep = '');
MetaFeaturesNNGC_F = rbind(MetaFeaturesNNGC_F[, -excludeSet], matrix(rep(0.0, (NoOfTimeSeries - 11) * NoOfMetafeatures), ncol = NoOfMetafeatures)[, -excludeSet]);
colnames(MetaFeaturesNNGC_F) = paste('GCF_', colnames(MetaFeaturesNNGC_F), sep = '');

#ClusterAnalysis(MFNN3, title = 'Cluster Dendrogram of NN3 Meta-features', filename = 'NN3Clusters');
#ClusterAnalysis(MFNN5, title = 'Cluster Dendrogram of NN5 Meta-features', filename = 'NN5Clusters');
#ClusterAnalysis(cbind(MFNN3, MFNN5), title = 'Cluster Dendrogram of NN3 and NN5 Meta-features', filename = 'NN3_5Clusters');
#ClusterAnalysis(cbind(MetaFeaturesNNGC_A, MetaFeaturesNNGC_B, MetaFeaturesNNGC_C, MetaFeaturesNNGC_D, MetaFeaturesNNGC_E, MetaFeaturesNNGC_F), title = 'Cluster Dendrogram of NNGC Meta-features', filename = 'NNGCClusters');
ClusterAnalysis(cbind(MFNN3, MFNN5, MetaFeaturesNNGC_A, MetaFeaturesNNGC_B, MetaFeaturesNNGC_C, MetaFeaturesNNGC_D, MetaFeaturesNNGC_E, MetaFeaturesNNGC_F), title = 'Cluster Dendrogram of NN and NNGC Meta-features', filename = 'NNNNGCClusters');