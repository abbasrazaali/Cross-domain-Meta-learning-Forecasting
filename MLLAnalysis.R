#####################################################################################################################
#
# Name: MLLAnalysis.R
#
# Description: BLL and MLL algorithm Analysis
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
#
#####################################################################################################################

# library
LoadLibrary('gmodels'); library('gmodels');
LoadLibrary('pvclust'); library('pvclust');
LoadLibrary('grid'); library('grid');
LoadLibrary('ape'); library('ape');
LoadLibrary('ggplot2'); library('ggplot2');

##########################
# BLL algorithm analysis #
##########################

ClusterAnalysis = function(MF, ScoringNNGC = NULL, title, filename, width = 22, height = 12, methods = 'euclidean', link = 'cutree', series, k = 5)
{
  print(series);
  
  pdf(paste("Report/Figures/", filename, '.pdf', sep = ''), width = width, height = height);
  
#   fit = pvclust(scale(t(MF)), method.hclust = methods, method.dist = dist);   # dendogram with p values
#   plot(fit, main = title, cex = 0.5);       
#   pvrect(fit, alpha = 0.95);
#   labels = write.tree(as.phylo(fit$hclust), file = paste("Report/Figures/", filename, '.txt', sep = ''), digits = 1);
  
  fit = hclust(dist(scale(MF), method = methods), method = link);
  plot(fit, main = title, cex = 0.5, labels = rownames(MF));         # display dendogram
  rect = rect.hclust(fit, k = k, border = "red");    # cut tree into 5 clusters
  #labels = write.tree(as.phylo(fit$hclust), file = paste("Report/Figures/", filename, '.txt', sep = ''), digits = 1);

  
#   cutree(fit, 3);
#   table(cutree(fit, 5))  
#   count = sapply(2:6, function(nc1) );

  dev.off();

  pdf(paste("Report/Figures/", filename, 'Clustering_heatmap.pdf', sep = ''), width = width, height = height);
   
  MF = subset(MF, select = -c(dim(MF)[1]));
  ggplot(melt(t(scale(MF))), aes(Var1,Var2, fill=value)) + geom_raster() + xlab("Meta-features") + ylab("Time-series") + ggtitle(filename) + scale_fill_continuous(name = "distance");
   
  #rownames(MF) = strtrim(rownames(MF), 3)
  #strtrim(rownames(MF), 3)
# 
  dev.off();
  
  if(!is.null(ScoringNNGC)) {
    #pdf(paste("Report/Figures/", 'Scoring_heatmap.pdf', sep = ''), width = 9, height = 6);

    ScoringNNGC = subset(ScoringNNGC, select = -c(2, 4, 6, 8, 10, 12));
    
    colnames(ScoringNNGC) = c('NNGC-A', 'NNGC-B', 'NNGC-C', 'NNGC-D', 'NNGC-E', 'NNGC-F', 'Average')
    
    ggplot(melt(scale(t(ScoringNNGC))), aes(Var1, Var2, fill = value)) + geom_raster() + xlab("Datasets") + ylab("Methods") + ggtitle('SMAPES of NN and GC series') + scale_fill_continuous(name = "distance");
    
    #dev.off();
  }

  return(list(fit = fit, rect = rect));
}

TreePlot = function(model, title, filename, width = 22, height = 12)
{
  if(tolower(model$method) == "c5.0") {
    pdf(paste("Report/Figures/", filename, '.pdf', sep = ''), width = width, height = height);

    grid.newpage();
    print(model$finalModel);
    plot(model$finalModel, title); 
    
    dev.off();
  }
}

BLLAnalysis = function(MethodNN, series, methods, filename = series, multiPlot = FALSE, horz = 3, vert = 2, store = TRUE, devOff = TRUE, width = 5, height = 4, plotPerformances = T)
{
  # Plot charts
  if(plotPerformances == T) {
    # par(mar = rep(2, 4));
    
    if(store == TRUE) {
      pdf(paste("Report/Figures/", filename, '.pdf', sep = ''), width = width, height = height);
    }
    
    if(multiPlot == TRUE) {
      par(mfrow = c(horz, vert));
    }
    
    dataset = rbind(table(factor(MethodNN[, 1], levels = rep(1:length(methods)))), table(factor(MethodNN[, 2], levels = rep(1:length(methods)))), table(factor(MethodNN[, 3], levels = rep(1:length(methods)))));
    colnames(dataset) = methods;
    rownames(dataset) = c('Best', 'Worst', 'MLL');
    
    barplot(dataset, main = series, xlab = "Method Number", ylab = "Time-series", beside = TRUE, col = terrain.colors(3), cex.axis = 1);
    legend("topright", cex = 0.6, rownames(dataset), fill = terrain.colors(3), horiz = FALSE);
    
    if(devOff == TRUE) {
      dev.off();
    }
  }
}

##########################
# MLL algorithm analysis #
##########################

MLLAnalysis = function(MethodNN, SMAPENN, series, filename = series, store = TRUE, devOff = TRUE, methods, NoOfTimeSeriesMethods, accuracyAnalysis = T, contingencyMatrix = T, plotPerformances = T)
{
  # Accuracy Analysis
  if(accuracyAnalysis == T) {
    Analysis = cbind(round(t(SMAPENN), 2), MethodNN, round(apply(t(SMAPENN), 1, min, na.rm = TRUE), 2), round(apply(t(SMAPENN), 1, max, na.rm = TRUE), 2), round(apply(t(SMAPENN), 1, mean, na.rm = TRUE), 2));
    colnames(Analysis) = c(methods, 'Best Method', 'Worst Method', 'MLL Method', 'Minimum Accuracy', 'Maximum Accuracy', 'Average Accuracy');
    write.csv(Analysis, file = paste('Implementation/Analysis/Analysis', series, '.csv', sep = ''), col.names = TRUE, sep = ",");
  }

  # Contingency matrix
  if(contingencyMatrix == T) {
    #CrossTable(MethodNN[, 1], MethodNN[, 3]);
    #CrossTable(MethodNN[, 2], MethodNN[, 3]);
#     if(store == TRUE) {
#       pdf(paste("Report/Figures/", filename, '.pdf', sep = ''), height = 10, width = 5);
#     }
    
    Best = factor(MethodNN[, 1], levels = rep(1:length(methods)));
    Worst = factor(MethodNN[, 2], levels = rep(1:length(methods)));
    MLL = factor(MethodNN[, 3], levels = rep(1:length(methods)));

    print(series);
#     grid.stext("Best");
#     textGrob("test sub", gp=gpar(font=2));
#     grid.table(table(Best, MLL), cols = methods, rows = methods, gpar.coltext = gpar(col = "black", cex = 1, fontface = "bold"), gpar.rowtext = gpar(col = "black", cex = 0.8, fontface = "bold"));
#     grid.stext("test")
#     grid.table(table(Worst, MLL), cols = methods, rows = methods, gpar.coltext = gpar(col = "black", cex = 1, fontface = "bold"), gpar.rowtext = gpar(col = "black", cex = 0.8, fontface = "bold"));
    print(table(Best, MLL));
    print(table(Worst, MLL));
    
#     if(devOff == TRUE) {
#       dev.off();
#     }
  }

  # MLL Line chart
  if(plotPerformances == T) {
    colnames(MethodNN) = c('Best', 'Worst', 'MLL');
    g_range <- range(0, MethodNN[, 1], MethodNN[, 2], MethodNN[, 3]);

    plot(MethodNN[, 1], type = "o", col = 'blue');
    axis(2, at = 1:NoOfTimeSeriesMethods);
    
    box()

    lines(MethodNN[, 2], type = "o", pch = 22, lty = 2, col = "green");
    lines(MethodNN[, 3], type = "o", pch = 22, lty = 2, col = "red");
    
    title(main = paste("Meta-learning Anaysis ", series), col.main = "black", font.main = 4);
    
    title(xlab = "Method", col.lab = rgb(0, 0.5, 0));
    title(ylab = "Time-series", col.lab = rgb(0, 0.5, 0));
    
    legend(1, g_range[2], c("Best Method", "Worst Method", "MLL Method"), cex = 0.8, col = c("blue", "green", "red"), pch = 21:22, lty = 1:2);
  }
}