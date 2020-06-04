#####################################################################################################################
#
# Name: MetaFeaturing.R
#
# Description: Generating Meta-Features
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
#
#####################################################################################################################

# libraries
LoadLibrary('car'); library('car'); 
LoadLibrary('fractal'); library('fractal');
LoadLibrary('tseriesChaos'); library('tseriesChaos');

######################
# General Statistics #
######################

std = function(detrend) {
  return(sd(detrend, na.rm = T));
}

skew = function(detrend) {
  return(skewness(detrend));
}

kurt = function(detrend) {
  return(kurtosis(detrend));
}

seriesLength = function(feature) {
  return(length(feature));
}

dw = function(feature) {
  model = lm(feature ~ poly(index(feature), 3, raw = TRUE));

  return(durbinWatsonTest(model)$r);
}

turn = function(feature) {
  tp = 0;
  for(i in 2:(length(feature) - 1)) {
    if(((feature[i - 1] < feature[i]) & (feature[i + 1] < feature[i])) | ((feature[i - 1] > feature[i]) & (feature[i + 1] > feature[i]))) {
      tp = tp + 1;
    }
  }
  
  return(tp);
}

step = function(feature) {
  sb = 0;
  for(i in 5:(length(feature))) {
    if(abs(feature[i] - mean(feature[1:(i - 1)])) > 2 * sd(feature[1:(i - 1)])) {
      sb = sb + 1;
    }
  }
  
  return(sb);
}

pred = function(feature) {
  return(0);
}

nonlin = function(feature) {
  LinModel = lm(feature ~ index(feature));
  nonLinModel = lm(feature ~ poly(index(feature), 2, raw = TRUE));
  sig = anova(LinModel, nonLinModel);

  return(sig$"Pr(>F)"[is.na(sig$"Pr(>F)") == FALSE]);
}

lyap = function(feature) {
  return(0);
  #return(lyap_k(series = feature, m = 1, k = 1, d = 1, t = 20, ref = length(feature), s = 10, eps = 1));
}

##########################
# Power Frequency domain #
##########################

ffta = function(feature) {
  return(spec.ar(feature, plot = FALSE));
}

####################
# Autocorrelatoins #
####################

acfMF = function(feature) {
  return(acf(feature, lag.max = NULL, type = "correlation", plot = FALSE, na.action = na.pass));
}

pacfMF = function(feature) {
  return(pacf(feature, lag.max = NULL, plot = FALSE, na.action = na.pass));
}

######################
# Diversity measures #
######################

div12 = function(SMAPE, series) {
  avg = 0;
  methods = dim(SMAPE)[1];
  means = 0;
  avgs = 0;
  avgSMAPE = 0;
  
  for(i in 1:methods) {
    avgSMAPE = avgSMAPE + 1 / methods * SMAPE[i, series];
  }
  
#   for(i in 1:methods) {
#     means = means + 1 / methods * 100 * abs(SMAPE[i, series]);
#     avgs = avgs + 1 / methods * 100 * abs(avgSMAPE - SMAPE[i, series]);
#   }
  
  div1 = mean(avgSMAPE);
  div2 = sd(avgSMAPE);
  
  return(list(div1 = div1, div2 = div2));
}

div3 = function(SMAPE, series) {
  return(mean(cor(SMAPE[, series], SMAPE[, select = -series])));
}

div4 = function(SMAPE, series) {
  return(sd(cor(SMAPE[, series], SMAPE[, select = -series])));
}

MetaFeatures = function(MetaFeaturesNN, dataset, SMAPENN, forcastingHorizonNN, TS = Analysis, header, NoOfTimeSeries = 111)
{
  print(paste('Meta-features of Series: ', TS, sep = ''));
  
  # iterating through 111 series
  for(series in 1:NoOfTimeSeries) 
  {
    if(header != 0) {
      datasetNN = dataset[-(1:header), series];
    } else {
      datasetNN = dataset[, series];
    }
    
    if(TS == 'NN5') {
      datasetNN = removeSparseness(datasetNN = datasetNN, valueFilled = 7);
    }
  
    datasetNN = as.vector(na.omit(datasetNN));

    if(forcastingHorizonNN == 0) {
      forcastingHorizonNN = as.integer(length(datasetNN) * 0.25); 
    }
    
    # detrend series
    #model = lm(na.omit(datasetNN[-(1:header), series]) ~ poly(index(na.omit(datasetNN[-(1:header), series])), 3, raw = TRUE));
    #detrend = zoo(resid(model), index(na.omit(datasetNN[-(1:header), series])));
    #print(na.omit(datasetNN[-(1:header), series]))
    
    detrendSeries = detrend(polyval(polyfit((1:length(datasetNN)), datasetNN, 3), (1:length(datasetNN))));
    
    #detrendSeries = fmri.detrend(datasetNN, degree = 3, accoef = 0);
        
    # general statistics
    MetaFeaturesNN[series, 1] = std(detrendSeries);      # standard deviation
    MetaFeaturesNN[series, 2] = skew(detrendSeries);     # skewness
    MetaFeaturesNN[series, 3] = kurt(detrendSeries);     # kurtosis
    MetaFeaturesNN[series, 4] = seriesLength(datasetNN);     # length
    MetaFeaturesNN[series, 5] = sd(datasetNN) / MetaFeaturesNN[series, 1];     # trend
    MetaFeaturesNN[series, 6] = as.numeric(durbinWatsonTest(lm(datasetNN ~ 1 + index(datasetNN) + I(index(datasetNN)^2) + I(index(datasetNN)^3)))[1]);     # durbin-watson
    
    MetaFeaturesNN[series, 7] = turn(datasetNN) / MetaFeaturesNN[series, 4];     # turning points
    MetaFeaturesNN[series, 8] = step(datasetNN) / MetaFeaturesNN[series, 4];       # step changes
    MetaFeaturesNN[series, 9] = pred(detrendSeries);     # predictibility measure
    MetaFeaturesNN[series, 10] = nonlin(detrendSeries);     # nonlinearity measure
    MetaFeaturesNN[series, 11] = lyap(datasetNN);     # lyapunov exponent
    
    # power frequency domain
    spect = ffta(detrendSeries);
#     MetaFeaturesNN[series, 12] = spect$freq[length(spect$freq)];     # three biggest power specturm frequencies
#     MetaFeaturesNN[series, 13] = spect$freq[length(spect$freq) - 1];
#     MetaFeaturesNN[series, 14] = spect$freq[length(spect$freq) - 2];
    MetaFeaturesNN[series, 12] = 0;
    MetaFeaturesNN[series, 13] = 0;
    MetaFeaturesNN[series, 14] = 0;
    
    MetaFeaturesNN[series, 15] = max(spect$spec);      # maximal power spectrum frequency
    MetaFeaturesNN[series, 16] = length(spect$spec[spect$spec >= (max(spect$spec) * 0.6)]);      # number of peaks greater than 60%
    
    # autocorrelations
    acfFeatures = acfMF(datasetNN);     # acf
    MetaFeaturesNN[series, 17] = acfFeatures$acf[2];
    MetaFeaturesNN5[series, 18] = acfFeatures$acf[3];
    
    pacfFeatures = pacfMF(datasetNN);     # pacf
    MetaFeaturesNN[series, 19] = pacfFeatures$acf[1];
    MetaFeaturesNN[series, 20] = pacfFeatures$acf[2];
    
    MetaFeaturesNN[series, 21] = pacfFeatures$acf[12];       # season
    
    # diversity
    divs = div12(SMAPENN, series)
    MetaFeaturesNN[series, 22] = 0;       # diversity1
    MetaFeaturesNN[series, 23] = 0;       # diversity2
    
    MetaFeaturesNN[series, 24] = 0;       # diversity3
    MetaFeaturesNN[series, 25] = 0;       # diversity4

#     MetaFeaturesNN[series, 22] = divs$div1;       # diversity1
#     MetaFeaturesNN[series, 23] = divs$div2;       # diversity2
#     
#     MetaFeaturesNN[series, 24] = div3(SMAPENN, series);       # diversity3
#     MetaFeaturesNN[series, 25] = div4(SMAPENN, series);       # diversity4
    
    print(paste("Series: ", series), sep = '');
  }
  
  colnames(MetaFeaturesNN) = c('std', 'skew', 'kurt', 'len', 'trend', 'dw', 'turn', 'step', 'pred', 'nonlinear', 'lyap', 'spec1', 'spec2', 'spec3', 'maxSpec', 'peaks', 'acf2', 'acf3', 'pacf1', 'pacf2', 'season', 'div1', 'div2', 'div3', 'div4');
  
  return(list(MetaFeaturesNN = MetaFeaturesNN));
}
