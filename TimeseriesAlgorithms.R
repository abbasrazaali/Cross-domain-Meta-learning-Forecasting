#####################################################################################################################
#
# Name: TimeseriesAlgorithms.R
#
# Description: Time-series and their combinations
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
#
#####################################################################################################################

# libraries
LoadLibrary('caret'); library('caret');
LoadLibrary('forecast'); library('forecast');
LoadLibrary('pracma'); library('pracma');
LoadLibrary("RSNNS"); library("RSNNS");
LoadLibrary('Metrics'); library('Metrics');
LoadLibrary('plyr'); library('plyr');
LoadLibrary('TTR'); library('TTR');
LoadLibrary('nnet'); library('nnet');

##############
# Timeseries #
##############

TimeSeries = function(SMAPE, dataset, seriesNN, Method, Accuracy, NoOfTimeSeries = 111, header = 0, forcastingHorizon = 0, Lag, smaN = 24, NNLayers)
{
  print(paste('Performance Measures of Series: ', seriesNN, sep = ''));
  
  # iterating through all time-series
  for(series in 1:NoOfTimeSeries) 
  {
    if(header != 0) {
      datasetNN = dataset[-(1:header), series];
    } else {
      datasetNN = dataset[, series];
    }
    
    if(seriesNN == 'NN5') {
      datasetNN = removeSparseness(datasetNN = datasetNN, valueFilled = 7);
    } else {
      datasetNN = removeSparseness(datasetNN = datasetNN, valueFilled = 1);
    }
    
    timeSeriesNN = as.vector(na.omit(datasetNN));
    
    if(forcastingHorizon == 0) {
      forcastingHorizon = as.integer(length(timeSeriesNN) * 0.25); 
    }
    
    #timeSeriesNN = detrend(timeSeriesNN, tt = 'linear', bp = 3);
    
    trainingSize = length(timeSeriesNN) - forcastingHorizon;
    predictionStart = length(timeSeriesNN) - forcastingHorizon + 1;
    methodNo = 1;

#     if(seriesNN == 'NNGC_F') {
#       SMAPE[methodNo, series] = 99.0;
#     } else {
    # Simple Forecasting Models
    # Moving Average
    ma = MA(timeSeriesNN, pastPeriods = smaN, horizon = forcastingHorizon, step = 3);
    SMAPE[methodNo, series] = performance(prediction = ma[predictionStart:length(ma)], actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    #SMAPE[methodNo, series] = performance(prediction = ma$mean, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon);
#     }
    methodNo = methodNo + 1;
  
    # Single Exponential Smooting
    ses = SES(timeSeriesNN, alpha = 0.2, horizon = forcastingHorizon);

    #SMAPE[methodNo, series] = performance(prediction = ses[predictionStart:length(ses)], actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    SMAPE[methodNo, series] = performance(prediction = as.numeric(ses$prediction$mean), actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    methodNo = methodNo + 1;

    # Taylors Exponential Smoothing
#     taylor = Taylor(timeSeriesNN, horizon = forcastingHorizon);
#     SMAPE[methodNo, series] = performance(prediction = taylor[predictionStart:length(taylor)], actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
#     methodNo = methodNo + 1;
  
    # Polynomial Regression
#     polyn = Poly(timeSeriesNN, polynome = 6, horizon = forcastingHorizon);
#     SMAPE[methodNo, series] = performance(prediction = polyn[predictionStart:length(polyn)], actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
# #     SMAPE[methodNo, series] = performance(prediction = polyn$prediction$mean, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
#     methodNo = methodNo + 1;

    # Automatic Box-Jenkins Models - Automatic ARMA Selection
    arimaAuto = ARIMA(timeSeriesNN[1:trainingSize], startQ = 1, maxQ = 2, horizon = forcastingHorizon);  # ARMA
    SMAPE[methodNo, series] = performance(prediction = arimaAuto$prediction$mean, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    methodNo = methodNo + 1;
    
    # State-Space Model including (p and q selected usingAIC)
#     arimaSSM = ARIMASSM(timeSeriesNN[1:trainingSize], ic = "aic", horizon = forcastingHorizon);    # AIC
#     SMAPE[methodNo, series] = performance(prediction = arimaSSM$prediction$mean, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
#     methodNo = methodNo + 1;

    # Structural Time-series Models
    struct = Structural(timeSeriesNN, horizon = forcastingHorizon);
    SMAPE[methodNo, series] = performance(prediction = struct$prediction$mean, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
#     struct = Structural(timeSeriesNN, horizon = forcastingHorizon);
    #SMAPE[methodNo, series] = performance(prediction = struct[predictionStart:length(struct)], actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    methodNo = methodNo + 1;

    # Computational Intelligence Models - NN
    #nn = neuralNetwork(timeSeriesNN, lags = Lag, size = NNLayers, repeats = 10, maxit = 100, horizon = forcastingHorizon);
    nn = neuralNetwork(timeSeriesNN, lag = Lag, size = NNLayers, repeats = 10, maxit = 200, decay = 0.1, horizon = forcastingHorizon);
    #nn = neuralNetwork(timeSeriesNN, lag = Lag, repeats = 15, size = NNLayers, lambda = Lambda, horizon = forcastingHorizon);
    #SMAPE[methodNo, series] = performance(prediction = nn$bestPrediction$mean, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    SMAPE[methodNo, series] = performance(prediction = nn, actuals = timeSeriesNN[predictionStart:length(timeSeriesNN)], horizon = forcastingHorizon); 
    #SMAPE[methodNo, series] = 10;
  #   if(SMAPE[methodNo, series] < 0 || SMAPE[methodNo, series] > 100) {
  #     SMAPE[methodNo, series] = 0;    
  #   }
    #} else {
     # SMAPE[methodNo, series] = 0;
    #}
  #   if(SMAPE[methodNo, series] == 'NaN') {
  #     SMAPE[methodNo, series] = 0.0;
  #   }
  # #   tune = expand.grid(.size = c(12), .decay = c(0.01));
  #   model = train(y = as.factor(timeSeriesNN[1:trainingSize], x = index(timeSeriesNN[1:trainingSize]), method = 'nnet', tuneGrid = tune, verbose = FALSE, linout = TRUE);
  #   print(model)
    # Forecasting Combination Models 
    #SMAPE[16, series] = 0;   # simple average
    
    #SMAPE[17, series] = 0;   # simple trimmed average
    
    #SMAPE[18, series] = 0;   # outperformance
    
    #SMAPE[19, series] = 0;   # variance-based
    
    #SMAPE[20, series] = 0;   # varience-based pooling - kmeans (2) 
    
    #SMAPE[21, series] = 0;   # varience-based pooling - kmeans (3)
    
    ################################
    # Meta-knowledge Preprocessing #
    ################################
    
    #SMAPE = SMAPE[, -nearZeroVar(SMAPE)];     # eliminate near to zero variables
    #print(series)
  #   Method[series, 1] = which(SMAPE[, series] == min(SMAPE[(SMAPE[, series] > 0) == TRUE, series]))[1];
  #   Method[series, 2] = which(SMAPE[, series] == max(SMAPE[(SMAPE[, series] > 0) == TRUE, series]))[1];
  
    #Accuracy[series] = min(SMAPE[(SMAPE[, series] > 0) == TRUE, series]);

    Method[series, 1] = which(SMAPE[, series] == min(SMAPE[, series]))[1];
    Method[series, 2] = which(SMAPE[, series] == max(SMAPE[, series]))[1];
  
    Accuracy[series] = min(SMAPE[, series]);

    print(paste("Series: ", series, " - Best Method: ", Method[series], " and Accuracy: ", Accuracy[series], ', NN SMAPE: ', SMAPE[methodNo, series], sep = ''));
  }

  return(list(SMAPE = SMAPE, Accuracy = Accuracy, Method = Method));
}

########################################
# compute SMAPE and standard deviation #
########################################

SMAPE = function(SMAPE, performance, series)
{
  # compute SMAPE and standard deviation
  for(method in 1:NoOfTimeSeriesMethods) 
  {
    performance[1, method] = sum(SMAPE[method, ]) / series;
    performance[2, method] = sd(SMAPE[method, ]);
  }
  
  return(performance);
}

#####################
# remove sparseness #
#####################

removeSparseness = function(datasetNN, valueFilled) 
{
  naValues = which(is.na(datasetNN));
  naValues[is.na(naValues)] = 0;
  
  for(instance in 1:length(naValues)) 
  {
    datasetNN[naValues[instance]] = (datasetNN[naValues[instance] - valueFilled] + datasetNN[naValues[instance] + valueFilled]) / 2;
  }

  return(datasetNN);
}

########################
# creating time-series #
########################

# createTimeseries = function(datasetNN, header, frequency, forcastingHorizon) 
# {
#   timeSeriesNN = na.omit(datasetNN[-(1:header)]);
# 
#   seriesLength = length(timeSeriesNN);
#   #endTS = c(datasetNN[1] + (seriesLength - forcastingHorizon) %/% frequency, datasetNN[2] + (seriesLength - forcastingHorizon) %% frequency - 1);
#   #StartWTS = c(datasetNN[1] + (seriesLength - forcastingHorizon) %/% frequency, datasetNN[2] + (seriesLength - forcastingHorizon) %% frequency);
# 
#   # timeSeries
#   #timeSeries = ts(na.omit(dataset[-(1:header)]), start = c(datasetNN[1], datasetNN[2]), end = endTS, frequency = frequency);
#   
#   # forecasting horizon
#   #horizon = na.omit(datasetNN[(header + seriesLength - forcastingHorizon + 1):(seriesLength + header + 1)]); 
#   #window = ts(na.omit(datasetNN[(header + seriesLength - forcastingHorizon + 1):(seriesLength + header + 1)]), start = StartWTS, frequency = frequency); 
#   
#   return(list(timeSeriesNN = timeSeriesNN, timeSeries = timeSeries, horizon = horizon, window = window));
# }

##########################
# time-series algorithms #
##########################

# Simple Forecasting Models
# Moving Average
# MA = function(timeSeries, pastPeriods, horizon) 
# {
#   trainingSize = length(timeSeries) - horizon;
#   
#   model = SMA(timeSeries, n = 1);
#   
#   prediction = forecast(model, horizon);
#   print(prediction$residuals)
#   print(prediction$model$mse)
#   
#   bestPrediction = prediction;
#   minimum = mse(timeSeries[(trainingSize + 1):length(timeSeries)], prediction$mean);
#   print(minimum);
#   wfd = ksnf;
#   # grid search
#   for(i in 2:pastPeriods) {
#     model = SMA(timeSeries, n = i);
#     prediction = forecast(model, horizon);
#     error = mse(timeSeries[(trainingSize + 1):length(timeSeries)], prediction$mean);
#  
#     if((error < minimum) && (error > 0)) {
#       minimum = error;
#       bestPrediction = prediction;
#     }
#   }
# 
#   return(bestPrediction);
# }

MA = function(timeSeries, pastPeriods, horizon, step = 1)
{
  trainingSize = length(timeSeries) - horizon;

  model = MAAverage(timeSeries, 2, step);
  bestModel = model;
  minimum = mse(timeSeries[(trainingSize + 1):length(timeSeries)], model[(trainingSize + 1):length(timeSeries)]);

  # grid search
  for(i in 3:pastPeriods) {
    model = MAAverage(timeSeries, i, step);
    error = mse(timeSeries[(trainingSize + 1):length(timeSeries)], model[(trainingSize + 1):length(timeSeries)]);
    
    if((error < minimum) && (error > 0)) {
      minimum = error;
      bestModel = model;
    }
  }

  return(bestModel);
}

MAAverage = function(timeSeries, size, step)
{
  fc = timeSeries;

  for(i in (1 + step):length(timeSeries)) {
    if(i <= (size + step)) {
      fc[i] = sum(timeSeries[1:(i - step)]) / length(timeSeries[1:(i - step)]);
    } else {
      fc[i] = sum(timeSeries[(i - size - step + 1):(i - step)]) / length(timeSeries[(i - size - step + 1):(i - step)]);
    }
  }
  
  return(fc);
}

# Single Exponential Smooting
SES = function(timeSeries, alpha = NULL, horizon) 
{
  trainingSize = length(timeSeries) - horizon;
  
  model = HoltWinters(timeSeries[1:trainingSize], alpha = alpha, beta = FALSE, gamma = FALSE);
  prediction = forecast(model, horizon);
  
  return(list(model = model, prediction = prediction));
  
#   trainingSize = length(timeSeries) - horizon;
#   bestAlpha = alpha_val[1];
#   model = HoltWinters(timeSeries[1:trainingSize], alpha = alpha_val[1], beta = FALSE, gamma = FALSE);
#   bestModel = model;
#   prediction = forecast(model, horizon);
#   minimum = mse(timeSeries[(trainingSize + 1):length(timeSeries)], as.numeric(prediction$mean));
#   #print(timeSeries[(trainingSize + 1):length(timeSeries)])
#  
#   # grid search
#   for(i in 2:length(alpha_val)) {
#     model = HoltWinters(timeSeries[1:trainingSize], alpha = alpha_val[i], beta = FALSE, gamma = FALSE);
#     prediction = forecast(model, horizon);
#     error = mse(timeSeries[(trainingSize + 1):length(timeSeries)], as.numeric(prediction$mean));
#     print(timeSeries[(trainingSize + 1):length(timeSeries)])
#     print(as.numeric(prediction$mean))
#     print(error)
#     if((error < minimum)) {
#       minimum = error;
#       bestModel = model;
#       bestAlpha = alpha_val[i];
#     }
#   }
#   
#   return(bestModel);
}

# I_SES = function(timeSeries, series, horizon) 
# {
#   model = timeSeries;
#   alpha = 0.2;    
#   beta = 0.2;     
#   a = 0;          
#   m = 0;          
#   e = 0;   
#   predictionStart = length(timeSeries) - horizon + 1
# 
#   model[predictionStart] = alpha * timeSeries[predictionStart - 1] + (1 - alpha) * model[predictionStart - 1];
#   #print(model[predictionStart])
#   #print(timeSeries[predictionStart - 1])
#   for(i in (predictionStart + 1):length(timeSeries)) {
#     e = timeSeries[i - 1] - model[i - 1];
#     m = beta * abs(e) + (1 - beta) * m;
#     a = beta * e + (1 - beta) * a;
#     alpha = abs(a / m);
#     #if(series == 77) {print(timeSeries[i - 1]); print(model[i - 1])}
#     model[i] = alpha * timeSeries[i - 1] + (1 - alpha) * model[i - 1];
#   }
# 
#   return(model);
# }  

# SES = function(timeSeries, alpha, horizon) 
# {
#   trainingSize = length(timeSeries) - horizon;
#   
#   model = SESSmooth(timeSeries, 0.05);
#   bestModel = model;
#   minimum = mse(timeSeries[1:trainingSize], model[1:trainingSize]);
#   
#   # grid search
#   for(i in alpha) {
#     model = SESSmooth(timeSeries, i);
#     error = mse(timeSeries[1:trainingSize], model[1:trainingSize]);
#     
#     if(error < minimum) {
#       minimum = error;
#       bestModel = model;
#     }
#   }
#   
#   return(bestModel);
# }
# 
# SESSmooth = function(timeSeries, alpha)
# {
#   fc = timeSeries;
#   for(i in 2:length(timeSeries)) {
#     fc[i] = alpha * timeSeries[i - 1] + (1 - alpha) * fc[i - 1];
#   }
# 
#   return(fc);
# }

# Taylor's Exponential Smoothing
Taylor = function(timeSeries, horizon) 
{
  trainingSize = length(timeSeries) - horizon;
  
  model = TSmooth(timeSeries, 0.05, 0.05, 0.05);
  bestModel = model;
  minimum = mse(timeSeries[1:trainingSize], model[1:trainingSize]);
  
  # grid search
  for(i in seq(0.1, 1, 0.1)) {
    for(j in seq(0.1, 1, 0.1)) {
      for(k in seq(0.1, 1, 0.1)) {
        model = TSmooth(timeSeries, i, j, k);
        error = mse(timeSeries[1:trainingSize], model[1:trainingSize]);
        
        if(error < minimum) {
          minimum = error;
          bestModel = model;
        }
      }
    }
  }

  return(bestModel);
}

TSmooth = function(timeSeries, alpha, beta, phi) 
{
  fc = timeSeries;
  l = timeSeries[1];        
  r = timeSeries[2] / timeSeries[1]; 
  fc[1] = l * r^phi;
  
  for(i in 2:length(timeSeries)) {
    lOld = l;
    l = alpha * timeSeries[i - 1] + (1 - alpha) * (lOld + r^phi);
    r = beta * (l / lOld) + (1 - beta) * r^phi;
    
    fc[i] = l * r^phi;
  }
  
  return(fc);
}  

# Polynomial Regression
Poly = function(timeSeries, polynome, horizon)
{
  nums = t(1:length(timeSeries));
  endTrain = length(timeSeries) - horizon;
  nValid = length(timeSeries) - endTrain;

  p = polyfit(nums[1:(endTrain - nValid)], timeSeries[1:(endTrain - nValid)], 1);
  model = polyval(p, nums);
  bestModel = model;
  minimum = mse(timeSeries[(endTrain - nValid + 1):endTrain], model[(endTrain - nValid + 1):endTrain]);
  
  # grid search
  for(i in 2:polynome) {
    p = polyfit(nums[1:(endTrain - nValid)], timeSeries[1:(endTrain - nValid)], i);
    model = polyval(p, nums);
    error = mse(timeSeries[(endTrain - nValid + 1):endTrain], model[(endTrain - nValid + 1):endTrain]);
    
    if(error < minimum) {
      minimum = error;
      bestModel = model;
    }
  }

  return(bestModel);
}
 
# Poly = function(timeSeries, polynome, horizon) 
# {
#   model2 = lm(timeSeries ~ poly(index(timeSeries), 2, raw = TRUE));
#   model3 = lm(timeSeries ~ poly(index(timeSeries), 3, raw = TRUE));
# 
#   print(names(model3))
# 
#   print(summary(model3))
# 
#   print(model2$residuals)
#   
#   prediction2 = forecast(model2, h = horizon);
#   prediction3 = forecast(model3, h = horizon);
#   
#   print(prediction2)
# 
#   print(prediction3)
#   
#   return(list(model = model, prediction = prediction));
# }

# Theta Models
# Theta = function(timeSeries, horizon)
# {
#   a = 0;      
#   b = 0;      
#   model = timeSeries;
# 
#   wSum = 0;
#   trainingSize = length(timeSeries) - horizon;
# 
#   xMean = mean(timeSeries[1:trainingSize]);
#   wSum = t(sum((1:(trainingSize - 1))) * timeSeries[1:(trainingSize - 1)]);     
# 
#   # calculate parameters a and b for theta = 0
#   theta = 0;  
#   b = 6 * (1 - theta) / (trainingSize^2 -1) * (2 / trainingSize * wSum - (trainingSize + 1) * xMean);
#   a = (1 - theta) * xMean - b * (trainingSize - 1) / 2;
# 
#   # calculate forecast vector by linear extrapolation
#   theta1 = timeSeries;
#   theta1 = a + b * (1:length(timeSeries));
# 
#   # calculate parameters a and b for theta = 2
#   theta = 2;  
#   b = 6 * (1 - theta) / (trainingSize^2 - 1) * (2 / trainingSize * wSum - (n + 1) * xMean);
#   a = (1 - theta) * xMean - b * (trainingSize - 1) / 2;
# 
#   # calculate forecast vector with exponential smoothing
#   theta2 = timeSeries;
#   theta2 = a + b * (1:length(timeSeries)) + theta * t(timeSeries);
#   theta2 = SES(theta2, trainingSize);
# 
#   #  simple average
#   model = 0.5 * (t(theta1) + t(theta2));         
#   
#   return(model);
# }
  

# Automatic Box-Jenkins Models
ARIMA = function(timeSeries, startQ = 1, maxQ = 2, horizon) 
{
  model = auto.arima(timeSeries, start.q = startQ, max.Q = maxQ);
  #model = auto.arima(timeSeries, start.q = startQ, max.Q = maxQ);
  
  #model = arima(timeSeries, order = c(1, 2, 1), method = "ML");
  
  prediction = forecast(model, horizon);
  
  return(list(model = model, prediction = prediction));
}

ARIMASSM = function(timeSeries, ic = "aic", horizon) 
{
  model = ets(y = timeSeries, model = "ZZZ", opt.crit = "mse", ic = ic, nmse = 5, restrict = TRUE);
  
  prediction = forecast(model, horizon);
  
  return(list(model = model, prediction = prediction));
}

# Structural Time-series Models
Structural = function(timeSeries, horizon) 
{
  #model = ets(y = timeSeries, model = "ZZZ", opt.crit = "mse", ic = "aic", nmse = 5, restrict = TRUE);
  model = StructTS(timeSeries, type = 'level');
  prediction = forecast(model, horizon);
  
  return(list(model = model, prediction = prediction));  
}

# Structural = function(timeSeries, horizon) 
# {
#   trainingSize = length(timeSeries) - horizon;
#   model = timeSeries;
#   
#   model = StructSmooth(timeSeries, 0.05);
#   bestModel = model;
#   minimum = mse(timeSeries[1:trainingSize], model[1:trainingSize]);
#   
#   # grid search
#   for(i in seq(0.1, 1, 0.05)) {
#     model = StructSmooth(timeSeries, i);
#     error = mse(timeSeries[1:trainingSize], model[1:trainingSize]);
#         
#     if((error < minimum) && (error > 0)) 
#     {
#       minimum = error;
#       bestModel = model;
#     }
#   }
#   
#   return(bestModel);
# }
# 
# StructSmooth = function(timeSeries, alpha)
# {
#   fc = timeSeries;
#   
#   for(i in 2:length(timeSeries)) {
#     fc[i] = alpha * timeSeries[i - 1] + (1 - alpha) * fc[i - 1];
#   }
#   
#   return(fc);
# }

# Neural Network Time-series Models
nnts = function(data, lags, size, retry = 1, maxit = 1000, decay = 0.01, nntrace = F)
{
  minval = 1e37;
  p = length(lags);
  maxlag = max(lags);
  n = length(data);
  x = matrix(NA, n - maxlag, p);
  
  for(i in 1:p) {
    x[, i] = data[(maxlag + 1 - lags[i]):(n-lags[i])];
  }
  
  y = data[(maxlag+1):n];
  
  rang = 1/max(abs(data));
  for(i in 1:retry) {
    g = nnet(x, y, size = size, rang =  rang, decay = decay, linout = T, maxit = maxit, trace = nntrace);
    if( g$val < minval) {
      gbest = g;
      minval = g$val;
    }
  }
  
  gbest$x = x;
  gbest$y = y;
  gbest$lags = lags;
  
  return(structure(gbest, class = c("nnts","nnet")));
}

predict.nnts = function(net, horizon = 1, testData)
{
  lags = net$lags;
  alldata = net$y;

  pred = numeric(horizon);
  for(i in 1:horizon) {
    n = length(alldata)
    pred[i] = nnet:::predict.nnet(net, alldata[n-lags + 1]);
    alldata = c(alldata, testData[i]);
  }

  return(pred);
}

neuralNetwork = function(timeSeries, lag, size, repeats = 1, maxit, decay = 0.01, horizon) 
{
  trainingSize = length(timeSeries) - horizon;

  model = nnts(timeSeries[1:trainingSize],lags = lag, size = size, retry = repeats, maxit = maxit, decay = decay);
  prediction = predict.nnts(model, horizon, timeSeries[(trainingSize + 1):length(timeSeries)]);
  
  return(prediction);
}

##########################
# compute SMAPE and Std. #
##########################

performance = function(prediction, actuals, horizon, divisor = 2) 
{
  SMAPE = ((sum(abs(actuals - prediction) / ((actuals + prediction) / divisor))) * 100) / horizon;
  
  return(SMAPE);
}

performanceAcc = function(prediction, actuals, horizon) 
{
  Accuracy = sum(abs(actuals - as.numeric(prediction)) == 0) / horizon * 100;
  
  return(Accuracy);
}

#####################
# time-series plots #
#####################

plotSeries = function(timeSeries, series = 1, xlabel = NULL, ylabel = NULL, title = NULL) 
{
  par(mar = rep(2, 4));
  plot(timeSeries, xlab = xlabel, ylab = ylabel, main = title);
}

plotTimeseries = function(timeSeries) 
{
  plot.ts(timeSeries);
}