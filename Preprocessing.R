#####################################################################################################################
#
# Name: Preprocessing.R
#
# Description: Meta-knowledge pre-processing
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
#
#####################################################################################################################

# libraries
LoadLibrary('randomForest'); library(randomForest);

#################################
# Meta-knowledge Pre-processing #
#################################

MKPreprocess = function(MetaFeatures)
{
  # Zero- and Near Zero-Variance Predictors
  excludeSet = nearZeroVar(MetaFeatures);
  
  return(excludeSet);
}

######################################
# Meta-knowledge Variable Importance #
######################################

VarImportance = function(MK, threshold = '3', fsAlgo = T)
{
  #[, -MKPreprocess(MK)]
  if(fsAlgo == T) {
    rf = randomForest(as.factor(Method) ~ ., data = MK, ntree = 50000, importance = TRUE, proximity  = FALSE);
    importnt = importance(rf, type = 1, scale = TRUE);    # either 1 or 2, specifying the type of importance measure (1=mean decrease in accuracy, 2=mean decrease in node impurity)
    
    print(importnt);
    
    save(importnt, file = paste(getwd(), "/Implementation/Objects/NN/importantFeatures", sep = ''));
  } else {
    importnt = get(load(file = paste(getwd(), "/Implementation/Objects/NN/importantFeatures", sep = ''))); 
  }

  if(threshold == '3') {
    vars = rownames(importnt)[(importnt[, 1] > as.numeric(importnt[order(-importnt), ])[4]) == TRUE];
  }
  
  if(threshold == 'mean') {
    vars = rownames(importnt)[(importnt[, 1] > mean(importnt[, 1])) == TRUE];
  } 
  
  if(threshold == 'all') {
    vars = rownames(importnt);
  }
  
  return(list(importantVariables = data.frame(importnt[order(-importnt), ]), vars = vars));
}

