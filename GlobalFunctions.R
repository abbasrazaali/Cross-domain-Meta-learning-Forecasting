###############################################
# NAME: GlobalFunctions
#
# Description: Global functions 
#
# Modification History:
# Start: December 03, 2013
# 
# Authorship: Abbas Raza Ali 
###############################################

# clear screen
clearScreen <- function() 
{
  cat(rep("\n", 100));
}

# installs and loads a package if necessary
LoadLibrary = function(package)
{
  package <- as.character(package);
  if (!require(package, character.only = TRUE))
  {
    install.packages(pkgs = package, repos="http://cran.r-project.org", dependencies = TRUE);
    require(package, character.only = TRUE);

    print(paste(package, " is installed successfully.", sep = ''));
  }
}

# load data
LoadData = function(storageMedium = "csv", filePath = NULL, saveLoadDatasetPath = NULL, header = TRUE)
{
  if(tolower(storageMedium) == "csv") {
    if(!is.null(filePath)) {
      dataset = read.csv(file = filePath, header = header);
    } else {
      print(paste(filePath, " is not correct.", sep = ''));
    }
    if(!is.null(saveLoadDatasetPath)) {
      save(dataset, file = paste(saveLoadDatasetPath, sep = ''));  	      # save database table in binary file
    }
  } else if(!is.null(saveLoadDatasetPath) && tolower(storageMedium) == "object") {
    dataset = get(load(file = paste(saveLoadDatasetPath, sep = '')));		  # load binary file on the memory 
  } else {
    print("Incorrect parameters.");
  }

  return(dataset);
}