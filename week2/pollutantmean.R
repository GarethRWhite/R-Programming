pollutantmean <- function(directory, pollutant, id = 1:332)
{
	## 'directory' is a character vector of length 1 indicating the location of the CSV file
	
	## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either 'sulfate' or 'nitrate'
	
	## 'id' is an integer vector indicating the monitor ID numbers to be used.
	
	## Return the mean of the pollutant across all monitors listed in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
	
	cwd <- getwd()
	setwd(directory)
	resultsDF <- data.frame()
	for (n in id)
	{
		fileName <- sprintf("%03d.csv", n)
		currentDF <- read.csv(fileName)
		resultsDF <- rbind(resultsDF, currentDF)
	}
	setwd(cwd)
	
	sprintf("%.3f", mean(subset(resultsDF)[[pollutant]], na.rm=TRUE))
}

## Example output
#source("pollutantmean.R")
#pollutantmean("specdata", "sulfate", 1:10)
### [1] 4.064
#pollutantmean("specdata", "nitrate", 70:72)
### [1] 1.706
#pollutantmean("specdata", "nitrate", 23)
### [1] 1.281

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
