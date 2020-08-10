## Graph function to save Boxplot, Histogram, Pie chart & Barchart.##

Univariate_Analysis <- function(data, var = c(seq(1,ncol(data))))
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in var)
  {
    if(is.numeric(data[,i]))
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)
    }
    
    else if(is.factor(data[,i]) | is.character(data[,i]) )
    {
      png(paste(names(data)[i], ".png", sep = "")) #No space between "names.png"
      par(mfrow=c(1,2))
      barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]),
              xlab = names(data)[i], ylab = "Frequency", col = "maroon", border = "grey5")
      
      pie(table(data[,i]), main = paste("Pie Chart of", names(data)[i]))
      
    }
    
    dev.off()
  }
}



