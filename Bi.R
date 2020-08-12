

Bi <- function(data, var = c(seq(1,ncol(data))), target)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  if(is.numeric(data[,target]))
  {
    for(i in var)
    {
      if(is.numeric(data[,i]))
      {
        png(paste(target,"_",names(data)[i], ".png", sep="")) #NOTE this step
        
        plot(data[,i],data[,target], xlab = names(data)[i] , ylab = target,
             main = paste('Association between', target,"&", names(data)[i]), col = "forestgreen")
        
        
        dev.off()
      }
      
      if(is.factor(data[,i]) | is.character(data[,i]))
      {
        png(paste(target,"_",names(data)[i], ".png", sep="")) #NOTE this step
        
        boxplot(data[,target]~ data[,i],
                data=data,
                main=paste("Side by Side Boxplot of", target, "&", names(data)[i]),
                xlab=names(data)[i],
                ylab=target,
                col="orange",
                border="brown", horizontal = FALSE)
        
        dev.off()
      }
      
    }
    
  }
  
  if(is.factor(data[,target]) | is.character(data[,target]))
  {
    for(i in var)
    {
      if(is.numeric(data[,i]))
      {
        png(paste(target,"_",names(data)[i], ".png", sep="")) #NOTE this step
        
        boxplot(data[,i]~data[,target],
                data=data,
                main=paste("Side by Side Boxplot of", target, "&", names(data)[i]),
                ylab=names(data)[i],
                xlab=target,
                col="orange",
                border="brown", horizontal = FALSE)
        
        
        dev.off()
      }
      
      else if (is.factor(data[,target]) | is.character(data[,target])) 
      {
        png(paste(target,"_",names(data)[i], ".png", sep="")) #NOTE this step
        barplot(table(data[,i], data[,target]), beside = T, legend.text = TRUE,
                args.legend = list(x = "topleft"), ylab = names(data)[i], xlab = target,
                main = paste("Barplot Between", target, "&", names(data)[i]))
        
        dev.off()
      }
      
      
    }
  }
}
