Bi <- function(data, var = c(seq(1,ncol(data))), target)
{ print("amhere")
  data = movemydfs_categorical_numeric(data)
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
  print("amherealso")
}


#Function : rearrange the columns so that they are grouped 
#according to datatype nemeric vs categorical.

movemydfs_categorical_numeric <-function(data)
{
  temp <- numeric()
  temp <-c()
  t = ncol(data)
  
  
  for(i in 1:ncol(data))
  {
    
    tt=table(data[,i])#Cross tabulation
    w = as.data.frame(tt)
    if (dim(w)[1]<=10)#Checking if there are <10factors
    { 
      print(paste("am categorical",names(data)[i],sep="="))
      data[,ncol(data)+1] =data[,i] 
      
      #Converting the categorical columns to Factors
      data[,ncol(data)] <- as.factor(data[,ncol(data)]) 
      temp<-append(temp,i)
    }
    
  } 
  
  j=1 #Creating a new categorical column at the end
  for (i in temp)
  {
    names(data)[t+j]= names(data)[i]  
    j=j+1
  }
  
  data = data[-temp] #deleting the duplicate categorical cols
  #View(data)
  q=length(temp)
  print(paste("Total cols =",ncol(data),"categorical cols = ",q,"categorical at the end"))
  #print(class(data))
  return (data)
}