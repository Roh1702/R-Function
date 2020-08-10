# Function to know missing values in dataset
missing_values <- function(data)
{
  #creating a local function
  pmiss <- function(x)
  {
    return(sum(is.na(x))/length(x)*100)
  }
  
  miss <- apply(data, 2, pmiss)
  miss <- as.matrix(miss)
  colnames(miss) <- "% Missing"
  
  return(miss)
}
