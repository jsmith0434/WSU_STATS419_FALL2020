transposeMatrix = function(mat)
{
  t(mat)
}


rotateMatrix90 = function(mat)
{
  t(apply(mat, 2, rev))
}

rotateMatrix270= function(mat)
{
  apply(t(mat), 2, rev)
}


doSampleVariance = function(sample, method="two-pass")
{
  n = length(sample)
  sumXs = sum(sample)
  myMean = sumXs / n
  
  if(method=="naive")
  {
    sumXSquared = 0
    for (x in sample)
    {
      sumXSquared = sumXSquared +  x^2
    }
    naiveVariance  = ((sumXSquared - ((sumXs * sumXs) / n))) / (n - 1)
    
    if (abs(sumXSquared-((sumXs * sumXs) / n))< 1e-16)
    {
      writeLines("Warning: The precision of this result may be very low. Use of another method 
            is recommended.")
    }
    vals = data.frame(sumXs, sumXSquared, naiveVariance)
    colnames(vals) = c("sumXs", "sumXSquared", "variance")
    return(vals)
  }
  
  else
  {
    sumDiffSquared = 0
    for (x in sample)
    {
      sumDiffSquared = sumDiffSquared +  ((x - myMean) * (x - myMean))
    }
    #two-pass algorithm
    twoPassVariance = sumDiffSquared / (n - 1)
    
    vals = data.frame(sumXs, sumDiffSquared, twoPassVariance)
    colnames(vals) = c("sumXs", "sumDiffSquared", "variance")
    return(vals)
  }
}




doMode = function(x)
{
  #find frequencies
  df = as.data.frame(table(unlist(x)))
  
  #check if multiple
  if ("TRUE" %in%  duplicated(x))
  {
    #If bi-modal, store all of the ties
    modes <- unique(x[duplicated(x)])
    return(modes)
  }
  else
  {
    #if there is only one mode, return a vector of length one
    mode =  c(as.numeric(as.character(df[which.max(df$Freq),][1,1])))
    return(mode)
  }
}

doSummary = function(x)
{
  x=as.vector(t(x))
  length_ = length(x)
  numNA = sum(is.na(x))
  mean_ = mean(x)
  median_ = median(x)
  mode_ = doMode(x)
  varianceN = doSampleVariance(x, "naive")$variance
  variance2 = doSampleVariance(x, "other")$variance
  sd_builtIn = sd(x)
  sd_customN = sqrt(varianceN)
  sd_custom2 = sqrt(variance2)
  
  results = data.frame("length" = length_, "NAs" = numNA, "mean" = mean_, 
                       "median" = median_, "mode" = mode_, "variance(naive)" = varianceN, 
                       "variance(2pass) " = variance2, "sd(built-in) " =     
                         sd_builtIn, "sd_naive" = sd_customN , "sd_2pass" = sd_custom2)
  results = results[1,]
  return(results)
}


zScores = function(x)
{
  rownames(x) <- c()
  zs = vector()
  i=0
  for (item in x)
  {
    z = (item - rowMeans(x))/sd(x)
    zs = append(zs,z, i)
    i = i + 1
  }
  results = rbind(x,(zs))
  rownames(results) <- c("value", "z-score")
  return(results)
}

education = function(one)
{
  result = list();
  result$who 		= one;
  result$think 	= c("intensitively", "critically");
  result$goal 	= "intelligences + character";
  result;	
}

