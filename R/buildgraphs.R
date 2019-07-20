buildgraphs <- function(data,vari=NULL,dir = NULL)
{
  
  
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  if(is.null(vari))
  {
    vari=1:ncol(data)
  }
  if(is.null(dir))
  {
    setwd("C:\\Users\\hi\\Desktop\\R")
  }
  for(i in vari)
  {
    if(is.factor(data[,i]))
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      par(mfrow=c(1,2))
      barplot(table(data[i]),main = paste("Barplot of", names(data)[i]),
              xlab = names(data)[i], ylab = "Frequency", col = "brown", border=F)
      b<-data.frame(table(data[,i]))
      pie(table(data[i]),labels = b[,2], main = paste("Piechart of", names(data)[i]),col = rainbow(length(unique(data[,i]))))
      legend("topright", legend=as.character(b[,1]), fill = rainbow(length(unique(data[,i]))))
      dev.off()
    }
    else if(is.numeric(data[,i]) &(length(unique(data[,i])))>10)
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(1,2))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = F)
      hist(data[,i], main = paste("Histogram of", names(data)[i]),
           xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
      dev.off()  #NOTE this step
    }
    else if(is.numeric(data[,i]) & length(unique(data[,i]<10)))
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      par(mfrow=c(1,2))
      barplot(table(data[i]),main = paste("Barplot of", names(data)[i]),
              xlab = names(data)[i], ylab = "Frequency", col = "brown", border=F)
      b<-data.frame(table(data[,i]))
      pie(table(data[i]),labels = b[,2], main = paste("Piechart of", names(data)[i]),col = rainbow(length(unique(data[,i]))))
      legend("topright", legend=as.character(b[,1]), fill = rainbow(length(unique(data[,i]))))
      dev.off()
    }
  }
}
