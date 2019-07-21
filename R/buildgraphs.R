 buildgraphs <- function(data,vari=NULL,dir = NULL)  ## function with 3 arguments- a dataset , variable(we can choose the varibles in the form of vectors, whose plots has to be generated) and directory where we need to save the different plots
 {
   if(!is.data.frame(data))  ## checks if the dataset is in dataframe format
     stop("The given object is not a data frame")  ## if not, the function stops right there
   if(is.null(vari))  # checks if the argument variable is null
   {
     vari=1:ncol(data)  ## if yes, will assign variable with the all the columns of the dataframe(data)
   }
   if(is.null(dir))  ## checks if the argument dir is null
   {
     setwd("C:\\Users\\hi\\Desktop\\buildgraphs") ## if yes,will set dir as assign our interested directory 
   }
   for(i in vari)  ## iteration starts from first variable and goes till last variable
   {
     if(is.factor(data[,i]))  ## checks if the column is categorical(character)
     {
       png(paste(names(data)[i], ".png", sep="")) # gives names to the saved plots in the format names.png
       par(mfrow=c(1,2))  ## prints 2 plots in a single picture
       barplot(table(data[i]),main = paste("Barplot of", names(data)[i]),  ## plots bar plot for categorical variables
               xlab = names(data)[i], ylab = "Frequency", col = "brown", border=F)
       b<-data.frame(table(data[,i]))
       pie(table(data[i]),labels = b[,2], main = paste("Piechart of", names(data)[i]),col = rainbow(length(unique(data[,i])))) ## plots pie charts
       legend("topright", legend=as.character(b[,1]), fill = rainbow(length(unique(data[,i])))) ## gives legend, rainbow colour for different divisions
       dev.off()  ## used to upload the plots in the selected folder
     }
     else if(is.numeric(data[,i]) &(length(unique(data[,i])))>10)  ## checks if the column is numeric and also the number of elements in the data column is more than 10(continous data)
     {
       png(paste(names(data)[i], ".png", sep="")) # names of the plot is saves as plotname.png format, where plot name is name of the column
       par(mfrow=c(1,2)) ## prints 2 plots in a single picture
       boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), ## plots boxplot of the continous numeric variables
               ylab = names(data)[i], col = "maroon", border = "grey5",
               horizontal = F)
       hist(data[,i], main = paste("Histogram of", names(data)[i]), ## plots histogram of the continous numeric variables
            xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
       dev.off()  
     }
     else if(is.numeric(data[,i]) & length(unique(data[,i]<10)))  ## checks if the data is numeric and number of unique elements in data column is less than 10(to check if data is numeric but discrete in nature)
     {
       png(paste(names(data)[i], ".png", sep="")) 
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
 
