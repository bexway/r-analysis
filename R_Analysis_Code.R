library(ggplot2)
library(reshape2)
library(plyr)

data <- read.csv("Randomized_Data.csv")
data <- na.omit(data)
#omits Na data

#Recasting to get average response for each subject on each condition. Removes data I'm not analyzing now
datarecast <- recast(data,id.var=c("subject","condition_a","condition_b","contrast"),
	measure.var="response",formula=subject+condition_a+condition_b+contrast ~., fun.aggregate = mean)

#For some reason, the recast response variable gets named '.' so I change that here
colnames(datarecast)[length(datarecast)] = "response"

#I want contrast to be treated as a factor, not continuous
datarecast$contrast <- as.factor(datarecast$contrast)

#A box plot with x axis contrast and y axis response
#boxplot.pdf
p <- qplot(contrast, response, data=datarecast, geom="boxplot", 
      fill=condition_b, main="Response by Contrast",
      xlab="Contrast", ylab="Response") + scale_fill_manual(values=c("blue", "red"))

ggsave(filename="boxplot.pdf", plot=p)


#A faceted jitter plot of four sections, split by condition a and condition b
#(the color is for fun)
p <- qplot(contrast, response, data=datarecast, geom="jitter", 
      facets=condition_a~condition_b, color=condition_b,
      xlab="Contrast", ylab="Response") + scale_color_manual(values=c("blue", "red"))

ggsave(filename="jitterplot.pdf", plot=p)


#Adding error bars

#Step1: use this function (Rebecca didn't write this)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                       conf.interval=.95, .drop=TRUE) {
     
    
     # New version of length which can handle NA's: if na.rm==T, don't count them
     length2 <- function (x, na.rm=FALSE) {
         if (na.rm) sum(!is.na(x))
         else       length(x)
     }
     
     # This does the summary. For each group's data frame, return a vector with
     # N, mean, and sd
     datac <- ddply(data, groupvars, .drop=.drop,
                    .fun = function(xx, col) {
                        c(N    = length2(xx[[col]], na.rm=na.rm),
                          mean = mean   (xx[[col]], na.rm=na.rm),
                          sd   = sd     (xx[[col]], na.rm=na.rm)
                        )
                    },
                    measurevar
     )
     
     # Rename the "mean" column    
     datac <- rename(datac, c("mean" = measurevar))
     
     datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
     
     # Confidence interval multiplier for standard error
     # Calculate t-statistic for confidence interval: 
     # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
     ciMult <- qt(conf.interval/2 + .5, datac$N-1)
     datac$ci <- datac$se * ciMult
     
     return(datac)
 }


#Create summarized data. The summary function will recast it.
#The first arg (data) is your data frame.
#The measurevar ("response") is the variable you want the error of (your y axis variable)
#The groupvars ("cond" etc) are the indep variables that you want included; if you don't care about something,
#then leave it out of here. (For example, I didn't care about the subject numbers, so "subj" isn't there.)
datasum = summarySE(data, measurevar="response", groupvars=c("condition_a", "condition_b", "contrast"))

p <- ggplot(datasum, aes(x=contrast,y=response,color=condition_b, shape=condition_b, group=condition_b), response) + 
	geom_point() + 
	geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.2) + 
	facet_wrap(~condition_a,ncol=2) + 
	geom_smooth(method=lm, se=FALSE) + scale_color_manual(values=c("blue", "red"))

ggsave(filename="errorbars.pdf", plot=p)

