install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)



load("test3_data.Rdata")
##1. Subset the data frame to include only the named fields. (2.5 points)
names(d)
fields<- names(d[,c(1:19)])
## SUBSETTING: making a df with only the columns we want from the dataset "d"
b<- d[,c(fields)]
b


##2. Sort the data by ‘transect.id’ and then for each by ‘dateTime’ so that the last observation is the last one in time.
##This will be done in ascending order
e<- arrange(d, transect.id, dateTime)
e

##3. Create a directory to store figures (2 points)
suppressWarnings(dir.create("plots"))


##4. For each transect with a tow type, plot the vertical path the instrument moved through the water
##column using functions from the ‘ggplot2' library. (10 points)
##a. Use 'dateTime' as your x-axis values. Set the x-axis tick interval to be 15 minutes with the label ‘hour:minute’.
##b. Make sure the water surface is at the top of the plot and the deepest depth is at the bottom of the plot
##c. Make the points hollow and outlined in dark blue.
##d. Label each plot with the transect’s name as the title.
##e. Add a smoother to the plot.
unique(b$tow)
##Spectral colour map
spectral <- function(n=6) {
  library("RColorBrewer")
  rev(brewer.pal(name="Spectral", n=n))
}

scale_fill_spectral <- function(...) {
  scale_fill_gradientn(colours=spectral(...))
}
scale_colour_spectral <- function(...) {
  scale_colour_gradientn(colours=spectral(...))
}
ddply(.data = b, .variables = c("transect.id"), function(x){

name <- unique(x$transect.id)
  
pl <- ggplot(data = x, aes(x = dateTime, y = depth)) +
geom_point(colour="blue")+
facet_wrap(~tow)
##Scale_x_datetime(name = "Time", labels = date_format("%H:%M"),
##breaks = date_breaks("15 min"), minor_breaks = "5 min") +
##Add a smoother 
geom_smooth() 
ggtitle(label = name)
  
ggsave(filename = paste0(name,'.png'),
plot = pl, width = 4, height = 3, units = 'in',dpi = 300)
}, .inform = T, .progress = "text")


##5. Create at custom function to correct 'study' type to each observation. (5 points)
unique(d$transect.id)

study.fxn <- function(x){
  
f<- str_split_fixed(string = x[['transect.id']], pattern = "-", n = 2)
g <- f[2]
  
if(str_detect(string = g, pattern = "L")){
study <- "lagrangian"
    
##NOTE: the "fixed' function allows you to match the EXACT character string rather than only part of the string
} else if(str_detect(string = g, pattern = fixed("Eddy"))){
  study = 'Eddy'
    
} else if(str_detect(string = g, pattern = "W")) {
  study <- "spatial"
    
} else if(str_detect(string = g, pattern = "E")) {
  study <- "spatial"
    
} else if(str_detect(string = g, pattern = "C")) {
  study <- "spatial"
    
} else {}
  
  return(study)
}


##6. Assign the correct 'study' type using this function in a ‘for loop'. (5 points)
d$study = NA

# Start the clock!
start <- proc.time()

# Run the loop
for(i in 1:nrow(d)){
##get the 'ith" row, from 1 to the end of data frame "d" (i.e., 503441)
d[i,]$study <- study.fxn(x = d[i,]) #apply the function to the input row of data
}

# Stop the clock!
time <- proc.time() - start; print(time)


##7. Assign the correct 'study' type using this function in 'apply' function. (5 points)
d$study = NA
start<-proc.time()
d$study <- apply(X = d, 1, FUN = study.fxn)
time <- proc.time() - start; print(time)

unique(d$study)

d$study_fac<- factor(x = d$study, levels = c("spatial","eddy","lagrangian"),
                     labels = c("Spatial","Eddy","Lagrangian"))



##8. Generate a histogram of ‘pressure’ values for each region of the 'Spatial' study,
##separating the three region-specific plots into by faceting. Order the facets 
##“west, central, east”

##Plot based on region
r<-ggplot(data = d, aes(x = pressure)) +
  geom_histogram(binwidth = 5, color="black", fill="white") +
  facet_wrap(.~region_fac)
r 

##Plot based on study
s<-ggplot(data = d, aes(x = pressure)) +
  geom_histogram(binwidth = 5, color="black", fill="white") +
  facet_wrap(.~study_fac)
s

##9. Calculate the mean and two standard deviations of water temperature for shallow and mid-depth
##tows in the western, central, and eastern regions if the 'Spatial' study. (5 points)
##a. One extra point for using the piping syntax. (1 point)

t=d%>%group_by(region)%>%summarise(avg.tempC = mean(temp, na.rm = T),
                                   sd1.tempC = sd(d[d$tow=='s',"temp"], na.rm = T),
                                   sd2.tempC = sd(d[d$tow=='m',"temp"], na.rm = T))
t

##10. Using a 'for loop', convert the standard deviation of water temperature (currently in Celsius) to
##degrees Fahrenheit and Kelvin. (5 points)

sd1.tempC = sd(d[d$tow=='s',"temp"], na.rm = T)
sd2.tempC = sd(d[d$tow=='m',"temp"], na.rm = T)

t$tempF<- NA
t$tempK<- NA

for(i in 1:nrow(t)){
t[i,]$tempF <- t[i,]$sd1.tempC * (9/5) + 32
t[i,]$tempK <- t[i,]$sd1.tempC + 273.15
}

for(i in 1:nrow(t)){
t[i,]$tempF <- t[i,]$sd2.tempC * (9/5) + 32
t[i,]$tempK <- t[i,]$sd2.tempC + 273.15
}

##11. Melt the data. Keep “region” and “tow’ as the id.variables. (5 points)
library(reshape2)

m.vars=c("temp")
measure.vars=c("tempF","tempK")
id.vars = c("region","tow")

dm <- melt(d, id.vars=id.vars, measure.vars=m.vars)
dm

Melt1 = melt(sd1.tempC, id.vars=c("region","tow"), measure.vars=m.vars)
Melt2 = melt(sd2.tempC, id.vars=c("region","tow"), measure.vars=m.vars)

##12. Generate a bar plots showing the 2 standard deviation temperatures in Celsius, 
##Fahrenheit, and Kelvin degrees. (5 points)

bar1=ggplot(dm,aes(x=variable, y= value)) +geom_bar(stat = "identity", 
                            position = "dodge") + facet_grid(.~region)
bar1


##14. Plot the values on a log-10 scale. (2.5 points)
bar.2=ggplot(dm,aes(x=variable, y= value)) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(.~region) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

load("ost2014_phy_t.Robj")
vars <- c("temp", "salinity", "sw.density", "chl.ug.l")
ddply(.data = phy_t, .variables = "transect.id", function(x){
x <- na.omit(x)
x$depth_round <- round(x$depth, digits = 1)
dm <- melt(x, id.vars=c("dateTime", "depth_round"), measure.vars=vars)
png(file = paste0("plots/",unique(x$transect.id), ".png"), width = 8.5,
height = 14, units = "in", res = 300)
plot()
dev.off()

}, .progress = "text")  
