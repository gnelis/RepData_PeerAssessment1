library(dplyr)
library(data.table)
library(ggplot2)
library(timeDate)

unzip('activity.zip')
dt<-fread('activity.csv')

per_day<-dt %>% group_by(date)%>% filter(!is.na(steps)) %>% summarise(total = sum(steps))
ggplot(data = per_day, aes(total))+
    geom_histogram(bins = 20)+
    labs(title = "Distributions of Steps per day", x="Total Steps", y = 'Count')

mean_value<-mean(per_day$total)
median_value<-median(per_day$total)


by_interval<-dt%>%
    filter(!is.na(steps))%>%
    group_by(interval)%>%
    summarise(avg_steps=mean(steps))

ggplot(data = by_interval, aes(x=interval, y=avg_steps))+
    geom_line()+
    labs(title = "Average Number of steps per interval", x="Time-interval (minutes)", 
         y = 'Average Steps')

max_interval<-by_interval$interval[which.max(by_interval$avg_steps)]

##Imputation
imputed_data<-with(by_interval,
                    avg_steps[match(dt$interval[is.na(dt$steps)],interval)])

dt_imputed<-dt
dt_imputed$steps[is.na(dt_imputed$steps)]<-imputed_data

per_day_imp<-dt_imputed %>% group_by(date) %>% summarise(total = sum(steps))

ggplot(data = per_day_imp, aes(total))+
         geom_histogram(bins = 20)+
         labs(title = "Distributions of Steps per day", x="Total Steps", y = 'Count')

mean_value_imp<-mean(per_day_imp$total)
median_value_imp<-median(per_day_imp$total)

dt_imputed$Is_Wday<-factor(isWeekday(dt$date, wday=1:5),
                           levels=c(TRUE, FALSE), 
                           labels=c('Weekday','Weekend')) 

by_interval_imp<-dt_imputed%>%
    group_by(Is_Wday,interval)%>%
    summarise(avg_steps=mean(steps))

ggplot(data=by_interval_imp,aes(x=interval,y=avg_steps))+
    geom_line()+
    facet_grid(Is_Wday~.)+
    labs(title = "Average Number of steps per interval", x="Time-interval (minutes)", 
         y = 'Average Steps')
        
