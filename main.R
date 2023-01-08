library(httr)
library(json)
library(png)
library(ggplot2)

url <- "http://api.weatherapi.com/v1/history.json"

secrets =jsonlite::read_json("secrets.json")

days=seq(as.Date("2022/12/01"), by = "day", length.out = 30)


min_temp_c_vector =c()
max_temp_c_vector <- c()
avg_temp_c_vector <- c()
condition_vector = c()

for (i in 1:length(days)){
  queryString <- list(
    q = "West Virginia",
    dt = base::toString(days[i]),
    lang = "en"
  )
  message = paste("Processing",days[i],sep=" ")
  print(message)
  response = httr::GET(url=url,query=queryString,
                       httr::add_headers(c("key"=base::toString(secrets["weather"],
                                                                "Content-Type"="application/json"))))
  print(response)
  data=jsonlite::fromJSON(httr::content(response,"text"))
  
  minimum_temperature_celsius=data$forecast$forecastday$day$mintemp_c
  maximum_temperature_celsius=data$forecast$forecastday$day$maxtemp_c
  average_temperature_celsius=data$forecast$forecastday$day$avgtemp_c
  condition=data$forecast$forecastday$day$condition$text
  
  min_temp_c_vector=append(min_temp_c_vector,minimum_temperature_celsius)
  max_temp_c_vector=append(max_temp_c_vector,maximum_temperature_celsius)
  avg_temp_c_vector=append(avg_temp_c_vector,average_temperature_celsius)
  condition_vector=append(condition_vector,condition)
}



#Plotting vectors 
df <- data.frame(min_temp_c_vector,max_temp_c_vector,avg_temp_c_vector,condition,days)
colors <- c("Avg Temperature" = "blue", "Max Temperature" = "red", "Min Temperature" = "orange")
ggplot(df, aes(days)) +                    # basic graphical object
  geom_line(aes(y=min_temp_c_vector, color="Min Temperature")) +  # first layer
  geom_line(aes(y=max_temp_c_vector, color="Max Temperature"))+  # second layer
  geom_line(aes(y=avg_temp_c_vector,color="Avg Temperature"))+
  ggtitle("Temperature in West Virginia during December")+
  labs(x = "Days",
       y = "Temperature in celsius",
       color = "Metrics") +
  scale_color_manual(values = colors)


