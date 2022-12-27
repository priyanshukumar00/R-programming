install.packages("hrbrthemes")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(viridisLite)
library(hrbrthemes)
library(tidyr)

covid <- read.csv("C:/Users/91999/Downloads/WHO-COVID-19-global-data.csv",sep=",", header=T)
dim(covid)
summary(covid)
view(covid)

#Albania and India
countries <- covid %>%
  filter(Country == "Albania" | Country == "India")
view(countries)

#---(1) we are going to do Covid analysis of two different countries on the basis of cases---#

# CUMULATIVE CASES
# we use Histogram plot to check cumulative cases in selected countries....

ggplot(countries, aes(x=Cumulative_cases, fill=Country)) +
  geom_histogram(binwidth = 2000000) +
  ggtitle("Histogram Plot 
Cumulative cases of Albania and India")+
  theme_grey()


# Now using Boxplot we can check cumulative cases in selected countries....

ggplot(countries,aes(x=Country, y=Cumulative_cases, fill=Cumulative_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative cases of Albania and India") +
  xlab("")

#Now using lineplot we can check cumulative cases in selected countries....

ggplot(countries, aes(x=ï..Date_reported, y=Cumulative_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()


# NEW CASES
# we use Histogram plot to check New cases in selected countries....

ggplot(countries, aes(x=New_cases, fill=Country)) +
  geom_histogram(binwidth = 15000) +
  ggtitle("Histogram Plot 
New cases of Albania and India")+
  theme_bw()+
  xlab("New cases")


# Now using Boxplot we can check New cases in selected countries....

ggplot(countries,aes(x=Country, y=New_cases, fill=New_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New cases of Albania and India") +
  xlab("")

#Now using lineplot we can check New cases in selected countries....

ggplot(countries, aes(x=ï..Date_reported, y=New_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()

# CUMULATIVE DEATHS
# we use Histogram plot to check Cumulative deaths in selected countries....

ggplot(countries, aes(x=Cumulative_deaths, fill=Country)) +
  geom_histogram(binwidth = 15000) +
  ggtitle("Histogram Plot 
Cumulative deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check Cumulative deaths in selected countries....

ggplot(countries,aes(x=Country, y=Cumulative_deaths, fill=Cumulative_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check Cumulative deaths in selected countries....

ggplot(countries, aes(x=ï..Date_reported, y=Cumulative_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative deaths")+
  ggtitle("Lineplot
Cumulative deaths of Albania and India")+
  theme_bw()

# NEW DEATHS
# we use Histogram plot to check new deaths in selected countries....

ggplot(countries, aes(x=New_deaths, fill=Country)) +
  geom_histogram(binwidth = 300) +
  ggtitle("Histogram Plot 
New deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check new deaths in selected countries....

ggplot(countries,aes(x=Country, y=New_deaths, fill=New_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check new deaths in selected countries....

ggplot(countries, aes(x=ï..Date_reported, y=New_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New deaths")+
  ggtitle("Lineplot
New deaths of Albania and India")+
  theme_bw()

#--(2) Now we are going to do Covid analysis with year of two different countries ......

year <- countries$ï..Date_reported
year

countries2020 <- countries %>%
  filter(year < '2021-01-01')
view(countries2020)

countries2021 <- countries %>%
  filter(year > '2021-1-1' & year < '2022-01-01')
view(countries2021)

countries2022 <- countries %>%
  filter(year > '2022-01-01')
view(countries2022)

# CUMULATIVE CASES
# we use Histogram plot to check cumulative cases in selected countries with specific year....

ggplot(countries2020, aes(x=Cumulative_cases, fill=Country)) +
  geom_histogram(binwidth = 400000) +
  ggtitle("Histogram Plot 
Cumulative cases of Albania and India")+
  theme_grey()


# Now using Boxplot we can check cumulative cases in selected countries....

ggplot(countries2020,aes(x=Country, y=Cumulative_cases, fill=Cumulative_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative cases of Albania and India") +
  xlab("")

#Now using lineplot we can check cumulative cases in selected countries....

ggplot(countries2020, aes(x=ï..Date_reported, y=Cumulative_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()


# NEW CASES
# we use Histogram plot to check New cases in selected countries....

ggplot(countries2020, aes(x=New_cases, fill=Country)) +
  geom_histogram(binwidth = 4000) +
  ggtitle("Histogram Plot 
New cases of Albania and India")+
  theme_bw()+
  xlab("New cases")


# Now using Boxplot we can check New cases in selected countries....

ggplot(countries2020,aes(x=Country, y=New_cases, fill=New_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New cases of Albania and India") +
  xlab("")

#Now using lineplot we can check New cases in selected countries....

ggplot(countries2020, aes(x=ï..Date_reported, y=New_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()

# CUMULATIVE DEATHS
# we use Histogram plot to check Cumulative deaths in selected countries....

ggplot(countries2020, aes(x=Cumulative_deaths, fill=Country)) +
  geom_histogram(binwidth = 10000) +
  ggtitle("Histogram Plot 
Cumulative deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check Cumulative deaths in selected countries....

ggplot(countries2020,aes(x=Country, y=Cumulative_deaths, fill=Cumulative_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check Cumulative deaths in selected countries....

ggplot(countries2020, aes(x=ï..Date_reported, y=Cumulative_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative deaths")+
  ggtitle("Lineplot
Cumulative deaths of Albania and India")+
  theme_bw()

# NEW DEATHS
# we use Histogram plot to check new deaths in selected countries....

ggplot(countries2020, aes(x=New_deaths, fill=Country)) +
  geom_histogram(binwidth = 100) +
  ggtitle("Histogram Plot 
New deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check new deaths in selected countries....

ggplot(countries2020,aes(x=Country, y=New_deaths, fill=New_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check new deaths in selected countries....

ggplot(countries2020, aes(x=ï..Date_reported, y=New_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New deaths")+
  ggtitle("Lineplot
New deaths of Albania and India")+
  theme_bw()

# year = 2021
# CUMULATIVE CASES
# we use Histogram plot to check cumulative cases in selected countries with specific year....

ggplot(countries2021, aes(x=Cumulative_cases, fill=Country)) +
  geom_histogram(binwidth = 10000000) +
  ggtitle("Histogram Plot 
Cumulative cases of Albania and India")+
  theme_grey()


# Now using Boxplot we can check cumulative cases in selected countries....

ggplot(countries2021,aes(x=Country, y=Cumulative_cases, fill=Cumulative_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative cases of Albania and India") +
  xlab("")

#Now using lineplot we can check cumulative cases in selected countries....

ggplot(countries2021, aes(x=ï..Date_reported, y=Cumulative_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()


# NEW CASES
# we use Histogram plot to check New cases in selected countries....

ggplot(countries2021, aes(x=New_cases, fill=Country)) +
  geom_histogram(binwidth = 4000) +
  ggtitle("Histogram Plot 
New cases of Albania and India")+
  theme_bw()+
  xlab("New cases")


# Now using Boxplot we can check New cases in selected countries....

ggplot(countries2021,aes(x=Country, y=New_cases, fill=New_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New cases of Albania and India") +
  xlab("")

#Now using lineplot we can check New cases in selected countries....

ggplot(countries2021, aes(x=ï..Date_reported, y=New_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()

# CUMULATIVE DEATHS
# we use Histogram plot to check Cumulative deaths in selected countries....

ggplot(countries2021, aes(x=Cumulative_deaths, fill=Country)) +
  geom_histogram(binwidth = 100000) +
  ggtitle("Histogram Plot 
Cumulative deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check Cumulative deaths in selected countries....

ggplot(countries2021,aes(x=Country, y=Cumulative_deaths, fill=Cumulative_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check Cumulative deaths in selected countries....

ggplot(countries2021, aes(x=ï..Date_reported, y=Cumulative_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative deaths")+
  ggtitle("Lineplot
Cumulative deaths of Albania and India")+
  theme_bw()

# NEW DEATHS
# we use Histogram plot to check new deaths in selected countries....

ggplot(countries2021, aes(x=New_deaths, fill=Country)) +
  geom_histogram(binwidth = 100) +
  ggtitle("Histogram Plot 
New deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check new deaths in selected countries....

ggplot(countries2021,aes(x=Country, y=New_deaths, fill=New_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check new deaths in selected countries....

ggplot(countries2021, aes(x=ï..Date_reported, y=New_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New deaths")+
  ggtitle("Lineplot
New deaths of Albania and India")+
  theme_bw()


#year = 2022
# CUMULATIVE CASES
# we use Histogram plot to check cumulative cases in selected countries with specific year....

ggplot(countries2022, aes(x=Cumulative_cases, fill=Country)) +
  geom_histogram(binwidth = 10000000) +
  ggtitle("Histogram Plot 
Cumulative cases of Albania and India")+
  theme_grey()


# Now using Boxplot we can check cumulative cases in selected countries....

ggplot(countries2022,aes(x=Country, y=Cumulative_cases, fill=Cumulative_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative cases of Albania and India") +
  xlab("")

#Now using lineplot we can check cumulative cases in selected countries....

ggplot(countries2022, aes(x=ï..Date_reported, y=Cumulative_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()


# NEW CASES
# we use Histogram plot to check New cases in selected countries....

ggplot(countries2022, aes(x=New_cases, fill=Country)) +
  geom_histogram(binwidth = 40000) +
  ggtitle("Histogram Plot 
New cases of Albania and India")+
  theme_bw()+
  xlab("New cases")


# Now using Boxplot we can check New cases in selected countries....

ggplot(countries2022,aes(x=Country, y=New_cases, fill=New_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New cases of Albania and India") +
  xlab("")

#Now using lineplot we can check New cases in selected countries....

ggplot(countries2022, aes(x=ï..Date_reported, y=New_cases, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New cases")+
  ggtitle("Lineplot
Cumulative cases of Albania and India")+
  theme_bw()

# CUMULATIVE DEATHS
# we use Histogram plot to check Cumulative deaths in selected countries....

ggplot(countries2022, aes(x=Cumulative_deaths, fill=Country)) +
  geom_histogram(binwidth = 100000) +
  ggtitle("Histogram Plot 
Cumulative deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check Cumulative deaths in selected countries....

ggplot(countries2022,aes(x=Country, y=Cumulative_deaths, fill=Cumulative_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
Cumulative deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check Cumulative deaths in selected countries....

ggplot(countries2022, aes(x=ï..Date_reported, y=Cumulative_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("Cumulative deaths")+
  ggtitle("Lineplot
Cumulative deaths of Albania and India")+
  theme_bw()

# NEW DEATHS
# we use Histogram plot to check new deaths in selected countries....

ggplot(countries2022, aes(x=New_deaths, fill=Country)) +
  geom_histogram(binwidth = 200) +
  ggtitle("Histogram Plot 
New deaths of Albania and India")+
  theme_bw()+
  xlab("Cumulative deaths")


# Now using Boxplot we can check new deaths in selected countries....

ggplot(countries2022,aes(x=Country, y=New_deaths, fill=New_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")

#Now using lineplot we can check new deaths in selected countries....

ggplot(countries2022, aes(x=ï..Date_reported, y=New_deaths, group=1,color=Country)) +
  geom_line(size=0.5)+
  geom_point()+
  xlab("Year")+
  ylab("New deaths")+
  ggtitle("Lineplot
New deaths of Albania and India")+
  theme_bw()

######------------------------ (3)   Region Specific    ---------------------------########

ggplot(covid, aes(x=WHO_region, fill=WHO_region)) + geom_bar() + 
  coord_flip()+
  theme_ipsum()

ggplot(covid, aes(x=WHO_region, fill=WHO_region)) + geom_bar() + 
  coord_polar()+
  theme_ipsum()

##---------------------------- (4)   comparing with china    ----------------------------##

IndiachinAlbania<-covid %>% 
  filter(Country == 'India'| Country == 'China'| Country == 'Albania')
View(IndiachinAlbania)


#cumulative cases

ggplot(IndiachinAlbania,aes(x=Country, y=Cumulative_cases, fill=Cumulative_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")

#cumulative deaths

ggplot(IndiachinAlbania,aes(x=Country, y=Cumulative_deaths, fill=Cumulative_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")

#New cases

ggplot(IndiachinAlbania,aes(x=Country, y=New_cases, fill=New_cases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New cases of Albania and India") +
  xlab("")

#New deaths

ggplot(IndiachinAlbania,aes(x=Country, y=New_deaths, fill=New_deaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot
New deaths of Albania and India") +
  xlab("")






