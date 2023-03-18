library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Loading Data and looking at few rows
video_stats <- read.delim("videos-stats.csv", sep=",", strip.white=T)
head(video_stats)

# Dimensions of dataset
dim(video_stats)
# This means that we have 1881 observations and 8 features (columns)

# Structure of dataset
str(video_stats)


# Summary of all columns
summary(video_stats)

# NULL values check
video_stats %>%
  summarise_all(~ sum(is.na(.)))

#There seem to be two NAs in Likes, Comments, and Views. 
#Since we cannot impute their values from the information provided, 
#we are going to drop them as they do not make up a huge portion 
#of the dataset

video_stats <- video_stats %>%
  drop_na()

## Data Wrangling
#Since we are interested in characteristics of a viral video, 
#let’s engineer some new features related to likes, comments, and 
#views.

# LikesPer1k: number of likes per 1000 views
# CommentsPer1k: number of comments per 1000 views
# TitleLen: number of characters in the title

video_stats <- video_stats %>%
  mutate(LikesPer1k = round(Likes/(Views/1000), 2),
         CommentsPer1k = round(Comments/(Views/1000), 2),
         TitleLen = nchar(Title))

# Adding Publication yearcolumn
video_stats <- video_stats %>%
  mutate(PubYear = as.factor(substr(Published.At,1,4)),
         Published.At = as.POSIXct(Published.At, format="%Y-%m-%d"))


## Static plots

# Bar Chart
video_stats %>%
  ggplot(aes(x=PubYear)) +
  geom_bar(fill="#765add") +
  theme_minimal() +
  labs(title = "Number of videos by year", x = "Publication Year", y = "Count")

# Histogram of title length
video_stats %>%
  ggplot(aes(x=TitleLen)) +
  geom_histogram(fill="#765add", bins=30) +
  theme_minimal() +
  labs(title = "Distribution of title length", x = "Title Length (char)", y = "Frequency")

## Interactive plots

# Line Chart
plot1 <- video_stats %>%
  # get the total comments per keyword per year; divide by 1000 to change scale
  group_by(PubYear, Keyword) %>%
  summarize(total_comments = sum(Comments)/1000) %>%
  # ggplot colored by keywords
  ggplot(aes(x=PubYear, y=total_comments, color=Keyword))+
  # group = 1 specifies we want 1 line per keyword
  geom_line(aes(group=1)) +
  geom_point(size=0.5,alpha=0.5) +
  ylab("Comment Count") +
  xlab("Published Year") +
  labs(title="Total Comments by Category Overtime (by 1k)")+
  theme_minimal()
# convert it into a plotly graph
ggplotly(plot1)


# Checking for trend of creating shorter titles for all content overtime.
plot2 <- video_stats %>%
  # average title length per keyword per year
  group_by(PubYear, Keyword) %>%
  summarize(avg_len = mean(TitleLen)) %>%
  # ggplot colored by keywords
  ggplot(aes(x=PubYear, y=avg_len, color=Keyword))+
  geom_line(aes(group=1)) +
  geom_point(size=0.5,alpha=0.5) +
  ylab("Avg Title Length (char)") +
  xlab("Published Year") +
  labs(title="Avg Title Length by Category Overtime (by 1k)")+
  theme_minimal()
# converting into a plotly graph
ggplotly(plot2)


## Looking for correlation between Likes & Comments per 1k Views
# Customizing popup information for interactive chart
video_stats %>%
  # Variables we want to include
  plot_ly(x=~LikesPer1k, y=~CommentsPer1k, color=~Keyword, type="scatter", mode="markers",
          size=~Views, sizes=c(5,70),
          # Adding markers for each point and specify information to display on hover
          marker=list(sizemode="diameter", opacity=0.5), hoverinfo="text",
          # Customize hover text
          text=~paste(
            paste0("Likes per 1k views: ", LikesPer1k),
            paste0("Comments per 1k views: ", CommentsPer1k),
            paste0("Views (100k): ", round(Views/100000, 2)),
            paste0("Keyword (Category): ", Keyword),
            sep="<br>")) %>%
  # axes label
  layout(title = 'Likes VS Comments per 1k Views',
         xaxis = list(title = 'Likes per 1k'),
         yaxis = list(title = 'Comments per 1k'),
         legend = list(title=list(text='<b> Keyword </b>')))


