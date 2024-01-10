install.packages("dplyr")
install.packages("readr")
install.packages("tm")
install.packages("lubridate")
install.packages("scales")
install.packages("gridExtra")


library(readr)
library(dplyr)
library(tm)
library(ggplot2)
library(tidytext)
library(scales)
library(tidyr)
library(forcats)
library(tidyverse)
library(lubridate)
library(gridExtra)




# Read the CSV files
guardian_df <- read_csv("Guardians_Russia_Ukraine.csv")
nyt_df <- read_csv("NYT_Russia_Ukraine.csv")

# Add a new column to each DataFrame for the journal source
guardian_df <- mutate(guardian_df, journal = "The Guardian")
nyt_df <- mutate(nyt_df, journal = "The New York Times")

# Combine the two DataFrames
article_data <- bind_rows(guardian_df, nyt_df)

# Export the combined DataFrame to a new CSV file
write_csv(article_data, "combined_Russia_Ukraine_articles.csv")

article_data

#################################################
#simple article analysis example
library(tidyverse)
library(lubridate)
library(gridExtra)

analyze_articles <- function(article_data, published, journal) {
 
  
  # Date Analysis: Distribution of articles over time
  article_data$month_year <- floor_date(article_data$published, "month")
  articles_per_month <- article_data %>% 
    count(month_year) %>% 
    arrange(month_year)
  
  # Journal Analysis: Count of articles per journal
  articles_per_journal <- article_data %>% 
    count(journal)
  
  # Creating plots
  
  # Plot for Article Distribution Over Time
  plot1 <- ggplot(articles_per_month, aes(x=month_year, y=n)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    labs(title="Article Distribution Over Time", x="Month", y="Number of Articles")
  
  # Plot for Journal Article Counts
  plot3 <- ggplot(articles_per_journal, aes(x=reorder(journal, n), y=n)) +
    geom_bar(stat="identity", fill="steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title="Articles per Journal", x="Journal", y="Number of Articles")
  
  
  
  grid.arrange(plot1, plot3, ncol=1)
}

analyze_articles(article_data, published, journal)



#####################################################


word_frequency_plot <- function(article_data, articles){

 
  # Tokenize the articles, remove stopwords
  articles_tokenized = article_data %>%
    unnest_tokens(word, articles) %>%
    anti_join(stop_words)
  
  specific_words <- c("civilians", "country","putin","biden","nato","russia","ukraine","war", "weapons")
  
  # Work out percentages for each date using the dplyr package
  proportions = articles_tokenized %>%
    count(published, word) %>%
    group_by(published) %>%
    mutate(p = n / sum(n))
  # Filter results by word(s)
  proportions_filtered = proportions %>%
    filter(word %in% specific_words)
  
  
  # Ensure the words are in the custom order
  proportions_filtered <- proportions %>%
    filter(word %in% specific_words) %>%
    mutate(word = factor(word, levels = specific_words))
  
  # Plot the contents using ggplot2
  plot <- ggplot(proportions_filtered, aes(x = published, y = p, colour = word, group = word)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "black") +
    geom_smooth(method = "loess", se = TRUE, formula = y ~ x, linetype = "solid", level = 0.95) +
    labs(y = "Percentage of words in Ukraine war articles in 2022", x = "Article date") +
    facet_wrap(~ word, scales = "free_y") +  # Allow each facet to have its own y-axis scale
    scale_x_datetime(labels = scales::date_format(" %b "),
                     limits = c(min(proportions$published) - 1, max(proportions$published) + 1)) +
    scale_y_continuous(labels = label_percent(scale = 100)) +  # Format y-axis as percentage
    theme(legend.position = "bottom")
  
  return(plot)
  
}

word_frequency_plot(guardian_nyt_df,specific_words)
 
  
 
#########################
    
    
calculate_and_plot_tfidf <- function(article_data,articles,journal) {
    
   
    
    
    
    # Tokenize the articles, remove stopwords
    tfidf_data <- article_data%>%
      mutate(articles = as.character(articles)) %>%
      unnest_tokens(word, articles) %>%
      anti_join(stop_words, by = "word") %>%
      count(journal, word) %>%
      group_by(journal) %>%
      bind_tf_idf(word, journal, n)
    
    # Extract top 10 tf-idf words for each journal
    top_tfidf_words <- tfidf_data %>%
      group_by(journal) %>%
      slice_max(tf_idf, n = 10, with_ties = TRUE) %>%
      ungroup()
    
    # Reorder words based on tf-idf for plotting
    top_tfidf_words <- top_tfidf_words %>%
      arrange(journal, desc(tf_idf)) %>%
      mutate(word = fct_reorder(word, tf_idf))
    
    # Plot the top tf-idf words
    plot <-ggplot(top_tfidf_words, aes(x = word, y = tf_idf, fill = journal)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~journal, scales = "free") +
      coord_flip() +
      labs(title = "Highest tf-idf Words in Ukraine war articles in 2022", x = "", y = "tf−idf index") +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 8))
    return(plot)
    
}

calculate_and_plot_tfidf(guardian_nyt_df,"articles","journal")
    
########################
    
   
plot_zipfs_law <- function(article_data,articles,journal) { 
    
    # Tokenize the words
    zipf_data <- article_data %>%
      mutate(articles = as.character(articles)) %>%
      unnest_tokens(word, articles) %>%
      count(journal, word) %>%
      bind_tf_idf(word, journal, n)
    
    # rank calculation 
    zipf_data <- zipf_data %>%
      group_by(journal) %>%
      arrange(desc(tf)) %>%
      mutate(rank = row_number()) %>%
      ungroup()
    
    # linear regression analysis
    regression_results <- lm(log(tf) ~ log(rank), data = zipf_data)
    
    # Plot log-log graph with regression line and different colors for each journal
     plot <- ggplot(zipf_data, aes(x = rank, y = tf, color = journal)) +
      geom_line(size = 1.5) + 
      geom_smooth(method = "lm", size = 1, se = FALSE, color = "black", aes(group = 1)) +
      scale_x_log10() +
      scale_y_log10(labels = scales::label_number()) + 
      labs(title = "Zipf's Law for Ukraine war articles in 2022 with Linear Regression",
           x = "World rank",
           y = "Term Frequency(linear regression)") +
      theme_minimal() +
      theme(legend.position = "bottom")
     
    return(list(article_data = zipf_data,regression = summary(regression_results),plot))
}
    
plot_zipfs_law(guardian_nyt_df ,"articles","journal")
    
    
    