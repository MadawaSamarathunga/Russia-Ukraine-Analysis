install.packages("dplyr")
install.packages("readr")
install.packages("tm")
install.packages("lubridate")
install.packages("scales")

library(readr)
library(dplyr)
library(tm)
library(lubridate)#1st one
library(ggplot2)
library(tidytext)
library(scales)
library(tidyr)
library(forcats) #2nd on#tf-df



# Read the CSV files
guardian_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Coursework Submission/Assesment/Guardians and NYT article analysis/Guardians_and_NYT_article_analysis/Guardians_Russia_Ukraine.csv")
nyt_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Coursework Submission/Assesment/Guardians and NYT article analysis/Guardians_and_NYT_article_analysis/NYT_Russia_Ukraine.csv")

# Add a new column to each DataFrame for the journal source
guardian_df <- mutate(guardian_df, journal = "The Guardian")
nyt_df <- mutate(nyt_df, journal = "The New York Times")

# Combine the two DataFrames
guardian_nyt_df <- bind_rows(guardian_df, nyt_df)

# Export the combined DataFrame to a new CSV file
write_csv(guardian_nyt_df, "D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Coursework Submission/Assesment/Guardians and NYT article analysis/Guardians_and_NYT_article_analysis/combined_Russia_Ukraine_articles.csv")

#guardian_nyt_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Coursework Submission/Assesment/Guardians and NYT article analysis/Guardians_and_NYT_article_analysis/NYT_Russia_Ukraine.csv")


tokenize_remove_stopwords <- function(text){
  articles_tokenized = article_data %>%
    unnest_tokens(word, articles) %>%
    anti_join(stop_words)
}
guardian_nyt_df$articles_tokenized <- lapply(guardian_nyt_df$articles, tokenize_remove_stopwords)



#####################################################


#word_frequency_plot(specific_words, "invented_dataset.csv")



  # Read in the data using read_csv from the readr package
  article_data = read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Coursework Submission/Assesment/Guardians and NYT article analysis/Guardians_and_NYT_article_analysis/combined_Russia_Ukraine_articles.csv")
  # Tokenize the articles, remove stopwords
  articles_tokenized = article_data %>%
    unnest_tokens(word, articles) %>%
    anti_join(stop_words)
  
  specific_words <- c("civilians", "country","putin","zelensky","biden","nato","russia","ukraine","war", "weapons")
  
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
  ggplot(proportions_filtered, aes(x = published, y = p, colour = word, group = word)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "black") +
    geom_smooth(method = "loess", se = TRUE, formula = y ~ x, linetype = "solid", level = 0.95) +
    labs(y = "Percentage of words in Ukraine war articles in 2022", x = "Article date") +
    facet_wrap(~ word, scales = "free_y") +  # Allow each facet to have its own y-axis scale
    scale_x_datetime(labels = scales::date_format(" %b "),
                     limits = c(min(proportions$published) - 1, max(proportions$published) + 1)) +
    scale_y_continuous(labels = label_percent(scale = 100)) +  # Format y-axis as percentage
    theme(legend.position = "none")
  


########################################


  
article_data = read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Coursework Submission/Assesment/Guardians and NYT article analysis/Guardians_and_NYT_article_analysis/combined_Russia_Ukraine_articles.csv")
 
  # Tokenize the articles, remove stopwords
  articles_tokenized <- article_data %>%
    unnest_tokens(word, articles) %>%
    anti_join(stop_words)
  # Calculate tf-idf
  tfidf_data <- articles_tokenized %>%
    count(journal, word) %>%
    bind_tf_idf(word, journal, n)
  
  
  # Extract top 10 tf-idf words for each journal
  top_tfidf_words <- tfidf_data %>%
    group_by(journal) %>%
    top_n(10, wt = tf_idf) %>%
    ungroup() %>%
    arrange(journal, desc(tf_idf)) %>%
    mutate(word = fct_inorder(word))  # Set factor levels in the order they appear
  
  # Plot the top tf-idf words
  ggplot(top_tfidf_words, aes(x = reorder(word, tf_idf), y = tf_idf, fill = journal)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~journal, scales = "free") +
    coord_flip() +
    labs(title = "Highest tf-idf Words in Ukraine war articles in 2022", x = "", y = "tf−idf index") +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 8))
         
  

 
 
