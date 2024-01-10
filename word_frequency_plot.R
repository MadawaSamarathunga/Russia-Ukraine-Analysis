word_frequency_plot <- function(article_data, articles){
  
  
  library(readr)
  library(dplyr)
  library(tm)
  library(ggplot2)
  library(tidytext)
  library(scales)
  library(tidyr)
  library(forcats) 
  
  # Read in the data using read_csv from the readr package
  article_data <- read_csv("combined_Russia_Ukraine_articles.csv")
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