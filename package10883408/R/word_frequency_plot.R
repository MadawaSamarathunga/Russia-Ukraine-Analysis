#' Word Frequency Plot
#'
#' Creates a plot of word frequencies over time from a set of articles.
#'
#' This function takes a dataframe containing articles and their metadata and
#' produces a plot showing the frequency of specific words over time. It is
#' particularly useful for analyzing trends in article content.
#'
#' @param article_data A dataframe containing the articles and their metadata.
#' @param articles The column name in `article_data` that contains the article text.
#' @return A ggplot object representing the word frequency plot.
#' @import dplyr
#' @import tm
#' @import ggplot2
#' @import tidytext
#' @import scales
#' @import tidyr
#' @import forcats
#' @import tidyverse
#' @export
#' @examples
#' # Example usage:
#' word_frequency_plot(article_data,specific_words)
word_frequency_plot <- function(article_data, articles) {
  library(dplyr)
  library(tm)
  library(ggplot2)
  library(tidytext)
  library(scales)
  library(tidyr)
  library(forcats)
  library(tidyverse)

  # Tokenize the articles, remove stopwords
  articles_tokenized = article_data %>%
    unnest_tokens(word, articles) %>%
    anti_join(stop_words)

  specific_words <- c("civilians", "country", "putin", "biden", "nato", "russia", "ukraine", "war", "weapons")

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
    facet_wrap(~ word, scales = "free_y") +
    scale_x_datetime(labels = scales::date_format(" %b "),
                     limits = c(min(proportions$published) - 1, max(proportions$published) + 1)) +
    scale_y_continuous(labels = label_percent(scale = 100)) +
    theme(legend.position = "bottom")

  return(plot)
}

