#' Plot Zipf's Law for Article Data
#'
#' This function applies Zipf's law to a given dataset of articles. It tokenizes the words in the articles,
#' performs a count and a TF-IDF (Term Frequency-Inverse Document Frequency) calculation, then uses linear
#' regression to analyze the relationship between the ranks of words and their frequencies. The result is
#' a log-log plot illustrating Zipf's law, with different colors for each journal.
#'
#' @param article_data A dataframe containing the articles and their metadata.
#' @param articles The column name in `article_data` that contains the article text.
#' @param journal The column name in `article_data` that contains the journal names.
#' @return A list containing the processed article data, regression analysis summary, and the ggplot object.
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
#' plot_zipfs_law(article_data ,"articles","journal")
plot_zipfs_law <- function(article_data, articles, journal) {
  library(dplyr)
  library(tm)
  library(ggplot2)
  library(tidytext)
  library(scales)
  library(tidyr)
  library(forcats)
  library(tidyverse)

  # Tokenize the words
  zipf_data <- article_data %>%
    mutate(articles = as.character(articles)) %>%
    unnest_tokens(word, articles) %>%
    count(journal, word) %>%
    bind_tf_idf(word, journal, n)

  # Rank calculation
  zipf_data <- zipf_data %>%
    group_by(journal) %>%
    arrange(desc(tf)) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  # Linear regression analysis
  regression_results <- lm(log(tf) ~ log(rank), data = zipf_data)

  # Plot log-log graph with regression line and different colors for each journal
  plot <- ggplot(zipf_data, aes(x = rank, y = tf, color = journal)) +
    geom_line(size = 1.5) +
    geom_smooth(method = "lm", size = 1, se = FALSE, color = "black", aes(group = 1)) +
    scale_x_log10() +
    scale_y_log10(labels = scales::label_number()) +
    labs(title = "Zipf's Law for Ukraine war articles in 2022 with Linear Regression",
         x = "Word Rank",
         y = "Term Frequency (linear regression)") +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(list(article_data = zipf_data, regression = summary(regression_results), plot = plot))
}
