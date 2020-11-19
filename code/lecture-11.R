# packages
library("here")

# load in text data
textdata <- read.csv(here::here("data", "sotu.csv"), header = TRUE, sep = ";", encoding = "UTF-8")

View(textdata)

library("quanteda")

sotu_corpus <- corpus(textdata$text, docnames = textdata$doc_id)
# have a look on the new corpus object
summary(sotu_corpus)
# getting a single text documents content
cat(texts(sotu_corpus[1]))


# text classification tutorial
# https://www.r-bloggers.com/2018/12/text-classification-with-tidy-data-principles/

# Austen vs. Wells
library(tidyverse)
library(gutenbergr)
library(dplyr)
library(ggplot2)
library(tidyr)

# we will be looking at Wells' War of the Worlds and Austen's Pride and Prejudice
titles <- c(
  "The War of the Worlds",
  "Pride and Prejudice"
)

# we want to create a tibble that breaks up the book into lines
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title") %>%
  dplyr::mutate(document = row_number())

# look at how the tibble looks like
books

# then we want to break it into even smaller pieces...words
library(tidytext)
tidy_books <- books %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>% # we are only keeping words that occur more than 10 times
  ungroup()

# see how it looks like
tidy_books

# exploratory analysis...see which words appear the most often in these books
tidy_books %>%
  dplyr::count(title, word, sort = TRUE) %>%
  anti_join(get_stopwords()) %>%
  group_by(title) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, title), n,
             fill = title
  )) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~title, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, y = "Word count",
    title = "Most frequent words after removing stop words",
    subtitle = "Words like 'said' occupy similar ranks but other words are quite different"
  )

# now you want to create a model that can tell you whether a line of text comes from P&P or WoW
library(rsample)
books_split <- books %>%
  dplyr::select(document) %>%
  initial_split()
train_data <- training(books_split)
test_data <- testing(books_split)

sparse_words <- tidy_books %>%
  dplyr::count(document, word) %>%
  inner_join(train_data) %>%
  cast_sparse(document, word, n)

class(sparse_words)

# text dimensions
dim(sparse_words)
# we have 12,064 observations and 1,652 unique features in the training data

word_rownames <- as.integer(rownames(sparse_words))
books_joined <- tibble(document = word_rownames) %>%
  left_join(books %>%
              dplyr::select(document, title))

# this tells you that, in your training data, which lines are associated with which book
View(books_joined)

# LASSO regularization

library(glmnet)
library(doMC)
registerDoMC(cores = 8)
is_jane <- books_joined$title == "Pride and Prejudice"
model <- cv.glmnet(sparse_words, is_jane,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)

# what is driving our model?

library(broom)
coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Coefficients that increase/decrease probability the most",
    subtitle = "A document mentioning Martians is unlikely to be written by Jane Austen"
  )

# let's consider the test data
intercept <- coefs %>%
  dplyr::filter(term == "(Intercept)") %>%
  pull(estimate)

classifications <- tidy_books %>%
  inner_join(test_data) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(document) %>%
  dplyr::summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score))

# view table
classifications

# we can graph this
library(yardstick)
classifications
comment_classes <- classifications %>%
  left_join(books %>%
              dplyr::select(title, document), by = "document") %>%
  mutate(title = as.factor(title))
comment_classes %>%
  roc_curve(title, probability) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "midnightblue",
    size = 1.5
  ) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) +
  labs(
    title = "ROC curve for text classification using regularized regression",
    subtitle = "Predicting whether text was written by Jane Austen or H.G. Wells"
  )
# a good resource on ROC curves:
# https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5
# sensitivity: true positive rate
# specificity: false positive rate

# how much % is in the area under the curve (AUC)
comment_classes %>%
  roc_auc(title, probability)

# confusion matrix

comment_classes %>%
  dplyr::mutate(
    prediction = case_when(
      probability > 0.5 ~ "Pride and Prejudice",
      TRUE ~ "The War of the Worlds"
    ),
    prediction = as.factor(prediction)
  ) %>%
  conf_mat(title, prediction)

# misclassified
# lines that were incorrectly predicted to be austen when it was wells
comment_classes %>%
  filter(
    probability > .8,
    title == "The War of the Worlds"
  ) %>%
  sample_n(10) %>%
  inner_join(books %>%
               dplyr::select(document, text)) %>%
  dplyr::select(probability, text)


# topic modeling

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 10, control = list(seed = 1234))
ap_lda

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))
