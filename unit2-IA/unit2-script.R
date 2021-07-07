library(tidyverse)
library(igraph)
library(rtweet)
library(tidytext)
library(tidygraph)
library(ggraph)
library(vader)

# summer_rdg <- search_tweets(q = "#summerreading", n=5000, include_rts = FALSE, lang = "en")
# 
# summer_rdg_2 <- rtweet::flatten(summer_rdg)
# 
# write_csv(summer_rdg_2, "data/summer-rdg-tweets.csv")

summer_rdg_2 <- read_csv("data/summer-rdg-tweets.csv")

ties_1 <-  summer_rdg_2 %>%
  rename(sender = screen_name, # rename scree_name to sender
         target = mentions_screen_name) %>% # rename to receiver
  select(sender,
         target,
         created_at,
         text)

ties_2 <- ties_1 %>%
  unnest_tokens(input = target,
                output = receiver,
                to_lower = FALSE) %>%
  relocate(sender, receiver)

ties <- ties_2 %>%
  drop_na(receiver)

actors_1 <- ties_2 %>%
  pivot_longer(cols = sender:receiver, 
               names_to = "nodes",
               values_to = "screen_name")

actors <- actors_1 %>%
  select(screen_name) %>%
  distinct() %>%
  drop_na()

network <- tbl_graph(edges = ties, 
                     nodes = actors)

network

network_1 <- network %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all")) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

network_1

node_measures <- network_1 %>% 
  activate(nodes) %>%
  data.frame()

summary(node_measures)

# network_2 <- network_1 %>%
#   mutate(centrality_closeness(
#     weights = NULL,
#     mode = "out",
#     normalized = FALSE,
#     cutoff = NULL
#   )) %>%
#   mutate(centrality_betweenness(
#     weights = NULL,
#     directed = TRUE,
#     cutoff = NULL,
#     nobigint = TRUE,
#     normalized = FALSE
#   ))
# 1: Problem with `mutate()` input `..1`.
# ℹ At centrality.c:2784 :closeness centrality is not well-defined for disconnected graphs
# ℹ Input `..1` is `centrality_closeness(...)`. 
# 2: In closeness(graph = graph, vids = V(graph), mode = mode, weights = weights,  :
#                   At centrality.c:2784 :closeness centrality is not well-defined for disconnected graphs

ggraph(network_1)
# Error in layout_with_sparse_stress(graph, pivots = pivots, weights = weights,  : 
                                     # only connected graphs are supported.

# ggraph(network_1, layout = "fr")
# ggraph(network_1, layout = "fr") + 
#   geom_node_point() 
ggraph(network_1, layout = "fr") + 
  geom_node_point(aes(size = in_degree, 
                      alpha = out_degree, 
                      colour = degree))
# ggraph(network_1, layout = "fr") + 
#   geom_node_point(aes(size = in_degree, 
#                       alpha = out_degree, 
#                       colour = degree)) +
#   geom_node_text(aes(label = screen_name, 
#                      size = in_degree/2,
#                      alpha = degree),
#                  repel=TRUE)

# ggraph(network_1, layout = "fr") + 
#   geom_node_point(aes(size = in_degree, 
#                       alpha = out_degree, 
#                       colour = degree)) +
#   geom_node_text(aes(label = screen_name, 
#                      size = degree/2,
#                      alpha = degree), 
#                  repel=TRUE) +
#   geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
#                  end_cap = circle(3, 'mm'),
#                  alpha = .3)
# 
# ggraph(network_1, layout = "fr") +
#   geom_node_point(aes(size = in_degree,
#                       alpha = out_degree,
#                       colour = degree)) +
#   geom_node_text(aes(label = screen_name,
#                      size = degree/2,
#                      alpha = degree),
#                  repel=TRUE) +
#   geom_edge_link(arrow = arrow(length = unit(1, 'mm')),
#                  end_cap = circle(3, 'mm'),
#                  alpha = .3) +
#   theme_graph()

# network_3 <- network_2 %>%
#   activate(nodes) %>%
#   mutate(group = group_infomap())
# 
# network_3
# 
# network_3 %>%
#   ggraph(layout = "kk") + 
#   geom_node_point(aes(size = in_degree, 
#                       colour = group)) +
#   geom_node_text(aes(label = screen_name, 
#                      size = degree/2,
#                      alpha = degree), 
#                  repel=TRUE) +
#   geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
#                  end_cap = circle(3, 'mm'),
#                  alpha = .3) + 
#   theme_graph()

summary_vader <- vader_df(ties$text)

summary_vader

tweet_sentiment <-inner_join(summary_vader, 
                             ties,
                             by = "text")

tweet_sentiment

user_sentiment <- tweet_sentiment %>%
  group_by(sender) %>%
  summarise(sentiment = mean(compound))

user_sentiment