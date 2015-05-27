# Plot as a network
library(networkD3)
#opt_results %>% group_by(orig_cluster, dest_cluster) %>%
#  dplyr::summarise(n = n()) %>% as.data.frame(.) %>% select(as.integer(orig_cluster), as.integer(dest_cluster))

links <- opt_results %>% group_by(orig_cluster, dest_cluster) %>%
  dplyr::summarise(n = n()) %>% as.data.frame(.) %>%
  transform(orig_cluster = as.integer(orig_cluster), dest_cluster = as.integer(dest_cluster)) %>%
  arrange(orig_cluster, dest_cluster)

nodes <- data.frame(name = as.factor(1:6), group = 1:6)

forceNetwork(Links = links, Nodes = nodes,
             Source = "orig_cluster", Target = "dest_cluster",
             Value = "n", NodeID = "name",
             Group = "group", opacity = 0.4)


data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)