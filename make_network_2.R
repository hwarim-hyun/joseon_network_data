library('readxl')
library('tidyverse')
library('tidygraph')
library('ggraph')
library('igraph')

setwd("C:/Users/hp/Documents/SNU/자유전공학부/족보 빅데이터")
raw <- readxl::read_excel(path = "고려중기 정치세력 및 경원이씨 해주최씨 개성왕씨 1090-1170.xlsx", sheet = "고려중기 정치세력", col_names = TRUE)

class(raw) #"tbl_df"     "tbl"        "data.frame"
graph1 <- as_tbl_graph(raw)
class(graph1)
graph1

#graph1 display
ggraph(graph1, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

#reduction1
graph2 <- delete.vertices(graph1, 
                  V(graph1)[ degree(graph1) == 1])
ggraph(graph2, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

#reduction2
graph3 <- delete.vertices(graph2, 
                          V(graph2)[ degree(graph2) == 1])
ggraph(graph3, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

#reduction3
graph4 <- delete.vertices(graph3, 
                          V(graph3)[degree(graph3) == 1 | degree(graph3) == 0])
ggraph(graph4, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

#reduction4
graph5 <- delete.vertices(graph4, 
                          V(graph4)[ degree(graph4) == 1])
ggraph(graph5, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

#reduction5
graph6 <- delete.vertices(graph5, 
                          V(graph5)[ degree(graph5) == 1])
ggraph(graph6, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

#reduction6
graph7 <- delete.vertices(graph6, 
                          V(graph6)[degree(graph6) == 1 | degree(graph6) == 0])
vertex_attr(graph7, "label") <- V(graph7)$name
ggraph(graph7, layout = 'kk') + geom_node_text(mapping = aes(label = V(graph7)$name)) + 
  geom_edge_link(aes(color = factor(type)), arrow = arrow(angle = 30, length = unit(0.08, "inches"),
                                                          ends = "last", type = "open"))
