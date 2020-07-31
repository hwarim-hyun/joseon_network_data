install.packages('readxl')
install.packages('tidyverse')
install.packages(c('tidygraph', 'ggraph'))
library('readxl')
library('tidyverse')
library('tidygraph')
library('ggraph')
library('igraph')

setwd("C:/Users/hp/Documents/SNU/자유전공학부/족보 빅데이터")
raw <- readxl::read_excel(path = "고려중기 정치세력 및 경원이씨 해주최씨 개성왕씨 1090-1170.xlsx", sheet = "고려중기 정치세력", col_names = TRUE)

class(raw) #"tbl_df"     "tbl"        "data.frame"
rawgraph <- as_tbl_graph(raw)
class(rawgraph)
rawgraph
nrow(distinct(raw, target)) #221
nrow(distinct(raw, source)) #156

all <- union(raw$target, raw$source) #281 node
intersect <- intersect(raw$target, raw$source) # 96
target2 <- raw %>% group_by(target) %>% filter(n()>1) %>% arrange(target) #136
source2 <- raw %>% group_by(source) %>% filter(n()>1) %>% arrange(source) #199

raw %>% group_by(target) %>% filter(n()==6) %>% nrow() #2-78 #3-30 #4-16 #6-12

raw2 <- distinct(rbind(target2, source2)) #263
rawgraph2 <- as_tbl_graph(raw2)
ggraph(rawgraph2, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

class(raw2)
nrow(distinct(target2, target)) #55
nrow(distinct(source2, source)) #53

#delete all leaves from rawgraph2
#leaves = only appear in target, not in source
leaves <-  setdiff(raw2$target, raw2$source)

rawgraph3 <- delete.vertices(rawgraph2, 
                V(rawgraph2)[ degree(rawgraph2) == 1])
for(v1 in V(rawgraph2)){
  for(i in seq(0:129)){
    if(get.vertex.attribute(rawgraph2, 'name', v1) == leaves[i]){
      v2
      delete.vertices(rawgraph2, v1)
    }
  }
}
v2
ggraph(rawgraph3, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))
seq(0:10)
leaves[130]
raw3 <- raw2 %>% group_by(target)

more_than_twice <- rep(0, 281)
node <- cbind(all, more_than_twice)
independence.number(rawgraph)
node <- as.data.frame(node)



#1
plot(rawgraph)
#2
ggraph(rawgraph) + geom_node_point() + geom_edge_link()
#3
ggraph(rawgraph, layout = 'kk') + geom_node_point() + geom_edge_link(aes(color = factor(type)))
#4
ggraph(rawgraph, layout = 'star') + geom_node_point() + geom_edge_link(aes(color = factor(type)))

layout1 <- create_layout(rawgraph, layout = 'kk')
attributes(layout1)

#find cycles
Cycles = NULL
for(v1 in V(rawgraph)) {
  for(v2 in neighbors(rawgraph, v1, mode="all")) {
    Cycles = c(Cycles, 
               lapply(all_simple_paths(rawgraph, v2,v1, mode="in"), function(p) c(v1,p)))
  }
}


LongCycles = Cycles[which(sapply(Cycles, length) > 3)]

#display cycles in graph
cycle = rep(0, 299)
raw2 <- cbind(raw, cycle)
cycle = rep(1, 3)
temp <- cbind(temp, cycle)
raw2 <- rbind(raw2, temp)

temp <- raw2 %>% filter((source =="李資謙" | source == "王俁(睿宗)" | source == "王楷(仁宗)")
                        & (target == "李資謙" | target == "王俁(睿宗)" | target == "王楷(仁宗)"))
temp <- select(temp, source, target, type)

raw <- anti_join(raw, temp)

class(raw2) #"tbl_df"     "tbl"        "data.frame"
rawgraph2 <- as_tbl_graph(raw2)
class(rawgraph2)
ggraph(rawgraph2, layout = 'mds') + geom_node_point() + geom_edge_link(aes(color = factor(cycle)))

degree <- degree(rawgraph2, mode="all")
