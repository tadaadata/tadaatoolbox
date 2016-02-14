#### Dependency tree ###

library(tools)
library(DiagrammeR)

dep1 <- package_dependencies("tadaatoolbox", db = available.packages(), recursive = F)
dep2 <- lapply(dep1[[1]], package_dependencies, db = available.packages())

nodes_total <- c("tadaatoolbox", dep1[[1]], unique(unlist(sapply(dep2, "[[", 1))))

from1 <- rep("tadaatoolbox", length(dep1[[1]]))
from2 <- unlist(sapply(dep2, function(x){
  rep(names(x), length(x[[1]]))
}))

from_pkgs <- c(from1, from2)

to_pkgs <- c(dep1[[1]],
             unlist(sapply(dep2, function(x){x[[1]]}))
             )


graph <- create_graph(nodes_df = create_nodes(nodes_total),
                      edges_df = create_edges(from = from_pkgs,
                                              to = to_pkgs,
                                              rel = "to_get",
                                              "penwidth = 2"),
                      graph_attrs = c("layout = twopi",
                                      "overlap = false",
                                      "outputorder = edgesfirst")
                      )
render_graph(graph = graph)



