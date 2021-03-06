% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{graph.plot}
\alias{graph.plot}
\title{graph.plot - Plots a weighted graph, with weighted edges as a default}
\usage{
graph.plot(graph, layout = layout_with_fr(graph, weight = E(graph)$weight^2,
  grid = "nogrid"), vertex.color = "black", vertex.fill = "grey60",
  vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
  vertex.order = FALSE, vertex.background = "white", edges = TRUE,
  edge.color = "darkblue", edge.alpha = E(graph)$weight, edge.size = 1,
  edge.line = "solid", edge.order = FALSE, text = FALSE,
  text.background = NULL, text.background.alpha = 0.4,
  text.background.border = 0, text.size = 3, text.color = "black",
  text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
  midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last",
  type = "closed"), edge.text = FALSE, edge.text.size = 3,
  edge.text.alpha = 0.9)
}
\arguments{
\item{graph}{a \link{igraph} network object}

\item{layout}{a two-column numerical matrix with coordinates for each vertex of graph}

\item{vertex.color}{a single value or a vector of the same length and order as the vertices in graph. See \link{colors} for valid single values.}

\item{vertex.fill}{a single value or a vector of the same length and order as the vertices in graph. See \link{colors} for valid single values.}

\item{vertex.shape}{a single value or a vector of the same length and order as the vertices in graph.}

\item{vertex.size}{a single value or a vector of the same length and order as the vertices in graph.}

\item{vertex.alpha}{a single value between 0 and 1 or a vector of the same length and order as the vertices in graph.}

\item{vertex.order}{a numeric vector of the same length and order as the vertices in the graph.}

\item{edges}{if TRUE edges are drawn.}

\item{edge.color}{a single value or a vector of the same length and order as the edges in graph. See \link{colors} for valid single values.}

\item{edge.alpha}{a single value between 0 and 1 or a vector of the same length and order as the edges in graph.}

\item{edge.size}{a single value or a vector of the same length and order as the edges in graph.}

\item{edge.line}{a single value or a vector of the same length and order as the edges in graph.}

\item{edge.order}{a vector of the same length and order as the edges in graph. Edges are drawn in increasing order, with the highest overlapping those at a lower order.#' @param text}

\item{text.size}{if TRUE vertex names are drawn.}

\item{text.color}{a single value or a vector of the same length and order as the vertices in graph. See \link{colors} for valid single values.}

\item{text.alpha}{a single value or a vector of the same length and order as the vertices in graph.}

\item{legend}{sets the position of the legend. "side" places the legend on the right hand side and "bottom" places the legend along the bottom of the plot.}

\item{text.vjust}{a number setting the amount of vertical adjustment of the position of the text}

\item{midpoints}{if TRUE edges have a arrow at their middle showing the direction of the edge. This naturally only applies to directed networks.}

\item{midpoint.arrow}{sets the character of the midpoint arrow, see \link{arrow}}

\item{edge.text}{if not FALSE, then a vector with the labels for each edge.}

\item{edge.text.size}{a single value or a vector of the same length and order as the edges in graph.}

\item{edge.text.alpha}{a single value or a vector of the same length and order as the edges in graph.}
}
\value{
a \link{ggplot2} plot
}
\description{
graph.plot - Plots a weighted graph, with weighted edges as a default
}
\examples{
data(den)
health.affil  <- has.tags(den, c("Health"))
den.health    <- droplevels(den[den$AFFILIATION \%in\% health.affil,])
net.org       <- elite.network.org(den.health)
lay.org       <- layout.fruchterman.reingold(net.org)
p             <- graph.plot(net.org, layout = lay.org, vertex.size = V(net.org)$members, vertex.fill = degree(net.org),
                            edge.color = "darkmagenta", edge.alpha = log(1/E(net.org)$weight))
p             <- p + scale_fill_continuous(low = "white", high = "magenta") + scale_size_continuous(range = c(3, 10))
p + scale_alpha_continuous(range = c(0,1))
}

