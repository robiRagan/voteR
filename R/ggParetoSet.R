ggParetoSet <- function (inIdeals) {
  idealsDF <- as.data.frame(inIdeals)
  paretoSetDF <- as.data.frame(paretoSet(inIdeals))
  ggplot(idealsDF, aes(x=V1, y=V2)) + geom_point(shape=1) + geom_polygon(data=paretoSetDF)
}
