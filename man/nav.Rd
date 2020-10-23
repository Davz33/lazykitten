\name{nav}
\alias{nav}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
navigate list/env function
}
\description{
navigate a list or environment object untile the very leaf, given a path
}
\usage{
nav(path_vect,current_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
  a vector of characters with the exact names of the list/env nodes to follow, in the right order (from root)
  the behaviour of this function is unpredictable in the connect-graphs scenario, meaning a child not is also a ancestor of itself
}
  \item{current_node}{
  the first node to start iteration from,
  if it's the root of your list/environment, then current_node(path[1]) will be current_node in the second iteration
  this means that you need to adapt the path argument to the current_node. e.g. path[-1] when starting from second node.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
list(first=list(second=list(three=list(four=c("hello world"))))) -> listA
nav(c("first","second","three","four"),listA)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
