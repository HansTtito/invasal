\name{generate_new_vars}
\alias{generate_new_vars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate_new_vars
%%  ~~function to do ... ~~
}
\description{

Genera indicadores de IGS y de temporadas

}
\usage{
generate_new_vars(data, months_prev = c(11, 12))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{

data de salmones proveniente del muestreo biológico.

}
  \item{months_prev}{

meses del año anterior considerados dentro de una temporada. Considerados por defecto los meses de noviembre (11) y diciembre (12).

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

generate_new_vars(data = salmones, months_prev = c(11, 12))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
