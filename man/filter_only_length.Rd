\name{filter_only_length}
\alias{filter_only_length}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
filter_only_length
%%  ~~function to do ... ~~
}
\description{

Identifica tallas mayores a un umbral y también registros que contienen NA; y los pasa a eliminar.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter_only_length(data, umbral_length = 150)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{

  Utiliza la data proveniente de clean_data()
%%     ~~Describe \code{data} here~~
}
  \item{umbral_length}{

  talla máxima que servirá de umbral. Viene por defecto 150 cm.
%%     ~~Describe \code{umbral_length} here~~
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

salmones = generate_new_vars(data = salmones)

salmones = clean_data(data = salmones)

filter_only_length(data = salmones)

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
