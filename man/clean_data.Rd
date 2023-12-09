\name{clean_data}
\alias{clean_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

clean_data
%%  ~~function to do ... ~~
}
\description{

Limpia y procesa la información biológica.
Identifica si hay registros sin año y los elimina.
Los individuos con sexo NA o algún otro diferente a "Hembra" o "Macho", serán considerados como indeterminados

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clean_data(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Información biológica proveniente de generate_new_vars()
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

clean_data(data = salmones)

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
