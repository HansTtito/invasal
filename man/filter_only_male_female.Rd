\name{filter_only_male_female}
\alias{filter_only_male_female}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
filter_only_male_female
%%  ~~function to do ... ~~
}
\description{

Permite filtrar solo los registros "Hembra" y "Macho" de la data.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter_only_male_female(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Data limpia proveniente de clean_data()
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

salmones = filter_only_length(salmones)

filter_only_male_female(salmones)

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
