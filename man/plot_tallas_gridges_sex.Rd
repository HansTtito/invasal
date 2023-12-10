\name{plot_tallas_gridges_sex}
\alias{plot_tallas_gridges_sex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

plot_tallas_gridges_sex
%%  ~~function to do ... ~~
}
\description{

Función plot de tallas según temporada y sexo.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_tallas_gridges_sex(data, order_years = "decreasing", colores, labels, name_plot, output_path, heigh_plot, width_plot, size_axis_text = 12, angle_text_x = 0, size_title_axis = 14, alpha = 0.7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{

Data proveniente de clean_data() y de filter_only_length().
%%     ~~Describe \code{data} here~~
}
  \item{order_years}{

Orden de los años, puede ser "increasing" o "decreasing"
%%     ~~Describe \code{order_years} here~~
}
  \item{colores}{

Cadena de colores para "Hembras", "Machos" e "Indeterminados".
%%     ~~Describe \code{colores} here~~
}
  \item{labels}{

Cadena de etiquetas para "Hembras", "Machos" e "Indeterminados".
%%     ~~Describe \code{labels} here~~
}
  \item{name_plot}{

Nombre con el que se guardará el plot.
%%     ~~Describe \code{name_plot} here~~
}
  \item{output_path}{

Carpeta dónde se guardará el plot.
%%     ~~Describe \code{output_path} here~~
}
  \item{heigh_plot}{

Alto con el que se guardará la imagen.
%%     ~~Describe \code{heigh_plot} here~~
}
  \item{width_plot}{

Ancho con el que se guardará la imagen.
%%     ~~Describe \code{width_plot} here~~
}
  \item{size_axis_text}{

Tamaño del texto de los ejes.
%%     ~~Describe \code{size_axis_text} here~~
}
  \item{angle_text_x}{

Ángulo del texto en el eje x.
%%     ~~Describe \code{angle_text_x} here~~
}
  \item{size_title_axis}{

Tamaño de las etiquetas del eje x e y
%%     ~~Describe \code{size_title_axis} here~~
}
  \item{alpha}{

grado de transparencia de colores. Toma valores entre 0 y 1.
%%     ~~Describe \code{alpha} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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

salmones = filter_only_length(data = salmones)

plot_tallas_gridges_sex(data = salmones,
                        order_years = "decreasing",
                        colores = c("red", "blue", "green", "gold"),
                        labels = c("Hembra", "Macho", "Indet", "total"),
                        name_plot = "tallas_sexo.png",
                        output_path = "",
                        heigh_plot = 10,
                        width_plot = 9,
                        alpha = 0.4)

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
