\name{formatDuration}
\alias{formatDuration}
\title{formatDuration}
\description{
\code{formatDuration} is a minimalist function that formats durations. You give it a duration and it formats it.
}
\usage{
formatDuration(durationSecs,padded=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{durationSecs}{
    The duration in seconds.
  }
  \item{padded}{
    Whether you want the duration padded. If you do, it adds space such that all durations occupy the same width (provided less than 100 hours for now... I'll fix it.).
  }
}
\details{
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
\href{https://www.linkedin.com/in/jonathan-dayley-371398102}{Jonathan Dayley}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
formatDuration(1231)
formatDuration(1231,padded=TRUE)
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
