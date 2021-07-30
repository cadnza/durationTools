\name{reportLoop}
\alias{reportLoop}
\title{reportLoop}
\description{
\code{reportLoop} is a nicely packaged progress reporter for your loop. It doesn't require any initialization like \code{txtProgressBar} does; you just drop the thing into your loop and it goes to work.
}
\usage{
reportLoop(
  x,
  max,
  label=NA,
  waitSeconds=0,
  includePB=TRUE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    The current numeric iteration.
  }
  \item{max}{
    The maximum number of iterations.
  }
  \item{label}{
    A label for your loop. If \code{NA} (default), the loop goes unlabeled.
  }
  \item{waitSeconds}{
    The number of seconds to wait before starting the progress bar. This can be handy for processes that may either finish quickly or take a while to run. Defaults to \code{0}, \emph{i.e.} start progress bar immediately.
  }
  \item{includePB}{
    Whether to include a progress bar. Defaults to \code{TRUE}.
  }
}
\details{
By the way, a few quirks of the thing:
\itemize{
\item{
  It stores two variables in the global environment during execution:
  \itemize{
    \item{\code{.reportLoopDurationToolsWorkingProgressBarId}}
    \item{\code{.reportLoopDurationToolsMostRecentMaxValue}}
  }
  They get removed when the loop finishes--\emph{i.e.} when \code{x==max}--so if you terminate your loop \emph{before} \code{x==max}, they'll still be around. They don't hurt anything, and if you're using a scientific computing IDE, your variable list will probably hide them. But if you happen across them by chance and you're wondering where they come from, that's where.
}
\item{
  And speaking of \code{.reportLoopDurationToolsMostRecentMaxValue}, the looper assumes you've started a new loop when you give it an \code{x} that's lower than an \code{x} from a previous loop. So kindly don't have your loop iterators go backwards.
}
\item{
  There are a lot of ways to improve this little function, but for now I just need it to report progress for work, and the meeting to report on it's in half an hour. So it'll just have to do for now.
}
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
\href{https://github.com/cadnza}{Jon Dayley}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
loopLimit <- 10
sleepTime <- 0.25
for(i in 1:loopLimit){
  durationTools::reportLoop(i,loopLimit,"Reporting a test loop...")
  Sys.sleep(sleepTime)
}
for(i in 1:loopLimit){
  durationTools::reportLoop(i,loopLimit,"Reporting another test loop...")
  Sys.sleep(sleepTime)
}
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
