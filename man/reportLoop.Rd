\name{reportLoop}
\alias{reportLoop}
\title{reportLoop}
\description{
\code{reportLoop} is a nicely packaged progress reporter for your loop. It doesn't require any initialization like \code{txtProgressBar} does; you just drop the thing into your loop and it goes to work. Admittedly it's not as pretty to look at, but there you go.
}
\usage{
reportLoop(
	x,
	max,
	label=NA,
	reportInterval=5,
	maxWidth=80,
	includePB=TRUE,
	progressChar="="
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
    A label for your loop. If \code{NA} (default), it goes unlabeled.
  }
  \item{reportInterval}{
    The interval of iterations to report. For example, if \code{reportInterval} is 5 (default), you get a progress update every 5 iterations.
  }
  \item{maxWidth}{
    The maximum width for a report; defaults to 80 characters. If this value is too short, reports will be truncated, so be sure you leave room.
  }
  \item{includePB}{
    Whether to include a progress bar; defaults to \code{TRUE}.
  }
  \item{progressChar}{
    The character to use for the progress bar; defaults to \code{=}. Ignored if \code{includePB} is \code{FALSE}.
  }
}
\details{
By the way, a few quirks of the thing:
\itemize{
\item{
	It stores \code{reportLoopTracker} and \code{reportLoopTimeStart} in the global environment during execution and removes them when the loop finishes (\emph{i.e.} when \code{x==max}). If you terminate your loop before \code{x==max}), those variables will still be around. They don't hurt anything, but if you're wondering where they come from, that's where.
}
\item{
  And speaking of \code{reportLoopTracker}, the looper assumes you've started a new loop when you give it an \code{x} that's lower than an \code{x} from a previous loop. So kindly don't have your loop iterators go backwards.
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
\href{https://www.linkedin.com/in/jon-dayley-371398102/}{Jon Dayley}
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
	reportLoop(i,loopLimit,"Reporting a test loop...",2)
	Sys.sleep(sleepTime)
}
for(i in 1:loopLimit){
	reportLoop(i,loopLimit,"Reporting a test loop...",3)
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
