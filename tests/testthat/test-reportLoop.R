test_that(
	"reportLoop looks fine",
	{
		ct <- 100
		seconds <- 5
		cat("\n")
		for(i in 1:ct){
			reportLoop(
				x=i,
				max=ct,
				label="Counting...",
				includePB=TRUE
			)
			Sys.sleep(seconds/ct)
		}
		testthat::expect_true(TRUE)
	}
)
