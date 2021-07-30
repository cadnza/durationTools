test_that(
	"reportLoop looks fine",
	{
		ct <- 20
		cat("\n")
		for(i in 1:20){
			reportLoop(
				x=i,
				max=ct,
				label="Counting...",
				reportInterval=1,
				maxWidth=80,
				includePB=TRUE,
				progressChar="="
			)
			Sys.sleep(0.2)
		}
		testthat::expect_true(TRUE)
	}
)
