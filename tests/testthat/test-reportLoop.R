test_that(
	"reportLoop looks fine",
	{
		ct <- 20
		for(i in 1:20){
			reportLoop(
				x=i,
				max=ct,
				label="Counting...",
				reportInterval=5,
				maxWidth=80,
				includePB=TRUE,
				progressChar="="
			)
			sleep(0.2)
		}
	}
)
