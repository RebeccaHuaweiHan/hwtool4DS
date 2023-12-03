test_that("DatInt works", {
  load(file='AHIdat_ap.rda')
  CDname<- names(AHIdat_ap)
  dat.k0 <- list()
  dat <- AHIdat_ap[1]
  names(dat) <- NULL
  dat <- data.frame(dat[1])
  dat.Int <- DataInt(dat)
  dat.compare <- read.csv("1209.datK0.csv")
  expect_equal(dat.Int,dat.compare)

})
