test_that("reproduces results from `lsa::cosine`", {
  x <- rbind(
    c(0,1),
    c(1,0),
    c(0,-1)
  )
  r <- rbind(
    c( 1, 0, -1),
    c( 0, 1,  0),
    c(-1, 0,  1)
  )
  expect_equal(cossim(x), r)
  x <- rbind(
    c(0,.5),
    c(1,0),
    c(0,-1)
  )
  expect_equal(cossim(x), r)
  x <- rbind(
    c(.5,.5),
    c(1,0),
    c(-.5,.5)
  )
  expect_equal(cossim(x), simple_cossim(x))
})
