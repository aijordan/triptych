set.seed(20230921)
n <- 1e3
m <- 10
sh1 <- c(1, 1/4, 4/1, 5/3, 3/5, 1, 1/4, 4/1, 5/3, 2/3)
sh2 <- c(1, 1/4, 4/1, 3/5, 5/3, 1, 1/4, 4/1, 3/5, 3/2)
ex_binary <- matrix(rnorm(n * m), nrow = m) |>
  apply(2, cumsum) |>
  (\(x) x[m:1, ])() |>
  pnorm(sd = sqrt(1:m)) |>
  pbeta(sh1, sh2) |>
  t() |>
  `colnames<-`(sprintf("X%02i", 1:m)) |>
  tibble::as_tibble() |>
  dplyr::mutate(y = rbinom(dplyr::n(), 1, X01)) |>
  dplyr::relocate(y)

usethis::use_data(ex_binary, overwrite = TRUE)
