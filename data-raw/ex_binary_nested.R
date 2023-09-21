set.seed(20230921)
n <- 1e3
ex_binary_nested <- matrix(rnorm(4 * n), nrow = 4) |>
  apply(2, cumsum) |>
  t() |>
  pnorm(sd = sqrt(4:1)) |>
  `colnames<-`(c("X3", "X2", "X1", "X0")) |>
  tibble::as_tibble() |>
  dplyr::mutate(y = rbinom(dplyr::n(), 1, X0)) |>
  dplyr::select(y, X0, X1, X2, X3)

usethis::use_data(ex_binary_nested, overwrite = TRUE)
