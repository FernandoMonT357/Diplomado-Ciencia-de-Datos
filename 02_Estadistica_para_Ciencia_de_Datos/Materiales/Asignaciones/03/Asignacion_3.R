A <- matrix(
  c(0, 1, 1,
    1, 0, 1,
    1, 1, 0),
  nrow = 3,
  byrow = TRUE
)

I <- matrix(
  c(1, 0, 0,
    0, 1, 0,
    0, 0, 1),
  nrow = 3,
  byrow = TRUE
)
Acuad <- A %*% A


resultado <- Acuad - A - 2 * I

A
I
Acuad
resultado
