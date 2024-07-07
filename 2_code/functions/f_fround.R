fround <- function(x, dig = 2, bm = "", dm = "."){
  format(round(x, digits = dig), nsmall = dig, big.mark = bm, decimal.mark = dm)
}
