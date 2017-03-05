#' Generate an ordinary difference sequence with order r.
#' @param r The order of the generated difference sequence.
#' @return The generated ordinary difference sequence.
#' @examples
#' r<-2
#' ordseq(r)
#' @export
ordseq=function(r)
{
  d=seq(r+1)
  for (i in 1:(r+1))
  {
    d[i]=(-1)^(i-1)*choose(r,(i-1))/(choose((2*r),r))^(1/2)
  }
  return(d)
}
