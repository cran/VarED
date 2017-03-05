#' Difference-based variance estimation.
#' @param x numeric Equally spaced design points.
#' @param y numeric Responses
#' @param type  character Taking "opt" or "ord", default as "ord"
#' @param r numeric The order of employed difference sequence.
#' @param m numeric The bandwidth or the number of regressors.
#' @return u numeric The estimated variance
#' @examples
#' x<-1:100/100
#' y<-5*sin(2*pi*x)+rnorm(100)*0.5
#' type="ord"
#' r<-2
#' m<-10
#' vardif(x,y,type,r,m)
#' @export
vardif=function(x,y,type,r,m)
{
  if (type=="opt")
  {d=optseq(r)}
  if(type=="ord")
  {d=ordseq(r)}
  n=length(x)
  M=n*m-r*m*(m+1)/2
  dw<-ds<-fm<-fz<-0
  s<- seq(length=m,from=0,by=0);
  dd<- seq(length=m,from=0,by=0);
  w<-seq(length=m,from=0,by=0)
  for  (j in 1:m)
  {
    w[j]<-(n-r*j)/M
    D=matrix(0,n,(n-r*j))
    for ( i in 1:(n-r*j))
    {
      for (jj in 0:r)
      {
        D[(i+jj*j),i]=d[jj+1]
      }
    }
    temp=t(y)%*%D
    s[j]=mean(temp^2)
    dd[j]<-(j/n)^2
  }
  dw=sum(w*dd)
  ds=sum(w*s)
  fm=sum(w*(dd-dw)^2)
  fz=sum(w*s*(dd-dw))
  if(fm<0.0000000000000001)
  {
    u<-ds
  }
  else
  {
    u<-ds-dw*fz/fm
  }
  return(u)
}
