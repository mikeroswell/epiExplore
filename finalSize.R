library(shellpipes)
finalSize <- function(R0){
  susFrac <- uniroot(function(x){R0*(x-1)/log(x)-1}
                     , upper = 1-1e-12
                     , lower =1e-12
                     )
  return(1-as.numeric(susFrac[1]))

}
saveEnvironment()


