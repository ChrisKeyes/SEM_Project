changeAlpha<-function(cols,alpha=1){
  # a little function to change transparency for base plots
  # cols can be a single color or a vector
  # alpha=1 will give completly opaque and alpha=0 completely 
  #   transparent
  color.box<-col2rgb(cols)
  if(length(cols>1)) return(apply(t(color.box/255),1,
                                  function(x,alpha) 
                                    rgb(red=x[1],green=x[2],blue=x[3],alpha),alpha=alpha))
  return(rgb(t(color.box/255),alpha=alpha))
}