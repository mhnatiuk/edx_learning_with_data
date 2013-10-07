

gen.line = function(){
x <- runif(n=2,min=-1,max=1)
y <- runif(n=2,min=-1,max=1)
#a = (y[2] - y[1]) / (x[2] - x[1])
#b = y[1] - ( a * x[1])
#plot(x=-1:1,y=-1:1,type="n")

#assert
#stopifnot(round(y[2],3) == round((a*x[2]) + b,3))
#points(x,y,col="green")
#abline(lm(y~ x))
return(list(model=lm(y~x), x=x,y=y))
}

map.point <- function(c.x,c.y,x,y){
    if( ((x[2] - x[1])*(c.y - y[1]) - (y[2] - y[1])*(c.x - x[1])) > 0 ){
        return(1)
    }
    else{
        return(-1)
    }
}


draw.random <- function(n=10){
x <- runif(n,min=-1,max=1)
y <- runif(n,min=-1,max=1)
#points(x,y,col="red")
return(list(x=x,y=y))
}

iters <- 100
iterations.number.vector <- vector(mode="numeric",length=iters)
P.f.ne.g <-vector(mode="numeric",length=iters)
for(n in 1:iters){
    t0 = proc.time()[3]
    g = gen.line()
    points  = draw.random(100)
    
    classified <- c()
    
    for(i in 1:length(points$x)){
    classified <- c(classified,map.point(points$x[i], points$y[i], g$x,g$y) )
    }
    
    #classified
    w = c(0,0,0)
    w.s <- data.frame(t(w))
    i=0
    times = data.frame(a=c(0),b=c(0),c=c(0),d=c(0),e=c(0))
    points.it <- matrix(c(rep(1,length(points$x)),points$x,points$y),ncol=3)
    #plot(-1:1,-1:1,type="n")
    #points(points.it[,2:3],col=map.col(classified))
    #abline(g$model,lwd=2,col="red")
    #map.col <- function(x,pos="red",neg="blue"){
     #   return(sapply(x, FUN=function(x) { if(x>0) { pos } else { neg } }))
    #}
     
    while(TRUE){
        i=i+1    
        pla <- sign(crossprod(w,t(points.it)))
        missed <- which((pla != classified)== T)
        if(length(missed)>0){
            miss.point <- missed[round(runif(1, min=1,max=length(missed)))]
            }
            else{
            iterations.number.vector[n] <- i
            break
            }
        if( i > 1000000 ){   #prevent infinite loop occuring when input is not linearly separable
            iterations.number.vector[n] <- NA
            break
        }    
        w.s[i,] <- w
        w <- w + classified[miss.point] * c(1,points$x[miss.point], points$y[miss.point])
    }
    
    points.check  = draw.random(100)
    
    classified <- c()
    
    for(i in 1:length(points.check$x)){
        classified <- c(classified,map.point(points.check$x[i], points.check$y[i], g$x,g$y) )
    }
    points.check.matrix <- matrix(c(rep(1,length(points.check$x)),points.check$x,points.check$y),ncol=3)
    pla <- sign(crossprod(w,t(points.check.matrix)))
    missed <- which((pla != classified)== T)
    P.f.ne.g[n] <- (length(missed) / length(pla) ) # fraction of missclassified points P[f(x)!=g(x)]
    
}






