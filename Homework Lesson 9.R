#Problem 1#
obj = function(x){
  (x[1]*(600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1]))*(-1)
}
Aeq = matrix(c(12,5),nrow = 1,byrow = TRUE)
Beq = matrix(c(2500))
x0 = c(5,5)
ans = solnl(x0,objfun = obj, Aeq=Aeq,Beq=Beq)
print(ans)
#Problem 2#
con = function(x){
  f = NULL
  f = rbind(f, 12*x[1]+5*x[2]-2500)
  return(list(ceq=f,c=NULL))
}
ans=solnl(x0,objfun=obj,confun=con)
print(ans)
#Problem 3#
#Problem 4#
X = list(x=seq(100,500),y=seq(100,500))
Z = Outer(obj,X)
contour(X$x,X$y,-Z)
abline(a = 500, b = -12/5, col = "red",lwd=3)
#Problem 5#
f = function(x){
  (x[1]*(10+22*x[1]^-.5+1.3*x[2]^-.1)-18*x[1]+x[2]*(5+15*x[2]^-.5+.8*x[2]^-.08)-10*x[2])*(-1)
}
x0 = c(5,5)
optim(x0,f,method = "L-BFGS-B")
#Problem 6#
A = matrix(c(2,3,-1,0,0,-1),nrow = 3,byrow = TRUE)
B = matrix(c(18,-3,-2),nrow=3,byrow=TRUE)
ans=solnl(x0,objfun=f,A=A,B=B)
print(ans)
X = list(x=seq(2,6),y=seq(2,6))
Z = Outer(f,X)
contour(X$x,X$y,Z)
abline(a=6,b=-2/3,col="red",lwd=3)
abline(h=2,col="red",lwd=3)
abline(v=3,col="red",lwd=3)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)
#Problem 7#
A = matrix(c(-50,-100,1,0,0,1),nrow = 3,byrow = TRUE)
B = matrix(c(-600,7,5),nrow=3,byrow=TRUE)
ans=solnl(x0,objfun=f,A=A,B=B)
print(ans)
X = list(x=seq(2,8),y=seq(2,8))
Z = Outer(f,X)
contour(X$x,X$y,Z)
abline(a=6,b=-1/2,col="red",lwd=3)
abline(h=5,col="red",lwd=3)
abline(v=7,col="red",lwd=3)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)
#Problem 8#
x0 = c(5,5,0,0,0)
Aeq = matrix(c(2,3,1,0,0,-1,0,1,0,0,0,-1,0,0,1),nrow = 3,byrow = TRUE)
Beq = matrix(c(18,3,2),nrow=3,byrow=TRUE)
A = matrix(c(0,0,-1,0,0,0,0,0,-1,0,0,0,0,0,-1),nrow = 3,byrow = TRUE)
B = matrix(c(0,0,0),nrow=3,byrow=TRUE)
ans=solnl(x0,objfun=f,A=A,B=B,Aeq=Aeq,Beq=Beq)
print(ans)
