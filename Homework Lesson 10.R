#Problem 1#
f = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5001,8001,10001,1,1),nrow=5)
solnl(x0,objfun=f,A=A,B=B)
#Problem 2#

#I expected the objective function to change by 1*lambda for each equations so around 6, however it didn't, this could have 
#happened because it is a nonlinear objective function, or I could have made a mistake#

#Problem 3#

constraint = seq(9990,10010,1)
ans = 0
ans.val=0
for (i in 1:length(constraint)){
  f = function(x){
    return(((339-0.01*x[1]-0.003*x[2])*x[1]
            +(399-0.004*x[1]-0.01*x[2])*x[2]
            -(400000+195*x[1]+225*x[2]))*-1)
  }
  x0=c(1000,1000)
  A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5)
  B = matrix(c(5000,8000,constraint[i],1,1),nrow=5)
  ans[i] = solnl(x0,objfun=f,A=A,B=B)  
  ans.val[i]= f(ans[i])
}
result = data.frame(fine = fine,optimalcrew=ans,cost=ans.cost, days = days)
print(result)
