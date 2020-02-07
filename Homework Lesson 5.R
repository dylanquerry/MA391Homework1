#Problem 3#
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
x = c(50000,50000)
ans=optim(x,Rev,method="L-BFGS-B")
print(ans$par)
print(-ans$val)
Blue(ans$par)
Fin(ans$par)
#Sensitivity R1#
R1 = function(r1){
  fr1 = function(x){(12*(r1*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                       6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans=optim(x,fr1,method="L-BFGS-B")
  return (ans)
}
R1(0.05)
ans.x1=0
ans.x2=0
ans.rev=0
r = seq(.01,.8,.01)
for (i in 1:length(r)){
  ans=R2(r[i])
  ans.x1[i]=ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.rev[i]=-ans$value
}
result = data.frame(growth_rate=r,x1=ans.x1,x2=ans.x2,rev=ans.rev)
print(result)
#Sensitivity R2#
R2 = function(r2){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                       6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans=optim(x,fr2,method="L-BFGS-B")
  return (ans)
}
R2(0.08)
ans.x1=0
ans.x2=0
ans.rev=0
r = seq(.04,.12,.01)
for (i in 1:length(r)){
  ans=R2(r[i])
  ans.x1[i]=ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.rev[i]=-ans$value
}
result = data.frame(growth_rate=r,x1=ans.x1,x2=ans.x2,rev=ans.rev)
print(result)

#Sensitivity a#
sa = function(a){
  fa = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-a*x[1]*x[2]) +
                       6*(0.08*x[2]*(1-x[2]/400000)-a*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans=optim(x,fa,method="L-BFGS-B")
  return (ans)
}
sa(1/(100000000))
ans.x1=0
ans.x2=0
ans.rev=0
a = c(10^-2,10^-3,10^-4,10^-5,10^-6,10^-7,10^-8,10^-9,10^-10,10^-11,10^-12,10^-13)
for (i in 1:length(a)){
  ans=sa(a[i])
  ans.x1[i]=ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.rev[i]=-ans$value
}
result = data.frame(a=a,x1=ans.x1,x2=ans.x2,rev=ans.rev)
print(result)
