#Problem 4#
#Optimal Amount of Crews#
r = 5/7
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
dfT = function(x){fprime(fT,x)}
ans = bisection(dfT,0,40)
print(ans)
plot(dfT)
fT(11)
#Rate Sensitivity#
rate = seq(.5,0.9,0.05)
ans = 0
ans.cost=0
for (i in 1:length(rate)){
  f = function(x){500*(200/(rate[i]*(x+1)))+(18000+800*200/(rate[i]*(x+1)))*x+(200/(rate[i]*(x+1))>14)*(10000*(200/(rate[i]*(x+1))-14))}
  df = function(x){fprime(f,x)}
  ans[i] = bisection(df,0,40)    
  ans.cost[i]=f(ans[i])
}
result = data.frame(rate=rate,optimalcrew=ans,cost=ans.cost)
print(result)
#Fine Size Analysis#
fine = seq(5000,15000,500)
ans = 0
ans.cost=0
days=0
for (i in 1:length(fine)){
  f = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(fine[i]*(200/(r*(x+1))-14))}
  df = function(x){fprime(f,x)}
  ans[i] = bisection(df,0,40)    
  ans.cost[i]=f(ans[i])
  days[i] = (200/(r*(ans[i]+1)))
}
result = data.frame(fine = fine,optimalcrew=ans,cost=ans.cost, days = days)
print(result)
#Problem 5#
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}

Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}



Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     
                     6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
