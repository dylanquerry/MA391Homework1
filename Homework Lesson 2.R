#Problem 1#
#Optimal Rebate Amount#
profit = function (x){
  return((1500-100*x)*(1+.15*x))
}
dProfit = function(x){fprime(profit,x)}
ans = bisection(dProfit,0,20)
print(ans)
#Sensitivity of Sales Increase#
sales = seq(0.05,0.25,0.01)
ans = 0
ans.profit=0
for (i in 1:length(sales)){
  profit = function (x){
    return((1500-100*x)*(1+sales[i]*x))
  }
  dProfit = function(x){fprime(profit,x)}
  ans[i] = bisection(dProfit,-10,20)    
  ans.profit[i]=profit(ans[i])
}
result = data.frame(salesIncrease=sales,optimalRebate=ans,profit=ans.profit)
print(result)
#Rebate Size Analysis#
rebateSize = seq(50,200,10)
ans = 0
ans.profit=0
for (i in 1:length(rebateSize)){
  profit = function (x){
    return((1500-rebateSize[i]*x)*(1+0.15*x))
  }
  dProfit = function(x){fprime(profit,x)}
  ans[i] = bisection(dProfit,-10,20)    
  ans.profit[i]=profit(ans[i])
}
result=data.frame(rebateSize=rebateSize,optimalNumber=ans,profit=ans.profit)
print(result)

#Problem 2#
#Sensitivity of Cost/Day)
cost = seq(0.3,0.6,0.05)
pr = array(0,length(cost))
ans = array(0,length(cost))
for (i in 1:length(cost)){
  p = function (x){
    return((0.65-0.01*x)*(200+5*x)-cost[i]*x)
  }
  dp = function(x){fprime(p,x,)}
  ans[i] = bisection(dp,-100,50,0.0001)
  pr[i] = p(round(ans[i]))
}
print(ans)
print(pr)
plot(cost,ans,"o",xlab="Cost/Day",ylab="x(Days to Sell)")
title("Sensitivity of Feed Cost of the Pig")

#New Function with new growth rate and feed price#

profit = function (x){
  return((0.65-0.01*x)*(200+7*x)-0.6*x)
}
dProfit = function(x){fprime(profit,x)}
ans=bisection(dProfit,0,20)
print(ans)
profit(ans)

#Minimum Improvement in growth rate#
g = seq(5,7,0.1)
pr = array(0,length(g))
ans = array(0,length(g))
for (i in 1:length(g)){
  profit = function (x){
    return((0.65-0.01*x)*(200+g[i]*x)-.6*x)
  }
  dProfit = function(x){fprime(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
  pr[i] = profit(round(ans[i]))
}
result = data.frame(growthRate = g,daysToSell=ans,profit=pr)
print(result)

#Problem 3#
#Graph of prices#
p1 = function(x)0.65-0.01*x
p2 = function(x)0.65-0.01*x+0.00004*x^2
t = seq(0,20)

plot(t,p1(t),"l",lwd=3)
points(t,p2(t),"o",col="red")
legend("top", c("old", expression(paste("new"))),col=c("black","red"),lty=1)

#New Price Optimal#
profit = function (x){((0.65-0.01*x+0.00004*x^2)*(200+5*x)-0.45*x)}
dProfit = function(x){fprime(profit,x)}
ans = bisection(dProfit,0,20)
print(ans)
profit(10)

#Sensitivity of price leveling#
lo = seq(0.00001,0.00008,0.00001)
label=0
ans.days = 0
ans.profit=0
t = 0:30
for (i in 1:length(lo)){
  c = lo[i]
  label[i]=sprintf("%.5f",c)
  profit = function (x){((0.65-0.01*x+c*x^2)*(200+5*x)-0.45*x)}
  dProfit = function(x){fprime(profit,x)}
  ans.days[i] = bisection(dProfit,0,20)
  ans.profit[i]=profit(ans.days[i])
}
result = data.frame(levelOff=lo,optimalDays = ans.days,profit=ans.profit)
print(result)

profit = function (x){((0.65-0.01*x)*(200+5*x)-0.45*x)}
dProfit = function(x){fprime(profit,x)}
ans = bisection(dProfit,0,20)
print(ans)
profit(8)
