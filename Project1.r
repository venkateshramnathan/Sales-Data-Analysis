a = 6
b = 2
r = 0.013
x = runif(500,1,20)
noise = rnorm(length(x),mean=0,sd=0.4)
y = a + (b*((1 - exp(-r*x))/r)) + noise
summary(y)


temp = data.frame(x,y)
#linear_reg1 = lm(y ~ x, data = temp)
linear_reg1 = function(par,data){
  a = par[1]
  b = par[2]
  r = par[3]
  sum((temp[,2] - a -b*(1 - exp(-r*temp[,1]))/r)**2)
}
nlminb(c(1,0,1),objective=linear_reg1,data = temp)
ans<-nlminb(start = c(1,0,1),objective=linear_reg1,data = temp,control = list(trace = 2))

temp3 = list()
linear_reg2 = list()
ans1 = list()
accum = 0
for(i in 1:5000)
{
temp3[[i]] = data.frame(temp[sample(nrow(temp), size = 500, replace = TRUE),])
linear_reg2[[i]] = function(par,data){
  a = par[1]
  b = par[2]
  sum((temp3[[i]][2] -(a + (b*temp3[[i]][1])))**2)
}
  ans1[[i]] <-nlminb(c(1,1),objective = linear_reg2[[i]],data=temp3[[i]])
  ans1[[i]] = ans1[[i]]$objective
  if(ans$objective > ans1[[i]]){
    accum = accum + 1 
  }
}
p_value = accum/5000 
#linear_reg2[[1]] = function(par,data){
  #a = par[1]
  #b = par[2]
  #sum((temp3[[1]][2] -(a + (b*temp3[[1]][1])))**2)
#}
  #ans1[[1]] <-nlminb(c(1,1),objective = linear_reg2[[1]],data=temp3[[1]])


#linear_reg2[[i]] = lm(as.matrix(temp3[[i]][2])~as.matrix(temp3[[i]][1]),data = temp3[[i]])

#full model
set.seed(42)
ad = runif(500,1,5)
rebate = runif(500,1,10)
xmas = round(runif(500,0,1))
noise1 = rnorm(length(ad),mean=0,sd=0.1)
b0 = 20
b1 = 30
b2 = 80
b3 = 5
b4 = 7
b5 = 36
b6 = 28
r1 = 0.5
r2 = 0.6
y1 = b0 + (b1*(1 - exp(-r1*rebate)))/r1 + (b2*(1 - exp(-r2*ad)))/r2 + b3*xmas + b4*xmas*rebate + b5*xmas*ad + b6*rebate*ad + noise1 
temp2 = data.frame(ad,rebate,xmas,y1)

linear_reg = function(par, data){
  a0 = par[1]
  a1 = par[2]
  a2 = par[3]
  a3 = par[4]
  a4 = par[5]
  a5 = par[6]
  a6 = par[7]
  s1 = par[8]
  s2 = par[9]
  
  ad = data[1]
  rebate = data[2]
  xmas = data[3]
  
  yhat =  a0 + (a1*(1 - exp(-s1*rebate)))/s1 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad + noise1 
  sum((data[4] - yhat)**2) 
}

z = nlminb(start=1:9,objective = linear_reg, data = temp2, control = list(trace= 2, iter.max=5000, eval.max=5000))
z

#model1 - without b0

y2 = (b1*(1 - exp(-r1*rebate)))/r1 + (b2*(1 - exp(-r2*ad)))/r2 + b3*xmas + b4*xmas*rebate + b5*xmas*ad + b6*rebate*ad + noise1 
temp4 = data.frame(ad,rebate,xmas,y2)
linear_reg3 = function(par,data){
  a1 = par[1]
  a2 = par[2]
  a3 = par[3]
  a4 = par[4]
  a5 = par[5]
  a6 = par[6]
  s1 = par[7]
  s2 = par[8]
  
  data = temp4
  ad = data[1]
  rebate = data[2]
  xmas = data[3]
  
  yhat = (a1*(1 - exp(-s1*rebate)))/s1 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad + noise1
  sum((data[4] - yhat)**2)
}

m1 = nlminb(start=1:8,objective = linear_reg3, data = temp4, control = list(trace= 2, iter.max=5000, eval.max=5000))
m1

#bootstrapping model1 1k times
temp3 = list()
linear_reg2 = list()
m = list()
accum = 0

for(i in 1:1000)
{
  temp3[[i]] = data.frame(temp4[sample(nrow(temp4), size = 500, replace = TRUE),])
  linear_reg2[[i]] = function(par,data){
    a1 = par[1]
    a2 = par[2]
    a3 = par[3]
    a4 = par[4]
    a5 = par[5]
    a6 = par[6]
    s1 = par[7]
    s2 = par[8]
  
    data = temp3[[i]]
    ad = temp3[[i]][1]
    rebate = temp3[[i]][2]
    xmas = temp3[[i]][3]
    
    yhat = (a1*(1 - exp(-s1*rebate)))/s1 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad + noise1
    sum((data[4] - yhat)**2)
  }  
  m[[i]] = nlminb(start=1:8,objective = linear_reg2[[i]], data = temp3[[i]], control = list(trace= 2, iter.max=5000, eval.max=5000))
  m[[i]] = m[[i]]$objective
  if(z$objective > m[[i]]){
    accum = accum + 1 
  }
}
p_value = accum/1000
p_value

#model2 - without b1
y3 = b0 + (b2*(1 - exp(-r2*ad)))/r2 + b3*xmas + b4*xmas*rebate + b5*xmas*ad + b6*rebate*ad + noise1 
temp5 = data.frame(ad,rebate,xmas,y3)
linear_reg4 = function(par,data)
  {
  a0 = par[1]
  a2 = par[2]
  a3 = par[3]
  a4 = par[4]
  a5 = par[5]
  a6 = par[6]
  s2 = par[7]
  
  data = temp5
  ad = data[1]
  rebate = data[2]
  xmas = data[3]

  yhat = a0 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad + noise1
  sum((data[4] - yhat)**2)
}
m2 = nlminb(start=1:7,objective = linear_reg4, data = temp5, control = list(trace= 2, iter.max=5000, eval.max=5000))
m2

#bootstrapping model2 1k times
temp3 = list()
linear_reg2 = list()
m = list()
accum = 0

for(i in 1:1000)
{
  temp3[[i]] = data.frame(temp5[sample(nrow(temp5), size = 500, replace = TRUE),])
  linear_reg2[[i]] = function(par,data){
    a0 = par[1]
    a2 = par[2]
    a3 = par[3]
    a4 = par[4]
    a5 = par[5]
    a6 = par[6]
    s2 = par[7]
    
    
    data = temp3[[i]]
    ad = temp3[[i]][1]
    rebate = temp3[[i]][2]
    xmas = temp3[[i]][3]
    
    yhat = a0 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad + noise1
    sum((data[4] - yhat)**2)
  }  
  m[[i]] = nlminb(start=1:7,objective = linear_reg2[[i]], data = temp3[[i]], control = list(trace= 2, iter.max=5000, eval.max=5000))
  m[[i]] = m[[i]]$objective
  if(z$objective > m[[i]]){
    accum = accum + 1 
  }
}
p_value = accum/1000
p_value

#original model with dataset
dataset = read.csv("project1.csv")
set.seed(42)
rebate = dataset[1]
ad = dataset[2]
xmas = dataset[3]
y = dataset[4]

linear_reg = function(par, data){
  a0 = par[1]
  a1 = par[2]
  a2 = par[3]
  a3 = par[4]
  a4 = par[5]
  a5 = par[6]
  a6 = par[7]
  s1 = par[8]
  s2 = par[9]

  rebate = dataset[1]
  ad = dataset[2]
  xmas = dataset[3]
  y = dataset[4]
  
  yhat = a0 + (a1*(1 - exp(-s1*rebate)))/s1 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad
  sum((y - yhat)**2)
}

z = nlminb(start=1:9,objective = linear_reg, data = dataset, control = list(trace= 2, iter.max=5000, eval.max=5000))
z

#bootstrapping model without a0
temp3 = list()
linear_reg = list()
m = list()
accum = 0

for(i in 1:100)
{
  temp3[[i]] = data.frame(dataset[sample(nrow(dataset), size = 260, replace = TRUE),])
  linear_reg[[i]] = function(par,data){
    a1 = par[1]
    a2 = par[2]
    a3 = par[3]
    a4 = par[4]
    a5 = par[5]
    a6 = par[6]
    s1 = par[7]
    s2 = par[8]
    
    data = temp3[[i]]
    ad = temp3[[i]][2]
    rebate = temp3[[i]][1]
    xmas = temp3[[i]][3]
    y = temp3[[i]][4]
    
    yhat = a1 + (a2*(1 - exp(-s2*ad)))/s2 + a3*xmas + a4*xmas*rebate + a5*xmas*ad + a6*rebate*ad
    sum((y - yhat)**2)
  }  
  m[[i]] = nlminb(start=1:8,objective = linear_reg1[[i]], data = temp3[[i]], control = list(trace= 2, iter.max=5000, eval.max=5000))
  m[[i]] = m[[i]]$objective
  if(z$objective > m[[i]]){
    accum = accum + 1 
  }
}
p_value = accum/100
p_value
