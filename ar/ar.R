# 人工データの作成（x_i=a_1*x_{i-1}+a2*x_{i-2}+epsilon_i,x_0=0,x_1=1,a_1=-1/4,a_2=1/8）
x = c(0, 0)
n = 100
set.seed(0)
for(i in 3:n){
  x = c(x, - 1 / 4 * x[i-1]+ 1 / 8 * x[i-2] + rnorm(1, mean=0, sd=1/2))
}

# ユールウォーカー法
ar(x, aic = FALSE, order.max=2, method='yule-walker')

# 最小二乗法
ar(x, aic = FALSE, order.max=2, method='ols')

# ユールウォーカー法
gamma0 = sum((x - mean(x))^2) / n # 分散
gamma1 = sum((x[2:n] - mean(x)) * (x[1:(n-1)] - mean(x))) / n # 1次の自己共分散 
gamma2 = sum((x[3:n] - mean(x)) * (x[1:(n-2)] - mean(x))) / n # 2次の自己共分散 
a = solve(matrix(c(gamma0, gamma1, gamma1, gamma0), nrow=2, ncol=2)) %*% c(gamma1, gamma2)
a
(gamma0 - a[1] * gamma1 - a[2] * gamma2) / (n - 3) * n 


# 最小二乗法

X = cbind(1, x[2:(n-1)]-mean(x),x[1:(n-2)]- mean(x))
y = x[3:n] - mean(x)

a = solve(t(X) %*% X) %*% t(X) %*% y
a
sigma2 = sum((y - X %*% a)^2) / (n - 2) 
sigma2
sqrt(solve(t(X) %*% X)[1, 1] * sigma2)

