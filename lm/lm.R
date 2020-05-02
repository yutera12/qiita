mtcars # Rにもともと入っているデータセット
# 線形回帰を実施（目的変数＝mpg、説明変数=それ以外）
result = lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, mtcars)
summary(result) # 線形回帰の結果を表示

### CoefficientsのEstimate
X = mtcars[, colnames(mtcars)!='mpg']
Intercept = 1
X = as.matrix(cbind(Intercept, X))
y = mtcars[, 'mpg']
beta = solve(t(X) %*% X) %*% t(X) %*% y 
beta

### Residuals
epsilon = y - X %*% beta
quantile(epsilon)

### Residual standard error
n = nrow(X)
p = ncol(X)
sigma2 = sum(epsilon ** 2) / (n - p)
sqrt(sigma2)
n - p

### CoefficientsのStd. Error
var_beta = diag(sigma2 * solve(t(X) %*% X))
sqrt(var_beta)

### Coefficientsのt value
t_value = beta / sqrt(var_beta)
t_value

### CoefficientsのPr(>|t|) 
pt(abs(t_value), n - p, lower.tail = FALSE) * 2

### Multiple R-squared
R2 = 1 - sum(epsilon ** 2) / sum((y - mean(y)) ** 2)
R2

### Adjusted R-squared
aR2 = 1 - (sum(epsilon ** 2) / (n - p)) / (sum((y - mean(y)) ** 2) / (n - 1))
aR2

### F-statistic
F_value = (sum((X %*% beta - mean(y)) ** 2) / (p - 1)) / (sum((y - X %*% beta) ** 2) / (n - p))
F_value

### p-value
pf(F_value, p - 1, n - p, lower.tail = FALSE)

