# 读取数据
library(readxl)
MS <- read_excel("MS.xlsx")
# 提取出货序列
q = as.matrix(MS[,3])

# 记录 n = 0, ... 9, 10种情况
p = matrix(0,10,2)

# n = 0 时，即 总加成/总样本
p[1,1] = sum(q)
p[1,2] = length(q)

# n = 1,...,9
for( n in 1:9){
  # 考虑到单次序列长度，调整样本
  # e.g.  n = 2时，如果 79和80次是 [0,0]
  #      我们没法考察81次的 [0,0,1] or [0,0,0]
  for( i in 1:(80-n-1) ){
    # 查看连续 n 次为出货的情况
    # e.g. n = 1 时，即 [0] 为总样本序列， [0,1] 为加成序列
    #      n = 2 时，即 [0,0] 为总样本序列， [0,0,1] 为加成序列
    if( sum(q[(i:(i+n-1))]) == 0){
      # 记录样本序列数
      p[(n+1),2] = p[(n+1),2]+1
      # 查看是否有加成序列
      if( q[(i+n)] == 1 ){
        # 记录加成序列
        p[(n+1),1] = p[(n+1),1]+1
      }
    }
  }
}

# 频率派方式
p = cbind(p,matrix(0,10,2))
p[,3] = p[,1]/p[,2]
p[,4] = sqrt( p[,3]*(1-p[,3])/p[,2])
rownames(p) = c('0次保底','1次保底','2次保底','3次保底','4次保底','5次保底',
                '6次保底','7次保底','8次保底','9次保底')
colnames(p) = c('加成次数','总数','出货率','标准差')
# 结果矩阵
round(p,4)


# 贝叶斯方式
# 为保证可重复性，确定seed
set.seed(1234)
# 为计算 \Delta_{i,j} 采样
samples = matrix(0,10000,10)
for( i in 1:10){
  samples[,i] = rbeta( 10000 , 0.001 + p[i,1], 0.001 + p[i,2] - p[i,1])
}

# 记录HPD interval
library(MCMCpack)
L = matrix(0,10,10)
U = matrix(0,10,10)
rownames(L) = 0:9
rownames(U) = 0:9
colnames(L) = 0:9
colnames(U) = 0:9
for( i in 1:10){
  for(j in 1:10){
    L[i,j] = HPDinterval( as.mcmc( samples[,i] - samples[,j]))[1]
    U[i,j] = HPDinterval( as.mcmc( samples[,i] - samples[,j]))[2]
  }
}

# 结果矩阵
round(L,2)
round(U,2)
(L*U)<=0
