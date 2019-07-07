# 提取出货序列
q = matrix(0,400,1) 
q[c(2,7,10,14,22,24,25,44,46,55,57,60,75,81,83,
    101,104,109,114,116,117,127,133,135,138,141,142,149,153,157,
    164,170,171,179,180,186,202,203,208,209,211,215,217,
    221,225,229,234,242,258,265,276,280,292,295,296,
    304,306,311,327,332,339,346,348,349,351,359,361,363,
    366,367,361,376,389,392,400)] = 1
# 记录 n = 0, ... 9, 10种情况
p = matrix(0,10,2)

for( n in 1:10){
  assign(paste0('s',n),c(1,rep(0,n-1)))
}

for( n in 1:10){
  for( i in 1:(length(q)-n-1) ){
    # 考察另一个定义, 考察是否存在序列
    #  [1,(n-1)个0, n = 1,2,3...] 
    if( paste(as.character(q[i:(i+n-1)]),collapse = '') == 
        paste(get(paste0('s',n)),collapse = '') ){
      p[n,2] = p[n,2]+1
      if( q[(i+n)] == 1 ){
        # 进一步考察是否是序列
        # [1,(n-1)个0, 1]
        p[n,1] = p[n,1]+1
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

# p-value 矩阵（均不显著）
# 因为均值差值在1个标准差内就明显不显著了…
# 写一个p值矩阵等某一天排的上用场时候用吧
p.value = matrix(0,10,10)
for(i in 1:10){
  for(j in 1:10){
    p.value[i,j] = prop.test( c(p[i,1],p[j,1]),c(p[i,2],p[j,2]))$p.value
  }
}

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
