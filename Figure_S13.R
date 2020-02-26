rm(list = ls())

setwd("E:\\project\\urine\\new3\\beta")


library('ggplot2')


prof <- as.data.frame(read.csv('file:///E:/CRTdata/weighted_unifrac_distance_matrix.tsv',head=T,row.names = 1,sep='\t'))
map <- as.data.frame(read.csv('file:///E:/project/urine/new/hclust-barplot/data/map.age.txt',head=T,row.names = 1,sep='\t'))

prof <- prof[rownames(map),rownames(map)] 

T1.M.prof <- prof[map$Time=="T1"&map$Gender=="M",map$Time=="T1"&map$Gender=="M"]
T1.F.prof <- prof[map$Time=="T1"&map$Gender=="F",map$Time=="T1"&map$Gender=="F"]
T1.FM.prof <- prof[map$Time=="T1"&map$Gender=="F",map$Time=="T1"&map$Gender=="M"]

T2.M.prof <- prof[map$Time=="T2"&map$Gender=="M",map$Time=="T2"&map$Gender=="M"]
T2.F.prof <- prof[map$Time=="T2"&map$Gender=="F",map$Time=="T2"&map$Gender=="F"]
T2.FM.prof <- prof[map$Time=="T2"&map$Gender=="F",map$Time=="T2"&map$Gender=="M"]

T3.M.prof <- prof[map$Time=="T3"&map$Gender=="M",map$Time=="T3"&map$Gender=="M"]
T3.F.prof <- prof[map$Time=="T3"&map$Gender=="F",map$Time=="T3"&map$Gender=="F"]
T3.FM.prof <- prof[map$Time=="T3"&map$Gender=="F",map$Time=="T3"&map$Gender=="M"]

T4.M.prof <- prof[map$Time=="T4"&map$Gender=="M",map$Time=="T4"&map$Gender=="M"]
T4.F.prof <- prof[map$Time=="T4"&map$Gender=="F",map$Time=="T4"&map$Gender=="F"]
T4.FM.prof <- prof[map$Time=="T4"&map$Gender=="F",map$Time=="T4"&map$Gender=="M"]


##  T1阶段

pdf(file = "beta.weighted.T.pdf",useDingbats = F)
boxplot(
  as.numeric(as.dist(T1.M.prof)),
  as.numeric(as.dist(T1.F.prof)),
  as.numeric(as.dist(T2.M.prof)),
  as.numeric(as.dist(T2.F.prof)),
  as.numeric(as.dist(T3.M.prof)),
  as.numeric(as.dist(T3.F.prof)),
  as.numeric(as.dist(T4.M.prof)),
  as.numeric(as.dist(T4.F.prof)),
  col = 2:3,
  
  names = c(
    "T1.M",
    "T1.F",
    "T2.M",
    "T2.F",
    "T3.M",
    "T3.F",
    "T4.M",
    "T4.F"
  )
  # horiz = TRUE
)
dev.off()



