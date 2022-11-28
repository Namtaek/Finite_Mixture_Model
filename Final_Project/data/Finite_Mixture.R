library(tidyverse)
library(factoextra)
library(cluster) 
library(mclust)
library(fpc)

data1 = read.csv("data_per_match.csv")
data2 = read.csv("data_advanced.csv")
data1 %>% dim()
data2 %>% dim()

data1 %>% colnames
data2 %>% colnames

## 비율스텟 기준으로 결합

data2$MPP = data2$MP
data1 = data1 %>% select(-Player.additional)
data2 = data2 %>% select(-X.1, -X, -Player.additional)

data = data1 %>% cbind(data2[, c(8:28)])
#data %>% write.csv("data_final.csv")

#데이터 중복 생략

# 여기서부터

data = read.csv("data_final.csv")
data %>% dim

## 가비지 멤버 기준 확인 후 삭제

data %>% 
  ggplot(aes(MPP, y = ..density..)) +
  geom_histogram(bins = 70)+
  geom_density(color = "#00BA38", fill = "#00BA38", adjust = 0.3, alpha = 0.2)

data[data$MPP < 300, ] %>% 
  ggplot(aes(MPP, y = ..density..)) +
  geom_histogram(bins = 40)+
  geom_density(color = "#00BA38", fill = "#00BA38", adjust = 0.3, alpha = 0.2) + 
  theme(
        #axis.ticks.x=element_blank(), 
        #axis.ticks.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=16), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.title = element_blank(), legend.text = element_text(size=26), legend.position="bottom",
        legend.box = "horizontal", legend.key = element_blank())


data = data %>% filter(MPP >= 200)
data %>% dim
## 일부 컬럼 삭제

data %>% colnames
is.na(data) %>% sum
data[is.na(data)] = 0  # 센터처럼 3점 안던지면 NA 발생 -> 0으로 대체

data = data %>% select(-Rk, -Age, -Tm, -G, -GS, -MP, -MPP,  # 정보 컬럼
                -FG, -FGA, -X3P, -X3PA, -X2P, -X2PA, -FT, -FTA, -ORB, -DRB, -FT., -FTr,  # 공격 수비 지표
                -OWS, -DWS, -WS, -OBPM, -DBPM) # 개인 지표

data %>% colnames
data %>% glimpse
data %>% dim

data$Pos %>% table

## 차원 축소 - 공격, 수비, 추가지표

data_off = data %>% select(FG., X3P., X2P., eFG., AST, PTS, TS., USG.)
data_def = data %>% select(PF, TRB., STL., BLK., TOV.)
data_sec = data %>% select(PER, WS.48, BPM, VORP)
data_some = data %>% select(USG., PF, TOV., BPM)

off_PC = data_off %>% prcomp(scale. = T)
fviz_eig(off_PC)
fviz_pca_var(off_PC,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
off_PC$rotation


def_PC = data_def %>% prcomp(scale. = T)
fviz_eig(def_PC)
fviz_pca_var(def_PC,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
def_PC$rotation

sec_PC = data_sec %>% prcomp(scale. = T)
fviz_eig(sec_PC)
fviz_pca_var(sec_PC,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
sec_PC$rotation


data_PCA = 
  data[, 1:2] %>% cbind(-off_PC$x[, 1]) %>% cbind(off_PC$x[, 1]) %>% 
  cbind(-def_PC$x[, 1]) %>% cbind(-def_PC$x[, 2]) %>% cbind(sec_PC$x[, 1])
rownames(data_PCA) = data_PCA$Player
colnames(data_PCA) = c("player", "Pos","off_PC1", "off_PC2", "def_PC1", "def_PC2", "sec_PC1")

kmeans_PCA3 = kmeans(data_PCA[, -(1:2)], 3)
fviz_cluster(kmeans_PCA3, data = data_PCA[, -(1:2)])


data_total_PCA = data_PCA[, -(1:2)] %>% prcomp()
data_total_PCA



silhouette_score <- function(k){
  km <- kmeans(data_PCA[, 3:7], centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(data_PCA[, 3:7]))
  mean(ss[, 3])
}

k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

kmeans_PCA3 = kmeans(data_PCA[, -(1:2)], 3)
fviz_cluster(kmeans_PCA3, data = data_PCA[, -(1:2)])

x## GMM으로!

(hc1 <- hc(data_PCA[, 3:7], modelName = "VVV", use = "SVD"))
# 초기값과 군집개수를 설정하기 위한 방법법
clust_result_bic = data_PCA[, 3:7] %>% mclustBIC(initialization = list(hcPairs = hc1))
plot(clust_result_bic)
clust_result_bic %>% summary

mod <- Mclust(data_PCA[, 3:7], x = clust_result_bic)
plot(mod, what = "classification")
plot(mod, what = "uncertainty")
gaussian_result = mod$classification
gaussian_result %>% table   

cs = cluster.stats(dist(data_PCA[, 3:7]), gaussian_result)
cs[c("within.cluster.ss","avg.silwidth")]
## 실루엣 0.36

remove_idx = which((abs(data_PCA$off_PC1) < 4.5) & (abs(data_PCA$sec_PC1) < 4.6))
player_name_text = data$Player
player_name_text[remove_idx] = ""

gmm_df = data.frame(data_PCA$off_PC1, data_PCA$sec_PC1, factor(gaussian_result))
colnames(gmm_df) = c('공격지표', '이차지표', 'cluster')
gmm_df %>% head


# 가우시안 믹스쳐 클러스터링 결과 시각화
gmm_df %>% 
  ggplot(aes(x = 공격지표, y = 이차지표)) +
  geom_point(aes(col = cluster), lwd = 2) + 
  stat_ellipse(aes(col = cluster), type = 'norm', lty = 2, lwd = 1) + 
  geom_text(aes(x = 공격지표, y = 이차지표, label = player_name_text)) + 
  #stat_ellipse(aes(col = cluster), geom = 'polygon', alpha = 0.1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 7, face = "bold", colour = "grey50"), 
        axis.text.y = element_text(size = 7, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(face = "bold", size = 10)) + 
  labs(x = '공격지표', y = "2차지표", title = '가우시안혼합모형 클러스터링 결과')


### 각 포지션에 대해서

data$Pos %>% table
data %>% filter(Pos %in% c("C-PF", "PF-C", "PF-SF", "SF-SG", "SG-PG-PF", "SG-SF", "SG-PG"))

center_PCA = data_PCA %>% filter(Pos %in% c("C", "C-PF", "PF-C"))
forward_PCA = data_PCA %>% filter(Pos %in% c("C-PF", "PF", "PF-C", "PF-SF", "SF", "SF-SG", "SG-PG-PF", "SG-SF"))
guard_PCA = data_PCA %>% filter(Pos %in% c("SF-SG", "SG-PG-PF", "SG-SF", "SG-PG", "SG", "PG"))

# Center

(hc_center <- hc(center_PCA[, c(3, 7)], modelName = "VVV", use = "SVD"))
# 초기값과 군집개수를 설정하기 위한 방법법
clust_center_result_bic = center_PCA[, c(3, 7)] %>% mclustBIC(initialization = list(hcPairs = hc_center))
plot(clust_center_result_bic)
clust_center_result_bic %>% summary

mod_center <- Mclust(center_PCA[, c(3, 7)], x = clust_center_result_bic)
plot(mod_center, what = "classification")
plot(mod_center, what = "uncertainty")
gaussian_center_result = mod_center$classification
gaussian_center_result %>% table   

cs_center = cluster.stats(dist(center_PCA[, c(3,7)]), gaussian_center_result)
cs_center[c("within.cluster.ss","avg.silwidth")]
## 실루엣 0.42

remove_idx = which((center_PCA$off_PC1 < 1.0) & (center_PCA$off_PC1 > -5.5) & 
                     (center_PCA$sec_PC1 < 3.5) & (center_PCA$sec_PC1 > -1.2))
player_name_text = center_PCA$player
player_name_text[remove_idx] = ""

gmm_center_df = data.frame(center_PCA$off_PC1, center_PCA$sec_PC1, factor(gaussian_center_result))
colnames(gmm_center_df) = c('공격지표', '이차지표', 'cluster')

# 가우시안 믹스쳐 클러스터링 결과 시각화
gmm_center_df %>% 
  ggplot(aes(x = 공격지표, y = 이차지표)) +
  geom_point(aes(col = cluster), lwd = 2) + 
  stat_ellipse(aes(col = cluster), type = 'norm', lty = 2, lwd = 1) + 
  geom_text(aes(x = 공격지표, y = 이차지표, label = player_name_text), size = 6, vjust = 1.5) + 
  #stat_ellipse(aes(col = cluster), geom = 'polygon', alpha = 0.1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        plot.title = element_text(face = "bold", size = 30),
        axis.text.x = element_text(size = 15, face = "bold", colour = "grey50"), 
        axis.text.y = element_text(size = 15, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(face = "bold", size = 10)) + 
  labs(x = '공격1지표', y = "공헌도지표", title = '센터 포지션에 대한 가우시안혼합모형 클러스터링 결과')

center_PCA %>% 
  mutate(cluster_label = gaussian_center_result) %>% 
  group_by(cluster_label) %>% 
  summarize(mean_offPC1 = off_PC1 %>% mean,
            mean_offPC2 = off_PC2 %>% mean,
            mean_defPC1 = def_PC1 %>% mean,
            mean_defPC2 = def_PC2 %>% mean,
            mean_secPC1 = sec_PC1 %>% mean,
            numter = n())

center_PCA[gaussian_center_result == 1, ]
center_PCA[gaussian_center_result == 2, ]
center_PCA[gaussian_center_result == 3, ]


## Forward

(hc_forward <- hc(forward_PCA[, c(4, 7)], modelName = "VVV", use = "SVD"))
# 초기값과 군집개수를 설정하기 위한 방법법
clust_forward_result_bic = forward_PCA[, c(4,7)] %>% mclustBIC(initialization = list(hcPairs = hc_forward))
plot(clust_forward_result_bic)
clust_forward_result_bic %>% summary

mod_forward <- Mclust(forward_PCA[, c(4, 7)], x = clust_forward_result_bic)
plot(mod_forward, what = "classification")
plot(mod_forward, what = "uncertainty")
gaussian_forward_result = mod_forward$classification
gaussian_forward_result %>% table   

cs_forward = cluster.stats(dist(forward_PCA[, c(4,7)]), gaussian_forward_result)
cs_forward[c("within.cluster.ss","avg.silwidth")]
## 실루엣 0.46

remove_idx = which((forward_PCA$off_PC2 < 2.5) & (forward_PCA$off_PC2 > -3.0) & 
                     (forward_PCA$sec_PC1 < 3.2) & (forward_PCA$sec_PC1 > -3.0))
player_name_text = forward_PCA$player
player_name_text[remove_idx] = ""

gmm_forward_df = data.frame(forward_PCA$off_PC2, forward_PCA$sec_PC1, factor(gaussian_forward_result))
colnames(gmm_forward_df) = c('공격지표', '이차지표', 'cluster')

# 가우시안 믹스쳐 클러스터링 결과 시각화
gmm_forward_df %>% 
  ggplot(aes(x = 공격지표, y = 이차지표)) +
  geom_point(aes(col = cluster), lwd = 2) + 
  stat_ellipse(aes(col = cluster), type = 'norm', lty = 2, lwd = 1) + 
  geom_text(aes(x = 공격지표, y = 이차지표, label = player_name_text), size = 6, vjust = 1.5) + 
  #stat_ellipse(aes(col = cluster), geom = 'polygon', alpha = 0.1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        plot.title = element_text(face = "bold", size = 30),
        axis.text.x = element_text(size = 15, face = "bold", colour = "grey50"), 
        axis.text.y = element_text(size = 15, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(face = "bold", size = 10)) + 
  labs(x = '공격2지표', y = "공헌도지표", title = '포워드 포지션에 대한 가우시안혼합모형 클러스터링 결과')

forward_PCA %>% 
  mutate(cluster_label = gaussian_forward_result) %>% 
  group_by(cluster_label) %>% 
  summarize(mean_offPC1 = off_PC1 %>% mean,
            mean_offPC2 = off_PC2 %>% mean,
            mean_defPC1 = def_PC1 %>% mean,
            mean_defPC2 = def_PC2 %>% mean,
            mean_secPC1 = sec_PC1 %>% mean,
            number = n())

forward_PCA[gaussian_forward_result == 1, ]
forward_PCA[gaussian_forward_result == 2, ]



## guard

(hc_guard <- hc(guard_PCA[, c(5, 6, 7)], modelName = "VVV", use = "SVD"))
# 초기값과 군집개수를 설정하기 위한 방법법
clust_guard_result_bic = guard_PCA[, c(5,6,7)] %>% mclustBIC(initialization = list(hcPairs = hc_guard))
plot(clust_guard_result_bic)
clust_guard_result_bic %>% summary

mod_guard <- Mclust(guard_PCA[, c(5,6, 7)], x = clust_guard_result_bic)
plot(mod_guard, what = "classification")
plot(mod_guard, what = "uncertainty")
gaussian_guard_result = mod_guard$classification
gaussian_guard_result %>% table   

cs_guard = cluster.stats(dist(guard_PCA[, c(5,6, 7)]), gaussian_guard_result)
cs_guard[c("within.cluster.ss","avg.silwidth")]
## 실루엣 0.36

remove_idx = which((guard_PCA$sec_PC1 < 3.4) & (guard_PCA$sec_PC1 > -3.0) & 
                     (guard_PCA$def_PC2 < 2.5) & (guard_PCA$def_PC2 > -1.5))
player_name_text = guard_PCA$player
player_name_text[remove_idx] = ""

gmm_guard_df = data.frame(guard_PCA$def_PC2, guard_PCA$sec_PC1, factor(gaussian_guard_result))
colnames(gmm_guard_df) = c('공격지표', '이차지표', 'cluster')

# 가우시안 믹스쳐 클러스터링 결과 시각화
gmm_guard_df %>% 
  ggplot(aes(x = 공격지표, y = 이차지표)) +
  geom_point(aes(col = cluster), lwd = 2) + 
  stat_ellipse(aes(col = cluster), type = 'norm', lty = 2, lwd = 1) + 
  geom_text(aes(x = 공격지표, y = 이차지표, label = player_name_text), size = 6, vjust = 1.5) + 
  #stat_ellipse(aes(col = cluster), geom = 'polygon', alpha = 0.1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        plot.title = element_text(face = "bold", size = 30),
        axis.text.x = element_text(size = 15, face = "bold", colour = "grey50"), 
        axis.text.y = element_text(size = 15, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(face = "bold", size = 10)) + 
  labs(x = '수비2지표', y = "공헌도지표", title = '가드 포지션에 대한 가우시안혼합모형 클러스터링 결과')

guard_PCA %>% 
  mutate(cluster_label = gaussian_guard_result) %>% 
  group_by(cluster_label) %>% 
  summarize(mean_offPC1 = off_PC1 %>% mean,
            mean_offPC2 = off_PC2 %>% mean,
            mean_defPC1 = def_PC1 %>% mean,
            mean_defPC2 = def_PC2 %>% mean,
            mean_secPC1 = sec_PC1 %>% mean,
            number = n())

guard_PCA[gaussian_guard_result == 1, ]
guard_PCA[gaussian_guard_result == 2, ]
guard_PCA[gaussian_guard_result == 3, ]







