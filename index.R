# =================================[데이터 전처리]===================================
# Data set 읽기 
dataSet <- read.csv("anime-dataset.csv", header = TRUE)
head(dataSet)

# Type이 TV인행만 조회 후 할당
dataSet <- subset(dataSet, Type=="TV")
head(dataSet)

# 결측값 개수 확인
# 0이 출력 하지만 csv파일 다운받을때 UNKNOWN값 존재했으므로 처리해야한다.
sum(is.na(dataSet))

# 중간 행 수 조회
nrow(dataSet)

# Tv행만 조회 할때 처럼 다시 필터링 하지만 != 조건를을 사용해 필터링
dataSet <- subset(dataSet, Name !="UNKNOWN") # 애니메이션 이름
dataSet <- subset(dataSet, Episodes !="UNKNOWN") # 에피소드 수 
dataSet <- subset(dataSet, Score !="UNKNOWN") # 평점

# UNKNOWN 필터링 후 중간 행 수 조회(차이 존재!)
nrow(dataSet)

# 애니이름, 에피소드 수, 평점 조회해 새로운 데이터 프레임 생성
dataSet <- data.frame(dataSet$Name, dataSet$Episodes, dataSet$Score)
colnames(dataSet) <-  c("Name","Episodes","Score")
head(dataSet)

# 그럼 지금까지 타입이 TV인 행의 애니이름, 에피소드 수, 평점을 얻음


# 열 이름 확인
colnames(dataSet)
class(dataSet[, 1]) # 이름 
class(dataSet[, 2]) # 에피소드
class(dataSet[, 3]) # 평점

# 하지만 타입을 전부 확인해보니 전부다 character형으로
# 존재함 DB에 저장하고 사용하기 쉽게 할려면 형변환 필요하다.
dataSet$Episodes <- as.double(dataSet$Episodes) # 에피소드
dataSet$Score <- as.double(dataSet$Score) # 평점
class(dataSet[, 2]) # 에피소드
class(dataSet[, 3]) # 평점
# =============================================================================


# ============================[DB 연동]==================================
# DB정보 입력
library(DBI)
con = dbConnect(
  drv = RMySQL::MySQL(),
  user = 'root',
  password = 'rlatmdgus0926',
  dbname = 'anime-Episodes',
  encoding = 'utf8'
)
# dbname은 스키마 이름인데 미리 생성을 해줘야합니다. 

encoding = if(grepl(pattern = 'utf-8|utf-8',
                    x = Sys.getlocale(), ignore.case = T)) 'utf8' else 'euckr'
dbGetQuery(con,paste("SET names", encoding))
dbGetQuery(con,paste0("SET SESSION character_set_server=",encoding))
dbGetQuery(con,paste0("SET SESSION character_set_database=",encoding))

# 데이터를 DB에 직접 저장이 가능하게 설정
dbSendQuery(con,
            "SET GLOBAL local_infile = TRUE;"
)

# 테이블 생성
# 테이블 컬럼 이름은 "Name", "Episodes", "Score" 3개로 생성
dbSendQuery(con,
            "CREATE TABLE anime(
            Name varchar(255),
            Episodes double,
            Score double)")



# anime테이블에 dataSet데이터를 넣는 코드
# overwrite = TRUE: 이미 존재하는 테이블에 데이터 추가하겠다는 의미
# row.names = FALSE: 새로운 열로 추가하지 않겠다는 의미
dbWriteTable(con, "anime", dataSet,
             overwrite = TRUE, row.names = FALSE
)
# 반환이 TRUE라면 성공적
# 에러나 flase일 경우 실패

# 쿼리를 보내고 결과를 sql_dataSet에 저장
sql_dataSet <- dbSendQuery(con, "SELECT * FROM anime")

# sql_dataSet데이터를 데이터 프레임으로 가져와 DBdataSet에 저장합니다.
DBdataSet <- dbFetch(sql_dataSet, n = -1)

# 최종 데이터 확인 및 DB연동 해제
DBdataSet
dbDisconnect(con)

# ================================[정규화]===================================

# 정규화 시작
DBdataSet

# 데이터 형식 확인
class(DBdataSet)

# 데이터 열 이름과 타입 확인
class(DBdataSet[, 1]) # 이름
class(DBdataSet[, 2]) # 에피소드
class(DBdataSet[, 3]) # 평점

# 데이터 정규화(에피소드, 평점)
std <- function(x){
  return((x-min(x)) /(max(x)-min(x)))
}
animeData <- apply(DBdataSet[, c(2, 3)], 2, std)

# 정규화 확인 후 데이터 프레임으로 변환
# $를 위해선 데이터프레임으로 변경을 해야한다. 
animeData
animeData <- as.data.frame(animeData)
colnames(animeData) <-  c("Episodes","Score")
animeData

# 그럼 이름과 방금 정규화 한 것을 합쳐서 하나의 데이터프레임으로 만들어줘야함
animeData <- data.frame(DBdataSet$Name, animeData$Episodes, animeData$Score)
colnames(animeData) <-  c("Name","Episodes","Score")
animeData

# =================================[군집 분석]====================================

# DB에서 가져온 데이터를 animeData에 저장
animeData <- DBdataSet

# 3가지 종류의 군집으로 만듬
library(cluster)
animeDataKmeans <- kmeans(x=DBdataSet[,"Episodes"], centers = 3)
animeDataKmeans
plot(animeData$Episodes, animeData$Score, col= c("red","blue","green")[animeDataKmeans$cluster])
# 정규화된 값으로 그래프가 잘 분포되어있음을 확인

clusplot(animeData[,2:3], animeDataKmeans$cluster, col.p = animeDataKmeans$cluster)
# 3개의 군집을 확인하고 어떻게 3군집이 나뉘어있는지 그래프로 확인
# 주의: 분석마다 군집의 번호가 달라지니 확인이 필요

# 군집번호가 추가된 데이터프레임 생성
animeDataNew <- data.frame(animeData$Name, animeData$Episodes, animeData$Score, animeDataKmeans$cluster)
colnames(animeDataNew) <-  c("Name","Episodes","Score","Cluster")
animeDataNew

# 그래프를 보면 3가지 종류의 군집으로 나뉘어져있다.
# 에피소드가 짧은 군집
# 에피소드가 어느정도 있는 군집
# 에피소드가 많은 군집

# 시각적으로 봤을 때는 제일 많이 분포되어있는게 에피소가 짧은 군집이다.
# 그 다음은 에피소드가 어느정도 있는 군집이다.
# 마지막은 에피소드가 많은 군집이다.

# 하지만 분석마다 군집의 번호가 달라져 달라진 군집번호를 알아야한다. 

# 그럼 cluster의 개수마다 행을 조회해 행이 제일 많은 cluster의 번호가 에피소드 짧은 애니메이션들의
# 군집일 것이다.
# 그 다음으로 많은 건 에피소드가 어느정도 있는 군집.
# 마지막은 에피소드가 많은 군집이다.

# cluster의 개수마다 행을 조회
sum(animeDataNew$Cluster == 3)
sum(animeDataNew$Cluster == 2)
sum(animeDataNew$Cluster == 1)

# 제가 지금 테스트 한 결과 
# 3번: 4369 -> 짧은양의 에피소드 군집 번호는 3
# 2번: 80 -> 중간양의 에피소드 군집 번호는 2
# 1번: 9 -> 많은양의 에피소드 군집 번호는 1

# 그럼 이제는 각 군집에 대한 분석을 해보겠습니다. 

cor_cluster1 <- cor(animeDataNew$Episodes[animeDataNew$Cluster == 1], 
                    animeDataNew$Score[animeDataNew$Cluster == 1])
cor_cluster2 <- cor(animeDataNew$Episodes[animeDataNew$Cluster == 2], 
                    animeDataNew$Score[animeDataNew$Cluster == 2])
cor_cluster3 <- cor(animeDataNew$Episodes[animeDataNew$Cluster == 3], 
                    animeDataNew$Score[animeDataNew$Cluster == 3])

# 에피소드 수가 약간 증가할 때 평점도 약간 증가하는 경향
cat("1번 군집 분석:", cor_cluster1, "\n") # 0.1822109 

# 에피소드 수가 증가할수록 평점이 약간 감소하는 경향
cat("2번 군집 분석:", cor_cluster2, "\n") # -0.2963907 

# 에피소드 수와 평점 간에 거의 아무런 관계가 없음
cat("3번 군집 분석:", cor_cluster3, "\n") # 0.03294024 

# 분석 기준
# 1에 가까울수록: 강한 양의 상관관계 (한 변수가 증가하면 다른 변수도 강하게 증가).
# -1에 가까울수록: 강한 음의 상관관계 (한 변수가 증가하면 다른 변수는 강하게 감소).
# 0에 가까울수록: 상관관계가 거의 없음 (두 변수가 서로 독립적으로 움직임).

# 최종설명
# 에피소드가 많은 애니메이션들은 평점이 증가하고 중간양의 에피소드는 평점이 약간 감소하는 경향이 있다
# 그리고 에피소드가 적은 애니메이션은 거의 독립적이라고 생각하면된다.
# 즉 내가 원하고자 한 내용은 "에피소드가 많을수록 평점이 높은가?"이다. 결론은 그렇다. 라고 결론이 난다.

# =======================================================================

# 참고: 예측하는 프로그램을 만들떄는 데이터를 가져와 어떤 군집에 해당하는가를 정한 후 예측을 해야합니다.!

# =======================[군집마다 선형회기분석을 위한 회기식 구하기]=========================
# Cluster 1에 대한 선형 회귀
lm_cluster1 <- lm(Score ~ Episodes, data = animeDataNew[animeDataNew$Cluster == 1, ])
coef(lm_cluster1)[1] # 절편
coef(lm_cluster1)[2] # 기울기

# Cluster 2에 대한 선형 회귀
lm_cluster2 <- lm(Score ~ Episodes, data = animeDataNew[animeDataNew$Cluster == 2, ])
coef(lm_cluster2)[1] # 절편
coef(lm_cluster2)[2] # 기울기

# Cluster 3에 대한 선형 회귀
lm_cluster3 <- lm(Score ~ Episodes, data = animeDataNew[animeDataNew$Cluster == 3, ])
coef(lm_cluster3)[1] # 절편
coef(lm_cluster3)[2] # 기울기

# 회기식: Score = 절편(Intercept) + (군집에 해당하는 기울기 * Episodes)
#  =================================================================================

# ===================================[예측 로직]================================

# 애니메이션 데이터프레임 생성
newdata <- data.frame(
  Name = "빅데이터",
  Episodes = 30,
  Score = 0 
)
# 참고: kmeans분석시 NA값이 존재하는 행은 자동 삭제되므로 Score를 0으로 추가
# 참고: Score는 군집분석때 사용되지 않으므로 값은 관계없음

# 데이터프레임을 테이블에 삽입 (append = TRUE로 기존 데이터에 추가)
dbWriteTable(con, "anime", newdata, append = TRUE, row.names = FALSE)

# 전체 데이터를 가져옴
sql_dataSet_new <- dbSendQuery(con, "SELECT * FROM anime")
DBdataSetNew <- dbFetch(sql_dataSet_new, n = -1)
nrow(DBdataSetNew)

# 에피소드 기준으로 군집 3개를 나눈다.
library(cluster)
animeDataKmeansNew <- kmeans(x=DBdataSetNew[,"Episodes"], centers = 3)
animeDataKmeansNew

# 군집번호를 포함한 데이터프레임 생성
animeDataNew <- data.frame(DBdataSetNew$Name, DBdataSetNew$Episodes, DBdataSetNew$Score, animeDataKmeansNew$cluster)
colnames(animeDataNew) <-  c("Name","Episodes","Score","Cluster")
animeDataNew

# DB에서 가져온 에피소드의 행만 얻기위한 코드
resultNewData <- subset(animeDataNew, Score == 0) # 에피소드 수 
resultNewData
# 참고: 군집번호는 달라질 수 있음.

# n번 군집의 기울기를 구하고 예측 (lm함수 내부에 숫자값 군집 번호에 맞게 변경)
lm_cluster <- lm(Score ~ Episodes, data = animeDataNew[animeDataNew$Cluster == 2, ])
coef(lm_cluster)[1] # 절편
coef(lm_cluster)[2] # 기울기

# Score = 절편(Intercept) + (군집에 해당하는 기울기 * Episodes)
Score_n <-  coef(lm_cluster)[1] + (coef(lm_cluster)[2] * resultNewData$Episodes)
Score_n

cat("에피소드의 수가",resultNewData$Episodes,"일때 예측된 평점은:", Score_n,"점 입니다.")

# 최종 업데이트(다음 예측을 위해)

# 기존 점수가 0인 행을 삭제
dbSendQuery(con,"DELETE FROM anime WHERE Score = 0")

# 완성된 데이터 프레임을 만듦
newdata <- data.frame(
  Name = resultNewData$Name, # 방금전 새로운 애니메이션 이름을 할당
  Episodes = resultNewData$Episodes, # 방금전 새로운 애니메이션 에피소드를 할당
  Score = Score_n # 예측된 평점을 할당
)
newdata

# 데이터프레임을 테이블에 추가
dbWriteTable(con, "anime", newdata, append = TRUE, row.names = FALSE)
# ====================================================================================

# DB연동해제
dbDisconnect(con)

# 코드 실행시 참고 사항
# dbSendQuery같은 함수를 사용하고 에러 발생시 dbDisconnect(con)를 사용해 해재 후
# 다시 연동코드(49~67)를 실행하고 테스트 해야한다. 
