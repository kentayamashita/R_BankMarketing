data<- read.csv("bank_marketing_train.csv")
summary(data)
head(data)
hist(data$age)

boxplot(age~y,data = data)

job_tab <- table(data$job,data$y)
##クロス集計（カテゴリごとのY/N）
job_tab[,1]
job_tab[,2]
job_yratio <- job_tab[,2]/(job_tab[,1]+job_tab[,2])
plot(job_yratio)
barplot(job_yratio)

#bd_age_y <- (bank_data$age, )
#boxplot(marital~y,data = data)


def_tab <- table(data$default,data$y)
def_yratio <- def_tab[,2]/(def_tab[,1]+def_tab[,2])
barplot(def_yratio)
def_yratio

loan_tab <- table(data$loan,data$y)
loan_yratio <- loan_tab[,2]/(loan_tab[,1]+loan_tab[,2])
barplot(loan_yratio)
loan_yratio

dow_tab <- table(data$day_of_week,data$y)
dow_yratio <- dow_tab[,2]/(dow_tab[,1]+dow_tab[,2])
barplot(dow_yratio)
dow_yratio

hou_data <- data[data$housing=="yes",]
hl_tab <- table(hou_data$loan,hou_data$y)
hl_tab

###RandomForestやってみる
library(randomForest)
myrf<-randomForest(y~job+age+contact,
             data=data,
             ntree=100,
             sampsize=c(1000,1000)
             )
##使い方を見たい場合
?randomForest
##勝手に裏で予測してConfusion Matrixを作成してくれている
myrf
##sampsizeはYから1000個、Nから1000個、という形でサンプリングできる
## ->Yの確率が少ないと、すべてNと答えたほうが確立が高くなるため、それを防止

myrf_score<- predict(myrf, newdata = data, type = "prob")
##prob = probability,確率,RFのときのみ
##responseとするとY/Nとなる

myrf_score[,2]
##yesだけを表示
varImpPlot(myrf)
##特徴量の効き方ごとのグラフ
##重要度の定義：MeanDecreaseGini
##->決定木で使ったジニ係数が、平均してどれぐらい減るか？
##->ジニ係数が減るほど効いているといえる
##いったんRFで特徴量を判別する、というやり方が楽

hwrf<-randomForest(y~job+contact+marital+education,
                       data=data,
                       ntree=100,
                       sampsize=c(1000,1000) )
hwrf
varImpPlot(hwrf)
                   
pre_hwrf<-randomForest(y~job+contact+marital+education+day_of_week,
                   data=data,
                   ntree=500,
                   sampsize=c(1000,1000))

pre_hwrf
varImpPlot(pre_hwrf)

###################################################
#GLM
#hw.glm<-glm(y~job+contact+marital+education+day_of_week,
#            data=data, family="binomial")
#summary(hw.glm)
#############
test<-read.csv("bank_marketing_test-1.csv")

score<-predict(pre_hwrf, test, type = "response")
score

#scoreがいくつ以上あれば、電話をするというアクションにすれば良いのでしょうか？
#とりあえず、train_dataのyの平均、すなわち平均リアクション率を閾値にしましょう
#mean(data$y)
#ypred_flag<-ifelse(score >  0.07588546, 1, 0)

#ypred_flagは1か0のフラグです。1は電話をすれば申し込んでくれると予想される人、
# 0は申し込んでくれないので、電話をしない人になります。

#confusion matrixをみてみましょう
conf_mat<-table(test$y, score )
conf_mat

#あなたが電話するのは1とフラグをつけた人たちです。
# conf_mat[3](表の右上)とconf_mat[4](表の右下)
attack_num<-conf_mat[3] + conf_mat[4]
attack_num

#4594人に電話をします。1人の架電に500円かかりますので、コストは以下のように計算できます
your_cost <- attack_num * 500
your_cost

#一方、申し込んでくれる人は電話した人の表の右下です
conf_mat[4] 
expected_revenue<-conf_mat[4] * 2000
expected_revenue

#コストと収益が計算できましたの粗利は？
expected_revenue - your_cost

########################

visual<- table(test$y,score, test$job )
visual

summary(test)
?partialPlot

partialPlot(,data )
