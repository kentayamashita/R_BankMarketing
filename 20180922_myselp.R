data<- read.csv("bank_marketing_train.csv")
summary(data)
head(data)
hist(data$age)

boxplot(age~y,data = data)

job_tab <- table(data$job,data$y)
##�N���X�W�v�i�J�e�S�����Ƃ�Y/N�j
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

###RandomForest����Ă݂�
library(randomForest)
myrf<-randomForest(y~job+age+contact,
             data=data,
             ntree=100,
             sampsize=c(1000,1000)
             )
##�g�������������ꍇ
?randomForest
##����ɗ��ŗ\������Confusion Matrix���쐬���Ă���Ă���
myrf
##sampsize��Y����1000�AN����1000�A�Ƃ����`�ŃT���v�����O�ł���
## ->Y�̊m�������Ȃ��ƁA���ׂ�N�Ɠ������ق����m���������Ȃ邽�߁A�����h�~

myrf_score<- predict(myrf, newdata = data, type = "prob")
##prob = probability,�m��,RF�̂Ƃ��̂�
##response�Ƃ����Y/N�ƂȂ�

myrf_score[,2]
##yes������\��
varImpPlot(myrf)
##�����ʂ̌��������Ƃ̃O���t
##�d�v�x�̒�`�FMeanDecreaseGini
##->����؂Ŏg�����W�j�W�����A���ς��Ăǂꂮ�炢���邩�H
##->�W�j�W��������قǌ����Ă���Ƃ�����
##��������RF�œ����ʂ𔻕ʂ���A�Ƃ����������y

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

#score�������ȏ゠��΁A�d�b������Ƃ����A�N�V�����ɂ���Ηǂ��̂ł��傤���H
#�Ƃ肠�����Atrain_data��y�̕��ρA���Ȃ킿���σ��A�N�V��������臒l�ɂ��܂��傤
#mean(data$y)
#ypred_flag<-ifelse(score >  0.07588546, 1, 0)

#ypred_flag��1��0�̃t���O�ł��B1�͓d�b������ΐ\������ł����Ɨ\�z�����l�A
# 0�͐\������ł���Ȃ��̂ŁA�d�b�����Ȃ��l�ɂȂ�܂��B

#confusion matrix���݂Ă݂܂��傤
conf_mat<-table(test$y, score )
conf_mat

#���Ȃ����d�b����̂�1�ƃt���O�������l�����ł��B
# conf_mat[3](�\�̉E��)��conf_mat[4](�\�̉E��)
attack_num<-conf_mat[3] + conf_mat[4]
attack_num

#4594�l�ɓd�b�����܂��B1�l�̉˓d��500�~������܂��̂ŁA�R�X�g�͈ȉ��̂悤�Ɍv�Z�ł��܂�
your_cost <- attack_num * 500
your_cost

#����A�\������ł����l�͓d�b�����l�̕\�̉E���ł�
conf_mat[4] 
expected_revenue<-conf_mat[4] * 2000
expected_revenue

#�R�X�g�Ǝ��v���v�Z�ł��܂����̑e���́H
expected_revenue - your_cost

########################

visual<- table(test$y,score, test$job )
visual

summary(test)
?partialPlot

partialPlot(,data )