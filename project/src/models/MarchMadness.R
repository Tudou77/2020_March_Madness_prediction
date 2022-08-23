library(data.table)
library(caret) #http://topepo.github.io/caret/index.html


#read data
test<-fread("./project/volume/data/interim/test.csv")
train<-fread("./project/volume/data/interim/train.csv")

train_y<-train$result
test_y<-test$result

#get rid of team id column for linear model
train$team_1<-NULL
train$team_2<-NULL
test$team_1<-NULL
test$team_2<-NULL

#dummy variables
dummies <- dummyVars(result ~ ., data = train)
train<-predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)

#reformat 
train<-data.table(train)
train$result<-train_y
test<-data.table(test)

#fit in a linear
glm_model<-glm(result~.,family=binomial,data=train)


summary(glm_model)

coef(glm_model)
#use saveRDS to save the model
saveRDS(dummies,"./project/volume/models/POM_glm.dummies")
saveRDS(glm_model,"./project/volume/models/POM_glm.model")

#type equal to response
test$result<-predict(glm_model,newdata = test,type="response")
pred<-test$result

#save test data for now
test<-fread('./project/volume/data/interim/test.csv')

test$result<-pred

sub<-fread('./project/volume/data/raw/MsampleSubmissionStage2.csv')
sub$order<-1:nrow(sub)
teams<-data.table(matrix(unlist(strsplit(sub$ID,"_")),ncol=3,byrow=T))
setnames(teams,c("V1","V2","V3"),c("Season","team_1","team_2"))

sub$team_1<-teams$team_1
sub$team_2<-teams$team_2

#change strings into characters
sub$team_1<-as.character(sub$team_1)
sub$team_2<-as.character(sub$team_2)
test$team_1<-as.character(test$team_1)
test$team_2<-as.character(test$team_2)

#get rid of the Pred column 
sub$Pred<-NULL

#alter our table into submission format 
submit<-merge(sub,test,all.x=T, by=c("team_1","team_2"))

submit<-submit[order(order)]

submit<-submit[,.(ID,result)]
setnames(submit,"result","Pred")

#use fwrite to save our predicitons
fwrite(submit,"./project/volume/data/processed/NCAA_pred.csv")

