library(randomForest)
library(caTools)
library(rpart)            #for single regression tree
library(rpart.plot)       #to viz regression tree
library(cowplot)          #to combine plots

## load data
print(load("rf_data.rsav"))


## factor
df3$win = ifelse(df3$points == 2, "win", "loss")
df3 = subset(df3, select = -c(id, points, X5v5.S..Sv))
df3$win = as.factor(df3$win)


## impute missing data
df3_imp = rfImpute(df3[,c(1:ncol(df3)-1)], y=df3$win)
colnames(df3_imp)[1] = "win"
save(df3_imp, file = "rfImp_data.rsav")

## split into testing and training
set.seed(1234) 
sample = sample.split(df3_imp, SplitRatio = .80)

train = subset(df3_imp, sample == TRUE)
dim(train)
#[1] 4983   52

test  = subset(df3_imp, sample == FALSE)
dim(test)
#[1] 1336   52



## -- get single decision tree -- ##
m1 = rpart(win ~ ., data=train,
            method  = "class")
rpart.plot(m1)




## -- run RF with multiple mtry -- ##
oob_values = vector(length=15)
for(i in 1:15) {
  print(i)
  temp_mod = randomForest(win ~ ., data=train, 
                           mtry=i, ntree=500, 
                           na.action = na.roughfix)
  print(tail(temp_mod$err.rate),3)
  oob_values[i] = temp_mod$err.rate[nrow(temp_mod$err.rate),1]
}


## find the optimal value for mtry...
which(oob_values == min(oob_values))


## create a model for proximities using the best value for mtry
model = randomForest(win ~ ., 
                      data=train,
                      ntree=1000, 
                      proximity=TRUE, 
                      importance=TRUE,
                      na.action = na.roughfix,
                      mtry=which(oob_values == min(oob_values)))

model



## -- plot confusion matrix -- ##
## how is this created (from oob?)
cm = data.frame(model$confusion)
cm = setDT(data.frame(model$confusion)) %>%
      melt(.,measure.vars=c("loss","win"))
cm$true_labels = c("loss","win","loss","win")
cm$variable = factor(cm$variable, levels=c("loss","win"))
cm$true_labels = factor(cm$true_labels, levels=c("win","loss"))

p0 = ggplot(cm, aes(y=true_labels, x=variable, fill=value, label=value)) +
  geom_tile(alpha=.65) +
  geom_text(size=4.5, fontface="bold") + 
  labs(title = "Confusion Matrix: OOB",
    x = "Predicted label", y="True label", fill="Count") +
  scale_fill_gradient2(high="darkred") + 
  theme_bw() +
  theme(axis.text = element_text(size=11.5, face="bold"),
          legend.position="none")



## -- test set -- ##
predicted_values = data.frame(prediction = predict(model, test), stringsAsFactors = F)
predicted_values$index = as.numeric(rownames(predicted_values))
predicted_values$true = df3$win[predicted_values$index]

## get cm
cm2 = data.frame(table(predicted_values$true, predicted_values$prediction))
colnames(cm2) = c("true_labels","variable","value")

## accuracy
(cm2[1,3] + cm2[4,3]) / sum(cm2$value)
#[1] 0.7140719

## precision
cm2[4,3] / (cm2[4,3] + cm2[3,3])
#[1] 0.6848921

## recall
cm2[4,3] / (cm2[4,3] + cm2[2,3])
#[1] 0.6848921

cm2$variable = factor(cm2$variable, levels=c("loss","win"))
cm2$true_labels = factor(cm2$true_labels, levels=c("win","loss"))

p1 = ggplot(cm2, aes(y=true_labels, x=variable, fill=value, label=value)) +
  geom_tile(alpha=.65) +
  geom_text(size=4.5, fontface="bold") + 
  labs(title = "Confusion Matrix: Test Set",
       x = "Predicted label", y="True label", fill="Count") +
  scale_fill_gradient2(high="dodgerblue4") + 
  theme_bw() +
  theme(axis.text = element_text(size=11.5, face="bold"),
        legend.position="none")

plot_grid(p0, p1, labels = c('A', 'B'))




  
  
## -- variable importance -- ##
mod_importance = data.frame(model$importance, stringsAsFactors = F)
mod_importance$feature = rownames(mod_importance)
mod_importance = mod_importance[order(mod_importance$MeanDecreaseAccuracy),]
mod_importance$feature = factor(mod_importance$feature, levels=unique(mod_importance$feature))

p2 = ggplot(mod_importance, aes(x=MeanDecreaseAccuracy, y=feature)) +
      geom_point(size=2.25, shape=16, color="dodgerblue4") +
      labs(x="Mean Decrease in Accuracy", y="Hockey Stat") +
      theme_bw() +
      theme(axis.text.x = element_text(size=9),
            axis.text.y = element_text(size=7.7, face="bold"))


mod_importance = mod_importance[order(mod_importance$MeanDecreaseGini),]
mod_importance$feature = factor(mod_importance$feature, levels=unique(mod_importance$feature))
p3 = ggplot(mod_importance, aes(x=MeanDecreaseGini, y=feature)) +
  geom_point(size=2.25, shape=16, color="dodgerblue4") +
  labs(x="Mean Decrease in Gini", y="Hockey Stat") +
  theme_bw() +
  theme(axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=7.7, face="bold"))


plot_grid(p2, p3, labels = c('A', 'B'))




