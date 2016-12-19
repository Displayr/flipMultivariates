data("BreastCancer", package="mlbench")

set.seed(10000)
splits <- sample(x=c("train","test"), size=nrow(BreastCancer), replace=T, prob=c(0.7,0.3))
any.na <- apply(BreastCancer, 1, function(x){any(is.na(x))})
train.ind <- which(splits=="train" & !any.na)
test.ind <- which(splits=="test" & !any.na)

dmod <- darch(Class~., data=BreastCancer[train.ind, -1],
              darch.fineTuneFunction = "backpropagation")
dpred <- predict(dmod, newdata=BreastCancer[test.ind,])
table(BreastCancer[test.ind,"Class"], dpred[,2] > 0.5)

require(h2o)
instance <- h2o.init()
train.h2o <- as.h2o(BreastCancer, destination_frame="train.h2o")

train.h2o <- as.h2o(as.data.frame(lapply(BreastCancer[train.ind, -1], as.numeric)),
                    destination_frame="train.dat")
test.h2o <- as.h2o(as.data.frame(lapply(BreastCancer[test.ind, -1], as.numeric)),
                    destination_frame="test.h2o")
hmod <- h2o.deeplearning(x=1:9, y=10, training_frame=train.h2o,
                         epochs = 50,
                         validation_frame=test.h2o,
                         variable_importances = T,
                         max_confusion_matrix_size = 20,
                         diagnostics = T)
hpred <- as.data.frame(predict(hmod, newdata=test.h2o))
table(BreastCancer[test.ind,"Class"], round(hpred$predict))

hmod@model$variable_importances
hmod@model$model_summary
hmod@model$training_metrics
hmod@model$scoring_history
