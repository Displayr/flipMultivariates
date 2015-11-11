#
#
# Accuracy <- function(observed, predicted) {
#   if(!is.factor(observed) | !is.factor(predicted))
#     stop("observed and predicted must be factors.")
#   n.levels <- nlevels(observed)
#   labs <- levels(observed)
#   counts <- tabulate(unclass(observed) + n.levels * (unclass(predicted) - 1))
#   counts <- c(counts, rep(0, n.levels ^ 2 - length(counts))) #filling in any empty cells
#   counts <- matrix(counts, n.levels, dimnames = list(Observed = labs, Predicted = labs))
#   n <- length(observed)
#   proportions <- prop.table(counts)
#   accuracy <- sum(diag(proportions))
#   accuracy.by.class <- diag(prop.table(counts,1))
#   names(accuracy.by.class) <- labs
#   worst.accuracy <- min(accuracy.by.class)
#   list(observed = prop.table(table(observed)), predicted = prop.table(table(predicted)),
#        counts = counts, proportions = proportions, accuracy.by.class = accuracy.by.class,
#        worst.accuracy = worst.accuracy,
#        accuracy = accuracy)
# }
#
#
# RemoveZeroVariance <- function(x) {
#   warning("Need to deal with factors")
#   warning("Test for positive number of variables")
#   sd <- apply(x, 2, sd)
#   to.remove <- sd < 0.000000001
#   if(sum(to.remove) == 0)
#     cat("No All variables have positive variances.\n")
#   else
#   cat("Removed", paste(names(x)[to.remove], collapse = ","),"\n")
#   x[,!to.remove]
# }
#
#
# PredictSegments <- function(x, y, max.n.variables = n.variables, method = "rpart",min.size = 10,  ...) {
#   set.seed(1)
#   n.variables <- ncol(x)
#   dep.name <-paste(substitute(y))#all.vars(formula)[1] # y.name <-
#   n.levels <- nlevels(y)
#   dat <- data.frame(y,x)
#   form <- formula(paste(dep.name,"~", paste(names(x),collapse = "+")))
#   if (max.n.variables < n.variables) {# Compute importance using a random forrest
#     require(randomForest)
#     importance <- randomForest(form, importance = TRUE, data = dat)$importance
#     orders <- array(NA,c(n.variables,n.levels))# + 1))
#     #orders[,1] <- (1:n.variables)[Order(-importance[,n.levels])]
#     for (i in 1:(n.levels))# + 1))
#       orders[, i ] <- (1:n.variables)[Order(-importance[,i])]
#     best.vars <- unique(as.numeric(t(orders)))[1:max.n.variables]
#     x <- x[,best.vars]
#     dat <- data.frame(y,x)
#     form <- formula(paste(dep.name,"~", paste(names(x),collapse = "+")))
#   }
#   if (method == "lda"){
#     require(MASS)
#     fit <- MASS::lda(form, data = dat)
#     predictions <- predict(fit, dat)$class
#   } else if(method == "tree") {
#     require(tree)
#     #x <- as.data.frame(x)
#     #x$nzkljds <- y
#     #y.name <- paste(substitute(y))
#     #names(x)[ncol(x)] <-y.name
#     fit <- tree(form, dat, x = TRUE, y = TRUE, control = tree.control(nrow(x),mincut = min.size / 2, minsize = min.size, mindev = 0.000001), ...)#paste(y.name,"~."), as.data.frame(cbind(y, x)))
#     predictions <- predict(fit, dat, type = "class")
#   }
#   else if(method == "rpart") {
#     require(rpart)
#     fit <- rpart(formula, data)#as.data.frame(cbind(y,x)))
#   } else if (method == "multinom") {
#     #dat <- as.data.frame(cbind(y, x))
#     require(nnet)
#     #require(MASS)
#     #require(caret)
#     #ctlr <- trainControl(method = "none")
#     #lda.fit <- caret::train(x, y, method = "lda", trcontrol = ctrl)
#     fit <- nnet::multinom(paste(substitute(y),"~."), as.data.frame(cbind(y, x)))
#     #update(bwt.mu," .~. + lwt")
#   }
#   list(predictions = predictions, fit = fit, names = names(x), accuracy = Accuracy(y, predictions))
# }
#
# ExportExcelWorkbook <- function(object, file = "") {#PredictSegments object
#   require(xlsx)
#   write.xlsx(object$fit$frame, file, append = FALSE, sheetName = "frame")
#   write.xlsx(data.frame(y = object$fit$y, Predictions = object$predictions, "Spreadsheet Predictions" = NA, object$fit$x), file, append = TRUE, sheetName = "data")
# }
#
# ExportExcelWorkbook(seg1, 'C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\z.xlsx')
#
#   help(WriteXLS))
#
# library(foreign)
# spss1 <- read.spss('C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\jwtFirstSegmentation.sav')
# attach(spss1)
# d1 <- cbind(q25r1, q25r2, q25r3, q25r4, q25r5, q25r17, q25r6, q25r7, q25r8, q25r9, q25r10, q25r11,q25r12, q25r13, q25r14, q25r15, q25r16,
#             q31r1, q31r2, q31r3, q31r4, q31r5, q31r6, q31r7, q31r8, q31r9, q31r10, q38r1_3, q38r2_3, q38r3_3, q38r4_3, q38r5_3,
#             q38r6_3,q38r7_3, q38r8_3, q38r9_3, q38r10_3, q38r11_3, q38r12_3, q38r13_3, q41r1_3, q41r2_3, q41r3_3, q41r4_3)
# x1 <- as.data.frame(RemoveZeroVariance(d1) - 1)
# y1 <- Segments_Unweighted2
# detach(spss1)
#
# seg1 <- PredictSegments(x1, y1, max.n.variables = 10, min.size = 10,  method = "tree")
# seg1$accuracy
# seg1$names
#
# ExportExcelWorkbook(seg1, 'C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\z.xlsx')
# write.csv(seg1$fit$frame, "C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\seg1frame.csv")
#
# spss2 <- read.spss('C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\jwtSecondSegmentation.sav')
# attach(spss2)
# d2 <- cbind(q21r1_6,q21r2_6,q21r3_6,q21r4_6,q21r5_6,q21r6_6,q21r7_6,q21r8_6,q21r9_6,q21r10_6, q21r11_6,
#             #q21r12_6,  contains missing values
#             q21r13_6, q21r14_6, q21r15_6, q21r16_6, q21r17_6,  q21r18_6,q21r19_6, q21r20_6,  q21r21_6,q21r22_6,
#             q32r1_7,q32r2_7,q32r3_7,q32r4_7,q32r5_7,q32r6_7,q32r7_7,q32r8_7,q32r9_7,q32r10_7, q32r11_7,q32r12_7,
#             q32r13_7, q32r14_7, q32r15_7, q32r16_7, q32r17_7, q32r23_7,  q32r24_7,q32r18_7, q32r19_7,  q32r20_7,q32r21_7, q32r22_7)
# x2 <- as.data.frame(RemoveZeroVariance(d2))
# y2 <- Segments_V7
# x2 <- x2[!is.na(y2),]
# y2 <- y2[!is.na(y2)]
# detach(spss2)
#
# seg2 <- PredictSegments(x2, y2, max.n.variables = 9, min.size = 5,  method = "tree")
# # forcing to be consistent with email sent to Heather
# seg2 <- PredictSegments(x2[,c("q32r16_7", "q21r7_6", "q32r23_7", "q32r4_7", "q32r21_7", "q21r5_6", "q21r15_6", "q32r24_7", "q21r4_6")], y2, max.n.variables = 9, min.size = 5,  method = "tree")
# seg2$accuracy
# seg2$names
#
# #ExportExcelWorkbook(seg2, 'C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\z.xlsx')
# write.csv(seg2$fit$frame, "C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\seg2frame.csv")
# write.csv(data.frame(y = seg2$fit$y, Predictions = seg2$predictions, "Spreadsheet Predictions" = NA, Test = NaN, seg2$fit$x), "C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\seg2Data.csv")
#
# write.xlsx(, file, append = TRUE, sheetName = "data")
# }
#
#
#
