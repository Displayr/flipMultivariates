incomplete.question.design <- function(number.alternatives, number.questions, alternatives.per.question, n.repeats = 1000){
    # Check that the parameters are appropriate
    # Sawtooth recommends that number.questions >= 3 * number.alternatives / alternatives.per.question
    if (number.questions < 3 * number.alternatives / alternatives.per.question)
        warning("It is recomended that number.questions >= 3 * number.alternatives / alternatives.per.question");
    library(AlgDesign)
    best.result = NULL
    best.D = -Inf
    for (i in 1:n.repeats){
        alg.results <- optBlock(~.,withinData=factor(1:number.alternatives),
                                blocksizes=rep(alternatives.per.question,number.questions),
                                nRepeats=5000) #BIB
        if (alg.results$D > best.D){
            best.result = alg.results
            best.D = alg.results$D
        }
    }
    design <- matrix(NA,number.questions,alternatives.per.question, dimnames = list(Question = 1:number.questions, Alternative = 1:alternatives.per.question))
    binary.design <- matrix(0,number.questions,number.alternatives, dimnames = list(Question = paste("Question", 1:number.questions), Alternative = 1:number.alternatives))
    counter <- 0
    for (question in best.result$Blocks){
        counter <- counter + 1
        blck <- unlist(question)
        design[counter,] <- blck
        for (a in blck)
            binary.design[counter,a] <- 1
    }
    n.appearances.per.alternative <- table(as.numeric(design))
    combinations.of.alternatives <- crossprod(table(c(rep(1:number.questions, rep(alternatives.per.question,number.questions))), best.result$design[,1]))
    binary.design = t(binary.design)
    rownames(binary.design) <- c("Alternative 1", 2:number.alternatives)
    list(binary.design = t(binary.design),
         design = t(design),
         frequencies = n.appearances.per.alternative,
         pairwise.frequencies=combinations.of.alternatives,
         binary.correlations = round(cor(binary.design),2))
}

design <- incomplete.question.design(number.alternatives = 10,
                        number.questions = 11,
                        alternatives.per.question = 5,
                        n.repeats = 1)


