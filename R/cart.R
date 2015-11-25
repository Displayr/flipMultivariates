#' \code{CART} Creats a classification or regression tree.
#' @param formula A formula expression. The left-hand-side (response) should
#' be either a numerical vector when a regression tree will be fitted or
#' a factor, when a classification tree is produced. The right-hand-side should
#' be a series of numeric or factor variables separated by \code{+}; there should be
#' no interaction terms. Both \code{.} and \code{-} are allowed: regression trees can have
#' offset terms
#' @param data A data frame in which to preferentially interpret formula, weights and subset
#' @param ... Additional arguments that are passed to  \code{\link{tree}}
#' and \code{\link{tree.control}}. Normally used for mincut, minsize or mindev
#'
#' @details Creates a \code{\link{tree}} and plots it as a \code{\link{sankeytree}}
#' @export

CART <- function(formula, data, weights = NULL, subset = NULL, ...)
{
    if (is.null(weights))
    {
        if(is.null(subset) || length(subset) == 1)
        {
            result <- tree::tree(formula, data = data, model = TRUE, ...)
        }
        else
        {
            data$sb <- subset
            result <- tree::tree(formula, data = data, subset = data$sb, model = TRUE, ...)
        }
    }
    else
    {
        if(is.null(subset) || length(subset) == 1)
            result <- tree::tree(formula, data = data, weights = weights, model = TRUE, ...)
        else
        {
            print("dog")
            data$sb <- subset
            result <- tree::tree(formula, data = data, subset = data$sb,
                           weights = weights, model = TRUE, ...)
        }
    }
    result$predicted <- predict(result, newdata = data, type = "tree", na.action = na.exclude)
    class(result) <- append("CART", class(result))

    return(result)
}

#' \code{treeFrameToList} Converts a \code{\link{tree}} into a format usable in a sankeytree.
#' @param max.tooltip.length The maximum length of the tooltip (determines the scale of the tree).
#' @param show.whole.factor Controls whether or not all the factor levels are displayed in the tooltip.
#' @param numeric.distribution Outputs additional diagnostics in the tooltip.
#' @param custom.color logical; if \code{true}, generates custom tree color, else use colors provided by sankeyTree package.
#' @param num.color.div positive integer in the range [2,inf]. Controls the color resolution of the tree. A higher value gives a smoother color transition.
#' @param const.bin.size logical; if \code{true}, each color spans an equal step of y-value or an equal number of points.
#' @param draw.legend logical; if \code{true}, output the colors as a sorted RBG value list to draw legend

treeFrameToList <- function(tree, max.tooltip.length = 150, show.whole.factor = FALSE, numeric.distribution = FALSE,
                            custom.color = TRUE, num.color.div = 101, const.bin.size = TRUE, draw.legend = TRUE)
{
    # Creating the names of a node from the frame.
    frame <- tree$frame
    attri <- attributes(tree)
    model <- tree$model
    assigned <- tree$where
    .terminalNode <- function(i) frame$var[i] == frame$var[nrow(frame)]
    # generate two hash tables that maps output of factor predictor to a meaningful string
    # e.g. a factor output variable with 3 levels: c("Much Better","Average","Much Worse")
    # this function converts words to an abbreviation, and generates a hash table with
    # keys = c("Much Better","Average","Much Worse"), so that "a" -> "MuBe", "b" -> "Ave",
    # "c" -> "MuWo"
    .getNodeHash <- function(tree.attri)
    {
        .appendNum <- function(text, text.hash, c) {
            text1 <- paste0(text,c)
            if (hash::has.key(text1, text.hash)) {
                text1 <- .appendNum(text, text.hash, c+1)
            }
            return(text1)
        }
        xlevels <- tree.attri$xlevels
        xlevels.fac <- xlevels[!sapply(xlevels, is.null)] # strip null
        if (length(xlevels.fac) == 0)
            return(NULL)
        # replace all non alphanumeric letters
        xlevels.fac <- lapply(xlevels.fac, function(obj) gsub("[^a-zA-Z0-9]", " ", obj))
        # replace first letter of all words with upper case
        xlevels.fac <- lapply(xlevels.fac, function(obj) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", obj, perl=TRUE))
        # get the first two or three letters of the words
        for (i in 1:length(xlevels.fac)) {
            text.hash = hash::hash()
            node.texts <- rep("",length(xlevels.fac[[i]]))
            for (j in 1:length(xlevels.fac[[i]])) {
                text <- xlevels.fac[[i]][j]
                text.len <- sapply(gregexpr("[[:alpha:]]+", text), function(x) sum(x > 0)) # count number of words
                if (text.len == 1) {
                    nchars <- nchar(text)
                    node.text <- ifelse(nchars > 3, substr(text,1,3), text) # one word
                } else {
                    text1 <- strsplit(text," ")[[1]]     # more than one word
                    node.text <- rep("",length(text1))
                    nchars <- nchar(text1)
                    for(l in 1:length(nchars)) {
                        node.text[l] = ifelse(nchars[l] > 2, substr(text1[l],1,2), text1[l])
                    }
                }
                node.text <- paste(node.text, collapse = "")
                if (!hash::has.key(node.text, text.hash)) {
                    hash::.set(text.hash, keys=node.text, values=TRUE)
                } else {
                    node.text <- .appendNum(node.text, text.hash, 1)
                }
                node.texts[j] <- node.text
            }
            hash::clear(text.hash)
            xlevels.fac[[i]] <- node.texts
        }
        # make hash tables to search for names
        # check uniqueness
        features.hash = hash::hash(keys = names(xlevels.fac), values = 1:length(xlevels.fac))
        xlevels.hash = c()
        for(node.texts in xlevels.fac) {
            # this approach will fail if more than 26 levels
            h = hash::hash(keys = letters[1:length(node.texts)],values = node.texts)
            xlevels.hash = c(xlevels.hash, h)
        }
        result = list(features.hash,xlevels.hash)
    }

    tree.hash = .getNodeHash(attri)
    #.trim = function(string) ifelse(nchar(string) >  max.tooltip.length, paste(0,strtrim(string, max.tooltip.length),"..."), string)
    nms = names(tree$where)
    outcome.variable = tree$model[,1]
    outcome.is.factor = is.factor(outcome.variable)
    outcome.name = names(tree$model)[[1]]

    if (outcome.is.factor)
    { # Classification tree.
        yprob = frame$yprob
        nms = colnames(yprob)
        if (show.whole.factor)
        {
            yprob = matrix(paste0(round(yprob*100),"% ", nms[col(yprob)]), ncol = length(nms))
            node.descriptions <- apply(yprob, 1, function(x) paste0(x,collapse = "<br>"))
        }
        else
        {
            node.descriptions <- rep("",nrow(frame))
            for(i in 1:length(node.descriptions))
            {
                col.idx <- yprob[i,] > 0.000001
                node.des <- paste0(round(yprob[i,col.idx]*100),"% ", nms[col.idx])
                node.descriptions[i] <- paste0(node.des, collapse = "<br>")
            }
        }
        node.descriptions <- paste0("<br>",node.descriptions)

        node.color <- rep("0", nrow(frame))
        if (custom.color)
        {
            if (num.color.div < 2) stop('number of colors for the tree cannot be < 2')
            if (num.color.div %% 2 == 0) num.color.div = num.color.div + 1
            hcl.color <- rev(colorspace::diverge_hcl(num.color.div,  h = c(260, 0), c = 100, l = c(50, 90)))
            divisions <- seq(0, 1, 1/num.color.div)
            node.color[1] <- "#ccc"
            for (i in 2:nrow(frame))
            {
                y <- max(yprob[i,])
                div.idx = max(which(y >= divisions))
                if (div.idx == length(divisions))
                {
                    div.idx <- div.idx - 1
                }
                node.color[i] <- hcl.color[div.idx]
            }
        }
        else
        {
            for (i in 1:nrow(frame))
            {
                node.color[i] <- ""
            }
        }
        terminal.description <- paste(" Highest =",frame$yval) # Change 1
    }
    else
    { # Regression tree.
        if (numeric.distribution)
        {
            node.mean <- paste0("<br>Mean(", outcome.name, ")", " = ", FormatAsReal(frame$yval, digits = 1, format = "f")) # Mean
            node.sse <- paste0("SumSE(", outcome.name, ")", " = ", FormatAsReal(frame$dev, digits = 1, format = "f")) # Sum of Square Error
            node.rmse <- paste0("RMSE(", outcome.name, ")", " = ", FormatAsReal(sqrt(frame$dev/frame$n), digits = 1, format = "f")) # Root Mean Square Error
            x <- matrix(cbind(node.mean,node.rmse, node.sse), ncol = 3)
            node.descriptions <- apply(x, 1, function(x) paste0(x,collapse = "<br>"))
        }
        else
        {
            node.mean = paste0("Mean(", outcome.name, ")", " = ", FormatAsReal(frame$yval, digits = 1, format = "f"), ":") # Mean
            node.descriptions <- node.mean
        }
        ymin <- min(frame$yval)
        ymax <- max(frame$yval)

        eps <- 0.001 # error margin
        # if y is too small, scale it first!
        if (ymin < -eps && ymax > eps) {
            # constant bin size not avilable
            ysmall = frame$yval[frame$yval < -eps]
            ybig = frame$yval[frame$yval > eps]
            divisions = c(quantile(ysmall, seq(0, 1, 1/(num.color.div-1)*2)), quantile(ybig, seq(0, 1, 1/(num.color.div-1)*2)))
        }
        else
        {
            if (const.bin.size)
            {
                divisions = seq(ymin, ymax, by = (ymax - ymin)/num.color.div)
            }
            else
            {
                divisions = quantile(frame$yval, seq(0, 1, 1/num.color.div))
            }
        }

        node.color <- rep("0", nrow(frame))
        if (custom.color)
        {
            if (num.color.div < 2) stop('number of colors for the tree cannot be < 2')
            if (num.color.div %% 2 == 0) num.color.div = num.color.div + 1
            hcl.color <- rev(colorspace::diverge_hcl(num.color.div,  h = c(260, 0), c = 100, l = c(50, 90)))
            node.color[1] <- "#ccc"
            for (i in 2:nrow(frame))
            {
                y <- frame$yval[i]
                div.idx <- max(which(y >= divisions))
                if (div.idx == length(divisions))
                {
                    div.idx <- div.idx - 1
                }
                node.color[i] <- hcl.color[div.idx]
            }
        }
        else
        {
            for (i in 1:nrow(frame))
            {
                node.color[i] <- ""
            }
        }

        terminal.description <- paste0("; Mean = ",FormatAsReal(frame$yval)) # Change 2
    }

    root.name <- outcome.name
    .constructNodeName <- function(node, i, i.parent, frame, tree.hash)
    {
        if (i == 1)
            return(root.name)
        features.hash <- tree.hash[[1]]
        xlevels.hash <- tree.hash[[2]]
        variable.name <- frame$var[i.parent]
        node.names <- frame$splits[i.parent,]
        node.name <- ifelse(node %% 2 == 0, node.names[1], node.names[2])

        if (grepl("<0.5", node.name))
        {   #Binary split (probably)
            node.name <- paste("Not", variable.name)
        }
        else if (grepl(">0.5", node.name))
        {
            node.name <- variable.name
        }
        else if (grepl("[<>]", node.name))
        {
            node.name <- paste(variable.name, node.name)
        }
        else
        {
            node.name <- sub(":", "", node.name)
            node.str <- strsplit(node.name,"") # split node string
            nd.txt <- rep("",length(node.str[[1]]))
            # for each letter generated by the tree,e.g."a","b", find its corresponding output string
            for(m in 1:length(node.str[[1]])){
                str <- node.str[[1]][m]
                feature.id <- hash::values(features.hash, keys = as.character(variable.name))
                nd.txt[m] <- hash::values(xlevels.hash[[feature.id]],keys = str)
            }
            node.name <- paste(nd.txt, collapse = " ")
            node.name <- paste0(variable.name, ": ", node.name)
        }
        if (.terminalNode(i))
            node.name <- paste0(node.name, terminal.description[i])
        node.name
    }
    # Function for creating a recursive list.
    .constructNodes <- function(node, nodes, frame, tree.hash) {
        parent.node <- floor(node / 2)
        i.parent <- match(parent.node, nodes)
        i <- match(node, nodes)
        result <- list(name = .constructNodeName(node, i, i.parent, frame, tree.hash),
                      n = frame$n[i], Percentage = FormatAsPercent(frame$n[i]/frame$n[1], digits = 1),
                      id = node, Description = node.descriptions[i], color = node.color[i])
        if((node * 2) %in% nodes) { # Adding child nodes, if they exist.
            result$children = vector("list", 2)
            for (branch in 1:2)
                result$children[[branch]] = .constructNodes(node * 2 + branch - 1, nodes, frame, tree.hash)
        }
        result
    }
    # Creating the recrusive list.
    nodes <- as.numeric(dimnames(frame)[[1]])
    tree.list <- .constructNodes(1, nodes, frame, tree.hash)
    if (custom.color && draw.legend)
    {
        if (outcome.is.factor) {
            tree.list <- c(list(hcl.color), list(paste0(seq(0,100,10), "%")), tree.list)
        } else {
            if (const.bin.size){
                tree.list <- c(list(hcl.color), list(FormatAsReal(seq(ymin, ymax,(ymax - ymin)/10),digits = 1, format = "f")), tree.list)
            } else {
                tree.list <- c(list(hcl.color), list(FormatAsReal(quantile(frame$yval, seq(0, 1, 1/10)),digits = 1, format = "f")), tree.list)
            }
        }
        names(tree.list)[1:2] = c("legendColor","legendText")
    }
    tree.list
}

#' @export
print.CART <- function(x, ...)
{
    tree.list <- treeFrameToList(x, custom.color = TRUE)
    plt <- sankeytreeR::sankeytree(tree.list, value = "n", nodeHeight = 100,
        tooltip = c("n", "Description"), treeColors = TRUE, legend = TRUE)
    print(plt)
}

#
#  set.seed(132)
#  data(cpus, package="MASS")
#  cpuss <- cpus
#  cpuss$weights = runif(nrow(cpus))
#  cpuss$subset = runif(nrow(cpus)) > 0.5
#  library(devtools)
# # install_github("NumbersInternational/sankeytree")
#  install_github("xtmwang/sankeytree")
# # install_github("NumbersInternational/flipMultivariates")
#  library(sankeytreeR)
#  #ibrary(flipMultivariates)
#  z <- CART(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpuss, weights = weights, subset = subset)
#  z
# # # cpuss$weights
# #
#
# #z <- tree::tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpuss, weights = weights, subset = subset)
#plot(z)
#text(z)
