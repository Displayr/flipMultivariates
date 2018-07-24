#' Train a machine learning model.
#'
#' @param algorithm The name of algorithm to perform the training.
#' @param ... Arguments to the function \code{algorithm}
#' @param warn.if.no.match Logical; If TRUE, a warning is shown if any arguments are not matched.
#' @importFrom methods formalArgs
#' @return A machine learning object.
#' @export

MachineLearning <- function(algorithm, ..., warn.if.no.match = FALSE)
{
    user.args <- list(...)

    # Identify function name
    machine.learning.function <- gsub(" ", "", algorithm)

    fun.and.pars <- getFunctionAndParameters(machine.learning.function)
    arguments <- substituteArgumentNames(fun.and.pars$parameters, user.args, warn.if.no.match)

    return(do.call(fun.and.pars$machine.learning.function, arguments))
}


#' getFunctionNameAndParameters
#'
#' Gets the function, loading parameters if necessary, and the parameters of the function.
#' @param function.name The name of the function used to perform the machine learning.
#' @return A list with the following elements:
#' \item{\code{machine.learning.function}}{The function}.
#' \item{\code{parameters}}{The parameters in \code{machine.learning.function}}.
#' @noRd
getFunctionAndParameters <- function(function.name)
{
    if (!is.character(function.name))
        stop("'function.name' must be of type 'character'.")

    machine.learning.function <- gsub('"', "", function.name, fixed = TRUE) # fixing mess created when 'type' is already a character

    # Getting the function
    if (machine.learning.function == "CART")
        machine.learning.function <- flipTrees::CART
    else if (machine.learning.function == "Regression")
        machine.learning.function <- flipRegression::Regression
    else
        machine.learning.function <- get0(machine.learning.function, mode = "function")

    if (!is.function(machine.learning.function))
        stop(paste0("Cannot find ", machine.learning.function, "."))
    parameters <- formalArgs(machine.learning.function)

    list(machine.learning.function = machine.learning.function, parameters = parameters)
}

#' substituteArgumentNames
#'
#' @param parameter.names The names of parameters.
#' @param arguments The arguments to match to the parameters.
#' @param warn.if.no.match If TRUE, a warning is shown if any arugments are not matched.
#' @noRd
substituteArgumentNames <- function(parameter.names, arguments, warn.if.no.match = TRUE)
{
    a.names <- names(arguments)
    p.names <- parameter.names
    a.unmatched <- !a.names %in% p.names
    p.unmatched <- !p.names %in% a.names
    if (sum(a.unmatched) > 0) # Some argument names do not match parameter names
    {
        # Perform matches and update a.names
        .replaceMatches <- function(aa, pp)
        {
            for (a in aa[a.unmatched])
                for(p in pp[p.unmatched]){
                    if (parametersEqual(p, a))
                        a.names[match(a, aa)] <- p.names[match(p, pp)]
                }
            a.names <<- a.names
        }
        ## Permuting order and ignoring case
        a.regularized <- tolower(a.names)
        p.regularized <- tolower(p.names)
        .replaceMatches(a.regularized, p.regularized)

        # Substituting synonyms
        a.unmatched <- !a.names %in% p.names
        p.unmatched <- !p.names %in% a.names
        if (sum(a.unmatched) > 0)
        {
            .replaceSynonyms <- function(names)
            {
                for (i in seq_along(synonyms))
                {
                    syns <- synonyms[[i]]
                    init.syn <- syns[1]
                    for (s in syns[-1])
                        names <- gsub(s, init.syn, names, fixed = TRUE)
                }
                names
            }
            a.regularized[a.unmatched] <- .replaceSynonyms(tolower(a.names[a.unmatched]))
            p.regularized[p.unmatched] <- .replaceSynonyms(tolower(p.names[p.unmatched]))
            .replaceMatches(a.regularized, p.regularized)
        }
    }
    a.unmatched <- !a.names %in% p.names
    if (sum(a.unmatched) > 0 && warn.if.no.match)
        warning("The following arguments have been ignored: ", paste(a.names[a.unmatched], collapse = ", "))
    names(arguments) <- a.names
    arguments[!a.unmatched]
}



#' Make sure that the first synonym is the briefiest, and from there they are ordered from
#' most more verbose to any roots.
#' These are swapped out in order. E.g., after the first line, all "colours" will be changed to 'col'.
#' @noRd
synonyms <- list(c("col", "colours", "colour", "colors", "color"),
                 c("size", "sizes"),
                 c("label", "labels"),
                 c("categories", "x", "categories", "x.axis", "xaxis"),
                 c("values", "y", "values", "y.axis", "yaxis"),
                 c("xtitle",  "x.title",   "xlab"),
                 c("ytitle", "y.title",  "ylab"),
                 c("title", "main"),
                 c("label.show", "data.label.show"),
                 c("fontsize", "font.size")
)

#' parametersEqual
#'
#' Checks if parameters are equal
#' @param recipient The name of the parameter in the function.
#' @param donor The provided name of the parameter.
#' @noRd
parametersEqual <- function(recipient, donor)
{
    # Exact match
    if (recipient == donor)
        return(TRUE)
    # Matching after re-ordering full stops
    recipient.split <- sort(strsplit(recipient, ".", fixed = TRUE)[[1]])
    donor.split <- sort(strsplit(donor, ".", fixed = TRUE)[[1]])
    if(length(recipient.split) == length(donor.split))
        return((all(recipient.split == donor.split)))
    return(FALSE)
}

