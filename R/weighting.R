weightedSurveyDesign <- function(data, weights)
{
    survey::svydesign(id = ~ 1, weights = weights, data = data)
}
