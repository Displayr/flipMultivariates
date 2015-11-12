weightedSurveyDesign <- function(data, weights) {
    require(survey)
    survey::svydesign(id=~1, weights = weights, data = data)
}
