if (identical(Sys.getenv("CIRCLECI"), "true")) {
    if (!dir.exists("reports")) {
        dir.create("reports")
    }
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    # Using getExportedValue instead of flipDevTools::RunTestsOnCircleCI to silence CI warning
    exit.code <- getExportedValue("flipDevTools", "RunTestsOnCircleCI")(filter = "^(?!ensemble)", perl = TRUE,
        load_package = "installed", output_file = out.file)
    q(status = exit.code, save = "no")
}
