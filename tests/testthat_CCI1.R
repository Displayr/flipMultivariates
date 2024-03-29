if (identical(Sys.getenv("CIRCLECI"), "true"))
{
    if (!dir.exists("reports"))
        dir.create("reports")
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    exit.code <- flipDevTools::RunTestsOnCircleCI(filter = "ensemble",
                                                  load_package = "installed", output_file = out.file)
    q(status = exit.code, save = "no")
}
