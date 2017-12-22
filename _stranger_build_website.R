my_build_site <- function (pkg = ".", path = "docs", examples = TRUE, run_dont_run = FALSE,
          mathjax = TRUE, articles=FALSE,preview = FALSE, seed = 1014, encoding = "UTF-8")
{
  old <- pkgdown:::set_pkgdown_env("true")
  on.exit(pkgdown:::set_pkgdown_env(old))
  pkg <- pkgdown:::as_pkgdown(pkg)
  path <- pkgdown:::rel_path(path, pkg$path)
  pkgdown:::init_site(pkg, path)
  pkgdown:::build_home(pkg, path = path, encoding = encoding)
  pkgdown:::build_reference(pkg, lazy = FALSE, examples = examples,
                  run_dont_run = run_dont_run, mathjax = mathjax, seed = seed,
                  path = file.path(path, "reference"), depth = 1L)
  if (articles) pkgdown:::build_articles(pkg, path = file.path(path, "articles"),
                 depth = 1L, encoding = encoding)
  pkgdown:::build_news(pkg, path = file.path(path, "news"), depth = 1L)
  if (preview) {
    pkgdown:::preview_site(path)
  }
  invisible(TRUE)
}

pkg <- pkgdown:::as_pkgdown(".")
# relpath=pkgdown:::rel_path(pkg$path
my_build_site(examples=TRUE,run_dont_run=TRUE)
# dir.create(file.path(relpath,"docs","articles"),showWarnings=FALSE)
pkgdown:::build_articles(".", path = "docs/articles", depth = 1L)
# pkgdown:::preview_site(pkgdown:::rel_path("docs", relpath))
