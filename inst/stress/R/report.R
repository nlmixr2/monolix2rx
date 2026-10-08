## Summaries of a kit run.

kitReport <- function(res, outDir) {
  utils::write.csv(res, file.path(outDir, "summary.csv"), row.names=FALSE)
  .fmt <- function(x) ifelse(is.na(x), "", formatC(x, digits=3, format="g"))
  .num <- c("dryMaxRel", "dryLikMaxRel", "dryOmegaDiff", "dryErrDiff", "ipredRtol", "predRtol",
            "iwresAtol", "mlxSeconds")
  .cols <- intersect(c("case", "status", "tags", .num, "note"), names(res))
  .tab <- res[, .cols, drop=FALSE]
  for (.n in intersect(.num, .cols)) .tab[[.n]] <- .fmt(.tab[[.n]])
  .tab$note <- ifelse(is.na(.tab$note), "",
                      gsub("[|\n]", " ", substr(.tab$note, 1, 160)))
  .counts <- table(factor(res$status,
                          levels=c("PASS", "FAIL", "ERROR", "XFAIL", "XPASS", "SKIP")))
  .md <- c("# monolix2rx kit results", "",
           paste0("Run: ", format(Sys.time()), "; mode: ",
                  paste(unique(res$mode), collapse=", "),
                  "; monolix2rx ", as.character(utils::packageVersion("monolix2rx")),
                  "; rxode2 ", as.character(utils::packageVersion("rxode2"))),
           "",
           paste(paste0(names(.counts), ": ", .counts), collapse=" | "), "",
           "Columns: `dryMaxRel` = max % difference between the translated",
           "model's PRED and the rxode2 truth (no Monolix);",
           "`dryLikMaxRel` = the same for each observation's likelihood",
           "(discrete endpoints);",
           "`dryOmegaDiff`/`dryErrDiff` = max relative difference of the",
           "imported omega/residual parameters from the truth;",
           "`ipredRtol`/`predRtol` = median % difference rxode2 vs Monolix",
           "(from the monolix2rx validation).", "",
           paste0("| ", paste(.cols, collapse=" | "), " |"),
           paste0("|", paste(rep("---", length(.cols)), collapse="|"), "|"),
           apply(.tab, 1, function(r) paste0("| ", paste(r, collapse=" | "), " |")))
  writeLines(.md, file.path(outDir, "summary.md"))
  invisible(.counts)
}
