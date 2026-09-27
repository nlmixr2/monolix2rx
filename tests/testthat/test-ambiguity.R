# Shapes that used to have more than one dparser parse (issue #51)

test_that("keywords only match whole words in equations (#51)", {
  .ret <- .equation(
    "elseifx = 1
elsewhere = 2
endTime = 3
iff = 4
if t<=10
   c = 1
elseif t<20
   c = 2
else
   c = 3
end
endx = c"
  )
  expect_equal(
    .ret$rx,
    c(
      "elseifx <- 1",
      "elsewhere <- 2",
      "endTime <- 3",
      "iff <- 4",
      "if (time <= 10) {",
      "c <- 1",
      "} else if (time < 20) {",
      "c <- 2",
      "} else {",
      "c <- 3",
      "}",
      "endx <- c"
    )
  )
})

test_that("function arguments and parenthesized conditions parse once (#51)", {
  .ret <- .equation(
    "if (a == b) & (c < d)
 y = exp(a - b)
else
 y = max(a -b, c + d)
end
z = (y)"
  )
  expect_equal(
    .ret$rx,
    c(
      "if ((a == b) && (c < d)) {",
      "y <- exp(a - b)",
      "} else {",
      "y <- max(a - b, c + d)",
      "}",
      "z <- (y)"
    )
  )
})

test_that("pkmodel parameters with values in a {Cc, Ce} model (#51)", {
  .ret <- .pk("{Cc, Ce} = pkmodel(ka, V, Cl, k12=Q/V, k21 = Q/V2, ke0 = 3)")
  expect_equal(
    .ret$pkmodel[c("V", "ka", "Cl", "k12", "k21", "ke0")],
    c(V = "", ka = "", Cl = "", k12 = "Q/V", k21 = "Q/V2", ke0 = "3")
  )
  expect_equal(
    as.character(.ret),
    "{Cc, Ce} = pkmodel(V, ka, Cl, k12 = Q/V, k21 = Q/V2, ke0 = 3)"
  )
})

test_that("categorical/count code does not keep the separating comma (#51)", {
  .ret <- .longDef("y = {type=count, P(y=k) = 1}")
  expect_equal(.ret$endpoint[[1]]$err$code, "P(y=k) = 1")
  .ret <- .longDef("y = {type=count P(y=k) = 1}")
  expect_equal(.ret$endpoint[[1]]$err$code, "P(y=k) = 1")
  .ret <- .longDef(
    "level = {type = categorical, categories = {1, 2}, logit(P(level <=1)) = th1}"
  )
  expect_equal(.ret$endpoint[[1]]$err$code, "logit(P(level <=1)) = th1")
})

test_that("unquoted file names parse to one file and no header (#51)", {
  for (.f in c(
    "data.csv",
    "data{1}.csv",
    "/a/b.txt",
    "my data.csv",
    "my big data.csv"
  )) {
    .fi <- .fileinfo(paste0(
      "file=",
      .f,
      "\ndelimiter = comma\nheader = {ID, TIME}"
    ))
    expect_equal(.fi$file, .f)
    expect_equal(.fi$header, c("ID", "TIME"))
    expect_equal(.fi$delimiter, "comma")
  }
  .ind <- .ind("input = {V, Cl}\nfile=lib:oral1_1cpt_kaVCl.txt")
  expect_equal(.ind$file, "lib:oral1_1cpt_kaVCl.txt")
  expect_equal(.ind$input, c("V", "Cl"))
})

test_that("parsers still work with the strict ambiguity check off (#51)", {
  withr::local_envvar(MONOLIX2RX_STRICT_AMBIGUITY = NA)
  expect_equal(
    .equation("if t<=10\n c = 1\nend")$rx,
    c("if (time <= 10) {", "c <- 1", "}")
  )
  expect_equal(.fileinfo("file=data.csv")$file, "data.csv")
})

test_that("a long equation block keeps its statement order (#51)", {
  .n <- 5000
  .ret <- .equation(paste(
    sprintf("y%d = a%d + 1", seq_len(.n), seq_len(.n)),
    collapse = "\n"
  ))
  expect_equal(.ret$rx, sprintf("y%d <- a%d + 1", seq_len(.n), seq_len(.n)))
})
