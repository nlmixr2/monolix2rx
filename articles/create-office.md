# Create PowerPoint and Word documents using monolix2rx

## Step 1: import the model into `monolix2rx`

``` r


library(monolix2rx)
library(babelmixr2)
library(nlmixr2rpt)
library(onbrand)

library(nonmem2rx)

# First we need the location of the monolix mlxtran file. Since we are
# running an example, we will use one of the built-in examples in
# `monolix2rx`
pkgTheo <- system.file("theo/theophylline_project.mlxtran", package="monolix2rx")
# You can use a control stream or other file. With the development
# version of `babelmixr2`, you can simply point to the listing file

mod <- monolix2rx(pkgTheo)
#> ℹ integrated model file 'oral1_1cpt_kaVCl.txt' into mlxtran object
#> ℹ updating model values to final parameter estimates
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesLin.txt
#> ℹ done
#> Warning in .dataRenameFromMlxtran(data, .mlxtran): NAs introduced by coercion
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_SAEM) imported to rxode2 compatible data ($etaData)
#> ℹ imported monolix pred/ipred data to compare ($predIpredData)
#> ℹ solving ipred problem
#> ℹ done
#> ℹ solving pred problem
#> ℹ done
```

## Step 2: convert the `rxode2` model to `nlmixr2`

You can convert the model, `mod`, to a nlmixr2 fit object:

``` r

fit <- as.nlmixr2(mod)
#> → loading into symengine environment...
#> → pruning branches (`if`/`else`) of full model...
#> ✔ done
#> → finding duplicate expressions in EBE model...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → optimizing duplicate expressions in EBE model...
#> [====|====|====|====|====|====|====|====|====|====] 0:00:00
#> → compiling EBE model...
#> ✔ done
#> rxode2 5.0.2 using 2 threads (see ?getRxThreads)
#>   no cache: create with `rxCreateCache()`
#> → Calculating residuals/tables
#> ✔ done
#> ℹ monolix parameter history integrated into fit object

fit
```

``` math
\begin{align*}
cmt({depot}) \\
cmt({central}) \\
{ka} & = \exp\left({ka\_pop}+{omega\_ka}\right) \\
{V} & = \exp\left({V\_pop}+{omega\_V}\right) \\
{Cl} & = \exp\left({Cl\_pop}+{omega\_Cl}\right) \\
\frac{d \: depot}{dt} & = -{ka} {\times} {depot} \\
\frac{d \: central}{dt} & = +{ka} {\times} {depot}-\frac{{Cl}}{{V}} {\times} {central} \\
{Cc} & = \frac{{central}}{{V}} \\
{CONC} & = {Cc} \\
{CONC} & \sim add({a})+prop({b})+combined1()
\end{align*}
```

## Step 3: Create a PowerPoint file

A PowerPoint can be created from your own [custom powerpoint
templates](https://nlmixr2.github.io/nlmixr2rpt/articles/Reporting_nlmixr_Fit_Results.html#customizing-reports-for-your-organization),
but in this example we will use the ones that come from `nlmixr2rpt`
directly:

``` r

obnd_pptx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

obnd_pptx = report_fit(
  fit     = fit,
  obnd    = obnd_pptx)
#> 
#> Attaching package: 'xpose'
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> 
#> Attaching package: 'ggPMX'
#> The following object is masked from 'package:xpose':
#> 
#>     get_data
#> → Calculating residuals/tables
#> ✔ done
#> Warning in xpose.nlmixr2::xpose_data_nlmixr(fit): Added CWRES to fit (using
#> nlmixr2est::addCwres)...
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Warning in melt.data.table(dx.cats, measure.vars = cats): id.vars and
#> measure.vars are internally guessed when both are 'NULL'. All
#> non-numeric/integer/logical type columns are considered id.vars, which in this
#> case are columns [EFFECT]. Consider providing at least one of 'id' or 'measure'
#> vars in future.
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Skipping table: skip_table (NA found, not generated)
#> Skipping figure: res_vs_pred_idv (NA found, not generated)
#> Skipping figure: eta_cont (NA found, not generated)
#> Skipping figure: eta_cat (NA found, not generated)
#> Skipping figure: skip_figure (NA found, not generated)

save_report(obnd_pptx, "mod-PowerPoint.pptx")
#> $isgood
#> [1] TRUE
#> 
#> $msgs
#> NULL
```

Which gives the powerpoint [here](mod-PowerPoint.pptx)

## Step 4: Create a Word file

Just like in PowerPoint, you can customizeown [custom word
templates](https://nlmixr2.github.io/nlmixr2rpt/articles/Reporting_nlmixr_Fit_Results.html#customizing-reports-for-your-organization),
but in this example we will use the ones that come from `nlmixr2rpt`
directly:

``` r

obnd_docx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

obnd_docx = report_fit(
  fit     = fit,
  obnd    = obnd_docx)
#> → Calculating residuals/tables
#> ✔ done
#> Warning in xpose.nlmixr2::xpose_data_nlmixr(fit): Added CWRES to fit (using
#> nlmixr2est::addCwres)...
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Warning in melt.data.table(dx.cats, measure.vars = cats): id.vars and
#> measure.vars are internally guessed when both are 'NULL'. All
#> non-numeric/integer/logical type columns are considered id.vars, which in this
#> case are columns [EFFECT]. Consider providing at least one of 'id' or 'measure'
#> vars in future.
#> Warning in is.na(p_res): is.na() applied to non-(list or vector) of type
#> 'object'
#> Skipping figure: res_vs_pred_idv (NA found, not generated)
#> Skipping figure: skip_figure (NA found, not generated)
#> Skipping figure: eta_cont (NA found, not generated)
#> Skipping figure: eta_cat (NA found, not generated)

save_report(obnd_docx, "mod-Word.docx")
#> $isgood
#> [1] TRUE
#> 
#> $msgs
#> NULL
```

Which gives the word document [here](mod-Word.docx)
