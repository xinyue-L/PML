---
title: "cran-comments"
output: pdf_document
---

## Test environments
* Windows x86_64: R 3.6.1
* Red Hat Enterprise Linux 7.5: R 3.4.1
* devtools::check_win_release()

## R CMD check results
There were no ERRORs or WARNINGs. 

## Revision according to CRAN feedback 08/23/2019

1. Please replace \dontrun{} by if(interactive()){} in your Rd-files.

Thank you for your suggestion. I have modified the examples in the Rd-files to include non-interactive examples only, and I have removed \dontrun{}.

## First Submission of the PML package
Thank you very much for your help!