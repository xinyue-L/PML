---
title: "cran-comments"
output: pdf_document
---

## Test environments
* Windows x86_64: R 3.6.1
* Red Hat Enterprise Linux 7.5: R 3.4.1
* devtools::check_win_release()

## R CMD check results
There were no ERRORs or WARNINGs. In windows, there are no NOTES.

In Linux, 2 NOTES pop up:

1. Rd file 
   examples lines wider than 100 characters
   These lines will be truncated in the PDF manual.
   
I checked the manual and the examples are not shortened, so I leave it as it is. 
(I actually revised the examples in windows but it automatically reformatted in Linux when running R CMD check.)

2. no visible global function definition for 'lowess' 'fft'
Consider adding
importFrom("stats","fft","lowess")

I actually added it to the NAMESPACE file, but it automatically removes it in Linux when running R CMD check,
so I leave it as it is.


In devtools::check_win_release(), 4 NOTES pop up:

1. Possibly mis-spelled words in DESCRIPTION:
  actigraphy, accelerometer, trelliscope
  
However, they are not mis-spelled, so I leave it as it is.

2. & 3. two examples with CPU or elapsed time > 10s

I revised the examples and then the notes no longer pop up.

4. Non-standard file/directory found at top level: 'KEYWORDS'

I removed it from the folder. 

## Small Notes
1. Sometimes it checks for vignettes in 'inst/doc' ... WARNING
Package vignette without corresponding PDF/HTML: 'PML.Rmd'

Therefore, I created and put the pdf manual in 'inst/doc'.
Then no warnings pop up anymore and Status: OK. 

2. checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'PML.pdf' from 700Kb to 188Kb

Therefore I submitted compressed pdf only, with 200Kb. (It was not achieved through tools::compactPDF(gs_quality = "ebook") because it did not work at all. I installed qpdf and tried --compact-vignettes but it did not work. The file is not compressed and no error messages pop up. Therefore I used alternative methods to generate compressed pdf files and build the package without the need to rebuild vignettes.)

3. I am submitting this R package under the request of the journal reviewer. After the journal gets published, I will be able to update the information on the reference, giving the exact location as to where it can be accessed.

## First Submission
This is my first submission and thank you very much for your help!