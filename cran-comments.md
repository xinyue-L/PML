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

## Revision according to CRAN feedback 08/22/2019

1. If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file

We have added reference in the description field of the DESCRIPTION file: Li, X., Kane, M., Zhang, Y., Sun, W., Song, Y., Dong, S., Lin, Q., Zhu, Q., Jiang, F., Zhao, H. (2019) A Novel Penalized Multi-band Learning Approach Characterizes the Consolidation of Sleep-Wake Circadian Rhythms During Early Childhood Development.

It is under revision at the Journal of Biological Rhythms and we are submitting this R package at the request of the reviewer. After the journal gets published, I will be able to update the information, giving the exact location as to where it can be accessed.

2. Some code lines in examples are commented out.
Please never do that. Ideally find toy examples that can be regularly 
executed and checked. Lengthy examples (> 5 sec), can be wrapped in 
\donttest{}.

We have uncommented the code examples accordingly. Lengthy examples for tre are wrapped because trelliscope interactive visualization panels are complex and time-consuming to generate.

3. Please always write TRUE and FALSE instead of T and F.

Thank you for your suggestion and I have replaced all T and F with TRUE and FALSE accordingly.

4. You write information messages to the console that cannot be easily 
suppressed.
Instead of print()/cat() rather use message()/warning()  or 
if(verbose)cat(..) if you really have to write text to the console.
(except for print() and summary() functions)

Thank you for your suggestion and I have changed all print functions to message functions.

## First Submission of the PML package
Thank you very much for your help!