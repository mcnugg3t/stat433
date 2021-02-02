HW1
================

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.5     ✓ dplyr   1.0.3
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#read html
download.file('https://guide.wisc.edu/faculty/', 'faculty.html')
file <- read_html('faculty.html', skip=0, remove.empty=T, trim=T)

#function to take character vector, split it into 4 columns
make.tibble = function(chr.vector, start.pos, end.trim, debug=F) {
  name <- c(); position <- c(); department <- c(); degree <- c();
  
  if (debug) {cat("starting length of chr.vector is: ", length(chr.vector))}
  
  chr.vector <- chr.vector[start.pos:length(chr.vector)]
  
  if (debug) {cat("\nlength of chr.vector trimmed to: ", length(chr.vector))}
  
  for (i in seq_len(length(chr.vector) - end.trim)) {
    if (i%%4 == 1) {
      name <- c(name,chr.vector[i])  
    } else if (i%%4==2) {
      position <- c(position, chr.vector[i])
    } else if (i%%4==3) {
      department <- c(department, chr.vector[i])
    } else if (i%%4==0) {
      degree <- c(degree,chr.vector[i])
    } else {
      cat("something wrong in modulo")
    }
  }
  if(debug) {cat("\nlength names: ", length(name), "\nlength positions: ", length(position), "\nlength schools: ", length(department), "\nlength degrees: ", length(degree))}
  
  tbl.to.return <- tibble(name, position, department, degree)
  
  return(tbl.to.return)
}

#run function on our data
faculty.tbl <- make.tibble(file, 60, 1, debug=T)
```

    ## starting length of chr.vector is:  16044
    ## length of chr.vector trimmed to:  15985
    ## length names:  3996 
    ## length positions:  3996 
    ## length schools:  3996 
    ## length degrees:  3996

``` r
head(faculty.tbl)
```

    ## # A tibble: 6 x 4
    ##   name           position          department           degree                  
    ##   <chr>          <chr>             <chr>                <chr>                   
    ## 1 AARLI,LISA     Lecturer          Counseling Psycholo… EDM 2000 Univ of Wiscon…
    ## 2 ABBOTT,DANIEL… Assoc Professor … Surgery              MD 2016 University of W…
    ## 3 ABBOTT,DAVID … Professor         Obstetrics & Gyneco… PHD 1979 University of …
    ## 4 ABBOTT,NICHOL… Professor         Chemical & Biologic… PHD 1991 Massachusetts …
    ## 5 ABD-ELSAYED,A… Asst Professor (… Anesthesiology       MD 2000                 
    ## 6 ABDUALLAH,FAI… Associate Profes… Art                  PHD 2012 Royal College …
