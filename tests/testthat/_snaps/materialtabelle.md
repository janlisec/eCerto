# Init of materialtable after Certifications uploaded

    Code
      mater_table()
    Output
         analyte   mean F1 F2 F3 cert_val     sd  n char U2 U3 U4 U5 U6 U7 com k  U
      1       Si 0.0493  1  1  1       NA 0.0034  3    0  0  0  0  0  0  0  NA 2 NA
      2       Fe     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      3       Cu     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      4       Mn     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      5       Mg     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      6       Cr     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      7       Ni     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      8       Zn     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      9       Ti     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      10      Sc     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      11      Sn     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA

# materialtable gets updated after another analyte gets selected

    Code
      mater_table()
    Output
         analyte   mean F1 F2 F3 cert_val     sd  n char U2 U3 U4 U5 U6 U7 com k  U
      1       Si 0.0493  1  1  1       NA 0.0034  3    0  0  0  0  0  0  0  NA 2 NA
      2       Fe     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      3       Cu     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      4       Mn     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      5       Mg     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      6       Cr     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      7       Ni     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      8       Zn     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      9       Ti     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      10      Sc     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA
      11      Sn     NA  1  1  1       NA     NA NA    0  0  0  0  0  0  0  NA 2 NA

---

    Code
      mater_table()
    Output
         analyte   mean F1 F2 F3 cert_val     sd  n       char U2 U3 U4 U5 U6 U7
      1       Si 0.0493  1  1  1   0.0493 0.0034  3 0.03981726  0  0  0  0  0  0
      2       Fe 0.0516  1  1  1   0.0516 0.0020  3 0.02237792  0  0  0  0  0  0
      3       Cu     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      4       Mn     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      5       Mg     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      6       Cr     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      7       Ni     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      8       Zn     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      9       Ti     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      10      Sc     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      11      Sn     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
                com k          U
      1  0.03981726 2 0.07963452
      2  0.02237792 2 0.04475583
      3  0.00000000 2 0.00000000
      4  0.00000000 2 0.00000000
      5  0.00000000 2 0.00000000
      6  0.00000000 2 0.00000000
      7  0.00000000 2 0.00000000
      8  0.00000000 2 0.00000000
      9  0.00000000 2 0.00000000
      10 0.00000000 2 0.00000000
      11 0.00000000 2 0.00000000

# Pooling on/off

    Code
      mater_table()
    Output
        analyte   mean F1 F2 F3 cert_val    sd  n char U2 U3 U4 U5 U6 U7 com k  U
      1      Si     NA  1  1  1       NA    NA NA    0  0  0  0  0  0  0  NA 2 NA
      2      Fe 0.0516  1  1  1       NA 0.002  3    0  0  0  0  0  0  0  NA 2 NA
      3      Cu     NA  1  1  1       NA    NA NA    0  0  0  0  0  0  0  NA 2 NA
      4      Mn     NA  1  1  1       NA    NA NA    0  0  0  0  0  0  0  NA 2 NA
      5      Mg     NA  1  1  1       NA    NA NA    0  0  0  0  0  0  0  NA 2 NA
      6      Cr     NA  1  1  1       NA    NA NA    0  0  0  0  0  0  0  NA 2 NA
      7      Ni     NA  1  1  1       NA    NA NA    0  0  0  0  0  0  0  NA 2 NA

---

    Code
      mater_table()
    Output
        analyte   mean F1 F2 F3 cert_val     sd  n       char U2 U3 U4 U5 U6 U7
      1      Si     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      2      Fe 0.0516  1  1  1   0.0516 0.0029  9 0.01873385  0  0  0  0  0  0
      3      Cu     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      4      Mn     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      5      Mg     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      6      Cr     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
      7      Ni     NA  1  1  1   1.0000     NA NA         NA  0  0  0  0  0  0
               com k         U
      1 0.00000000 2 0.0000000
      2 0.01873385 2 0.0374677
      3 0.00000000 2 0.0000000
      4 0.00000000 2 0.0000000
      5 0.00000000 2 0.0000000
      6 0.00000000 2 0.0000000
      7 0.00000000 2 0.0000000

