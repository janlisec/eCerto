# Successful RData (version 26) Upload

    Code
      bbb
    Output
      $user
      [1] "FK4"
      
      $study_id
      [1] "TEST4"
      
      $dataformat_version
      [1] "2021-05-27"
      
      $apm
      $apm$Si
      $apm$Si$precision
      [1] 4
      
      $apm$Si$sample_filter
      NULL
      
      $apm$Si$sample_ids
       [1]   1   8  15  22  29  36  43  50  57  64  71  78  85  92  99 106 113 120
      
      $apm$Si$analytename
      [1] "Si"
      
      $apm$Si$confirmed
      [1] FALSE
      
      
      $apm$Fe
      $apm$Fe$precision
      NULL
      
      $apm$Fe$sample_filter
      NULL
      
      $apm$Fe$sample_ids
       [1]   2   9  16  23  30  37  44  51  58  65  72  79  86  93 100 107 114 121
      
      $apm$Fe$lab_filter
      NULL
      
      $apm$Fe$analytename
      [1] "Fe"
      
      $apm$Fe$confirmed
      [1] FALSE
      
      
      $apm$Cu
      $apm$Cu$precision
      [1] 4
      
      $apm$Cu$sample_filter
      NULL
      
      $apm$Cu$sample_ids
       [1]   3  10  17  24  31  38  45  52  59  66  73  80  87  94 101 108 115 122
      
      $apm$Cu$lab_filter
      NULL
      
      $apm$Cu$analytename
      [1] "Cu"
      
      $apm$Cu$confirmed
      [1] TRUE
      
      
      $apm$Mn
      $apm$Mn$precision
      [1] 2
      
      $apm$Mn$sample_filter
      [1] "11" "25"
      
      $apm$Mn$sample_ids
       [1]   4  11  18  25  32  39  46  53  60  67  74  81  88  95 102 109 116 123
      
      $apm$Mn$lab_filter
      NULL
      
      $apm$Mn$analytename
      [1] "Mn"
      
      $apm$Mn$confirmed
      [1] TRUE
      
      
      $apm$Mg
      $apm$Mg$precision
      NULL
      
      $apm$Mg$sample_filter
      NULL
      
      $apm$Mg$sample_ids
       [1]   5  12  19  26  33  40  47  54  61  68  75  82  89  96 103 110 117 124
      
      $apm$Mg$lab_filter
      NULL
      
      $apm$Mg$analytename
      [1] "Mg"
      
      $apm$Mg$confirmed
      [1] FALSE
      
      
      $apm$Cr
      $apm$Cr$precision
      [1] 4
      
      $apm$Cr$sample_filter
      NULL
      
      $apm$Cr$sample_ids
       [1]   6  13  20  27  34  41  48  55  62  69  76  83  90  97 104 111 118 125
      
      $apm$Cr$lab_filter
      [1] "L2"
      
      $apm$Cr$analytename
      [1] "Cr"
      
      $apm$Cr$confirmed
      [1] TRUE
      
      
      $apm$Ni
      $apm$Ni$precision
      NULL
      
      $apm$Ni$sample_filter
      NULL
      
      $apm$Ni$sample_ids
       [1]   7  14  21  28  35  42  49  56  63  70  77  84  91  98 105 112 119 126
      
      $apm$Ni$lab_filter
      NULL
      
      $apm$Ni$analytename
      [1] "Ni"
      
      $apm$Ni$confirmed
      [1] FALSE
      
      
      

---

    Code
      shiny::reactiveValuesToList(getValue(rv, "Certification"))
    Output
      $input_files
      NULL
      
      $uploadsource
      [1] "RData"
      
      $data
           ID Lab analyte replicate   value unit S_flt L_flt
      1     1  L1      Si         1 0.05040 0.05 FALSE FALSE
      2     2  L1      Fe         1 0.04900 0.05 FALSE FALSE
      3     3  L1      Cu         1 4.37000  4.3 FALSE FALSE
      4     4  L1      Mn         1 0.80500  0.8 FALSE FALSE
      5     5  L1      Mg         1 1.50700  1.5 FALSE FALSE
      6     6  L1      Cr         1 0.05490 0.05 FALSE FALSE
      7     7  L1      Ni         1 0.04960 0.05 FALSE FALSE
      8     8  L1      Si         2 0.05120 0.05 FALSE FALSE
      9     9  L1      Fe         2 0.05630 0.05 FALSE FALSE
      10   10  L1      Cu         2 4.38500  4.3 FALSE FALSE
      11   11  L1      Mn         2 0.80890  0.8 FALSE FALSE
      12   12  L1      Mg         2 1.51100  1.5 FALSE FALSE
      13   13  L1      Cr         2 0.05520 0.05 FALSE FALSE
      14   14  L1      Ni         2 0.04970 0.05 FALSE FALSE
      15   15  L1      Si         3 0.05240 0.05 FALSE FALSE
      16   16  L1      Fe         3 0.05150 0.05 FALSE FALSE
      17   17  L1      Cu         3 4.34000  4.3 FALSE FALSE
      18   18  L1      Mn         3 0.80900  0.8 FALSE FALSE
      19   19  L1      Mg         3 1.50600  1.5 FALSE FALSE
      20   20  L1      Cr         3 0.05500 0.05 FALSE FALSE
      21   21  L1      Ni         3 0.04980 0.05 FALSE FALSE
      22   22  L1      Si         4 0.05200 0.05 FALSE FALSE
      23   23  L1      Fe         4 0.05050 0.05 FALSE FALSE
      24   24  L1      Cu         4 4.38800  4.3 FALSE FALSE
      25   25  L1      Mn         4 0.81300  0.8 FALSE FALSE
      26   26  L1      Mg         4 1.50900  1.5 FALSE FALSE
      27   27  L1      Cr         4 0.05550 0.05 FALSE FALSE
      28   28  L1      Ni         4 0.05060 0.05 FALSE FALSE
      29   29  L1      Si         5 0.05200 0.05 FALSE FALSE
      30   30  L1      Fe         5 0.05050 0.05 FALSE FALSE
      31   31  L1      Cu         5 4.32900  4.3 FALSE FALSE
      32   32  L1      Mn         5 0.80900  0.8 FALSE FALSE
      33   33  L1      Mg         5 1.49800  1.5 FALSE FALSE
      34   34  L1      Cr         5 0.05510 0.05 FALSE FALSE
      35   35  L1      Ni         5 0.04990 0.05 FALSE FALSE
      36   36  L1      Si         6 0.05100 0.05 FALSE FALSE
      37   37  L1      Fe         6 0.05050 0.05 FALSE FALSE
      38   38  L1      Cu         6 4.36500  4.3 FALSE FALSE
      39   39  L1      Mn         6 0.81200  0.8 FALSE FALSE
      40   40  L1      Mg         6 1.51100  1.5 FALSE FALSE
      41   41  L1      Cr         6 0.05540 0.05 FALSE FALSE
      42   42  L1      Ni         6 0.05050 0.05 FALSE FALSE
      43   43  L2      Si         1 0.04520 0.05 FALSE FALSE
      44   44  L2      Fe         1 0.05290 0.05 FALSE FALSE
      45   45  L2      Cu         1 4.40480  4.3 FALSE FALSE
      46   46  L2      Mn         1 0.81790  0.8 FALSE FALSE
      47   47  L2      Mg         1 1.51790  1.5 FALSE FALSE
      48   48  L2      Cr         1 0.05485 0.05 FALSE FALSE
      49   49  L2      Ni         1 0.04970 0.05 FALSE FALSE
      50   50  L2      Si         2 0.04350 0.05 FALSE FALSE
      51   51  L2      Fe         2 0.05270 0.05 FALSE FALSE
      52   52  L2      Cu         2 4.38020  4.3 FALSE FALSE
      53   53  L2      Mn         2 0.80840  0.8 FALSE FALSE
      54   54  L2      Mg         2 1.49880  1.5 FALSE FALSE
      55   55  L2      Cr         2 0.05470 0.05 FALSE FALSE
      56   56  L2      Ni         2 0.04930 0.05 FALSE FALSE
      57   57  L2      Si         3 0.04720 0.05 FALSE FALSE
      58   58  L2      Fe         3 0.04820 0.05 FALSE FALSE
      59   59  L2      Cu         3 4.39070  4.3 FALSE FALSE
      60   60  L2      Mn         3 0.81120  0.8 FALSE FALSE
      61   61  L2      Mg         3 1.51700  1.5 FALSE FALSE
      62   62  L2      Cr         3 0.05420 0.05 FALSE FALSE
      63   63  L2      Ni         3 0.04950 0.05 FALSE FALSE
      64   64  L2      Si         4 0.04560 0.05 FALSE FALSE
      65   65  L2      Fe         4 0.04780 0.05 FALSE FALSE
      66   66  L2      Cu         4 4.39100  4.3 FALSE FALSE
      67   67  L2      Mn         4 0.81160  0.8 FALSE FALSE
      68   68  L2      Mg         4 1.50480  1.5 FALSE FALSE
      69   69  L2      Cr         4 0.05440 0.05 FALSE FALSE
      70   70  L2      Ni         4 0.04900 0.05 FALSE FALSE
      71   71  L2      Si         5 0.04640 0.05 FALSE FALSE
      72   72  L2      Fe         5 0.04880 0.05 FALSE FALSE
      73   73  L2      Cu         5 4.37980  4.3 FALSE FALSE
      74   74  L2      Mn         5 0.80980  0.8 FALSE FALSE
      75   75  L2      Mg         5 1.50850  1.5 FALSE FALSE
      76   76  L2      Cr         5 0.05520 0.05 FALSE FALSE
      77   77  L2      Ni         5 0.04980 0.05 FALSE FALSE
      78   78  L2      Si         6 0.04520 0.05 FALSE FALSE
      79   79  L2      Fe         6 0.04660 0.05 FALSE FALSE
      80   80  L2      Cu         6 4.39240  4.3 FALSE FALSE
      81   81  L2      Mn         6 0.81120  0.8 FALSE FALSE
      82   82  L2      Mg         6 1.51020  1.5 FALSE FALSE
      83   83  L2      Cr         6 0.05420 0.05 FALSE FALSE
      84   84  L2      Ni         6 0.04960 0.05 FALSE FALSE
      85   85  L3      Si         1 0.05110 0.05 FALSE FALSE
      86   86  L3      Fe         1 0.04950 0.05 FALSE FALSE
      87   87  L3      Cu         1 4.44830  4.3 FALSE FALSE
      88   88  L3      Mn         1 0.80540  0.8 FALSE FALSE
      89   89  L3      Mg         1 1.50070  1.5 FALSE FALSE
      90   90  L3      Cr         1 0.05390 0.05 FALSE FALSE
      91   91  L3      Ni         1 0.05000 0.05 FALSE FALSE
      92   92  L3      Si         2 0.05080 0.05 FALSE FALSE
      93   93  L3      Fe         2 0.04890 0.05 FALSE FALSE
      94   94  L3      Cu         2 4.46660  4.3 FALSE FALSE
      95   95  L3      Mn         2 0.80480  0.8 FALSE FALSE
      96   96  L3      Mg         2 1.49950  1.5 FALSE FALSE
      97   97  L3      Cr         2 0.05380 0.05 FALSE FALSE
      98   98  L3      Ni         2 0.05010 0.05 FALSE FALSE
      99   99  L3      Si         3 0.05150 0.05 FALSE FALSE
      100 100  L3      Fe         3 0.05000 0.05 FALSE FALSE
      101 101  L3      Cu         3 4.46230  4.3 FALSE FALSE
      102 102  L3      Mn         3 0.80630  0.8 FALSE FALSE
      103 103  L3      Mg         3 1.50080  1.5 FALSE FALSE
      104 104  L3      Cr         3 0.05380 0.05 FALSE FALSE
      105 105  L3      Ni         3 0.05000 0.05 FALSE FALSE
      106 106  L3      Si         4 0.05080 0.05 FALSE FALSE
      107 107  L3      Fe         4 0.04990 0.05 FALSE FALSE
      108 108  L3      Cu         4 4.46740  4.3 FALSE FALSE
      109 109  L3      Mn         4 0.80580  0.8 FALSE FALSE
      110 110  L3      Mg         4 1.50160  1.5 FALSE FALSE
      111 111  L3      Cr         4 0.05400 0.05 FALSE FALSE
      112 112  L3      Ni         4 0.05040 0.05 FALSE FALSE
      113 113  L3      Si         5 0.05130 0.05 FALSE FALSE
      114 114  L3      Fe         5 0.04980 0.05 FALSE FALSE
      115 115  L3      Cu         5 4.48260  4.3 FALSE FALSE
      116 116  L3      Mn         5 0.80820  0.8 FALSE FALSE
      117 117  L3      Mg         5 1.51150  1.5 FALSE FALSE
      118 118  L3      Cr         5 0.05420 0.05 FALSE FALSE
      119 119  L3      Ni         5 0.05060 0.05 FALSE FALSE
      120 120  L3      Si         6 0.05210 0.05 FALSE FALSE
      121 121  L3      Fe         6 0.04960 0.05 FALSE FALSE
      122 122  L3      Cu         6 4.48570  4.3 FALSE FALSE
      123 123  L3      Mn         6 0.80820  0.8 FALSE FALSE
      124 124  L3      Mg         6 1.51070  1.5 FALSE FALSE
      125 125  L3      Cr         6 0.05420 0.05 FALSE FALSE
      126 126  L3      Ni         6 0.05040 0.05 FALSE FALSE
      

---

    Code
      shiny::reactiveValuesToList(getValue(rv, "Homogeneity"))
    Output
      $h_file
      NULL
      
      $h_sel_analyt
      NULL
      
      $data
         analyte H_type Flasche     value  unit            File
      1       Fe radial       3 0.2897693  mM/L Homog_Test.xlsx
      2       Fe radial      36 0.2967753  mM/L Homog_Test.xlsx
      3       Fe radial      62 0.3075952  mM/L Homog_Test.xlsx
      4       Fe radial      78 0.3006643  mM/L Homog_Test.xlsx
      5       Fe radial     109 0.2981130  mM/L Homog_Test.xlsx
      6       Fe radial     144 0.3018592  mM/L Homog_Test.xlsx
      7       Fe radial     162 0.3051646  mM/L Homog_Test.xlsx
      8       Fe  axial     200 0.2934924  mM/L Homog_Test.xlsx
      9       Fe  axial     225 0.2369928  mM/L Homog_Test.xlsx
      10      Fe  axial     239 0.2990771  mM/L Homog_Test.xlsx
      11      Fe  axial     256 0.2916479  mM/L Homog_Test.xlsx
      12      Fe  axial     295 0.2981732  mM/L Homog_Test.xlsx
      13      Fe  axial     325 0.3033135  mM/L Homog_Test.xlsx
      14      Fe  axial     351 0.3084570  mM/L Homog_Test.xlsx
      15      Fe  axial     397 0.2988461  mM/L Homog_Test.xlsx
      16      Fe radial       3 0.2938025  mM/L Homog_Test.xlsx
      17      Fe radial      36 0.2913924  mM/L Homog_Test.xlsx
      18      Fe radial      62 0.2910948  mM/L Homog_Test.xlsx
      19      Fe radial      78 0.2835857  mM/L Homog_Test.xlsx
      20      Fe radial     109 0.2985147  mM/L Homog_Test.xlsx
      21      Fe radial     144 0.2884778  mM/L Homog_Test.xlsx
      22      Fe radial     162 0.2940975  mM/L Homog_Test.xlsx
      23      Fe  axial     200 0.2865534  mM/L Homog_Test.xlsx
      24      Fe  axial     225 0.2940975  mM/L Homog_Test.xlsx
      25      Fe  axial     239 0.2965681  mM/L Homog_Test.xlsx
      26      Fe  axial     256 0.2907160  mM/L Homog_Test.xlsx
      27      Fe  axial     295 0.2911270  mM/L Homog_Test.xlsx
      28      Fe  axial     325 0.2884299  mM/L Homog_Test.xlsx
      29      Fe  axial     351 0.2839751  mM/L Homog_Test.xlsx
      30      Fe  axial     397 0.2967776  mM/L Homog_Test.xlsx
      31      Fe radial       3 0.2777090  mM/L Homog_Test.xlsx
      32      Fe radial      36 0.2886654  mM/L Homog_Test.xlsx
      33      Fe radial      62 0.2948648  mM/L Homog_Test.xlsx
      34      Fe radial      78 0.2924766  mM/L Homog_Test.xlsx
      35      Fe radial     109 0.2794004  mM/L Homog_Test.xlsx
      36      Fe radial     144 0.2918912  mM/L Homog_Test.xlsx
      37      Fe radial     162 0.2875026  mM/L Homog_Test.xlsx
      38      Fe  axial     200 0.2895977  mM/L Homog_Test.xlsx
      39      Fe  axial     225 0.2874214  mM/L Homog_Test.xlsx
      40      Fe  axial     239 0.2900750  mM/L Homog_Test.xlsx
      41      Fe  axial     256 0.2998802  mM/L Homog_Test.xlsx
      42      Fe  axial     295 0.2839409  mM/L Homog_Test.xlsx
      43      Fe  axial     325 0.2866047  mM/L Homog_Test.xlsx
      44      Fe  axial     351 0.2902880  mM/L Homog_Test.xlsx
      45      Fe  axial     397 0.2854040  mM/L Homog_Test.xlsx
      46      Mg radial       3 0.2897693 mg/mL Homog_Test.xlsx
      47      Mg radial      36 0.2967753 mg/mL Homog_Test.xlsx
      48      Mg radial      62 0.3075952 mg/mL Homog_Test.xlsx
      49      Mg radial      78 0.3006643 mg/mL Homog_Test.xlsx
      50      Mg radial     109 0.2981130 mg/mL Homog_Test.xlsx
      51      Mg radial     144 0.3018592 mg/mL Homog_Test.xlsx
      52      Mg radial     162 0.3051646 mg/mL Homog_Test.xlsx
      53      Mg  axial     200 0.2934924 mg/mL Homog_Test.xlsx
      54      Mg  axial     225 0.2369928 mg/mL Homog_Test.xlsx
      55      Mg  axial     239 0.2990771 mg/mL Homog_Test.xlsx
      56      Mg  axial     256 0.2916479 mg/mL Homog_Test.xlsx
      57      Mg  axial     295 0.2981732 mg/mL Homog_Test.xlsx
      58      Mg  axial     325 0.3033135 mg/mL Homog_Test.xlsx
      59      Mg  axial     351 0.3084570 mg/mL Homog_Test.xlsx
      60      Mg  axial     397 0.2988461 mg/mL Homog_Test.xlsx
      61      Mg radial       3 0.2938025 mg/mL Homog_Test.xlsx
      62      Mg radial      36 0.2913924 mg/mL Homog_Test.xlsx
      63      Mg radial      62 0.2910948 mg/mL Homog_Test.xlsx
      64      Mg radial      78 0.2835857 mg/mL Homog_Test.xlsx
      65      Mg radial     109 0.2985147 mg/mL Homog_Test.xlsx
      66      Mg radial     144 0.2884778 mg/mL Homog_Test.xlsx
      67      Mg radial     162 0.3040194 mg/mL Homog_Test.xlsx
      68      Mg  axial     200 0.2865534 mg/mL Homog_Test.xlsx
      69      Mg  axial     225 0.2940975 mg/mL Homog_Test.xlsx
      70      Mg  axial     239 0.2965681 mg/mL Homog_Test.xlsx
      71      Mg  axial     256 0.2907160 mg/mL Homog_Test.xlsx
      72      Mg  axial     295 0.2911270 mg/mL Homog_Test.xlsx
      73      Mg  axial     325 0.2884299 mg/mL Homog_Test.xlsx
      74      Mg  axial     351 0.2839751 mg/mL Homog_Test.xlsx
      75      Mg  axial     397 0.2967776 mg/mL Homog_Test.xlsx
      76      Mg radial       3 0.2777090 mg/mL Homog_Test.xlsx
      77      Mg radial      36 0.2886654 mg/mL Homog_Test.xlsx
      78      Mg radial      62 0.2948648 mg/mL Homog_Test.xlsx
      79      Mg radial      78 0.2924766 mg/mL Homog_Test.xlsx
      80      Mg radial     109 0.2794004 mg/mL Homog_Test.xlsx
      81      Mg radial     144 0.2918912 mg/mL Homog_Test.xlsx
      82      Mg radial     162 0.2875026 mg/mL Homog_Test.xlsx
      83      Mg  axial     200 0.2895977 mg/mL Homog_Test.xlsx
      84      Mg  axial     225 0.2874214 mg/mL Homog_Test.xlsx
      85      Mg  axial     239 0.2900750 mg/mL Homog_Test.xlsx
      86      Mg  axial     256 0.2998802 mg/mL Homog_Test.xlsx
      87      Mg  axial     295 0.2839409 mg/mL Homog_Test.xlsx
      88      Mg  axial     325 0.2866047 mg/mL Homog_Test.xlsx
      89      Mg  axial     351 0.2902880 mg/mL Homog_Test.xlsx
      90      Mg  axial     397 0.2854040 mg/mL Homog_Test.xlsx
      
      $h_precision
      NULL
      
      $uploadsource
      [1] "Excel"
      
      $h_vals
      NULL
      
      $h_Fig_width
      NULL
      

---

    Code
      shiny::reactiveValuesToList(getValue(rv, "Stability"))
    Output
      $s_vals
      NULL
      
      $data
      NULL
      
      $uploadsource
      NULL
      
      $file
      NULL
      

