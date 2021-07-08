# Successful RData (version 26) Upload

    Code
      bbb[!names(bbb) %in% "time_stamp"]
    Output
      $user
      [1] "FK2"
      
      $study_id
      [1] "TEST2"
      
      $dataformat_version
      [1] "2021-05-27"
      
      $apm
      $apm$Si
      $apm$Si$precision
      [1] 4
      
      $apm$Si$sample_filter
      [1] "13" "49"
      
      $apm$Si$sample_ids
       [1]   1  13  25  37  49  61  73  86  99 112 125 138 151 164 177 190 203 216
      
      $apm$Si$lab_filter
      [1] "L2"
      
      $apm$Si$analytename
      [1] "Si"
      
      
      $apm$Fe
      $apm$Fe$precision
      [1] 4
      
      $apm$Fe$sample_filter
      NULL
      
      $apm$Fe$sample_ids
       [1]   2  14  26  38  50  62  74  87 100 113 126 139 152 165 178 191 204 217
      
      $apm$Fe$lab_filter
      [1] "L1"
      
      $apm$Fe$analytename
      [1] "Fe"
      
      
      $apm$Cu
      $apm$Cu$precision
      [1] 4
      
      $apm$Cu$sample_filter
      NULL
      
      $apm$Cu$sample_ids
       [1]   3  15  27  39  51  63  75  88 101 114 127 140 153 166 179 192 205 218
      
      $apm$Cu$lab_filter
      NULL
      
      $apm$Cu$analytename
      [1] "Cu"
      
      
      $apm$Mn
      $apm$Mn$precision
      [1] 4
      
      $apm$Mn$sample_filter
      [1] "16" "64"
      
      $apm$Mn$sample_ids
       [1]   4  16  28  40  52  64  76  89 102 115 128 141 154 167 180 193 206 219
      
      $apm$Mn$lab_filter
      NULL
      
      $apm$Mn$analytename
      [1] "Mn"
      
      
      $apm$Mg
      $apm$Mg$precision
      [1] 4
      
      $apm$Mg$sample_filter
      NULL
      
      $apm$Mg$sample_ids
       [1]   5  17  29  41  53  65  77  90 103 116 129 142 155 168 181 194 207 220
      
      $apm$Mg$lab_filter
      NULL
      
      $apm$Mg$analytename
      [1] "Mg"
      
      
      $apm$Cr
      $apm$Cr$precision
      [1] 4
      
      $apm$Cr$sample_filter
      NULL
      
      $apm$Cr$sample_ids
       [1]   6  18  30  42  54  66  78  91 104 117 130 143 156 169 182 195 208 221
      
      $apm$Cr$lab_filter
      NULL
      
      $apm$Cr$analytename
      [1] "Cr"
      
      
      $apm$Ni
      $apm$Ni$precision
      [1] 4
      
      $apm$Ni$sample_filter
      NULL
      
      $apm$Ni$sample_ids
       [1]   7  19  31  43  55  67  79  92 105 118 131 144 157 170 183 196 209 222
      
      $apm$Ni$lab_filter
      NULL
      
      $apm$Ni$analytename
      [1] "Ni"
      
      
      $apm$Zn
      $apm$Zn$precision
      [1] 4
      
      $apm$Zn$sample_filter
      NULL
      
      $apm$Zn$sample_ids
       [1]   8  20  32  44  56  68  80  93 106 119 132 145 158 171 184 197 210 223
      
      $apm$Zn$lab_filter
      NULL
      
      $apm$Zn$analytename
      [1] "Zn"
      
      
      $apm$Ti
      $apm$Ti$precision
      [1] 4
      
      $apm$Ti$sample_filter
      NULL
      
      $apm$Ti$sample_ids
       [1]   9  21  33  45  57  69  81  94 107 120 133 146 159 172 185 198 211 224
      
      $apm$Ti$lab_filter
      NULL
      
      $apm$Ti$analytename
      [1] "Ti"
      
      
      $apm$Sn
      $apm$Sn$precision
      [1] 4
      
      $apm$Sn$sample_filter
      NULL
      
      $apm$Sn$sample_ids
       [1]  10  22  34  46  58  70  83  96 109 122 135 148 161 174 187 200 213 226
      
      $apm$Sn$lab_filter
      NULL
      
      $apm$Sn$analytename
      [1] "Sn"
      
      
      $apm$V
      $apm$V$precision
      [1] 4
      
      $apm$V$sample_filter
      NULL
      
      $apm$V$sample_ids
       [1]  11  23  35  47  59  71  84  97 110 123 136 149 162 175 188 201 214 227
      
      $apm$V$lab_filter
      NULL
      
      $apm$V$analytename
      [1] "V"
      
      
      $apm$Zr
      $apm$Zr$precision
      [1] 4
      
      $apm$Zr$sample_filter
      NULL
      
      $apm$Zr$sample_ids
       [1]  12  24  36  48  60  72  85  98 111 124 137 150 163 176 189 202 215 228
      
      $apm$Zr$lab_filter
      NULL
      
      $apm$Zr$analytename
      [1] "Zr"
      
      
      $apm$Sc
      $apm$Sc$precision
      [1] 4
      
      $apm$Sc$sample_filter
      NULL
      
      $apm$Sc$sample_ids
       [1]  82  95 108 121 134 147 160 173 186 199 212 225
      
      $apm$Sc$lab_filter
      NULL
      
      $apm$Sc$analytename
      [1] "Sc"
      
      
      

---

    Code
      getValue(rv, "Certifications")
    Output
      $data
           ID Lab analyte replicate   value  unit S_flt L_flt
      1     1  L1      Si         1 0.05040  0.05 FALSE FALSE
      2     2  L1      Fe         1 0.04900  0.05 FALSE FALSE
      3     3  L1      Cu         1 4.37000   4.3 FALSE FALSE
      4     4  L1      Mn         1 0.80500   0.8 FALSE FALSE
      5     5  L1      Mg         1 1.50700   1.5 FALSE FALSE
      6     6  L1      Cr         1 0.05490  0.05 FALSE FALSE
      7     7  L1      Ni         1 0.04960  0.05 FALSE FALSE
      8     8  L1      Zn         1 0.14590  0.14 FALSE FALSE
      9     9  L1      Ti         1 0.04340  0.05 FALSE FALSE
      10   10  L1      Sn         1 0.02870 0.025 FALSE FALSE
      11   11  L1       V         1 0.01097  0.01 FALSE FALSE
      12   12  L1      Zr         1 0.15560  0.15 FALSE FALSE
      13   13  L1      Si         2 0.05120  0.05 FALSE FALSE
      14   14  L1      Fe         2 0.05630  0.05 FALSE FALSE
      15   15  L1      Cu         2 4.38500   4.3 FALSE FALSE
      16   16  L1      Mn         2 0.80890   0.8 FALSE FALSE
      17   17  L1      Mg         2 1.51100   1.5 FALSE FALSE
      18   18  L1      Cr         2 0.05520  0.05 FALSE FALSE
      19   19  L1      Ni         2 0.04970  0.05 FALSE FALSE
      20   20  L1      Zn         2 0.14710  0.14 FALSE FALSE
      21   21  L1      Ti         2 0.04350  0.05 FALSE FALSE
      22   22  L1      Sn         2 0.02900 0.025 FALSE FALSE
      23   23  L1       V         2 0.01082  0.01 FALSE FALSE
      24   24  L1      Zr         2 0.15570  0.15 FALSE FALSE
      25   25  L1      Si         3 0.05240  0.05 FALSE FALSE
      26   26  L1      Fe         3 0.05150  0.05 FALSE FALSE
      27   27  L1      Cu         3 4.34000   4.3 FALSE FALSE
      28   28  L1      Mn         3 0.80900   0.8 FALSE FALSE
      29   29  L1      Mg         3 1.50600   1.5 FALSE FALSE
      30   30  L1      Cr         3 0.05500  0.05 FALSE FALSE
      31   31  L1      Ni         3 0.04980  0.05 FALSE FALSE
      32   32  L1      Zn         3 0.14740  0.14 FALSE FALSE
      33   33  L1      Ti         3 0.04340  0.05 FALSE FALSE
      34   34  L1      Sn         3 0.02860 0.025 FALSE FALSE
      35   35  L1       V         3 0.01064  0.01 FALSE FALSE
      36   36  L1      Zr         3 0.15600  0.15 FALSE FALSE
      37   37  L1      Si         4 0.05200  0.05 FALSE FALSE
      38   38  L1      Fe         4 0.05050  0.05 FALSE FALSE
      39   39  L1      Cu         4 4.38800   4.3 FALSE FALSE
      40   40  L1      Mn         4 0.81300   0.8 FALSE FALSE
      41   41  L1      Mg         4 1.50900   1.5 FALSE FALSE
      42   42  L1      Cr         4 0.05550  0.05 FALSE FALSE
      43   43  L1      Ni         4 0.05060  0.05 FALSE FALSE
      44   44  L1      Zn         4 0.14980  0.14 FALSE FALSE
      45   45  L1      Ti         4 0.04350  0.05 FALSE FALSE
      46   46  L1      Sn         4 0.02880 0.025 FALSE FALSE
      47   47  L1       V         4 0.01051  0.01 FALSE FALSE
      48   48  L1      Zr         4 0.15650  0.15 FALSE FALSE
      49   49  L1      Si         5 0.05200  0.05 FALSE FALSE
      50   50  L1      Fe         5 0.05050  0.05 FALSE FALSE
      51   51  L1      Cu         5 4.32900   4.3 FALSE FALSE
      52   52  L1      Mn         5 0.80900   0.8 FALSE FALSE
      53   53  L1      Mg         5 1.49800   1.5 FALSE FALSE
      54   54  L1      Cr         5 0.05510  0.05 FALSE FALSE
      55   55  L1      Ni         5 0.04990  0.05 FALSE FALSE
      56   56  L1      Zn         5 0.14870  0.14 FALSE FALSE
      57   57  L1      Ti         5 0.04330  0.05 FALSE FALSE
      58   58  L1      Sn         5 0.02860 0.025 FALSE FALSE
      59   59  L1       V         5 0.01037  0.01 FALSE FALSE
      60   60  L1      Zr         5 0.15560  0.15 FALSE FALSE
      61   61  L1      Si         6 0.05100  0.05 FALSE FALSE
      62   62  L1      Fe         6 0.05050  0.05 FALSE FALSE
      63   63  L1      Cu         6 4.36500   4.3 FALSE FALSE
      64   64  L1      Mn         6 0.81200   0.8 FALSE FALSE
      65   65  L1      Mg         6 1.51100   1.5 FALSE FALSE
      66   66  L1      Cr         6 0.05540  0.05 FALSE FALSE
      67   67  L1      Ni         6 0.05050  0.05 FALSE FALSE
      68   68  L1      Zn         6 0.15010  0.14 FALSE FALSE
      69   69  L1      Ti         6 0.04350  0.05 FALSE FALSE
      70   70  L1      Sn         6 0.02910 0.025 FALSE FALSE
      71   71  L1       V         6 0.01075  0.01 FALSE FALSE
      72   72  L1      Zr         6 0.15660  0.15 FALSE FALSE
      73   73  L2      Si         1 0.04520  0.05 FALSE FALSE
      74   74  L2      Fe         1 0.05290  0.05 FALSE FALSE
      75   75  L2      Cu         1 4.40480   4.3 FALSE FALSE
      76   76  L2      Mn         1 0.81790   0.8 FALSE FALSE
      77   77  L2      Mg         1 1.51790   1.5 FALSE FALSE
      78   78  L2      Cr         1 0.05485  0.05 FALSE FALSE
      79   79  L2      Ni         1 0.04970  0.05 FALSE FALSE
      80   80  L2      Zn         1 0.14630  0.14 FALSE FALSE
      81   81  L2      Ti         1 0.04300  0.05 FALSE FALSE
      82   82  L2      Sc         1 0.04920  0.05 FALSE FALSE
      83   83  L2      Sn         1 0.02850 0.025 FALSE FALSE
      84   84  L2       V         1 0.01040  0.01 FALSE FALSE
      85   85  L2      Zr         1 0.15180  0.15 FALSE FALSE
      86   86  L2      Si         2 0.04350  0.05 FALSE FALSE
      87   87  L2      Fe         2 0.05270  0.05 FALSE FALSE
      88   88  L2      Cu         2 4.38020   4.3 FALSE FALSE
      89   89  L2      Mn         2 0.80840   0.8 FALSE FALSE
      90   90  L2      Mg         2 1.49880   1.5 FALSE FALSE
      91   91  L2      Cr         2 0.05470  0.05 FALSE FALSE
      92   92  L2      Ni         2 0.04930  0.05 FALSE FALSE
      93   93  L2      Zn         2 0.14440  0.14 FALSE FALSE
      94   94  L2      Ti         2 0.04250  0.05 FALSE FALSE
      95   95  L2      Sc         2 0.04860  0.05 FALSE FALSE
      96   96  L2      Sn         2 0.02840 0.025 FALSE FALSE
      97   97  L2       V         2 0.01020  0.01 FALSE FALSE
      98   98  L2      Zr         2 0.14990  0.15 FALSE FALSE
      99   99  L2      Si         3 0.04720  0.05 FALSE FALSE
      100 100  L2      Fe         3 0.04820  0.05 FALSE FALSE
      101 101  L2      Cu         3 4.39070   4.3 FALSE FALSE
      102 102  L2      Mn         3 0.81120   0.8 FALSE FALSE
      103 103  L2      Mg         3 1.51700   1.5 FALSE FALSE
      104 104  L2      Cr         3 0.05420  0.05 FALSE FALSE
      105 105  L2      Ni         3 0.04950  0.05 FALSE FALSE
      106 106  L2      Zn         3 0.14860  0.14 FALSE FALSE
      107 107  L2      Ti         3 0.04290  0.05 FALSE FALSE
      108 108  L2      Sc         3 0.04860  0.05 FALSE FALSE
      109 109  L2      Sn         3 0.02830 0.025 FALSE FALSE
      110 110  L2       V         3 0.01010  0.01 FALSE FALSE
      111 111  L2      Zr         3 0.14970  0.15 FALSE FALSE
      112 112  L2      Si         4 0.04560  0.05 FALSE FALSE
      113 113  L2      Fe         4 0.04780  0.05 FALSE FALSE
      114 114  L2      Cu         4 4.39100   4.3 FALSE FALSE
      115 115  L2      Mn         4 0.81160   0.8 FALSE FALSE
      116 116  L2      Mg         4 1.50480   1.5 FALSE FALSE
      117 117  L2      Cr         4 0.05440  0.05 FALSE FALSE
      118 118  L2      Ni         4 0.04900  0.05 FALSE FALSE
      119 119  L2      Zn         4 0.14800  0.14 FALSE FALSE
      120 120  L2      Ti         4 0.04310  0.05 FALSE FALSE
      121 121  L2      Sc         4 0.04910  0.05 FALSE FALSE
      122 122  L2      Sn         4 0.02860 0.025 FALSE FALSE
      123 123  L2       V         4 0.01020  0.01 FALSE FALSE
      124 124  L2      Zr         4 0.15060  0.15 FALSE FALSE
      125 125  L2      Si         5 0.04640  0.05 FALSE FALSE
      126 126  L2      Fe         5 0.04880  0.05 FALSE FALSE
      127 127  L2      Cu         5 4.37980   4.3 FALSE FALSE
      128 128  L2      Mn         5 0.80980   0.8 FALSE FALSE
      129 129  L2      Mg         5 1.50850   1.5 FALSE FALSE
      130 130  L2      Cr         5 0.05520  0.05 FALSE FALSE
      131 131  L2      Ni         5 0.04980  0.05 FALSE FALSE
      132 132  L2      Zn         5 0.14660  0.14 FALSE FALSE
      133 133  L2      Ti         5 0.04260  0.05 FALSE FALSE
      134 134  L2      Sc         5 0.04920  0.05 FALSE FALSE
      135 135  L2      Sn         5 0.02840 0.025 FALSE FALSE
      136 136  L2       V         5 0.01040  0.01 FALSE FALSE
      137 137  L2      Zr         5 0.15000  0.15 FALSE FALSE
      138 138  L2      Si         6 0.04520  0.05 FALSE FALSE
      139 139  L2      Fe         6 0.04660  0.05 FALSE FALSE
      140 140  L2      Cu         6 4.39240   4.3 FALSE FALSE
      141 141  L2      Mn         6 0.81120   0.8 FALSE FALSE
      142 142  L2      Mg         6 1.51020   1.5 FALSE FALSE
      143 143  L2      Cr         6 0.05420  0.05 FALSE FALSE
      144 144  L2      Ni         6 0.04960  0.05 FALSE FALSE
      145 145  L2      Zn         6 0.14780  0.14 FALSE FALSE
      146 146  L2      Ti         6 0.04280  0.05 FALSE FALSE
      147 147  L2      Sc         6 0.04840  0.05 FALSE FALSE
      148 148  L2      Sn         6 0.02850 0.025 FALSE FALSE
      149 149  L2       V         6 0.01020  0.01 FALSE FALSE
      150 150  L2      Zr         6 0.14990  0.15 FALSE FALSE
      151 151  L3      Si         1 0.05110  0.05 FALSE FALSE
      152 152  L3      Fe         1 0.04950  0.05 FALSE FALSE
      153 153  L3      Cu         1 4.44830   4.3 FALSE FALSE
      154 154  L3      Mn         1 0.80540   0.8 FALSE FALSE
      155 155  L3      Mg         1 1.50070   1.5 FALSE FALSE
      156 156  L3      Cr         1 0.05390  0.05 FALSE FALSE
      157 157  L3      Ni         1 0.05000  0.05 FALSE FALSE
      158 158  L3      Zn         1 0.15260  0.14 FALSE FALSE
      159 159  L3      Ti         1 0.04330  0.05 FALSE FALSE
      160 160  L3      Sc         1 0.04890  0.05 FALSE FALSE
      161 161  L3      Sn         1 0.02990 0.025 FALSE FALSE
      162 162  L3       V         1 0.01070  0.01 FALSE FALSE
      163 163  L3      Zr         1 0.15430  0.15 FALSE FALSE
      164 164  L3      Si         2 0.05080  0.05 FALSE FALSE
      165 165  L3      Fe         2 0.04890  0.05 FALSE FALSE
      166 166  L3      Cu         2 4.46660   4.3 FALSE FALSE
      167 167  L3      Mn         2 0.80480   0.8 FALSE FALSE
      168 168  L3      Mg         2 1.49950   1.5 FALSE FALSE
      169 169  L3      Cr         2 0.05380  0.05 FALSE FALSE
      170 170  L3      Ni         2 0.05010  0.05 FALSE FALSE
      171 171  L3      Zn         2 0.15260  0.14 FALSE FALSE
      172 172  L3      Ti         2 0.04320  0.05 FALSE FALSE
      173 173  L3      Sc         2 0.04890  0.05 FALSE FALSE
      174 174  L3      Sn         2 0.02960 0.025 FALSE FALSE
      175 175  L3       V         2 0.01070  0.01 FALSE FALSE
      176 176  L3      Zr         2 0.15410  0.15 FALSE FALSE
      177 177  L3      Si         3 0.05150  0.05 FALSE FALSE
      178 178  L3      Fe         3 0.05000  0.05 FALSE FALSE
      179 179  L3      Cu         3 4.46230   4.3 FALSE FALSE
      180 180  L3      Mn         3 0.80630   0.8 FALSE FALSE
      181 181  L3      Mg         3 1.50080   1.5 FALSE FALSE
      182 182  L3      Cr         3 0.05380  0.05 FALSE FALSE
      183 183  L3      Ni         3 0.05000  0.05 FALSE FALSE
      184 184  L3      Zn         3 0.15290  0.14 FALSE FALSE
      185 185  L3      Ti         3 0.04340  0.05 FALSE FALSE
      186 186  L3      Sc         3 0.04890  0.05 FALSE FALSE
      187 187  L3      Sn         3 0.02980 0.025 FALSE FALSE
      188 188  L3       V         3 0.01070  0.01 FALSE FALSE
      189 189  L3      Zr         3 0.15430  0.15 FALSE FALSE
      190 190  L3      Si         4 0.05080  0.05 FALSE FALSE
      191 191  L3      Fe         4 0.04990  0.05 FALSE FALSE
      192 192  L3      Cu         4 4.46740   4.3 FALSE FALSE
      193 193  L3      Mn         4 0.80580   0.8 FALSE FALSE
      194 194  L3      Mg         4 1.50160   1.5 FALSE FALSE
      195 195  L3      Cr         4 0.05400  0.05 FALSE FALSE
      196 196  L3      Ni         4 0.05040  0.05 FALSE FALSE
      197 197  L3      Zn         4 0.15480  0.14 FALSE FALSE
      198 198  L3      Ti         4 0.04330  0.05 FALSE FALSE
      199 199  L3      Sc         4 0.04900  0.05 FALSE FALSE
      200 200  L3      Sn         4 0.02970 0.025 FALSE FALSE
      201 201  L3       V         4 0.01070  0.01 FALSE FALSE
      202 202  L3      Zr         4 0.15440  0.15 FALSE FALSE
      203 203  L3      Si         5 0.05130  0.05 FALSE FALSE
      204 204  L3      Fe         5 0.04980  0.05 FALSE FALSE
      205 205  L3      Cu         5 4.48260   4.3 FALSE FALSE
      206 206  L3      Mn         5 0.80820   0.8 FALSE FALSE
      207 207  L3      Mg         5 1.51150   1.5 FALSE FALSE
      208 208  L3      Cr         5 0.05420  0.05 FALSE FALSE
      209 209  L3      Ni         5 0.05060  0.05 FALSE FALSE
      210 210  L3      Zn         5 0.15570  0.14 FALSE FALSE
      211 211  L3      Ti         5 0.04350  0.05 FALSE FALSE
      212 212  L3      Sc         5 0.04890  0.05 FALSE FALSE
      213 213  L3      Sn         5 0.03000 0.025 FALSE FALSE
      214 214  L3       V         5 0.01070  0.01 FALSE FALSE
      215 215  L3      Zr         5 0.15420  0.15 FALSE FALSE
      216 216  L3      Si         6 0.05210  0.05 FALSE FALSE
      217 217  L3      Fe         6 0.04960  0.05 FALSE FALSE
      218 218  L3      Cu         6 4.48570   4.3 FALSE FALSE
      219 219  L3      Mn         6 0.80820   0.8 FALSE FALSE
      220 220  L3      Mg         6 1.51070   1.5 FALSE FALSE
      221 221  L3      Cr         6 0.05420  0.05 FALSE FALSE
      222 222  L3      Ni         6 0.05040  0.05 FALSE FALSE
      223 223  L3      Zn         6 0.15500  0.14 FALSE FALSE
      224 224  L3      Ti         6 0.04340  0.05 FALSE FALSE
      225 225  L3      Sc         6 0.04900  0.05 FALSE FALSE
      226 226  L3      Sn         6 0.03010 0.025 FALSE FALSE
      227 227  L3       V         6 0.01070  0.01 FALSE FALSE
      228 228  L3      Zr         6 0.15390  0.15 FALSE FALSE
      
      $input_files
      NULL
      
      $uploadsource
      [1] "RData"
      
      $lab_means
      NULL
      
      $cert_mean
      NULL
      
      $cert_sd
      NULL
      
      $normality_statement
      NULL
      
      $precision
      NULL
      
      $data_kompakt
      NULL
      
      $CertValPlot
      NULL
      
      $stats
      NULL
      
      $boxplot
      NULL
      
      $opt
      NULL
      
      $mstats
      NULL
      
      $materialtabelle
      NULL
      

---

    Code
      getValue(rv, "Homogeneity")
    Output
      $data
      NULL
      
      $uploadsource
      NULL
      
      $h_file
      NULL
      
      $h_vals
      NULL
      
      $h_sel_analyt
      NULL
      
      $h_precision
      NULL
      
      $h_Fig_width
      NULL
      

---

    Code
      getValue(rv, "Stability")
    Output
      $file
      NULL
      
      $data
      NULL
      
      $uploadsource
      NULL
      
      $s_vals
      NULL
      

