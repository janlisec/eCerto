

## Materialtabelle

`pooling` bedeutet, das man cert_val nicht aus den Labormittelwerten schätzt, sondern aus allen Mess-Werten. Im Beispiel wäre n=15 ohne und n=10 mit Laborfilter.
In Summe: für die korrekte Darstellung der Materialtabelle benötigen wir für jeden Analyten neben cert_val und cert_sd die Informationen zu 'pooling', 'S_flt' und 'L_flt', sowie die gewünschte 'precision'. (!S_flt ist relevant, wenn pooling==TRUE, denn hat der User ein einzelnes Sample gefiltert, dann ändert sich n auf n-1).

### Parameter

#### com
com describes the *com*bined uncertainty from all U columns: `sqrt(sum(x ^ 2))`


#### n
n ist die Anzahl der für die mean-Berechnung verwendeten Werte. Beispiel: Wir haben für einen beliebigen Analyten 3 Labore mit je 5 Replikaten. Wir können `cert_mean` berechnen aus den 3 Mittelwerten der Labore (n=3). Wenn ein Labor gefiltert wird, dann wird es im Plot grau angezeigt aber für cert_mean nicht berücksichtigt (n=2). Beides gilt für den Fall pooling==FALSE. 

