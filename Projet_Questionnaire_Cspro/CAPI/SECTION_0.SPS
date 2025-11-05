DATA LIST FILE='C:\USERS\HP\DOCUMENTS\CSPRO_2024\CAPI\SECTION_0.DAT' RECORDS=1
 /
 S0Q1        1-2   
 S0Q2        3-4   
 S0Q3        5-7   
 S0Q4        8-12  
 S0Q5       13-22   (A)
 S0Q6       23-30   (A)
 S0Q7       31-38   (A)
.
VARIABLE LABELS
  S0Q1     "Region de l' enquêté"
 /S0Q2     "Province de l' enquêté"
 /S0Q3     "Départements de l' enquêté"
 /S0Q4     "Numéro du ménage"
 /S0Q5     "Date d'interview"
 /S0Q6     "Heure de début d'interview"
 /S0Q7     "Heure de fin d'interview"
.
VALUE LABELS
  S0Q1    
     1 "Boucle du Mouhoun"
     2 "Cascade"
     3 "Centre"
     4 "Centre-Est"
     5 "Centre-Nord"
     6 "Centre-Ouest"
     7 "Centre-Sud"
     8 "Est"
     9 "Haut-Bassin"
    10 "Nord"
    11 "Plateau-Central"
    12 "Sahel"
    13 "Sud-Ouest"
 /S0Q2    
     1 "Balé"
     2 "Banwa"
     3 "Kossi"
     4 "Mouhoun"
     5 "Nayala"
     6 "Sourou"
 /S0Q3    
     1 "Bagassi"
     2 "Bana"
     3 "Boromo"
     4 "Fara"
     5 "Oury"
     6 "Pâ"
     7 "Pompoie"
     8 "Poura"
     9 "Sibi"
    10 "Yaho"
.
EXECUTE.
