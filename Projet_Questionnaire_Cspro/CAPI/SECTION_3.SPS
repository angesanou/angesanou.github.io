DATA LIST FILE='C:\USERS\HP\DOCUMENTS\CSPRO_2024\CAPI\SECTION_3.DAT' RECORDS=1
 /
 S0Q1        1-2   
 S0Q2        3-4   
 S0Q3        5-7   
 S0Q4        8-12  
 S3Q1       13-13  
 S3Q2       14-24   (A)
 S3Q2_A     25-34   (A)
 S3Q3       35-35   (A)
 S3Q4       36-36   (A)
 S3Q5       37-37  
 S3Q5_A     38-52   (A)
 S3Q6       53-53  
.
VARIABLE LABELS
  S0Q1     "Region de l' enquêté"
 /S0Q2     "Province de l' enquêté"
 /S0Q3     "Départements de l' enquêté"
 /S0Q4     "Numéro du ménage"
 /S3Q1     "Endroit de lavage des mains"
 /S3Q2     "Sources d'eau du ménage"
 /S3Q2_A   "Autre sources"
 /S3Q3     "Source d'eau principale pour boire"
 /S3Q4     "Source d'eau principale pour autres besoins"
 /S3Q5     "Type de toilette"
 /S3Q5_A   "Autre type de toilette"
 /S3Q6     "Partagez vous ces toilettes avec d'autres ménages ou le public?"
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
 /S3Q1    
     1 "OUI"
     2 "NON"
 /S3Q2    
  'A          ' "Eau du robinet"
  'B          ' "Puits à pompe/forage"
  'C          ' "Puits creusé"
  'D          ' "Eau de sources"
  'E          ' "Eau de pluie"
  'F          ' "Camion citerne"
  'G          ' "Charette avec pétit citerne"
  'H          ' "Eau de surface"
  'I          ' "Eau de bouteille"
  'J          ' "Eau en sachet"
  'K          ' "Autre(à préciser)"
 /S3Q3    
  'A' "Eau du robinet"
  'B' "Puits à pompe/forage"
  'C' "Puits creusé"
  'D' "Eau de sources"
  'E' "Eau de pluie"
  'F' "Camion citerne"
  'G' "Charette avec pétit citerne"
  'H' "Eau de surface"
  'I' "Eau de bouteille"
  'J' "Eau en sachet"
  'K' "Autre(à préciser)"
 /S3Q4    
  'A' "Eau du robinet"
  'B' "Puits à pompe/forage"
  'C' "Puits creusé"
  'D' "Eau de sources"
  'E' "Eau de pluie"
  'F' "Camion citerne"
  'G' "Charette avec pétit citerne"
  'H' "Eau de surface"
  'I' "Eau de bouteille"
  'J' "Eau en sachet"
  'K' "Autre(à préciser)"
 /S3Q5    
     1 "Latrines à fosse"
     2 "Fosse septique"
     3 "Système d'égout"
     4 "Buisson/nature"
     5 "Autre (à préciser)"
 /S3Q6    
     1 "OUI"
     2 "NON"
.
EXECUTE.
