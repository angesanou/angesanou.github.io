DATA LIST FILE='C:\USERS\HP\DOCUMENTS\CSPRO_2024\CAPI\SECTION_2.DAT' RECORDS=1
 /
 S0Q1        1-2   
 S0Q2        3-4   
 S0Q3        5-7   
 S0Q4        8-12  
 S2Q1       13-13  
 S2Q1_A     14-14   (A)
 S2Q2       15-15  
 S2Q2_A     16-16   (A)
 S2Q3       17-18  
 S2Q4       19-20  
 S2Q5       21-30   (A)
 S2Q5_A     31-31  
 S2Q5_B     32-32  
 S2Q5_C     33-33  
 S2Q5_D     34-34  
 S2Q5_E     35-35  
 S2Q5_F     36-36  
 S2Q5_G     37-37  
 S2Q5_H     38-38  
 S2Q5_I     39-39  
 S2Q5_J     40-40  
 S2Q5_AUTRE   41-55   (A)
.
VARIABLE LABELS
  S0Q1     "Region de l' enquêté"
 /S0Q2     "Province de l' enquêté"
 /S0Q3     "Départements de l' enquêté"
 /S0Q4     "Numéro du ménage"
 /S2Q1     "Type de logement"
 /S2Q1_A   "Autre type de logement"
 /S2Q2     "Statut d'occupation"
 /S2Q2_A   "Autre statut d'occupation"
 /S2Q3     "Nombre de pièces de la maison"
 /S2Q4     "Nombre de chambres"
 /S2Q5     "Possession du ménage"
 /S2Q5_A   "Possession de télévision"
 /S2Q5_B   "Possession de radio"
 /S2Q5_C   "Possession de téléphone"
 /S2Q5_D   "Possession de Réfrigirateur"
 /S2Q5_E   "Possession de ordinateurs"
 /S2Q5_F   "Possession de Vélo"
 /S2Q5_G   "Possession de Moto"
 /S2Q5_H   "Possession de voiture"
 /S2Q5_I   "Autre possesion"
 /S2Q5_J   "possession d'aucun biens"
 /S2Q5_AUTRE "Préciser les autres biens"
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
 /S2Q1    
     1 "Maison individuelle simple"
     2 "Celibatérium"
     3 "Appartement"
     4 "Villa"
     5 "Autre(à préciser)"
 /S2Q2    
     1 "Propriétaire"
     2 "Locataire"
     3 "Logé par un tiers"
     4 "Autre (à préciser)"
 /S2Q5    
  'A         ' "Télévision"
  'B         ' "Radio"
  'C         ' "Téléphone"
  'D         ' "Réfrigirateur"
  'E         ' "Ordinateur"
  'F         ' "Vélo"
  'G         ' "Moto"
  'H         ' "Voiture"
  'I         ' "Autre"
  'J         ' "Aucun"
 /S2Q5_A  
     1 "OUI"
     0 "NON"
 /S2Q5_B  
     1 "OUI"
     0 "NON"
 /S2Q5_C  
     1 "OUI"
     0 "NON"
 /S2Q5_D  
     1 "OUI"
     0 "NON"
 /S2Q5_E  
     1 "OUI"
     0 "NON"
 /S2Q5_F  
     1 "OUI"
     0 "NON"
 /S2Q5_G  
     1 "OUI"
     0 "NON"
 /S2Q5_H  
     1 "OUI"
     0 "NON"
 /S2Q5_I  
     1 "OUI"
     0 "NON"
 /S2Q5_J  
     1 "OUI"
     0 "NON"
.
EXECUTE.
