DATA LIST FILE='C:\USERS\HP\DOCUMENTS\CSPRO_2024\CAPI\SECTION_4.DAT' RECORDS=1
 /
 S0Q1        1-2   
 S0Q2        3-4   
 S0Q3        5-7   
 S0Q4        8-12  
 S4Q0_A$01   13-13   (A)
 S4Q0_A$02   14-14   (A)
 S4Q0_A$03   15-15   (A)
 S4Q0_A$04   16-16   (A)
 S4Q0_A$05   17-17   (A)
 S4Q0_A$06   18-18   (A)
 S4Q0_A$07   19-19   (A)
 S4Q0_A$08   20-20   (A)
 S4Q0_A$09   21-21   (A)
 S4Q0_A$10   22-22   (A)
 S4Q0_A$11   23-23   (A)
 S4Q0_B$01   24-48   (A)
 S4Q0_B$02   49-73   (A)
 S4Q0_B$03   74-98   (A)
 S4Q0_B$04   99-123  (A)
 S4Q0_B$05  124-148  (A)
 S4Q0_B$06  149-173  (A)
 S4Q0_B$07  174-198  (A)
 S4Q0_B$08  199-223  (A)
 S4Q0_B$09  224-248  (A)
 S4Q0_B$10  249-273  (A)
 S4Q0_B$11  274-298  (A)
 S4Q1$01   299-300 
 S4Q1$02   301-302 
 S4Q1$03   303-304 
 S4Q1$04   305-306 
 S4Q1$05   307-308 
 S4Q1$06   309-310 
 S4Q1$07   311-312 
 S4Q1$08   313-314 
 S4Q1$09   315-316 
 S4Q1$10   317-318 
 S4Q1$11   319-320 
 S4Q2$01   321-322 
 S4Q2$02   323-324 
 S4Q2$03   325-326 
 S4Q2$04   327-328 
 S4Q2$05   329-330 
 S4Q2$06   331-332 
 S4Q2$07   333-334 
 S4Q2$08   335-336 
 S4Q2$09   337-338 
 S4Q2$10   339-340 
 S4Q2$11   341-342 
 S4Q3$01   343-348  (A)
 S4Q3$02   349-354  (A)
 S4Q3$03   355-360  (A)
 S4Q3$04   361-366  (A)
 S4Q3$05   367-372  (A)
 S4Q3$06   373-378  (A)
 S4Q3$07   379-384  (A)
 S4Q3$08   385-390  (A)
 S4Q3$09   391-396  (A)
 S4Q3$10   397-402  (A)
 S4Q3$11   403-408  (A)
 S4Q3_A$01  409-423  (A)
 S4Q3_A$02  424-438  (A)
 S4Q3_A$03  439-453  (A)
 S4Q3_A$04  454-468  (A)
 S4Q3_A$05  469-483  (A)
 S4Q3_A$06  484-498  (A)
 S4Q3_A$07  499-513  (A)
 S4Q3_A$08  514-528  (A)
 S4Q3_A$09  529-543  (A)
 S4Q3_A$10  544-558  (A)
 S4Q3_A$11  559-573  (A)
.
VARIABLE LABELS
  S0Q1     "Region de l' enquêté"
 /S0Q2     "Province de l' enquêté"
 /S0Q3     "Départements de l' enquêté"
 /S0Q4     "Numéro du ménage"
 /S4Q0_A$01 "Code de la source"
 /S4Q0_A$02 "Code de la source"
 /S4Q0_A$03 "Code de la source"
 /S4Q0_A$04 "Code de la source"
 /S4Q0_A$05 "Code de la source"
 /S4Q0_A$06 "Code de la source"
 /S4Q0_A$07 "Code de la source"
 /S4Q0_A$08 "Code de la source"
 /S4Q0_A$09 "Code de la source"
 /S4Q0_A$10 "Code de la source"
 /S4Q0_A$11 "Code de la source"
 /S4Q0_B$01 "Libellé de la source"
 /S4Q0_B$02 "Libellé de la source"
 /S4Q0_B$03 "Libellé de la source"
 /S4Q0_B$04 "Libellé de la source"
 /S4Q0_B$05 "Libellé de la source"
 /S4Q0_B$06 "Libellé de la source"
 /S4Q0_B$07 "Libellé de la source"
 /S4Q0_B$08 "Libellé de la source"
 /S4Q0_B$09 "Libellé de la source"
 /S4Q0_B$10 "Libellé de la source"
 /S4Q0_B$11 "Libellé de la source"
 /S4Q1$01  "Distance par rapport à la maison"
 /S4Q1$02  "Distance par rapport à la maison"
 /S4Q1$03  "Distance par rapport à la maison"
 /S4Q1$04  "Distance par rapport à la maison"
 /S4Q1$05  "Distance par rapport à la maison"
 /S4Q1$06  "Distance par rapport à la maison"
 /S4Q1$07  "Distance par rapport à la maison"
 /S4Q1$08  "Distance par rapport à la maison"
 /S4Q1$09  "Distance par rapport à la maison"
 /S4Q1$10  "Distance par rapport à la maison"
 /S4Q1$11  "Distance par rapport à la maison"
 /S4Q2$01  "Disponibilité( nombre de mois)"
 /S4Q2$02  "Disponibilité( nombre de mois)"
 /S4Q2$03  "Disponibilité( nombre de mois)"
 /S4Q2$04  "Disponibilité( nombre de mois)"
 /S4Q2$05  "Disponibilité( nombre de mois)"
 /S4Q2$06  "Disponibilité( nombre de mois)"
 /S4Q2$07  "Disponibilité( nombre de mois)"
 /S4Q2$08  "Disponibilité( nombre de mois)"
 /S4Q2$09  "Disponibilité( nombre de mois)"
 /S4Q2$10  "Disponibilité( nombre de mois)"
 /S4Q2$11  "Disponibilité( nombre de mois)"
 /S4Q3$01  "Usage"
 /S4Q3$02  "Usage"
 /S4Q3$03  "Usage"
 /S4Q3$04  "Usage"
 /S4Q3$05  "Usage"
 /S4Q3$06  "Usage"
 /S4Q3$07  "Usage"
 /S4Q3$08  "Usage"
 /S4Q3$09  "Usage"
 /S4Q3$10  "Usage"
 /S4Q3$11  "Usage"
 /S4Q3_A$01 "Autre usage"
 /S4Q3_A$02 "Autre usage"
 /S4Q3_A$03 "Autre usage"
 /S4Q3_A$04 "Autre usage"
 /S4Q3_A$05 "Autre usage"
 /S4Q3_A$06 "Autre usage"
 /S4Q3_A$07 "Autre usage"
 /S4Q3_A$08 "Autre usage"
 /S4Q3_A$09 "Autre usage"
 /S4Q3_A$10 "Autre usage"
 /S4Q3_A$11 "Autre usage"
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
 /S4Q3$01 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$02 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$03 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$04 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$05 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$06 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$07 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$08 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$09 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$10 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
 /S4Q3$11 
  'A     ' "Boire"
  'B     ' "Cuisiner"
  'C     ' "Lavage"
  'D     ' "Agriculture"
  'E     ' "Elevage"
  'F     ' "Autre"
.
EXECUTE.
