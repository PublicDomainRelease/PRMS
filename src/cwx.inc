C
      COMMON /WX/     CSEL,ELV,TLN,TLPS,TLX,TMN,TMX,TNAJ,TXAJ
C
      REAL CSEL(5),ELV(50),TLN(12),TLPS,TLX(12),TMN(5),TMX(5)
      REAL TNAJ(50),TXAJ(50)
C
      COMMON /SOLCHR/ SA
C
      CHARACTER*4 SA(20)                                                 KF0993
C
      SAVE /WX/, /SOLCHR/
