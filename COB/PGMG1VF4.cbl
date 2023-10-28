      *======================================================*
      *SOUS-PROGRAMME DE MODIFICATION D UNE MAISON DE DISTRIBUTION*
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VA4.
       AUTHOR. RBN.
       DATE-WRITTEN. 21/10/23.

      *======================================================*
      *   E N V I R O N M E N T       D I V I S I O N        *
      *======================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

      *======================================================*
      *           D A T A         D I V I S I O N            *
      *======================================================*

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77 RC-1        PIC S9(8) COMP.
       77 RC-2        PIC S9(8) COMP.
       01 CODE-ERR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01 W-MAISON-DIST.
           05 W-MA-SIREN           PIC X(05).
           05 W-MA-RAISON-SOC      PIC X(30).
           05 W-MA-ADRESSE         PIC X(15).
           05 W-MA-CODEP-DIR       PIC X(04).
           05 W-MA-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(25).

       01 W2-MAISON-DIST.
           05 W2-MA-SIREN          PIC X(05).
           05 W2-MA-RAISON-SOC     PIC X(30).
           05 W2-MA-ADRESSE        PIC X(15).
           05 W2-MA-CODEP-DIR      PIC X(04).
           05 FILLER               PIC X(26).

      *------------------------------------------------------*
      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
      *------------------------------------------------------*

       COPY MAISONDI.

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-MAISON-DIST
           INITIALIZE W-MAISON-DIST
           INITIALIZE W2-MAISON-DIST

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(W-MAISON-DIST)
           END-EXEC

           EVALUATE W-MA-CODE-MODIF
      *    CONSULTATION DU FICHIER VSAM A PARTIR DU CODE SIREN
               WHEN 0
                   EXEC CICS
                       READ FILE('MAISONG1')
                       INTO (W2-MAISON-DIST)
                       RIDFLD (W-MA-SIREN)
                       RESP (RC-1)
                       RESP2 (RC-2)
                       UPDATE
                   END-EXEC

                   MOVE W2-MAISON-DIST TO W-MAISON-DIST

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE 1 TO W-MA-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-MA-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-MA-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG(W-MAISON-DIST)
                       INPUTMSGLEN(LENGTH OF W-MAISON-DIST)
                   END-EXEC

               WHEN 1
                   EXEC CICS
                       READ FILE('MAISONG1')
                       RIDFLD (W-MA-SIREN)
                       INTO (W2-MAISON-DIST)
                       UPDATE
                   END-EXEC
      *                RESP (RC-1)
      *                RESP2 (RC-2)

                   MOVE W-MA-SIREN      TO W2-MA-SIREN
                   MOVE W-MA-RAISON-SOC TO W2-MA-RAISON-SOC
                   MOVE W-MA-ADRESSE    TO W2-MA-ADRESSE
                   MOVE W-MA-CODEP-DIR  TO W2-MA-CODEP-DIR

                   EXEC CICS
                       REWRITE FILE('MAISONG1')
                       FROM (W2-MAISON-DIST)
                       LENGTH (LENGTH OF W2-MAISON-DIST)
                       RESP (RC-1)
                       RESP2 (RC-2)
                   END-EXEC

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE W2-MAISON-DIST TO W-MAISON-DIST
                           MOVE 1 TO W-MA-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-MA-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-MA-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG (W-MAISON-DIST)
                       INPUTMSGLEN (LENGTH OF W-MAISON-DIST)
                   END-EXEC

           END-EVALUATE

           GOBACK
           .
