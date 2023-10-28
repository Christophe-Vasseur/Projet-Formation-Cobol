      *======================================================*
      *SOUS-PROGRAMME DE CREATION D UNE MAISON DE DISTRIBUTION *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VF1.
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

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(E-MAISON-DIST)
           END-EXEC

      *    ECRITURE DANS LE FICHIER VSAM

           EXEC CICS
               WRITE FILE('MAISONG1')
               FROM (E-MAISON-DIST)
               RIDFLD (MA-SIREN)
               RESP (RC-1)
               RESP2 (RC-2)
           END-EXEC

      *    ANALYSE DU CODE RETOUR 000059
      *    1 : ECRITURE VSAM OK 000060
      *    2 : CLE EXISTANTE, CREATION IMPOSSIBLE
      *    3 : PORBLEME FICHIER VSAM

           EVALUATE RC-1
               WHEN DFHRESP(NORMAL)
                   MOVE 1 TO CODE-ERR-1
               WHEN DFHRESP(DUPREC)
                   MOVE 2 TO CODE-ERR-1
               WHEN OTHER
                   MOVE RC-1 TO CODE-ERR-1
                   MOVE RC-2 TO CODE-ERR-2
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (CODE-ERR)
               INPUTMSGLEN (LENGTH OF CODE-ERR)
           END-EXEC

           GOBACK
           .
