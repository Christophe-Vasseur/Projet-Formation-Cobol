      *======================================================*
      *   SOUS-PROGRAMME DE CREATION D UNE CHANSON           *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VC3.
       AUTHOR. RAZ.
       DATE-WRITTEN. 20/10/23.

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

       77 RC-1 PIC S9(8) COMP.
       77 RC-2 PIC S9(8) COMP.
       01 CODE-ERR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01  W-PERSONNE.
           05 W-PE-CODEP           PIC X(04).
           05 W-PE-CODE-TYPE       PIC X(01).
           05 W-PE-IDENTITE        PIC X(25).
           05 W-PE-CODE-SUPP       PIC 9(01).
           05 FILLER               PIC X(49).

      *------------------------------------------------------*
      *   ZONE DE MESSAGE TAMPON POUR LE SEND FROM           *
      *------------------------------------------------------*

       COPY PERSONNE.

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-PERSONNE

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE
               INTO (E-PERSONNE)
           END-EXEC

           EXEC CICS
               READ
               FILE ('PERSONG1')
               INTO (E-PERSONNE)
               RIDFLD (PE-CODEP)
               UPDATE
           END-EXEC

           MOVE E-PERSONNE TO W-PERSONNE

           EXEC CICS
               DELETE
               FILE ('PERSONG1')
               RESP (RC-1)
               RESP2 (RC-2)
           END-EXEC

           EVALUATE RC-1
               WHEN DFHRESP(NORMAL)
                   MOVE 1 TO W-PE-CODE-SUPP
               WHEN DFHRESP(DUPREC)
                   MOVE 2 TO W-PE-CODE-SUPP
               WHEN OTHER
                   MOVE RC-1 TO W-PE-CODE-SUPP
      *            MOVE RC-2 TO CODE-ERR-2
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (W-PERSONNE)
               INPUTMSGLEN (LENGTH OF W-PERSONNE)
           END-EXEC

           GOBACK
           .
