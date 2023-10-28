      *======================================================*
      *   SOUS-PROGRAMME DE MODIFICATION D UNE PERSONNE      *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VC4.
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

       77 RC-1        PIC S9(8) COMP.
       77 RC-2        PIC S9(8) COMP.
       01 CODE-ERR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).
       01  W-PERSONNE.
           05 W-PE-CODEP           PIC X(04).
           05 W-PE-CODE-TYPE       PIC X(01).
           05 W-PE-IDENTITE        PIC X(25).
           05 W-PE-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(49).
       01  W2-PERSONNE.
           05 W2-PE-CODEP           PIC X(04).
           05 W2-PE-CODE-TYPE       PIC X(01).
           05 W2-PE-IDENTITE        PIC X(25).
           05 FILLER                PIC X(50).

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
           INITIALIZE W-PERSONNE
           INITIALIZE W2-PERSONNE

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(W-PERSONNE)
           END-EXEC

           EVALUATE W-PE-CODE-MODIF
      *    CONSULTATION DU FICHIER VSAM A PARTIR DU CODE PERSONNE
               WHEN 0
                   EXEC CICS
                       READ FILE('PERSONG1')
                       INTO (W2-PERSONNE)
                       RIDFLD (W-PE-CODEP)
                       RESP (RC-1)
                       RESP2 (RC-2)
                       UPDATE
                   END-EXEC

                   MOVE W2-PERSONNE TO W-PERSONNE

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE 1 TO W-PE-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-PE-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-PE-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG(W-PERSONNE)
                       INPUTMSGLEN(LENGTH OF W-PERSONNE)
                   END-EXEC

               WHEN 1
                   EXEC CICS
                       READ FILE('PERSONG1')
                       RIDFLD (W-PE-CODEP)
                       INTO (W2-PERSONNE)
                       UPDATE
                   END-EXEC
      *                RESP (RC-1)
      *                RESP2 (RC-2)

                   MOVE W-PE-CODEP     TO W2-PE-CODEP
                   MOVE W-PE-IDENTITE  TO W2-PE-IDENTITE
                   MOVE W-PE-CODE-TYPE TO W2-PE-CODE-TYPE

                   EXEC CICS
                       REWRITE FILE('PERSONG1')
                       FROM (W2-PERSONNE)
                       LENGTH (LENGTH OF W2-PERSONNE)
                       RESP (RC-1)
                       RESP2 (RC-2)
                   END-EXEC

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE W2-PERSONNE TO W-PERSONNE
                           MOVE 1 TO W-PE-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-PE-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-PE-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG (W-PERSONNE)
                       INPUTMSGLEN (LENGTH OF W-PERSONNE)
                   END-EXEC

           END-EVALUATE

           GOBACK
           .
