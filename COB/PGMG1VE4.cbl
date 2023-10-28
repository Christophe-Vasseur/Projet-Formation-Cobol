      *======================================================*
      *   SOUS-PROGRAMME DE MODIFICATION DES VENTES          *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VE4.
       AUTHOR. RAZ.
       DATE-WRITTEN. 23/10/23.

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

       COPY VENTES.

      *======================================================*
      *     P R O C E D U R E     D I V I S I O N            *
      *======================================================*

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-VENTES
           INITIALIZE W-VENTES
           INITIALIZE W2-VENTES

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(W-VENTES)
           END-EXEC

           EVALUATE W-VE-CODE-MODIF
      *    CONSULTATION DU FICHIER VSAM A PARTIR DE LA CLE VENTES
               WHEN 0
                   EXEC CICS
                       READ FILE('VENTESG1')
                       INTO (W2-VENTES)
                       RIDFLD (W-VE-KEY)
                       RESP (RC-1)
                       RESP2 (RC-2)
                       UPDATE
                   END-EXEC

                   MOVE W2-VENTES TO W-VENTES

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE 1 TO W-VE-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-VE-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-VE-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG(W-VENTES)
                       INPUTMSGLEN(LENGTH OF W-VENTES)
                   END-EXEC

               WHEN 1
                   EXEC CICS
                       READ FILE('VENTESG1')
                       RIDFLD (W-VE-KEY)
                       INTO (W2-VENTES)
                       UPDATE
                   END-EXEC
      *                RESP (RC-1)
      *                RESP2 (RC-2)

                   MOVE W-VE-CODEA      TO W2-VE-CODEA
                   MOVE W-VE-SIREN      TO W2-VE-SIREN
                   MOVE W-VE-DATE-VENTE TO W2-VE-DATE-VENTE
                   MOVE W-VE-PRIX       TO W2-VE-PRIX
                   MOVE W-VE-QTE        TO W2-VE-QTE

                   EXEC CICS
                       REWRITE FILE('VENTESG1')
                       FROM (W2-VENTES)
                       LENGTH (LENGTH OF W2-VENTES)
                       RESP (RC-1)
                       RESP2 (RC-2)
                   END-EXEC

                   EVALUATE RC-1
                       WHEN DFHRESP(NORMAL)
                           MOVE W2-VENTES TO W-VENTES
                           MOVE 1 TO W-VE-CODE-MODIF
                       WHEN DFHRESP(NOTFND)
                           MOVE 2 TO W-VE-CODE-MODIF
                       WHEN OTHER
                           MOVE 3 TO W-VE-CODE-MODIF
                   END-EVALUATE

                   EXEC CICS
                       RETURN
                       INPUTMSG (W-VENTES)
                       INPUTMSGLEN (LENGTH OF W-VENTES)
                   END-EXEC

           END-EVALUATE

           GOBACK
           .
