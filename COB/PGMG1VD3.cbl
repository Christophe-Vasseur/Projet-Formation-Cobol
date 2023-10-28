      * ////////////////////////////////////////////////////////////// *
      *                                                                *
      *            SERVICE VSAM DE SUPPRESSION D UN CONTRAT            *
      *                                                                *
      * ////////////////////////////////////////////////////////////// *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VD3.
       AUTHOR. EDR.
       DATE-WRITTEN. 23/10/23.

      * ============================================================== *
      *                                                                *
      *            E N V I R O N M E N T   D I V I S I O N             *
      *                                                                *
      * ============================================================== *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY CONTRAT.

      * ============================================================== *
      *                                                                *
      *              P R O C E D U R E   D I V I S I O N               *
      *                                                                *
      * ============================================================== *

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-CONTRAT
           INITIALIZE W-CONTRAT

      *    RECEPTION DE LA DEMANDE
      *    DU PROGRAMME APPELANT

           EXEC CICS
               RECEIVE
               INTO (E-CONTRAT)
           END-EXEC

      *    CONSULTATION DU FICHIER VSAM
      *    RECHERCHE DE L ENREG A SUPPR

           EXEC CICS
               READ
               FILE ('CONTRAG1')
               INTO (E-CONTRAT)
               RIDFLD (E-CO-KEY)
               RESP (W-CO-RC-CICS-1)
               RESP2 (W-CO-RC-CICS-2)
               UPDATE
           END-EXEC

           MOVE E-CONTRAT
               TO W-CONTRAT(1:80)

           PERFORM P-EVAL-RC-1

           IF W-CO-RC-NORMAL THEN

      *        SI LA LECTURE S EST BIEN PASSEE
      *        SUPPRESSION DE L ENREG TROUVE

               EXEC CICS
                   DELETE
                   FILE ('CONTRAG1')
                   RESP (W-CO-RC-CICS-1)
                   RESP2 (W-CO-RC-CICS-2)
               END-EXEC

               PERFORM P-EVAL-RC-1

           END-IF

      *    ENVOI DE LA REPONSE
      *    AU PROGRAMME APPELANT

           EXEC CICS
               RETURN
               INPUTMSG (W-CONTRAT)
               INPUTMSGLEN (LENGTH OF W-CONTRAT)
           END-EXEC

           GOBACK
           .

      * EVALUATION DU CODE RETOUR CICS 1

       P-EVAL-RC-1.
           EVALUATE W-CO-RC-CICS-1
               WHEN DFHRESP(NORMAL)
                   SET W-CO-RC-NORMAL  TO TRUE
               WHEN DFHRESP(NOTOPEN)
                   SET W-CO-RC-NOTOPEN TO TRUE
               WHEN DFHRESP(NOTFND)
                   SET W-CO-RC-NOTFND  TO TRUE
               WHEN OTHER
                   SET W-CO-RC-OTHER   TO TRUE
           END-EVALUATE
           .
