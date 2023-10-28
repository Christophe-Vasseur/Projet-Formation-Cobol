      * ////////////////////////////////////////////////////////////// *
      *                                                                *
      *            SERVICE VSAM DE SUPPRESSION DES VENTES              *
      *                                                                *
      * ////////////////////////////////////////////////////////////// *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VE3.
       AUTHOR. EDR.
       DATE-WRITTEN. 23/10/23.
      *DERNIERE MODIF: RAZ
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

       COPY VENTES.

      * ============================================================== *
      *                                                                *
      *              P R O C E D U R E   D I V I S I O N               *
      *                                                                *
      * ============================================================== *

       PROCEDURE DIVISION.

      *    INITIALISATION

           INITIALIZE E-VENTES
           INITIALIZE W-VENTES

      *    RECEPTION DE LA DEMANDE
      *    DU PROGRAMME APPELANT

           EXEC CICS
               RECEIVE
               INTO (E-VENTES)
           END-EXEC

      *    CONSULTATION DU FICHIER VSAM
      *    RECHERCHE DE L ENREG A SUPPR

           EXEC CICS
               READ
               FILE ('VENTESG1')
               INTO (E-VENTES)
               RIDFLD (E-VE-KEY)
               RESP (W-VE-RC-CICS-1)
               RESP2 (W-VE-RC-CICS-2)
               UPDATE
           END-EXEC

           MOVE E-VENTES
               TO W-VENTES(1:80)

           PERFORM P-EVAL-RC-1

           IF W-VE-RC-NORMAL THEN

      *        SI LA LECTURE S EST BIEN PASSEE
      *        SUPPRESSION DE L ENREG TROUVE

               EXEC CICS
                   DELETE
                   FILE ('VENTESG1')
                   RESP (W-VE-RC-CICS-1)
                   RESP2 (W-VE-RC-CICS-2)
               END-EXEC

               PERFORM P-EVAL-RC-1

           END-IF

      *    ENVOI DE LA REPONSE
      *    AU PROGRAMME APPELANT

           EXEC CICS
               RETURN
               INPUTMSG (W-VENTES)
               INPUTMSGLEN (LENGTH OF W-VENTES)
           END-EXEC

           GOBACK
           .

      * EVALUATION DU CODE RETOUR CICS 1

       P-EVAL-RC-1.
           EVALUATE W-VE-RC-CICS-1
               WHEN DFHRESP(NORMAL)
                   SET W-VE-RC-NORMAL  TO TRUE
               WHEN DFHRESP(NOTOPEN)
                   SET W-VE-RC-NOTOPEN TO TRUE
               WHEN DFHRESP(NOTFND)
                   SET W-VE-RC-NOTFND  TO TRUE
               WHEN OTHER
                   SET W-VE-RC-OTHER   TO TRUE
           END-EVALUATE
           .
