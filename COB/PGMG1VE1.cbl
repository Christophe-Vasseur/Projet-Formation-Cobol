      * ////////////////////////////////////////////////////////////// *
      *                                                                *
      *             SERVICE VSAM DE CREATION DES VENTES                *
      *                                                                *
      * ////////////////////////////////////////////////////////////// *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VE1.
       AUTHOR. RAZ.
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

           EXEC CICS
               WRITE
               FILE ('VENTESG1')
               FROM (E-VENTES)
               RIDFLD (E-VE-KEY)
               RESP (W-VE-RC-CICS-1)
               RESP2 (W-VE-RC-CICS-2)
           END-EXEC

      *    ENVOI DE LA REPONSE
      *    AU PROGRAMME APPELANT

           MOVE E-VENTES  TO W-VENTES(1:80)

           EVALUATE W-VE-RC-CICS-1
               WHEN DFHRESP(NORMAL)
                   SET W-VE-RC-NORMAL  TO TRUE
               WHEN DFHRESP(NOTOPEN)
                   SET W-VE-RC-NOTOPEN TO TRUE
               WHEN DFHRESP(DUPREC)
                   SET W-VE-RC-DUPREC  TO TRUE
               WHEN OTHER
                   SET W-VE-RC-OTHER   TO TRUE
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (W-VENTES)
               INPUTMSGLEN (LENGTH OF W-VENTES)
           END-EXEC

           GOBACK
           .
