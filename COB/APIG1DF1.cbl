       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DF1.
       AUTHOR. RBN.
       DATE-WRITTEN. 22/10/23.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1F1.
       COPY APIG1DWK.
       COPY MAISONDI.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1F1'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1F1'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DF1'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01 W-MAISON-DIST.
           05 W-MA-SIREN           PIC X(05).
           05 W-MA-RAISON-SOC      PIC X(30).
           05 W-MA-ADRESSE         PIC X(15).
           05 W-MA-CODEP-DIR       PIC X(04).
           05 W-MA-CODE-CONSULT    PIC 9(01).
           05 FILLER               PIC X(25).

       LINKAGE SECTION.

       01 DFHCOMMAREA.
           05 LK-COMM-AREA PIC X(1024).

      * ============================================================== *
      *                                                                *
      *              P R O C E D U R E   D I V I S I O N               *
      *                                                                *
      * ============================================================== *

       PROCEDURE DIVISION.

       P-MAIN.
           MOVE DFHRESP(MAPFAIL) TO RC-MAPFAIL.
           PERFORM P-BRWS-MAIN
           .

       COPY APIG1DPC.

      * -------------------------------------------------------------- *
      *                                                                *
      *                        SOUS-PARAGRAPHES                        *
      *                                                                *
      * -------------------------------------------------------------- *

       P-ON-LAND.
           EXIT
           .

       P-ON-INIT.
           EXIT
           .

       P-ON-DISPLAY.
           EVALUATE CHOICE-2
               WHEN 2
                   MOVE 'CONSULTATION D''UNE MAISON DE DISTRIBUTION'
                       TO TITRFLDO
                   MOVE 'CONSULTER' TO ACTFLDO
               WHEN 3
                   MOVE 'SUPPRESSION D''UNE MAISON DE DISTRIBUTION'
                       TO TITRFLDO
                   MOVE 'SUPPRIMER' TO ACTFLDO
               WHEN OTHER
                   MOVE ERR-BAD-CHOICE TO MSGFLDO
           END-EVALUATE
           .

       P-ON-SUBMIT.
      *    ON FAIT DU SURPLACE
           MOVE PGM-NAME TO DEST-PGM
           EVALUATE CHOICE-2
               WHEN 2
                   PERFORM P-CONSULTATION-MAISON-DIST
               WHEN 3
                   PERFORM P-SUPPRESSION-MAISON-DIST
           END-EVALUATE
           .

       P-CONSULTATION-MAISON-DIST.
           MOVE SIRENI TO MA-SIREN
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VF2')
               INPUTMSG (E-MAISON-DIST)
               INPUTMSGLEN (LENGTH OF E-MAISON-DIST)
           END-EXEC

      *ON REVIENT DU FICHIER D'ACCES AUX DONNEES ET ON RECOIT DES PAR
      *EN RETOUR (CODE RETOUR, I.E ECRITURE EFFECTUE OU NON)
           EXEC CICS
               RECEIVE
               INTO (W-MAISON-DIST)
           END-EXEC

           EVALUATE W-MA-CODE-CONSULT
               WHEN 1
                   MOVE 'MAISON DE DISTRIBUTION TROUVEE'     TO MSGFLDO
                   MOVE W-MA-ADRESSE      TO ADRESSEO
                   MOVE W-MA-RAISON-SOC   TO RAISSOCO
                   MOVE W-MA-CODEP-DIR    TO CODEPO
               WHEN 2
                   MOVE 'MAISON DE DISTRIBUTION NON TROUVEE' TO MSGFLDO
               WHEN OTHER
                   MOVE 'ECHEC CONSULTATION, PROBLEME FIC'   TO MSGFLDO
           END-EVALUATE
           .

       P-SUPPRESSION-MAISON-DIST.
           MOVE SIRENI TO MA-SIREN
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VF3')
               INPUTMSG (E-MAISON-DIST)
               INPUTMSGLEN (LENGTH OF E-MAISON-DIST)
           END-EXEC
           EXEC CICS
               RECEIVE
               INTO (W-MAISON-DIST)
           END-EXEC
           EVALUATE W-MA-CODE-CONSULT
               WHEN 1
                   MOVE W-MA-ADRESSE      TO ADRESSEO
                   MOVE W-MA-RAISON-SOC   TO RAISSOCO
                   MOVE W-MA-CODEP-DIR    TO CODEPO
                   MOVE 'MAISON DE DISTRIBUTION SUPPRIMEE'   TO MSGFLDO
               WHEN 2
                   MOVE 'MAISON DE DISTRIBUTION NON TROUVEE' TO MSGFLDO
               WHEN OTHER
                   MOVE 'PROBLEME FICHIER'  TO MSGFLDO
           END-EVALUATE
           .

       P-ON-LEAVE.
           EXIT
           .

      * -------------------------------------------------------------- *
      *                                                                *
      *                         COMMANDES CICS                         *
      *                                                                *
      * -------------------------------------------------------------- *

       P-CICS-TIME.
           EXEC CICS ASKTIME
               ABSTIME (DATE-TMP)
           END-EXEC
           EXEC CICS FORMATTIME
               ABSTIME (DATE-TMP)
               DDMMYYYY (DATEFLDO)
               DATESEP ('/')
           END-EXEC
           EXEC CICS FORMATTIME
               ABSTIME (DATE-TMP)
               TIME (TIMEFLDO)
               TIMESEP (':')
           END-EXEC
           .

       P-CICS-XCTL.
           EXEC CICS
               XCTL
               PROGRAM (NEXT-PGM)
               COMMAREA (WK-COMM-AREA)
               LENGTH (LENGTH OF WK-COMM-AREA)
           END-EXEC
           .

       P-CICS-RECV.
           EXEC CICS
               RECEIVE
               MAPSET (MAPSET)
               MAP ('MAP01')
               RESP (RC-1)
           END-EXEC
           .

       P-CICS-SEND.
           IF PREV-PGM = PGM-NAME THEN
               EXEC CICS
                   SEND
                   MAPSET (MAPSET)
                   MAP ('MAP01')
                   FROM (MAP01O)
                   LENGTH (LENGTH OF MAP01O)
               END-EXEC
           ELSE
               EXEC CICS
                   SEND
                   MAPSET (MAPSET)
                   MAP ('MAP01')
                   FROM (MAP01O)
                   LENGTH (LENGTH OF MAP01O)
                   ERASE
               END-EXEC
           END-IF
           .

       P-CICS-WAIT.
           EXEC CICS
               RETURN
               TRANSID (TRANS-ID)
               COMMAREA (WK-COMM-AREA)
               LENGTH (LENGTH OF WK-COMM-AREA)
           END-EXEC
           .

       P-CICS-BBYE.
           EXEC CICS
               SEND
               FROM (GOODBYE)
               LENGTH (LENGTH OF GOODBYE)
               ERASE
           END-EXEC
           .

       P-CICS-QUIT.
           EXEC CICS
               RETURN
           END-EXEC
           .
