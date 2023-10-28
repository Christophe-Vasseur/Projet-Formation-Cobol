       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DA2.
       AUTHOR. EDR.
       DATE-WRITTEN. 21/10/23.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1A2.
       COPY APIG1DWK.
       COPY ALBUM.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1A2'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1A2'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DA2'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01 CODE-ERREUR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01  W-ALBUM-MODIF.
           05 W-AL-CODEA           PIC X(04).
           05 W-AL-TITREA          PIC X(30).
           05 W-AL-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(45).

       01  W2-ALBUM-MODIF.
           05 W2-AL-CODEA          PIC X(04).
           05 W2-AL-TITREA         PIC X(30).
           05 FILLER               PIC X(46).

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
           INITIALIZE W-ALBUM-MODIF
           .

       P-ON-DISPLAY.
           EVALUATE CHOICE-2
               WHEN 1
                   MOVE 'CREATION D''UN ALBUM' TO TITRFLDO
                   MOVE 'SAISIR LES CHAMPS DE L ALBUM A CREER'
                       TO FIELD2O
               WHEN 4
                   MOVE 'MODIFICATION D''UN ALBUM' TO TITRFLDO
                   MOVE 'ETAPE 1 - SAISIR LE CODE DE L ALBUM A MODIFIER
      -'+ ENTREE'
                       TO FIELD1O
                   MOVE 'ETAPE 2 - SAISIR LES CHAMPS DE L ALBUM A MODIFI
      -'ER + ENTREE'
                       TO FIELD3O
               WHEN OTHER
                   MOVE ERR-BAD-CHOICE TO MSGFLDO
           END-EVALUATE
           .

       P-ON-SUBMIT.
      *    ON FAIT DU SURPLACE
           MOVE PGM-NAME TO DEST-PGM
           EVALUATE CHOICE-2
               WHEN 1
                   PERFORM P-CREATION-ALBUM
               WHEN 4
                   PERFORM P-MODIF-ALBUM
           END-EVALUATE
           .

       P-CREATION-ALBUM.
           MOVE CODEAI TO AL-CODEA
           MOVE TITREAI TO AL-TITREA
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VA1')
               INPUTMSG (E-ALBUM)
               INPUTMSGLEN (LENGTH OF E-ALBUM)
           END-EXEC
      *    ON REVIENT DU PROG D ACCES AUX DONNEES
      *    ET ON RECOIT DES PARAM EN RETOUR (CODE
      *    RETOUR = ECRITURE EFFECTUE OU NON)
           EXEC CICS
               RECEIVE
               INTO (CODE-ERREUR)
           END-EXEC
           EVALUATE CODE-ERR-1
               WHEN 1
                   MOVE 'ENREGISTREMENT CREE AVEC SUCCES' TO MSGFLDO
               WHEN 2
                   MOVE 'CLE EXISTANTE, ECHEC CREATION'   TO MSGFLDO
               WHEN 3
                   MOVE 'ECHEC CREATION, PROBLEME FICHER' TO MSGFLDO
               WHEN OTHER
                   MOVE CODE-ERREUR                       TO MSGFLDO
           END-EVALUATE
           .

       P-MODIF-ALBUM.
           EVALUATE FLAG-MODIF
               WHEN 0 PERFORM P-MODIF-ALBUM-STEP-1
               WHEN 1 PERFORM P-MODIF-ALBUM-STEP-2
           END-EVALUATE
           .

       P-MODIF-ALBUM-STEP-1.
           MOVE CODEAI TO W-AL-CODEA
           MOVE FLAG-MODIF TO W-AL-CODE-MODIF
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VA4')
               INPUTMSG (W-ALBUM-MODIF)
               INPUTMSGLEN (LENGTH OF W-ALBUM-MODIF)
           END-EXEC
           EXEC CICS
               RECEIVE
               INTO (W-ALBUM-MODIF)
           END-EXEC
           EVALUATE W-AL-CODE-MODIF
               WHEN 1
                   MOVE 'ALBUM TROUVE'     TO MSGFLDO
                   MOVE W-AL-TITREA        TO TITREAO
                   MOVE 1 TO FLAG-MODIF
               WHEN 2
                   MOVE 'ALBUM NON TROUVE' TO MSGFLDO
               WHEN OTHER
                   MOVE 'ERREUR FICHIER'   TO MSGFLDO
           END-EVALUATE
           .

       P-MODIF-ALBUM-STEP-2.
           MOVE FLAG-MODIF TO W-AL-CODE-MODIF
           MOVE CODEAI TO W-AL-CODEA
           MOVE TITREAI TO W-AL-TITREA
           EXEC CICS
               LINK
               PROGRAM ('PGMG1VA4')
               INPUTMSG (W-ALBUM-MODIF)
               INPUTMSGLEN (LENGTH OF W-ALBUM-MODIF)
           END-EXEC
           EXEC CICS
               RECEIVE
               INTO(W-ALBUM-MODIF)
           END-EXEC
           EVALUATE W-AL-CODE-MODIF
               WHEN 1
                   MOVE 'MODIFICATION ENREGISTREE' TO MSGFLDO
               WHEN 2
                   MOVE 'ECHEC DE LA MODIFICATION' TO MSGFLDO
               WHEN OTHER
                   MOVE 'ERREUR FICHIER'           TO MSGFLDO
           END-EVALUATE
           MOVE 0 TO FLAG-MODIF
           .

       P-ON-LEAVE.
           MOVE 0 TO FLAG-MODIF
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
