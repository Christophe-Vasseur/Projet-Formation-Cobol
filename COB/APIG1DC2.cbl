       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DC2.
       AUTHOR. EDR.
       DATE-WRITTEN. 21/10/23.

      * ============================================================== *
      *                                                                *
      *            E N V I R O N M E N T   D I V I S I O N             *
      *                                                                *
      * ============================================================== *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

      * ============================================================== *
      *                                                                *
      *                   D A T A   D I V I S I O N                    *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.
       COPY MBLG1C2.
       COPY APIG1DWK.
       COPY PERSONNE.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1C2'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1C2'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DC2'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01 CODE-ERREUR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01 W-PERSONNE-MODIF.
           05 W-PE-CODEP           PIC X(04).
           05 W-PE-CODE-TYPE       PIC X(01).
           05 W-PE-IDENTITE        PIC X(25).
           05 W-PE-CODE-MODIF      PIC 9(01).
           05 FILLER               PIC X(49).

       01 W2-PERSONNE-MODIF.
           05 W2-PE-CODEP          PIC X(04).
           05 W2-PE-CODE-TYPE      PIC X(01).
           05 W2-PE-IDENTITE       PIC X(25).
           05 FILLER               PIC X(50).

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
               WHEN 1
                   MOVE 'CREATION D''UNE PERSONNE' TO TITRFLDO
                   MOVE 'SAISIR LES CHAMPS DE LA PERSONNE A CREER'
                       TO FIELD2O
               WHEN 4
                   MOVE 'MODIFICATION D''UNE PERSONNE'  TO TITRFLDO
                   MOVE 'ETAPE 1 - SAISIR LE CODE DE LA PERSONNE A MODIF
      -'IER + ENTREE'
                       TO FIELD1O
                   MOVE 'ETAPE 2 - SAISIR LES CHAMPS DE LA PERSONNE A MO
      -'DIFIER + ENTREE'
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
                   PERFORM P-CREATION-PERSONNE
               WHEN 4
                   PERFORM P-MODIF-PERSONNE
           END-EVALUATE
           .

       P-CREATION-PERSONNE.
           MOVE CODEPI TO PE-CODEP
           MOVE IDENTI TO PE-IDENTITE
           MOVE CODETI TO PE-CODE-TYPE

           EXEC CICS
               LINK PROGRAM('PGMG1VC1')
               INPUTMSG(E-PERSONNE)
               INPUTMSGLEN(LENGTH OF E-PERSONNE)
           END-EXEC

      *    ON REVIENT DU PROG D ACCES AUX DONNEES
      *    ET ON RECOIT DES PARAM EN RETOUR (CODE
      *    RETOUR = ECRITURE EFFECTUE OU NON)

           EXEC CICS RECEIVE
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

       P-MODIF-PERSONNE.
           EVALUATE FLAG-MODIF
               WHEN 0
                   MOVE CODEPI TO W-PE-CODEP
                   MOVE FLAG-MODIF TO W-PE-CODE-MODIF

                   EXEC CICS
                       LINK
                       PROGRAM ('PGMG1VC4')
                       INPUTMSG (W-PERSONNE-MODIF)
                       INPUTMSGLEN (LENGTH OF W-PERSONNE-MODIF)
                   END-EXEC

                   EXEC CICS
                       RECEIVE
                       INTO (W-PERSONNE-MODIF)
                   END-EXEC

                   EVALUATE W-PE-CODE-MODIF
                       WHEN 1
                           MOVE 'PERSONNE TROUVEE' TO MSGFLDO
                           MOVE W-PE-IDENTITE      TO IDENTO
                           MOVE W-PE-CODE-TYPE     TO CODETO
                           MOVE 1 TO FLAG-MODIF
                       WHEN 2
                           MOVE 'PERSONNE NON TROUVEE' TO MSGFLDO
                       WHEN OTHER
                           MOVE 'ERREUR FICHIER'   TO MSGFLDO
                   END-EVALUATE

               WHEN 1
                   MOVE FLAG-MODIF TO W-PE-CODE-MODIF
                   MOVE CODEPI TO W-PE-CODEP
                   MOVE IDENTI TO W-PE-IDENTITE
                   MOVE CODETI TO W-PE-CODE-TYPE

                   EXEC CICS
                       LINK
                       PROGRAM ('PGMG1VC4')
                       INPUTMSG (W-PERSONNE-MODIF)
                       INPUTMSGLEN (LENGTH OF W-PERSONNE-MODIF)
                   END-EXEC

                   EXEC CICS
                       RECEIVE
                       INTO (W-PERSONNE-MODIF)
                   END-EXEC

                   EVALUATE W-PE-CODE-MODIF
                       WHEN 1
                           MOVE 'MODIFICATION ENREGISTREE' TO MSGFLDO
                       WHEN 2
                           MOVE 'ECHEC DE LA MODIFICATION' TO MSGFLDO
                       WHEN OTHER
                           MOVE 'ERREUR FICHIER'           TO MSGFLDO
                   END-EVALUATE

                   MOVE 0 TO FLAG-MODIF

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
