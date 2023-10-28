       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIG1DF2.
       AUTHOR. RBN.
       DATE-WRITTEN. 22/10/23.

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
       COPY MBLG1F2.
       COPY APIG1DWK.
       COPY MAISONDI.

       77 MAP02O   PIC X.
       77 MAP      PIC 9(02) VALUE 1.
       77 TRANS-ID PIC X(04) VALUE 'G1F2'.
       77 MAPSET   PIC X(07) VALUE 'MBLG1F2'.
       77 PGM-NAME PIC X(08) VALUE 'APIG1DF2'.
       77 PF3-PGM  PIC X(08) VALUE 'APIG1DSM'.

      * SOUS-PARAGRAPHES

       01 CODE-ERREUR.
           02 CODE-ERR-1 PIC 9(04).
           02 CODE-ERR-2 PIC 9(04).

       01 W-MAISON-DIST.
           05 W-MA-SIREN           PIC X(05).
           05 W-MA-RAISON-SOC      PIC X(30).
           05 W-MA-ADRESSE         PIC X(15).
           05 W-MA-CODEP-DIR       PIC X(04).
           05 W-MA-CODE-CONSULT    PIC 9(01).
           05 FILLER               PIC X(25).

       01 W2-MAISON-DIST.
           05 W2-MA-SIREN          PIC X(05).
           05 W2-MA-RAISON-SOC     PIC X(30).
           05 W2-MA-ADRESSE        PIC X(15).
           05 W2-MA-CODEP-DIR      PIC X(04).
           05 FILLER               PIC X(26).

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
           INITIALIZE W-MAISON-DIST
           .

       P-ON-DISPLAY.
           EVALUATE CHOICE-2
               WHEN 1
                   MOVE 'CREATION D''UNE MAISON DE DISTRIBUTION'
                       TO TITRFLDO
                   MOVE 'SAISIR LES CHAMPS DE LA MAISON DE DISTRIBUTION
      -'A CREER'
                       TO FIELD2O
               WHEN 4
                   MOVE 'MODIFICATION D''UNE MAISON DE DISTRIBUTION'
                       TO TITRFLDO
                   MOVE 'ETAPE 1 - SAISIR LE SIREN DE LA MAISON DE DISTR
      -'IBUTION A MODIFIER + ENTREE'
                       TO FIELD1O
                   MOVE 'ETAPE 2 - SAISIR LES CHAMPS DE LA MAISON DE DIS
      -'TRIBUTION A MODIFIER + ENTREE'
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
                   PERFORM P-CREATION-MAISON-DIST
               WHEN 4
                   PERFORM P-MODIF-MAISON-DIST
           END-EVALUATE
           .

       P-CREATION-MAISON-DIST.
           MOVE SIRENI   TO MA-SIREN
           MOVE RAISSOCI TO MA-RAISON-SOC
           MOVE ADRESSEI TO MA-ADRESSE
           MOVE CODEPI   TO MA-CODEP-DIR

           EXEC CICS
               LINK
               PROGRAM ('PGMG1VF1')
               INPUTMSG (E-MAISON-DIST)
               INPUTMSGLEN (LENGTH OF E-MAISON-DIST)
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

       P-MODIF-MAISON-DIST.
           EVALUATE FLAG-MODIF
               WHEN 0
                   MOVE SIRENI TO W-MA-SIREN
                   MOVE FLAG-MODIF TO W-MA-CODE-CONSULT

                   EXEC CICS
                       LINK
                       PROGRAM ('PGMG1VF4')
                       INPUTMSG (W-MAISON-DIST)
                       INPUTMSGLEN (LENGTH OF W-MAISON-DIST)
                   END-EXEC

                   EXEC CICS
                       RECEIVE
                       INTO (W-MAISON-DIST)
                   END-EXEC

                   EVALUATE W-MA-CODE-CONSULT
                       WHEN 1
                           MOVE 'MAISON DE DISTRIBUTION TROUVEE'
                               TO MSGFLDO
                           MOVE W-MA-ADRESSE      TO ADRESSEO
                           MOVE W-MA-RAISON-SOC   TO RAISSOCO
                           MOVE W-MA-CODEP-DIR    TO CODEPO
                           MOVE 1 TO FLAG-MODIF
                       WHEN 2
                           MOVE 'MAISON DE DISTRIBUTION NON TROUVEE'
                               TO MSGFLDO
                       WHEN OTHER
                           MOVE 'ERREUR FICHIER'  TO MSGFLDO
                   END-EVALUATE

               WHEN 1
                   MOVE FLAG-MODIF TO W-MA-CODE-CONSULT
                   MOVE SIRENI     TO W-MA-SIREN
                   MOVE ADRESSEI   TO W-MA-ADRESSE
                   MOVE RAISSOCI   TO W-MA-RAISON-SOC
                   MOVE CODEPI     TO W-MA-CODEP-DIR

                   EXEC CICS
                       LINK
                       PROGRAM ('PGMG1VF4')
                       INPUTMSG (W-MAISON-DIST)
                       INPUTMSGLEN (LENGTH OF W-MAISON-DIST)
                   END-EXEC

                   EXEC CICS
                       RECEIVE
                       INTO (W-MAISON-DIST)
                   END-EXEC

                   EVALUATE W-MA-CODE-CONSULT
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
