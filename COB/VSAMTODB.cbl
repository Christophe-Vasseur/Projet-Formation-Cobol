      * ////////////////////////////////////////////////////////////// *
      *                                                                *
      *             BATCH DE MISE A JOUR DE LA TABLE ALBUM             *
      *                                                                *
      * ////////////////////////////////////////////////////////////// *

      * ============================================================== *
      *                                                                *
      *                    IDENTIFICATION DIVISION                     *
      *                                                                *
      * ============================================================== *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAMTODB.
       AUTHOR. CHRISEDR.
       DATE-WRITTEN. 24/10/23.
       DATE-COMPILED. 24/10/23.

      * ============================================================== *
      *                                                                *
      *                      ENVIRONMENT DIVISION                      *
      *                                                                *
      * ============================================================== *

       ENVIRONMENT DIVISION.

      * -------------------------------------------------------------- *
      *                                                                *
      *                     CONFIGURATION SECTION                      *
      *                                                                *
      * -------------------------------------------------------------- *

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      * -------------------------------------------------------------- *
      *                                                                *
      *                      INPUT-OUTPUT SECTION                      *
      *                                                                *
      * -------------------------------------------------------------- *

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ALBUM-KSDS
               ASSIGN TO DDENTREE
               ORGANIZATION IS INDEXED
      *        ACCESS MODE IS DYNAMIC
               ACCESS IS SEQUENTIAL
               RECORD KEY IS AL-CODEA
               FILE STATUS F-STATUS
               .

      * ============================================================== *
      *                                                                *
      *                         DATA DIVISION                          *
      *                                                                *
      * ============================================================== *

       DATA DIVISION.

      * -------------------------------------------------------------- *
      *                                                                *
      *                          FILE SECTION                          *
      *                                                                *
      * -------------------------------------------------------------- *

       FILE SECTION.

       FD ALBUM-KSDS
           RECORD 80 CHARACTERS
           DATA RECORD IS E-ALBUM
           .

       COPY ALBUM.

      * -------------------------------------------------------------- *
      *                                                                *
      *                    WORKING-STORAGE SECTION                     *
      *                                                                *
      * -------------------------------------------------------------- *

       WORKING-STORAGE SECTION.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC
           EXEC SQL
               INCLUDE ALBUM
           END-EXEC

      * GARDE-FOUS
       77 I          PIC 9(03).
       77 J          PIC 9(03).
      * CODE DE RETOUR DE LECTURE DU FICHIER
       77 F-STATUS   PIC X(02) VALUE '00'.
      * SQLCODE
       77 SQLCODE-ED PIC +9(03).

      * INDICATEUR DE FIN DE FICHIER VSAM
       01 FIN-DE-FICHIER-IND PIC 9.
           88 FIN-DE-FICHIER     VALUE 1
           88 NON-FIN-DE-FICHIER VALUE 2
      * INDICATEUR DE FIN DE TABLE DB2
       01 FIN-DE-TABLE-IND    PIC 9.
           88 FIN-DE-TABLE       VALUE 1
           88 NON-FIN-DE-TABLE   VALUE 2
      * INDICATEUR DE FICHIER VSAM VIDE
       01 FICHIER-VIDE-IND   PIC 9.
           88 FICHIER-VIDE       VALUE 1
           88 NON-FICHIER-VIDE   VALUE 2

      *    DECLARATION CURSEUR
           EXEC SQL
               DECLARE CURSEUR CURSOR FOR
               SELECT CODEA, TITREA
               FROM ALBUM
               ORDER BY CODEA
           END-EXEC

      * -------------------------------------------------------------- *
      *                                                                *
      *                        LINKAGE SECTION                         *
      *                                                                *
      * -------------------------------------------------------------- *

       LINKAGE SECTION.

      * ============================================================== *
      *                                                                *
      *                       PROCEDURE DIVISION                       *
      *                                                                *
      * ============================================================== *

       PROCEDURE DIVISION.

       INITIALISATION.
           MOVE 0 TO I J
           SET FIN-DE-FICHIER TO TRUE
           SET FIN-DE-TABLE    TO TRUE
      *    OUVERTURE DU FICHIER VSAM
           OPEN INPUT ALBUM-KSDS
      *    VERIFICATION OUVERTURE FICHIER VSAM
           IF F-STATUS NOT = ZERO
               DISPLAY 'ERREUR OUVERTURE FICHIER VSAM'
               DISPLAY 'CODE ERREUR = ' F-STATUS
               PERFORM FIN-TRAITEMENT
           END-IF
           EXEC SQL
               OPEN CURSEUR
           END-EXEC
      *    IF STATUS-CURSEUR-DB2 NOT = ZERO
      *        DISPLAY 'ERREUR OUVERTURE CURSEUR DB2'
      *        DISPLAY 'CODE ERREUR = ' STATUS-CURSEUR-DB2
      *        PERFORM FIN-TRAITEMENT
      *    END-IF
           .

       TRAITEMENT-PRINCIPAL.

      *    LECTURE DU 1ER ENRG VSAM ET DB2
           PERFORM LECTURE-ENRG-VSAM
           PERFORM LECTURE-ENRG-DB2

           IF FIN-DE-FICHIER
               SET FICHIER-VIDE TO TRUE
           ELSE
               SET NON-FICHIER-VIDE TO TRUE
           END-IF

           PERFORM UNTIL
               FIN-DE-FICHIER AND
               FIN-DE-TABLE OR
               I = 999
      *    END UNTIL
               ADD 1 TO I
               IF FICHIER-VIDE
                   PERFORM VIDER-TABLE
               END-IF
               IF FIN-DE-FICHIER
                   PERFORM SUPPR-ENREG-RESTANTS
               END-IF
               IF FIN-DE-TABLE
                   PERFORM UNTIL FIN-DE-FICHIER OR J = 999
                       ADD 1 TO J
                       PERFORM CREER-ENREG
                       PERFORM LECTURE-ENRG-VSAM
                   END-PERFORM
               END-IF
               IF
                   NOT FIN-DE-FICHIER AND
                   NOT FIN-DE-TABLE
      *        END
                   IF AL-CODEA = CODEA
      *                MODIFIER L ENREG DB2
      *                LIRE VSAM ET DB2
                       CONTINUE
                   END-IF
                   IF AL-CODEA < CODEA
      *                CREER L ENREG DB2
      *                LIRE VSAM UNIQUEMENT
                       CONTINUE
                   END-IF
                   IF AL-CODEA > CODEA
      *                SUPPRIMER L ENREG DB2
      *                LIRE DB2 UNIQUEMENT
                       CONTINUE
                   END-IF
               END-IF
           END-PERFORM
           PERFORM FIN-TRAITEMENT
           .

      * -------------------------------------------------------------- *
      *                                                                *
      *                      FONCTIONS PERFORMEES                      *
      *                                                                *
      * -------------------------------------------------------------- *

       FIN-TRAITEMENT.
           CLOSE ALBUM-KSDS
           EXEC SQL
              CLOSE CURSEUR
           END-EXEC
           STOP RUN
           .

       VIDER-TABLE.
           EXEC SQL
               DELETE FROM API5.ALBUM
           END-EXEC
           .

       SUPPR-ENREG.
           EXEC SQL
               DELETE FROM API5.ALBUM
               WHERE CODEA = :AL-CODEA
           END-EXEC
           .

       SUPPR-ENREG-RESTANTS.
           EXEC SQL
               DELETE FROM API5.ALBUM
               WHERE CODEA > :AL-CODEA
           END-EXEC
           .

       CREER-ENREG.
           EXEC SQL
               INSERT INTO API5.ALBUM (CODEA, TITREA)
               VALUES (:AL-CODEA, :AL-TITREA)
           END-EXEC
           .

       LECTURE-ENRG-VSAM.
           READ ALBUM-KSDS
               AT END
                   SET FIN-DE-FICHIER TO TRUE
           END-READ
           .

       LECTURE-ENRG-DB2.
           EXEC SQL
               FETCH CURSEUR
               INTO :CODEA, :TITREA
           END-EXEC
           .
