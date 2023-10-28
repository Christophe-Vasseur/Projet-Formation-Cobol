      *======================================================*
      *   SOUS-PROGRAMME DE CONSULTATION D UN ALBUM          *
      *======================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMG1VC2.
       AUTHOR. RAZ.
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
           05 W-PE-CODE-CONSULT    PIC 9(01).
           05 FILLER               PIC X(49).

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

      *    RECEPTION DES DONNEES

           EXEC CICS
               RECEIVE INTO(E-PERSONNE)
           END-EXEC

      *    CONSULTATION DU FICHIER VSAM

           EXEC CICS
               READ FILE('PERSONG1')
               INTO (E-PERSONNE)
               RIDFLD (PE-CODEP)
               RESP (RC-1)
               RESP2 (RC-2)
           END-EXEC

      *    ANALYSE DU CODE RETOUR 000059
      *    1 : ECRITURE VSAM OK 000060
      *    2 : ENREGISTREMENT NON TROUVE, CONSULTATION IMPOSSIBLE
      *    3 : PROBLEME FICHIER VSAM
           MOVE E-PERSONNE TO W-PERSONNE

           EVALUATE RC-1
               WHEN DFHRESP(NORMAL)
                   MOVE 1 TO W-PE-CODE-CONSULT
               WHEN DFHRESP(NOTFND)
                   MOVE 2 TO W-PE-CODE-CONSULT
               WHEN OTHER
                   MOVE 3 TO W-PE-CODE-CONSULT
      * DEBUGGING
      *        WHEN OTHER
      *            MOVE RC-1 TO CODE-ERR-1
      *            MOVE RC-2 TO CODE-ERR-2
           END-EVALUATE

           EXEC CICS
               RETURN
               INPUTMSG (W-PERSONNE)
               INPUTMSGLEN (LENGTH OF W-PERSONNE)
           END-EXEC

           GOBACK
           .
