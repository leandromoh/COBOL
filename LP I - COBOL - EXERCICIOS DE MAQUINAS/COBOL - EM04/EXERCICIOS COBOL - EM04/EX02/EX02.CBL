       IDENTIFICATION DIVISION.

       PROGRAM-ID.    EX02.
       AUTHOR.        LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION.  FATEC-SP.
       DATE-WRITTEN.  07/11/2013.
       DATE-COMPILED.
       SECURITY.
     
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADANT   ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT ARQMOV   ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADATU   ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELOCORR ASSIGN TO DISK.
      
       DATA DIVISION.

       FILE SECTION.
       FD CADANT
          LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "CADANT.DAT".
    
       01 REG-ANT.
         02 CC-ANT    PIC 9(03).
         02 TIPO-ANT  PIC X(01).
         02 ENDER-ANT PIC X(50).
      
       FD ARQMOV
          LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "ARQMOV.DAT".
      
       01 REG-MOV.
         02 CC-MOV    PIC 9(03).
         02 TIPO-MOV  PIC X(01).
         02 ENDER-MOV PIC X(50).
      
       FD CADATU
          LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "CADATU.DAT".
      
       01 REG-ATU.
         02 CC-ATU    PIC 9(03).
         02 TIPO-ATU  PIC X(01).
         02 ENDER-ATU PIC X(50).

       FD RELOCORR
          LABEL RECORD IS OMITTED.

       01 REG-ATR     PIC X(80).

    
       WORKING-STORAGE SECTION.

       77 FIM-ARQ    PIC 9(01) VALUE 0.
       77 CH-CADANT  PIC X(04) VALUE SPACES.
       77 CH-ARQMOV  PIC X(04) VALUE SPACES.
       77 CT-PAG     PIC 9(03) VALUE 0.
       77 CT-LIN     PIC 9(02) VALUE 41.

       01 CAB-01.
          02 FILLER      PIC X(29) VALUE SPACES.
          02 FILLER      PIC X(22) VALUE "Relação de atualização".
          02 FILLER      PIC X(15) VALUE SPACES.
          02 FILLER      PIC X(05) VALUE "Pag. ".
          02 VAR-PAG     PIC ZZ9.
          02 FILLER      PIC X(06) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(06) VALUE SPACES.
          02 FILLER    PIC X(06) VALUE "codigo".
          02 FILLER    PIC X(06) VALUE SPACES.
          02 FILLER    PIC X(04) VALUE "tipo".
          02 FILLER    PIC X(15) VALUE SPACES.
          02 FILLER    PIC X(08) VALUE "mensagem".
          02 FILLER    PIC X(35) VALUE SPACES.

       01 DETALHE.
          02 FILLER       PIC X(08) VALUE SPACES.
          02 CC-REL       PIC 999.
          02 FILLER       PIC X(08) VALUE SPACES.
          02 TIPO-REL     PIC X(01).
          02 FILLER       PIC X(08) VALUE SPACES.
          02 MENSAGEM-REL PIC X(30).
          02 FILLER       PIC X(22) VALUE SPACES.

       PROCEDURE DIVISION.
      
       PROGRAM02.
      
       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL CH-CADANT EQUAL HIGH-VALUES AND
                               CH-ARQMOV EQUAL HIGH-VALUES.
       PERFORM FIM.
       STOP RUN.
      
       INICIO.
              OPEN INPUT CADANT ARQMOV OUTPUT CADATU RELOCORR.
              PERFORM LER-CADANT.
              PERFORM LER-ARQMOV.
      
       LER-CADANT.
              READ CADANT AT END MOVE HIGH-VALUES TO CH-CADANT.
              IF CH-CADANT EQUAL HIGH-VALUES
                 NEXT SENTENCE
              ELSE
                 MOVE CC-ANT TO CH-CADANT
              END-IF.
      
       LER-ARQMOV.
              READ ARQMOV AT END MOVE HIGH-VALUES TO CH-ARQMOV.
              IF CH-ARQMOV EQUAL HIGH-VALUES
                 NEXT SENTENCE
              ELSE
                 MOVE CC-MOV TO CH-ARQMOV
              END-IF.
      
       PRINCIPAL.
              IF CT-LIN GREATER THAN 40
                 PERFORM CABECALHO
              END-IF
      *IGUAL
              IF CH-CADANT EQUAL CH-ARQMOV
                 IF TIPO-MOV EQUAL "A"
                    MOVE CC-ANT TO CC-REL
                    MOVE TIPO-ANT TO TIPO-REL
                    MOVE "REGISTRO SUBSTITUIDO" TO MENSAGEM-REL
                    WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE

                    MOVE TIPO-MOV TO TIPO-REL
                    MOVE "REGISTRO SUBSTITUTO" TO MENSAGEM-REL
                    WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE

                    PERFORM GRAVA-ARQMOV
                 END-IF

                 IF TIPO-MOV EQUAL "E"
                    MOVE CC-ANT TO CC-REL
                    MOVE TIPO-ANT TO TIPO-REL
                    MOVE "REGISTRO EXCLUIDO" TO MENSAGEM-REL
                    WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
                 END-IF

                 IF TIPO-MOV EQUAL "I"
                    MOVE CC-MOV TO CC-REL
                    MOVE TIPO-MOV TO TIPO-REL
                    MOVE "INCLUSÃO P/REG. JÁ EXISTENTE" TO MENSAGEM-REL
                    WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE

                    PERFORM GRAVA-CADANT
                 END-IF

                 PERFORM LER-CADANT
                 PERFORM LER-ARQMOV

              ELSE
      *CADANT
                 IF CH-CADANT LESS CH-ARQMOV
                    PERFORM GRAVA-CADANT
                    PERFORM LER-CADANT
      *CADMOV
                 ELSE
                    MOVE CC-MOV   TO CC-REL
                    MOVE TIPO-MOV TO TIPO-REL

                    IF TIPO-MOV EQUAL "A" 
                       MOVE "ALTERAÇÃO P/REG. INEXISTENTE"
                       TO MENSAGEM-REL
                    END-IF

                    IF TIPO-MOV EQUAL "E" 
                       MOVE "EXCLUSÃO P/REG. INEXISTENTE"
                       TO MENSAGEM-REL
                    END-IF

                    IF TIPO-MOV EQUAL "I" 
                        MOVE "REGISTRO INCLUIDO" TO MENSAGEM-REL
                        PERFORM GRAVA-ARQMOV
                    END-IF
                    
                    WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE
                    PERFORM LER-ARQMOV

                 END-IF
              END-IF.
           
       GRAVA-ARQMOV.
              MOVE CC-MOV    TO CC-ATU.
              MOVE TIPO-MOV  TO TIPO-ATU.
              MOVE ENDER-MOV TO ENDER-ATU.
              WRITE REG-ATU.
      
       GRAVA-CADANT.
              MOVE CC-ANT    TO CC-ATU.
              MOVE TIPO-ANT  TO TIPO-ATU.
              MOVE ENDER-ANT TO ENDER-ATU.
              WRITE REG-ATU.

       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 2 LINES.
              MOVE 0 TO CT-LIN.
       FIM.
              CLOSE CADANT ARQMOV CADATU RELOCORR.