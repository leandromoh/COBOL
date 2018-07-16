       IDENTIFICATION DIVISION.

       PROGRAM-ID.    EX03.
       AUTHOR.        LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION.  FATEC-SP.
       DATE-WRITTEN.  20/11/2013.
       DATE-COMPILED.
       SECURITY.
     
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMERC ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT MOVMERC ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT ATUMERC ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELERRO ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELMERC ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
      
       DATA DIVISION.

       FILE SECTION.
       FD CADMERC
         LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "CADMERC.DAT".
    
       01 REG-CADMERC.
         02 CODIGO-CADMERC      PIC 9(03).
         02 DESCRICAO-CADMERC   PIC X(30).
         02 ESTOQ-MIN-CADMERC   PIC 9(03).
         02 QTD-ESTOQ-CADMERC   PIC 9(03).
         02 PRECO-UNIT-CADMERC  PIC 9(04)V9(02).
      
       FD MOVMERC
         LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "MOVMERC.DAT".
      
       01 REG-MOVMERC.
         02 CODIGO-MOVMERC           PIC 9(03).
         02 DESCRICAO-MOVMERC        PIC X(30).
         02 ESTOQ-MIN-MOVMERC        PIC 9(03).
         02 QTD-ESTOQ-MOVMERC        PIC 9(03).
         02 PRECO-UNIT-MOVMERC       PIC 9(04)V9(02).
         02 TIPO-ATUALIZACAO-MOVMERC PIC X(01).
      
       FD ATUMERC 
         LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "ATUMERC.DAT".
      
       01 REG-ATUMERC.
         02 CODIGO-ATUMERC      PIC 9(03).
         02 DESCRICAO-ATUMERC   PIC X(30).
         02 ESTOQ-MIN-ATUMERC   PIC 9(03).
         02 QTD-ESTOQ-ATUMERC   PIC 9(03).
         02 PRECO-UNIT-ATUMERC  PIC 9(04)V9(02).
      
       FD RELERRO
          LABEL RECORD IS OMITTED.

       01 REL-ERRO              PIC X(80).

       FD RELMERC
          LABEL RECORD IS OMITTED.

       01 REL-MERC              PIC X(80).

       WORKING-STORAGE SECTION.

       77 CH-CADMERC           PIC X(04) VALUE SPACES.
       77 CH-MOVMERC           PIC X(04) VALUE SPACES.
       77 CT-PAG-RELERRO       PIC 9(03) VALUE 0.
       77 CT-PAG-RELMERC       PIC 9(03) VALUE 0.
       77 CT-LIN-RELERRO       PIC 9(02) VALUE 41.
       77 CT-LIN-RELMERC       PIC 9(02) VALUE 41.

       01 CAB-01-RELERRO.
          02 FILLER            PIC X(29) VALUE SPACES.
          02 FILLER            PIC X(22) VALUE "RELAÇÃO DE MERCADORIAS".
          02 FILLER            PIC X(15) VALUE SPACES.
          02 FILLER            PIC X(05) VALUE "PAG. ".
          02 VAR-PAG-RELERRO   PIC ZZ9.
          02 FILLER            PIC X(06) VALUE SPACES.

       01 CAB-02-RELERRO.
          02 FILLER            PIC X(30) VALUE SPACES.
          02 FILLER            PIC X(20) VALUE "ERROS DE ATUALIZACAO".
          02 FILLER            PIC X(30) VALUE SPACES.

       01 CAB-03-RELERRO.
          02 FILLER            PIC X(03) VALUE SPACES.
          02 FILLER            PIC X(06) VALUE "CODIGO".
          02 FILLER            PIC X(10) VALUE SPACES.
          02 FILLER            PIC X(09) VALUE "DESCRIÇÀO".
          02 FILLER            PIC X(26) VALUE SPACES.
          02 FILLER            PIC X(08) VALUE "MENSAGEM".
          02 FILLER            PIC X(18) VALUE SPACES.

       01 DETALHE-RELERRO.
          02 FILLER            PIC X(04) VALUE SPACES.
          02 CODIGO-RELERRO    PIC 999.
          02 FILLER            PIC X(04) VALUE SPACES.
          02 DESCRICAO-RELERRO PIC X(30).
          02 FILLER            PIC X(03) VALUE SPACES.
          02 MENSAGEM-RELERRO  PIC X(31).
          02 FILLER            PIC X(08) VALUE SPACES.

       01 CAB-01-RELMERC.
          02 FILLER            PIC X(29) VALUE SPACES.
          02 FILLER            PIC X(22) VALUE "RELAÇÃO DE MERCADORIAS".
          02 FILLER            PIC X(15) VALUE SPACES.
          02 FILLER            PIC X(05) VALUE "PAG. ".
          02 VAR-PAG-RELMERC   PIC ZZ9.
          02 FILLER            PIC X(06) VALUE SPACES.

       01 CAB-02-RELMERC.
          02 FILLER            PIC X(32) VALUE SPACES.
          02 FILLER            PIC X(15) VALUE "ESTOQUE CRÍTICO".
          02 FILLER            PIC X(32) VALUE SPACES.

       01 CAB-03-RELMERC.
          02 FILLER            PIC X(02) VALUE SPACES.
          02 FILLER            PIC X(06) VALUE "CODIGO".
          02 FILLER            PIC X(04) VALUE SPACES.
          02 FILLER            PIC X(09) VALUE "DESCRIÇÀO".
          02 FILLER            PIC X(20) VALUE SPACES.
          02 FILLER            PIC X(07) VALUE "ESTOQUE".
          02 FILLER            PIC X(05) VALUE SPACES.
          02 FILLER            PIC X(10) VALUE "QUANTIDADE".
          02 FILLER            PIC X(06) VALUE SPACES.
          02 FILLER            PIC X(05) VALUE "PREÇO".
          02 FILLER            PIC X(06) VALUE SPACES.

       01 CAB-04-RELMERC.
          02 FILLER            PIC X(42) VALUE SPACES.
          02 FILLER            PIC X(06) VALUE "MÍNIMO".
          02 FILLER            PIC X(06) VALUE SPACES.
          02 FILLER            PIC X(07) VALUE "ESTOQUE".
          02 FILLER            PIC X(07) VALUE SPACES.
          02 FILLER            PIC X(08) VALUE "UNITARIO".
          02 FILLER            PIC X(04) VALUE SPACES.

       01 DETALHE-RELMERC.
          02 FILLER             PIC X(03) VALUE SPACES.
          02 CODIGO-RELMERC     PIC 999.
          02 FILLER             PIC X(02) VALUE SPACES.
          02 DESCRICAO-RELMERC  PIC X(30).
          02 FILLER             PIC X(06) VALUE SPACES.
          02 ESTOQ-MIN-RELMERC  PIC 999.
          02 FILLER             PIC X(09) VALUE SPACES.
          02 QTD-ESTOQ-RELMERC  PIC 999.
          02 FILLER             PIC X(09) VALUE SPACES.
          02 PRECO-UNIT-RELMERC PIC Z.Z99,99.
          02 FILLER             PIC X(04) VALUE SPACES.

       PROCEDURE DIVISION.
      
       PROGRAM03.
      
       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL CH-CADMERC EQUAL CH-MOVMERC AND
                               CH-MOVMERC  EQUAL HIGH-VALUES.
       PERFORM FIM.
       STOP RUN.
      
       INICIO.
              OPEN INPUT CADMERC MOVMERC OUTPUT ATUMERC RELERRO RELMERC.
              PERFORM LER-CADMERC.
              PERFORM LER-MOVMERC.
      
       LER-CADMERC.
              READ CADMERC AT END MOVE HIGH-VALUES TO CH-CADMERC.
              IF CH-CADMERC EQUAL HIGH-VALUES
                 NEXT SENTENCE
              ELSE
                 MOVE CODIGO-CADMERC TO CH-CADMERC
              END-IF.
      
       LER-MOVMERC.
              READ MOVMERC AT END MOVE HIGH-VALUES TO CH-MOVMERC.
              IF CH-MOVMERC EQUAL HIGH-VALUES
                 NEXT SENTENCE
              ELSE
                 MOVE CODIGO-MOVMERC TO CH-MOVMERC
              END-IF.
      
       PRINCIPAL.
              IF CT-LIN-RELERRO GREATER THAN 40
                 PERFORM CABECALHO-RELERRO
              END-IF

              IF CT-LIN-RELMERC GREATER THAN 40
                 PERFORM CABECALHO-RELMERC
              END-IF
      *IGUAL
              IF CH-CADMERC EQUAL CH-MOVMERC
                 IF TIPO-ATUALIZACAO-MOVMERC EQUAL "A"
                    PERFORM GRAVA-MOVMERC
                 END-IF

                 IF TIPO-ATUALIZACAO-MOVMERC EQUAL "I"
                    MOVE CODIGO-MOVMERC     TO CODIGO-RELERRO
                    MOVE DESCRICAO-MOVMERC  TO DESCRICAO-RELERRO
                    MOVE "INCLUSÃO P/REG. JÁ EXISTENTE" TO 
                                                     MENSAGEM-RELERRO
                    WRITE REL-ERRO FROM DETALHE-RELERRO AFTER
                                                     ADVANCING 1 LINE

                    PERFORM GRAVA-CADMERC
                 END-IF

                 PERFORM LER-CADMERC
                 PERFORM LER-MOVMERC
              ELSE
      *CADANT
                 IF CH-CADMERC LESS CH-MOVMERC
                    PERFORM GRAVA-CADMERC
                    PERFORM LER-CADMERC
      *CADMOV
                 ELSE
                    MOVE CODIGO-MOVMERC     TO CODIGO-RELERRO
                    MOVE DESCRICAO-MOVMERC  TO DESCRICAO-RELERRO

                    IF TIPO-ATUALIZACAO-MOVMERC EQUAL "A" 
                       MOVE "ALTERAÇÃO P/REG. INEXISTENTE"
                       TO MENSAGEM-RELERRO
                    END-IF

                    IF TIPO-ATUALIZACAO-MOVMERC EQUAL "E" 
                       MOVE "EXCLUSÃO P/REG. INEXISTENTE"
                       TO MENSAGEM-RELERRO
                    END-IF

                    IF TIPO-ATUALIZACAO-MOVMERC EQUAL "I" 
                       PERFORM GRAVA-MOVMERC
                    ELSE
                       WRITE REL-ERRO FROM DETALHE-RELERRO AFTER
                                                ADVANCING 1 LINE
                    END-IF

                    PERFORM LER-MOVMERC
                 END-IF
              END-IF.
        
       GRAVA-MOVMERC.
              MOVE CODIGO-MOVMERC     TO CODIGO-ATUMERC.
              MOVE DESCRICAO-MOVMERC  TO DESCRICAO-ATUMERC.
              MOVE ESTOQ-MIN-MOVMERC  TO ESTOQ-MIN-ATUMERC.
              MOVE QTD-ESTOQ-MOVMERC  TO QTD-ESTOQ-ATUMERC.
              MOVE PRECO-UNIT-MOVMERC TO PRECO-UNIT-ATUMERC.
              WRITE REG-ATUMERC.
              PERFORM CARIMBA-DETALHE-RELMERC.
      
       GRAVA-CADMERC.
              MOVE CODIGO-CADMERC     TO CODIGO-ATUMERC.
              MOVE DESCRICAO-CADMERC  TO DESCRICAO-ATUMERC.
              MOVE ESTOQ-MIN-CADMERC  TO ESTOQ-MIN-ATUMERC.
              MOVE QTD-ESTOQ-CADMERC  TO QTD-ESTOQ-ATUMERC.
              MOVE PRECO-UNIT-CADMERC TO PRECO-UNIT-ATUMERC.
              WRITE REG-ATUMERC.
              PERFORM CARIMBA-DETALHE-RELMERC.

       CARIMBA-DETALHE-RELMERC.
              IF QTD-ESTOQ-ATUMERC NOT GREATER THAN ESTOQ-MIN-ATUMERC
                 MOVE CODIGO-ATUMERC     TO CODIGO-RELMERC
                 MOVE DESCRICAO-ATUMERC  TO DESCRICAO-RELMERC
                 MOVE ESTOQ-MIN-ATUMERC  TO ESTOQ-MIN-RELMERC
                 MOVE QTD-ESTOQ-ATUMERC  TO QTD-ESTOQ-RELMERC
                 MOVE PRECO-UNIT-ATUMERC TO PRECO-UNIT-RELMERC
                 WRITE REL-MERC FROM DETALHE-RELMERC
                 ADD 1 TO CT-LIN-RELMERC
              END-IF.

       FIM.
              CLOSE CADMERC MOVMERC ATUMERC RELERRO RELMERC.

       CABECALHO-RELERRO.
              ADD 1 TO CT-PAG-RELERRO.
              MOVE CT-PAG-RELERRO TO VAR-PAG-RELERRO.
              MOVE 0 TO CT-LIN-RELERRO.
              MOVE SPACES TO REL-ERRO.
              WRITE REL-ERRO AFTER ADVANCING PAGE.
              WRITE REL-ERRO FROM CAB-01-RELERRO
                                     AFTER ADVANCING 1 LINE.
              WRITE REL-ERRO FROM CAB-02-RELERRO
                                     AFTER ADVANCING 2 LINES.
              WRITE REL-ERRO FROM CAB-03-RELERRO
                                     AFTER ADVANCING 2 LINES.
              MOVE SPACES TO REL-ERRO.
              WRITE REL-ERRO AFTER ADVANCING 1 LINE.

       CABECALHO-RELMERC.
              ADD 1 TO CT-PAG-RELMERC.
              MOVE CT-PAG-RELMERC TO VAR-PAG-RELMERC.
              MOVE 0 TO CT-LIN-RELMERC.
              MOVE SPACES TO REL-MERC.
              WRITE REL-MERC AFTER ADVANCING PAGE.
              WRITE REL-MERC FROM CAB-01-RELMERC
                                     AFTER ADVANCING 1 LINE.
              WRITE REL-MERC FROM CAB-02-RELMERC
                                     AFTER ADVANCING 2 LINES.
              WRITE REL-MERC FROM CAB-03-RELMERC
                                     AFTER ADVANCING 2 LINES.
              WRITE REL-MERC FROM CAB-04-RELMERC
                                     AFTER ADVANCING 1 LINE.
              MOVE SPACES TO REL-MERC.
              WRITE REL-MERC AFTER ADVANCING 1 LINE.