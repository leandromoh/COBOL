       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX08.
       AUTHOR. LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 12/08/2013.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADFUN ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADSAI ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADFUN
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADFUN.DAT".

       01 REG-ENT.
              02 CODIGO    PIC 9(5).
              02 NOME     PIC X(20).
              02 SALARIO-BRUTO  PIC 9(5)V99.

       FD CADSAI
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADSAI.DAT".

       01 REG-SAI.
              02 CODIGO-SAI    PIC 9(5).
              02 NOME-SAI     PIC X(20).
              02 SALARIO-REAJUSTADO PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ  PIC 9(1) VALUE 0.
       77 REAJUSTE PIC 9(5)V99.

       PROCEDURE DIVISION.

       PROGRAMA08.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADFUN OUTPUT CADSAI.
              PERFORM VERIFICA-FIM.
       COPIA.
              PERFORM CALCULA-REAJUSTE.
              MOVE CODIGO TO CODIGO-SAI.
              MOVE NOME TO NOME-SAI.
              MOVE REAJUSTE TO SALARIO-REAJUSTADO.
              WRITE REG-SAI.
       VERIFICA-FIM.
              READ CADFUN AT END MOVE 1 TO FIM-ARQ.
       CALCULA-REAJUSTE.
              IF SALARIO-BRUTO NOT GREATER 1000
                     MULTIPLY SALARIO-BRUTO BY 1,12
                                          GIVING REAJUSTE
              ELSE
                     IF SALARIO-BRUTO NOT GREATER 2000
                            MULTIPLY SALARIO-BRUTO BY 1,11
                                          GIVING REAJUSTE
                     ELSE
                            MULTIPLY SALARIO-BRUTO BY 1,10
                                          GIVING REAJUSTE
                     END-IF
              END-IF.
       PRINCIPAL.
              PERFORM COPIA.
              PERFORM VERIFICA-FIM.
       TERMINO.
              CLOSE CADFUN CADSAI.