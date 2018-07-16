       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX06.
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
       SELECT CADENT ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADSAI ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADENT
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADENT.DAT".

       01 REG-ENT.
              02 MATRICULA  PIC 9(5).
              02 NOME    PIC X(30).
              02 SALARIO-BRUTO PIC 9(5)V99.

       FD CADSAI
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADSAI.DAT".

       01 REG-SAI.
              02 MATRICULA-SAI  PIC 9(5).
              02 NOME-SAI    PIC X(30).
              02 SALARIO-BRUTO-SAI PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAMA06.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADENT OUTPUT CADSAI.
       COPIA.
              MOVE MATRICULA TO MATRICULA-SAI.
              MOVE NOME TO NOME-SAI.
              MOVE SALARIO-BRUTO TO SALARIO-BRUTO-SAI.
              WRITE REG-SAI.
       VERIFICA-FIM.
              READ CADENT AT END MOVE 1 TO FIM-ARQ.
       SELECAO.
              IF SALARIO-BRUTO GREATER THAN 3000
                     PERFORM COPIA.
       PRINCIPAL.
              PERFORM SELECAO.
              PERFORM VERIFICA-FIM.
       TERMINO.
              CLOSE CADENT CADSAI.