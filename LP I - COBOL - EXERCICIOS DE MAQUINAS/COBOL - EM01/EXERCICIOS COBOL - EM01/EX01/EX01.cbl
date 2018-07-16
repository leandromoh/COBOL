       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX01.
       AUTHOR. LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 08/08/2013.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCLI1 ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADCLI2 ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADCLI1 
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADCLI1.DAT".

       01 REG-ENT.
              02 COD-ENT  PIC 9(5).
              02 NOME-ENT PIC X(20).

       FD CADCLI2
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADCLI2.DAT".

       01 REG-SAI.
              02 NOME-SAI PIC X(20).
              02 COD-SAI  PIC 9(5).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAMA01.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADCLI1 OUTPUT CADCLI2.
              PERFORM VERIFICA-FIM.
       COPIA.
              MOVE NOME-ENT TO NOME-SAI.
              MOVE COD-ENT TO COD-SAI.
              WRITE REG-SAI.
       VERIFICA-FIM.
              READ CADCLI1 AT END MOVE 1 TO FIM-ARQ.
       TERMINO.
              CLOSE CADCLI1 CADCLI2.
       PRINCIPAL.
              PERFORM COPIA.
              PERFORM VERIFICA-FIM.