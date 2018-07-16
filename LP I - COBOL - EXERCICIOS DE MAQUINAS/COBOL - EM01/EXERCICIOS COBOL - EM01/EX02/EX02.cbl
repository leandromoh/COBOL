       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX02.
       AUTHOR. LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 10/08/2013.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADALU ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADATU ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADALU
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADALU.DAT".

       01 REG-ENT.
              02 NUMERO PIC 9(05).
              02 NOME PIC X(20).
              02 SEXO PIC X(01).
              02 DATA-NASCIMENTO.
                     03 DD PIC 9(2).
                     03 MM PIC 9(2).
                     03 AAAA PIC 9(4).

       FD CADATU
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADATU.DAT".

       01 REG-SAI.
              02 NUMERO-SAI PIC 9(05).
              02 NOME-SAI PIC X(20).
              02 DATA-NASCIMENTO-SAI.
                     03 DD PIC 9(2).
                     03 MM PIC 9(2).
                     03 AAAA PIC 9(4).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAMA02.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADALU OUTPUT CADATU.
              PERFORM VERIFICA-FIM.
       COPIA.
              MOVE NUMERO TO NUMERO-SAI.
              MOVE NOME TO NOME-SAI.
              MOVE DATA-NASCIMENTO TO DATA-NASCIMENTO-SAI.
              WRITE REG-SAI.
       VERIFICA-FIM.
              READ CADALU AT END MOVE 1 TO FIM-ARQ.
       TERMINO.
              CLOSE CADALU CADATU.
       PRINCIPAL.
              PERFORM COPIA.
              PERFORM VERIFICA-FIM.