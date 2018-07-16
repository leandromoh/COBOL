       IDENTIFICATION DIVISION.
       PROGRAM-ID. NIVEL88.
       AUTHOR. HIROMASA NAGATA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 30/09/2005.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.  LE OS REGISTRO DO ARQUIVO DE ENTRADA E
      *          FAZ A VALIDACAO UTILIZANDO O NIVEL 88.
 
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD-ENT  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CAD-OK   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CAD-RUIM ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD  CAD-ENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-ENT.DAT".

       01  REG-ENT.
           02 COD-ENT   PIC 9(02).
           02 NOME-ENT  PIC X(30).
  
           02 SEXO-ENT  PIC X(01).
              88 SEXO-VALIDO      VALUE "F" "f" "M" "m".
     
           02 MES-ENT   PIC 9(02).
              88 MES-VALIDO       VALUE 1 THRU 12.


       FD  CAD-OK
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-OK.DAT".
       01  REG-OK         PIC X(35).

       FD  CAD-RUIM
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-RUIM.DAT".
       01  REG-RUIM       PIC X(35).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ     PIC X(03) VALUE "NAO".
       77  CH-ERRO     PIC X(03) VALUE "NAO".

       
       PROCEDURE DIVISION.
       
       PGM-NIVEL88.
           PERFORM INICIO.
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.

       INICIO.
           OPEN INPUT  CAD-ENT
                OUTPUT CAD-OK
                       CAD-RUIM.
           PERFORM LEITURA.
                         
       LEITURA.
           READ CAD-ENT
               AT END MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CONSISTENCIA.
           PERFORM LEITURA.

       CONSISTENCIA.
           MOVE      "NAO"  TO  CH-ERRO.
           PERFORM   VAL-NOME.
           PERFORM   VAL-SEXO.
           PERFORM   VAL-MES.
           IF        CH-ERRO EQUAL "SIM"
             PERFORM GRAVA-RUIM
           ELSE
             PERFORM GRAVA-OK.

       VAL-NOME.
           IF NOME-ENT EQUAL SPACES
              MOVE   "SIM"  TO  CH-ERRO
           ELSE
              NEXT SENTENCE.

       VAL-SEXO.
            IF SEXO-VALIDO
               NEXT SENTENCE
            ELSE
               MOVE "SIM"    TO  CH-ERRO.

       VAL-MES.
           IF MES-VALIDO
              NEXT SENTENCE
           ELSE
              MOVE "SIM"     TO  CH-ERRO.

       GRAVA-OK.
          MOVE  REG-ENT  TO  REG-OK.
          WRITE REG-OK.

       GRAVA-RUIM.
          MOVE  REG-ENT  TO  REG-RUIM.
          WRITE REG-RUIM.


       FIM.
          CLOSE CAD-ENT
                       CAD-OK
                       CAD-RUIM.
                      
