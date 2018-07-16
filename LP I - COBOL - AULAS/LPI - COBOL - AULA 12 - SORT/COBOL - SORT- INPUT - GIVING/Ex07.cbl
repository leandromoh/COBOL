       IDENTIFICATION  DIVISION.
       PROGRAM-ID. EXEMPLO07.
       AUTHOR. ANGELA/HIRO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 24/09/2004.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.  FAZ O SORT DO ARQUIVO DE ENTRADA.
      *          USANDO INPUT E GIVING
      *          SELECIONAR SOMENTE PESSOA DO SEXO MASCULINO (M)
      *          GERANDO COMO SAIDA UM ARQUIVO CLASSIFICADO.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT    SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN  TO DISK
           ORGANIZATION   IS LINE SEQUENTIAL.
           SELECT TRAB    ASSIGN  TO DISK.
           SELECT SAIDA   ASSIGN  TO DISK
           ORGANIZATION   IS LINE SEQUENTIAL.

       DATA            DIVISION.
       FILE            SECTION.

       FD  ENTRADA
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "ENT.DAT".

       01  REG-ENT.
           02 COD-ENT  PIC 9(04).
           02 SEXO-ENT PIC X(01).
           02 NOME-ENT PIC X(30).

       SD  TRAB.
       01  REG-TRAB.
           02 COD-TRAB  PIC 9(04).
           02 SEXO-TRAB PIC X(01).
           02 NOME-TRAB PIC X(30).

       FD  SAIDA
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "SAI.DAT".

       01  REG-SAI.
           02 COD-SAI  PIC 9(04).
           02 SEXO-SAI PIC X(01).
           02 NOME-SAI PIC X(30).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ      PIC X(03) VALUE "NAO".
       
       PROCEDURE       DIVISION.
       
       PGM-EX07.
           SORT        TRAB
                       ASCENDING  KEY  COD-TRAB
                       INPUT      PROCEDURE        ROT-ENTRADA
                       GIVING     SAIDA.
           STOP        RUN.

       ROT-ENTRADA             SECTION.
           PERFORM     INICIO-ENTRADA.
           PERFORM     PRINCIPAL-ENTRADA
                       UNTIL      FIM-ARQ  EQUAL   "SIM".
           PERFORM     FIM-ENTRADA.

       INICIO-ENTRADA          SECTION.
           OPEN        INPUT   ENTRADA.
           PERFORM     LEITURA-ENTRADA.

       LEITURA-ENTRADA         SECTION.
           READ        ENTRADA
                       AT END
                       MOVE      "SIM"     TO      FIM-ARQ.
   
       PRINCIPAL-ENTRADA       SECTION.
           PERFORM     SELECAO-ENTRADA.
           PERFORM     LEITURA-ENTRADA.

       SELECAO-ENTRADA         SECTION.
           IF          SEXO-ENT        EQUAL       "M"
               PERFORM GRAVACAO-ENTRADA.

       GRAVACAO-ENTRADA        SECTION.
           MOVE        COD-ENT         TO          COD-TRAB.
           MOVE        SEXO-ENT        TO          SEXO-TRAB.
           MOVE        NOME-ENT        TO          NOME-TRAB.
           RELEASE     REG-TRAB.

       FIM-ENTRADA             SECTION.
           CLOSE       ENTRADA.

