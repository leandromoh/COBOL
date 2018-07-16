       IDENTIFICATION  DIVISION.
       PROGRAM-ID. EXEMPLO08.
       AUTHOR. ANGELA/HIRO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 21/09/2004.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.  FAZ O SORT DO ARQUIVO DE ENTRADA.
      *          USANDO INPUT E OUTPUT
      *          SELECIONAR SOMENTE PESSOA DO SEXO MASCULINO (M)
      *          GERANDO COMO SAIDA UM RELATORIO CLASSIFICADO.

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
           SELECT RELAT   ASSIGN  TO DISK.

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

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-REL      PIC X(80).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ      PIC X(03) VALUE "NAO".
       77  CT-LIN       PIC 9(02) VALUE 25.
       77  CT-PAG       PIC 9(02) VALUE ZEROES.

       01 CAB-01.
          02 FILLER     PIC X(70) VALUE SPACES.
          02 FILLER     PIC X(05) VALUE "PAG. ".
          02 VAR-PAG    PIC 99.
          02 FILLER     PIC X(03) VAlUE SPACES.

       01 CAB-02.
           02 FILLER    PIC X(15) VALUE SPACES.
           02 FILLER    PIC X(25) VALUE "RELATORIO DE USUARIOS DO".
           02 FILLER    PIC X(15) VALUE " SEXO MASCULINO".
           02 FILLER    PIC X(25) VALUE SPACES.
       
       01 CAB-03.
           02 FILLER   PIC X(05)  VALUE SPACES.
           02 FILLER   PIC X(06)  VALUE "CODIGO".
           02 FILLER   PIC X(10) VALUE SPACES.
           02 FILLER   PIC X(04)  VALUE "SEXO".
           02 FILLER   PIC X(05) VALUE SPACES.
           02 FILLER   PIC X(04) VALUE "NOME".
           02 FILLER   PIC X(46)  VALUE SPACES.

       01 DETALHE.
          02 FILLER      PIC X(05) VALUE SPACES.
          02 COD-DET     PIC 9(04).
          02 FILLER      PIC X(13) VALUE SPACES.
          02 SEXO-DET    PIC X(01).
          02 FILLER      PIC X(02) VALUE SPACES.
          02 NOME-DET    PIC X(30) VALUE SPACES.
          02 FILLER      PIC X(25) VALUE SPACES.
       
       PROCEDURE       DIVISION.
       
       PGM-EX08.
           SORT        TRAB
                       ASCENDING  KEY  COD-TRAB
                       INPUT      PROCEDURE        ROT-ENTRADA
                       OUTPUT     PROCEDURE        ROT-SAIDA.
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

       ROT-SAIDA               SECTION.
           PERFORM     INICIO-SAIDA.
           PERFORM     PRINCIPAL-SAIDA
                       UNTIL      FIM-ARQ  EQUAL   "SIM".
           PERFORM     FIM-SAIDA.

       INICIO-SAIDA            SECTION.
           MOVE        "NAO"           TO          FIM-ARQ.      
           OPEN        OUTPUT  RELAT.
           PERFORM     LE-SAIDA.

       LE-SAIDA                SECTION.
           RETURN      TRAB
                       AT END
                       MOVE       "SIM"    TO      FIM-ARQ.

       PRINCIPAL-SAIDA         SECTION.
           PERFORM     IMPRESSAO-SAIDA.
           PERFORM     LE-SAIDA.

       IMPRESSAO-SAIDA         SECTION.
          IF           CT-LIN          GREATER     THAN    24
               PERFORM CABECALHO-SAIDA.
          PERFORM      DETALHE-SAIDA.

       CABECALHO-SAIDA         SECTION.
            ADD        1 TO CT-PAG.
            MOVE       CT-PAG          TO          VAR-PAG.
            WRITE      REG-REL
                       AFTER           ADVANCING   PAGE.
            WRITE      REG-REL         FROM        CAB-01
                       AFTER           ADVANCING   1       LINE.
            WRITE      REG-REL         FROM        CAB-02
                       AFTER           ADVANCING   2       LINES.
            WRITE      REG-REL         FROM        CAB-03
                       AFTER           ADVANCING   2       LINES.
            MOVE       ZEROES          TO          CT-LIN.

       DETALHE-SAIDA           SECTION.
          MOVE         COD-TRAB        TO          COD-DET.
          MOVE         SEXO-TRAB       TO          SEXO-DET.
          MOVE         NOME-TRAB       TO          NOME-DET.
          WRITE        REG-REL         FROM        DETALHE
                       AFTER           ADVANCING   1       LINE.
          ADD          1               TO          CT-LIN.

       FIM-SAIDA               SECTION.
          CLOSE        RELAT.
