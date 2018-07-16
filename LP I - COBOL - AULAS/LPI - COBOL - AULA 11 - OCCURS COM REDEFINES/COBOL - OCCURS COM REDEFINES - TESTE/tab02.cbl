       IDENTIFICATION          DIVISION.

       PROGRAM-ID.     TAB02.
       AUTHOR.         HIRO.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   23/05/2005.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.  LE UM ARQUIVO COM  DATA (DIA, MES, ANO) 
      *          E GRAVA A DATA POR EXTENSO UTILIZANDO UMA
      *          TABELA PRE-DEFINIDA (REDEFINES).

       ENVIRONMENT             DIVISION.

       CONFIGURATION           SECTION.

       SOURCE-COMPUTER.        IBM-PC.
       OBJECT-COMPUTER.        IBM-PC.
       SPECIAL-NAMES.          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT            SECTION.

       FILE-CONTROL.
           SELECT      ARQ-ENT         ASSIGN      TO      DISK
           ORGANIZATION                IS          LINE    SEQUENTIAL.
           SELECT      CAD-SAI         ASSIGN      TO      DISK
           ORGANIZATION                IS          LINE    SEQUENTIAL.

       DATA                    DIVISION.

       FILE                    SECTION.

       FD  ARQ-ENT
           LABEL       RECORD          ARE         STANDARD
           VALUE       OF FILE-ID      IS          "ARQ-ENT.DAT".

       01  REG-ENT.
           02 COD-ENT              PIC  9(003).
           02 DATA-ENT.
              03   DD-ENT          PIC  9(002).
              03   MM-ENT          PIC  9(002).
              03   AA-ENT          PIC  9(002).

       FD  CAD-SAI
           LABEL       RECORD          ARE         STANDARD.

       01  REG-SAI.
           02 COD-SAI                  PIC  9(003).
           02 DATA-SAI.
              03   DD-SAI              PIC  9(002).
              03   MM-SAI              PIC  X(009).
              03   AA-SAI              PIC  9(002).

       WORKING-STORAGE         SECTION.

       77  FIM-ARQ                     PIC X(03)    VALUE "NAO".

       01  TABELA-MES.
           02      DEF-MES.
              03   FILLER              PIC X(009)   VALUE  "  JANEIRO".
              03   FILLER              PIC X(009)   VALUE  "FEVEREIRO".
              03   FILLER              PIC X(009)   VALUE  "    MARCO".
              03   FILLER              PIC X(009)   VALUE  "    ABRIL".
              03   FILLER              PIC X(009)   VALUE  "     MAIO".
              03   FILLER              PIC X(009)   VALUE  "    JUNHO".
              03   FILLER              PIC X(009)   VALUE  "    JULHO".
              03   FILLER              PIC X(009)   VALUE  "   AGOSTO".
              03   FILLER              PIC X(009)   VALUE  " SETEMBRO".
              03   FILLER              PIC X(009)   VALUE  "  OUTUBRO".
              03   FILLER              PIC X(009)   VALUE  " NOVEMBRO".
              03   FILLER              PIC X(009)   VALUE  " DEZEMBRO".

       01  TABELA-MES-RED      REDEFINES            TABELA-MES.
           02      MES                 PIC X(009)
                                       OCCURS       12     TIMES.     

       
       PROCEDURE               DIVISION.
       
       PGM-TAB02.
          PERFORM      INICIO.
          PERFORM      PRINCIPAL       UNTIL
                                       FIM-ARQ     EQUAL   "SIM".
          PERFORM      FIM.
          STOP         RUN.

       INICIO.
          OPEN         INPUT           ARQ-ENT
                       OUTPUT          CAD-SAI.
          PERFORM      LEITURA.

       LEITURA.
          READ         ARQ-ENT
                       AT END
                       MOVE            "SIM"       TO      FIM-ARQ.

       PRINCIPAL.
          PERFORM      PESQUISA-TABELA.
          PERFORM      GRAVA-SAI.
          PERFORM      LEITURA.

      
       PESQUISA-TABELA.
          MOVE         MES (MM-ENT)    TO          MM-SAI.

       GRAVA-SAI.
          MOVE         COD-ENT         TO          COD-SAI.
          MOVE         DD-ENT          TO          DD-SAI.
          MOVE         AA-ENT          TO          AA-SAI.
          WRITE        REG-SAI.

       FIM.
          CLOSE        ARQ-ENT
                       CAD-SAI.
                      
