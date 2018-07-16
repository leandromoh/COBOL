       IDENTIFICATION          DIVISION.

       PROGRAM-ID.     REDF01.
       AUTHOR.         HIROMASA NAGATA.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   23/05/2005.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.  LE UM ARQUIVO COM  DOIS TIPOS DE REGISTRO. 
      *          O PRIMEIRO REGISTRO E O CAPA DE LOTE E OS DEMAIS
      *          SAO DETALHES. A PARTIR DESTES REGISTROS SAO
      *          MOSTRADOS DISPLAY'S, CONTENDO A MENSAGEM
      *          'LOTE OK', SE A TOTALIZACAO DOS REGISTROS FOR
      *          ADEQUADA. SE A TOTALIZACAO DOS REGISTROS FOR
      *          INADEQUADA, SERA GRAVADO A MENSAGEM 'LOTE REJEITADO'.

       ENVIRONMENT             DIVISION.

       CONFIGURATION           SECTION.

       SOURCE-COMPUTER.        IBM-PC.
       OBJECT-COMPUTER.        IBM-PC.
       SPECIAL-NAMES.          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT            SECTION.

       FILE-CONTROL.
           SELECT      ARQ-ENT         ASSIGN      TO      DISK
           ORGANIZATION                IS          LINE    SEQUENTIAL.

       DATA                    DIVISION.

       FILE                    SECTION.

       FD  ARQ-ENT
           LABEL       RECORD          ARE         STANDARD
           VALUE       OF FILE-ID      IS          "ARQ-ENT.DAT".

       01  REG-LOTE.
           02 QUANT-ENT            PIC  9(003).
           02 VALOR-ENT            PIC  9(007)V9(002).
           02 FILLER               PIC  X(018).

       01  REG-DET.
           02 FILLER               PIC  X(010).
           02 VALOR-DET            PIC  9(005)V9(002).
           02 FILLER               PIC  X(013).


       WORKING-STORAGE         SECTION.

       77  FIM-ARQ                     PIC X(003)  VALUE "NAO".
       77  CONT                        PIC 9(003)  VALUE ZEROS.
       77  VALOR-TOTAL                 PIC 9(007)V9(002)
                                                   VALUE ZEROS.
       77  AUX-QUANT                   PIC 9(003)  VALUE ZEROS.
       77  AUX-VALOR                   PIC 9(007)V9(002)
                                                   VALUE ZEROS.

       PROCEDURE               DIVISION.
       
       PGM-REDF01.
          PERFORM      INICIO.
          PERFORM      PRINCIPAL       UNTIL
                                       FIM-ARQ     EQUAL   "SIM".
          PERFORM      VERIFICA-TOTAL.
          PERFORM      FIM.
          STOP         RUN.

       INICIO.
          OPEN         INPUT           ARQ-ENT.
          PERFORM      LEITURA-LOTE.
          PERFORM      LEITURA-DET.

       LEITURA-LOTE.
          READ         ARQ-ENT
                       AT END
                       MOVE            "SIM"       TO      FIM-ARQ.
          IF           FIM-ARQ         EQUAL       "SIM"
               NEXT    SENTENCE
          ELSE
               MOVE    QUANT-ENT       TO          AUX-QUANT
               MOVE    VALOR-ENT       TO          AUX-VALOR.    

       LEITURA-DET.

          IF           FIM-ARQ         EQUAL       "SIM"
               NEXT    SENTENCE
          ELSE
               READ    ARQ-ENT
                       AT END
                       MOVE            "SIM"       TO      FIM-ARQ.


       PRINCIPAL.
          PERFORM      ACUMULA-DET.
          PERFORM      LEITURA-DET.

      
       ACUMULA-DET.
          ADD          VALOR-DET       TO          VALOR-TOTAL.
          ADD          1               TO          CONT.


       VERIFICA-TOTAL.
          IF           (VALOR-TOTAL    EQUAL       AUX-VALOR) AND
                       (CONT           EQUAL       AUX-QUANT)
               DISPLAY "LOTE OK"
               DISPLAY "VALOR-TOTAL: " VALOR-TOTAL
               DISPLAY "CONT       : " CONT
          ELSE
               DISPLAY "LOTE REJEITADO"
               DISPLAY "VALOR TOTAL INFORMADO         : " AUX-VALOR
               DISPLAY "VALOR CONTABILIZADO           : " VALOR-TOTAL
               DISPLAY "QUANTIDADE INFORMADOS         : " AUX-QUANT
               DISPLAY "TOTAL DE REGISTROS LIDO       : " CONT.

       FIM.
          CLOSE        ARQ-ENT.
                      
