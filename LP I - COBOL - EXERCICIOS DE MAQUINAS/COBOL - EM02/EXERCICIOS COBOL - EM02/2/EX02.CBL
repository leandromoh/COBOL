       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX02.
       AUTHOR. LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 08/09/2013.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCLI ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELCLI ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD CADCLI 
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADCLI.DAT".

       01 REG-ENT.
              02 CPF-CLI            PIC 9(11).
              02 NOME-CLI           PIC X(30).
              02 ESTADO-CLI         PIC X(02).
              02 CIDADE-CLI         PIC X(30).
              02 TELEFONE-CLI       PIC 9(08).
              02 ULTIMA-VISITA-CLI.
                     03 DD          PIC 9(2).
                     03 MM          PIC 9(2).
                     03 AAAA        PIC 9(4).

       FD  RELCLI 
           LABEL        RECORD         IS          OMITTED.

       01  REG-ATR                  PIC X(80).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ     PIC 9(01) VALUE 0.
       77 CT-LIN      PIC 9(02) VALUE 26.
       77 CT-PAG      PIC 9(02) VALUE ZEROES.

       01 CAB-01.
          02 FILLER    PIC X(70) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC 99.
          02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(31) VALUE SPACES.
          02 FILLER    PIC X(17) VALUE "TURISMAR TURISMOS".
          02 FILLER    PIC X(32) VALUE SPACES.

       01 CAB-03.
          02 FILLER    PIC X(22) VALUE SPACES.
          02 FILLER    PIC X(35)
                       VALUE "CLIENTES NO PERÍODO DE: 2010 A 2011".
          02 FILLER    PIC X(23) VALUE SPACES.
       
       01 CAB-04.
          02 FILLER    PIC X(12) VALUE SPACES.
          02 FILLER    PIC X(04) VALUE "NOME".
          02 FILLER    PIC X(26) VALUE SPACES.
          02 FILLER    PIC X(06) VALUE "ESTADO".
          02 FILLER    PIC X(11) VALUE SPACES.
          02 FILLER    PIC X(08) VALUE "TELEFONE".
          02 FILLER    PIC X(13) VALUE SPACES.

       01 CAB-05.
          02 FILLER      PIC X(05)  VALUE SPACES.
          02 FILLER      PIC X(19) VALUE "Total de Clientes: ".
          02 TOTAL-CLI   PIC 9(05) VALUE ZEROES.
          02 FILLER      PIC X(51) VALUE SPACES.

       01 CAB-06.
          02 FILLER      PIC X(80) VALUE ALL "-".

       01 DETALHE.
          02 FILLER    PIC X(06) VALUE SPACES.
          02 NOME      PIC X(30).
          02 FILLER    PIC X(08) VALUE SPACES.
          02 ESTADO    PIC X(02).
          02 FILLER    PIC X(13) VALUE SPACES.
          02 TELEFONE  PIC X(09) VALUE "XXXX-XXXX".
          02 FILLER    PIC X(08) VALUE SPACES.

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM CABECALHO-FINAL.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADCLI OUTPUT RELCLI.
              PERFORM VERIFICA-FIM.
       VERIFICA-FIM.
              READ CADCLI AT END MOVE 1 TO FIM-ARQ.
       IMPRESSAO.
              IF CT-LIN GREATER THAN 25
                     PERFORM CABECALHO.
              PERFORM IMPDET.
       TERMINO.
              CLOSE CADCLI RELCLI.
       PRINCIPAL.
              IF AAAA >= 2010 AND <= 2011
                     PERFORM IMPRESSAO.
              PERFORM VERIFICA-FIM.
       IMPDET.
              MOVE NOME-CLI           TO NOME.
              MOVE ESTADO-CLI         TO ESTADO.
              MOVE TELEFONE-CLI (1:4) TO TELEFONE (1:4).
              MOVE TELEFONE-CLI (5:4) TO TELEFONE (6:4).
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.
              ADD 1 TO TOTAL-CLI.
       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 1 LINES.
              WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 3 LINES.
              WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 3 LINES.
              WRITE REG-ATR FROM CAB-06 AFTER ADVANCING 1 LINE.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING 1 LINES.
              MOVE 1 TO CT-LIN.
       CABECALHO-FINAL.
              WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 2 LINES.