       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX05.
       AUTHOR. LEANDRO FERNANDES & EDEL CORADI.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 15/09/2013.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQALU   ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT ARQREP   ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELAPROV ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD ARQALU
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "ARQALU.DAT".

       01 REG-ALU.
              02 MATRICULA  PIC 9(07).
              02 NOME       PIC X(30).
              02 NOTA1      PIC 9(02)V9(2).
              02 NOTA2      PIC 9(02)V9(2).
              02 NOTA3      PIC 9(02)V9(2).
              02 FALTAS     PIC 9(02).
              02 SEXO       PIC X(01).

       FD ARQREP
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "ARQREP.DAT".

       01 REG-REP.
              02 MATRICULA-SAI  PIC 9(07).
              02 NOME-SAI       PIC X(30).
              02 NOTA1-SAI      PIC 9(02)V9(2).
              02 NOTA2-SAI      PIC 9(02)V9(2).
              02 NOTA3-SAI      PIC 9(02)V9(2).
              02 FALTAS-SAI     PIC 9(02).
              02 SEXO-SAI       PIC X(01).

       FD  RELAPROV 
           LABEL        RECORD         IS          OMITTED.

       01  REG-ATR                  PIC X(80).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ     PIC 9(01) VALUE 0.
       77 CT-LIN      PIC 9(02) VALUE 31.
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       77 MEDIA       PIC 9(02)V9(02).
       77 SOMA-NOTAS  PIC 9(04)V9(02) VALUE ZEROES.

       01 CAB-01.
          02 FILLER      PIC X(25) VALUE SPACES.
          02 FILLER      PIC X(28) VALUE "RELAÇÃO DE ALUNOS APROVADOS".
          02 FILLER      PIC X(16) VALUE SPACES.
          02 FILLER      PIC X(05) VALUE "PAG. ".
          02 VAR-PAG     PIC 999.
          02 FILLER      PIC X(03) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(06) VALUE "NÚMERO".
          02 FILLER    PIC X(14) VALUE SPACES.
          02 FILLER    PIC X(13) VALUE "NOME DO ALUNO".
          02 FILLER    PIC X(15) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "MÉDIA".
          02 FILLER    PIC X(08) VALUE SPACES.
          02 FILLER    PIC X(06) VALUE "FALTAS".
          02 FILLER    PIC X(08) VALUE SPACES.
 
       01 CAB-03.
          02 FILLER      PIC X(05) VALUE SPACES.
          02 FILLER      PIC X(27) VALUE "Total de alunos aprovados: ".
          02 TOTAL-APROV PIC 9(04) VALUE ZEROES.
          02 FILLER      PIC X(44) VALUE SPACES.

       01 CAB-04.
          02 FILLER           PIC X(05) VALUE SPACES.
          02 FILLER           PIC X(23) VALUE "Media geral da turma:  ".
          02 MASC-MEDIA-GERAL PIC Z9,99.
          02 FILLER           PIC X(48) VALUE SPACES.

       01 DETALHE.
          02 FILLER         PIC X(05)  VALUE SPACES.
          02 MATRICULA-REL  PIC X(08)  VALUE "999999-9".
          02 FILLER         PIC X(05)  VALUE SPACES.
          02 NOME-REL       PIC X(30).
          02 FILLER         PIC X(05)  VALUE SPACES.
          02 MEDIA-REL      PIC Z9,99.
          02 FILLER         PIC X(10)  VALUE SPACES.
          02 FALTAS-REL     PIC 9(02).
          02 FILLER         PIC X(11)  VALUE SPACES.

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM CABECALHO-FINAL.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT ARQALU OUTPUT ARQREP RELAPROV.
              PERFORM VERIFICA-FIM.
       VERIFICA-FIM.
              READ ARQALU AT END MOVE 1 TO FIM-ARQ.
       CALCULA-MEDIA.
              ADD NOTA1 NOTA2 NOTA3 GIVING MEDIA.
              DIVIDE 3 INTO MEDIA.
       COPIA.
              MOVE MATRICULA  TO MATRICULA-SAI.
              MOVE NOME       TO NOME-SAI.
              MOVE NOTA1      TO NOTA1-SAI.
              MOVE NOTA2      TO NOTA2-SAI.
              MOVE NOTA3      TO NOTA3-SAI.
              MOVE FALTAS     TO FALTAS-SAI.
              MOVE SEXO       TO SEXO-SAI.
              WRITE REG-REP.
       IMPRESSAO.
              PERFORM CALCULA-MEDIA.
              IF MEDIA NOT LESS THAN 7 AND FALTAS LESS THAN 16
                     IF CT-LIN GREATER THAN 30
                            PERFORM CABECALHO
                     END-IF
                     PERFORM IMPDET
              ELSE
                     PERFORM COPIA
              END-IF.
       TERMINO.
              CLOSE ARQALU ARQREP RELAPROV.
       PRINCIPAL.
              PERFORM IMPRESSAO.
              PERFORM VERIFICA-FIM.
       IMPDET.
              MOVE MATRICULA (1:6) TO MATRICULA-REL (1:6).
              MOVE MATRICULA (7:1) TO MATRICULA-REL (8:1).
              MOVE NOME      TO NOME-REL.
              MOVE MEDIA     TO MEDIA-REL.
              MOVE FALTAS    TO FALTAS-REL.
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.
              ADD 1 TO TOTAL-APROV.
              ADD MEDIA TO SOMA-NOTAS.
       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 2 LINES.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING 1 LINE.
              MOVE 1 TO CT-LIN.
       CABECALHO-FINAL.
              DIVIDE TOTAL-APROV INTO SOMA-NOTAS 
                                     GIVING MASC-MEDIA-GERAL.
              WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 1 LINE.