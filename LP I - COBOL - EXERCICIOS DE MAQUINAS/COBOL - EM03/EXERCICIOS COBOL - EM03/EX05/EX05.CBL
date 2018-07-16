       IDENTIFICATION DIVISION.

       PROGRAM-ID.   EX05.
       AUTHOR.       LEANDRO & EDEL.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 23/10/13.
       DATE-COMPILED.
       SECURITY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQVENDA ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELVENDA ASSIGN TO DISK.
       SELECT TRAB     ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD ARQVENDA 
          LABEL RECORD ARE STANDARD VALUE OF FILE-ID IS "ARQVENDA.DAT".

       01 REG-ENT.
          02 ESTADO-ENT        PIC X(02).
          02 NR-VENDEDOR-ENT   PIC 9(05).
          02 NOME-VENDEDOR-ENT PIC X(20).
          02 VAL-VENDA-ENT     PIC 9(07)V99.

       FD RELVENDA 
          LABEL RECORD IS OMITTED.

       01 REG-ATR              PIC X(80).

       SD TRAB.

       01 REG-TRAB.
          02 ESTADO-TRAB        PIC X(02).
          02 NR-VENDEDOR-TRAB   PIC 9(05).
          02 NOME-VENDEDOR-TRAB PIC X(20).
          02 VAL-VENDA-TRAB     PIC 9(07)V99.

       WORKING-STORAGE SECTION.

       77 FIM-ARQ           PIC 9(01) VALUE 0.
       77 CT-PAG            PIC 9(03) VALUE 0.
       77 CT-LIN            PIC 9(02) VALUE 21.
       77 ESTADO-ATUAL      PIC X(02).
       77 NR-VENDEDOR-ATUAL PIC 9(05).
       77 TOTAL-VENDEDOR    PIC 9(07)V99.
       77 TOTAL-ESTADO      PIC 9(09)V99.
       77 TOTAL-COMPANHIA   PIC 9(12)V99 VALUE 0.
       77 AUX               PIC 9(01) VALUE 1.
       77 ESTADOS           PIC X(19) OCCURS 27 TIMES.
       77 INDICE            PIC 9(02).

       01 CAB-01.
          02 FILLER    PIC X(22) VALUE SPACES.
          02 FILLER    PIC X(35) VALUE 
                      "VENDAS TOTAIS POR ESTADO - VENDEDOR".
          02 FILLER    PIC X(10) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC ZZ9.
          02 FILLER    PIC X(05) VALUE SPACES.

       01 CAB-02.
          02 FILLER       PIC X(01) VALUE SPACES.
          02 FILLER       PIC X(08) VALUE "ESTADO: ".
          02 ESTADO-REL   PIC X(19).
          02 FILLER       PIC X(52) VALUE SPACES.

       01 CAB-03.
          02 FILLER       PIC X(04) VALUE SPACES.
          02 FILLER       PIC X(15) VALUE "NRO DO VENDEDOR".
          02 FILLER       PIC X(09) VALUE SPACES.
          02 FILLER       PIC X(16) VALUE "NOME DO VENDEDOR".
          02 FILLER       PIC X(14) VALUE SPACES.
          02 FILLER       PIC X(14) VALUE "VALOR DA VENDA".
          02 FILLER       PIC X(08) VALUE SPACES.

       01 CAB-04.
          02 FILLER          PIC X(35) VALUE SPACES.
          02 FILLER          PIC X(18) VALUE "TOTAL DO VENDEDOR:".
          02 FILLER          PIC X(03) VALUE SPACES.
          02 TOTAL-VEND-REL  PIC ZZZ.ZZZ.ZZ9,99.
          02 FILLER          PIC X(10) VALUE SPACES.

       01 CAB-05.
          02 FILLER          PIC X(35) VALUE SPACES.
          02 FILLER          PIC X(16) VALUE "TOTAL DO ESTADO:".
          02 FILLER          PIC X(05) VALUE SPACES.
          02 TOTAL-EST-REL   PIC ZZZ.ZZZ.ZZ9,99.
          02 FILLER          PIC X(10) VALUE SPACES.

       01 CAB-06.
          02 FILLER          PIC X(35) VALUE SPACES.
          02 FILLER          PIC X(19) VALUE "TOTAL DA COMPANHIA:".
          02 FILLER          PIC X(02) VALUE SPACES.
          02 TOTAL-COMP-REL  PIC ZZZ.ZZZ.ZZ9,99.
          02 FILLER          PIC X(10) VALUE SPACES.

       01 DETALHE.
          02 FILLER          PIC X(08)  VALUE SPACES.
          02 NR-REL          PIC ZZZZZ.
          02 FILLER          PIC X(14)  VALUE SPACES.
          02 NOME-REL        PIC X(20)  VALUE SPACES.
          02 FILLER          PIC X(11)  VALUE SPACES.
          02 VAL-VENDA-REL   PIC Z.ZZZ.ZZ9,99.
          02 FILLER          PIC X(10) VALUE SPACES.

       PROCEDURE DIVISION.

       PROGRAM05.

       SORT TRAB
            ASCENDING ESTADO-TRAB
            ASCENDING NR-VENDEDOR-TRAB
            USING ARQVENDA 
            OUTPUT PROCEDURE GERA-REL.

       STOP RUN.

       INICIO.
              OPEN INPUT ARQVENDA OUTPUT RELVENDA.
              PERFORM VERIFICA-FIM.

       VERIFICA-FIM.
              RETURN TRAB AT END MOVE 1 TO FIM-ARQ.

       FIM.
           CLOSE ARQVENDA RELVENDA.

       PRINCIPAL.

              IF ESTADO-ATUAL NOT EQUAL ESTADO-TRAB

                 IF CT-PAG NOT EQUAL 0
                    MOVE TOTAL-VENDEDOR TO TOTAL-VEND-REL
                    WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 2 LINES
                    MOVE 1 TO AUX
                    MOVE TOTAL-ESTADO TO TOTAL-EST-REL
                    ADD  TOTAL-ESTADO TO TOTAL-COMPANHIA
                    WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 2 LINES
                 END-IF

                 MOVE ESTADO-TRAB TO ESTADO-ATUAL
                 PERFORM PEGA-NOME-ESTADO
                 MOVE ESTADOS(INDICE) TO ESTADO-REL

                 MOVE 0 TO TOTAL-VENDEDOR
                 MOVE 0 TO TOTAL-ESTADO

                 PERFORM AVANCA-PAGINA
                 PERFORM CABECALHO

              END-IF

              IF NR-VENDEDOR-ATUAL NOT EQUAL NR-VENDEDOR-TRAB

                 IF AUX NOT EQUAL 1
                    MOVE TOTAL-VENDEDOR TO TOTAL-VEND-REL
                    WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 2 LINES
                 END-IF

                 MOVE 0 TO AUX
                 MOVE NR-VENDEDOR-TRAB TO NR-VENDEDOR-ATUAL

                 MOVE NR-VENDEDOR-TRAB   TO NR-REL
                 MOVE NOME-VENDEDOR-TRAB TO NOME-REL
                 MOVE VAL-VENDA-TRAB     TO VAL-VENDA-REL

                 WRITE REG-ATR FROM DETALHE AFTER ADVANCING 2 LINES
                 ADD 1 TO CT-LIN.

                 MOVE VAL-VENDA-TRAB     TO TOTAL-VENDEDOR
                 MOVE 0                  TO NR-REL
                 MOVE SPACES             TO NOME-REL

                 ADD  VAL-VENDA-TRAB     TO TOTAL-ESTADO

              ELSE

                 PERFORM IMPRESSAO

              END-IF

              PERFORM VERIFICA-FIM.

       IMPRESSAO.
              IF CT-LIN GREATER THAN 20
                 PERFORM AVANCA-PAGINA
      *          PERFORM CABECALHO.
              END-IF
              PERFORM IMPDET.

       IMPDET.
              MOVE VAL-VENDA-TRAB TO VAL-VENDA-REL.
              ADD  VAL-VENDA-TRAB TO TOTAL-VENDEDOR.
              ADD  VAL-VENDA-TRAB TO TOTAL-ESTADO.

              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.

       CABECALHO.
              MOVE CT-PAG TO VAR-PAG.
              WRITE REG-ATR FROM CAB-01.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 2 LINES.

       AVANCA-PAGINA.
              ADD  1 TO CT-PAG.
              MOVE 0 TO CT-LIN.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.

       CABECALHO-FIM.
              MOVE TOTAL-VENDEDOR TO TOTAL-VEND-REL
              MOVE TOTAL-ESTADO TO TOTAL-EST-REL
              ADD  TOTAL-ESTADO TO TOTAL-COMPANHIA
              MOVE TOTAL-COMPANHIA TO TOTAL-COMP-REL.
              WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-06 AFTER ADVANCING 2 LINES.

       ALIMENTA-ESTADOS.
              MOVE "ACRE"                TO ESTADOS(01).
              MOVE "ALAGOAS"             TO ESTADOS(02).
              MOVE "AMAPA"               TO ESTADOS(03).
              MOVE "AMAZONAS"            TO ESTADOS(04).
              MOVE "BAHIA"               TO ESTADOS(05).
              MOVE "CEARA"               TO ESTADOS(06).
              MOVE "DISTRITO FEDERAL"    TO ESTADOS(07).
              MOVE "ESPIRITO SANTO"      TO ESTADOS(08).
              MOVE "GOIAS"               TO ESTADOS(09).
              MOVE "MARANHAO"            TO ESTADOS(10).
              MOVE "MATO GROSSO"         TO ESTADOS(11).
              MOVE "MATO GROSSO DO SUL"  TO ESTADOS(12).
              MOVE "MINAS GERAIS"        TO ESTADOS(13).
              MOVE "PARA"                TO ESTADOS(14).
              MOVE "PARAIBA"             TO ESTADOS(15).
              MOVE "PARANA"              TO ESTADOS(16).
              MOVE "PERNAMBUCO"          TO ESTADOS(17).
              MOVE "PIAUI"               TO ESTADOS(18).
              MOVE "RIO DE JANEIRO"      TO ESTADOS(19).
              MOVE "RIO GRANDE DO NORTE" TO ESTADOS(20).
              MOVE "RIO GRANDE DO SUL"   TO ESTADOS(21).
              MOVE "RONDONIA"            TO ESTADOS(22).
              MOVE "RORAIMA"             TO ESTADOS(23).
              MOVE "SANTA CATARINA"      TO ESTADOS(24).
              MOVE "SAO PAULO"           TO ESTADOS(25).
              MOVE "SERGIPE"             TO ESTADOS(26).
              MOVE "TOCANTINS"           TO ESTADOS(27).

       PEGA-NOME-ESTADO.
              IF ESTADO-ATUAL EQUAL "AC" OR "ac" MOVE 01 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "AL" OR "al" MOVE 02 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "AP" OR "ap" MOVE 03 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "AM" OR "am" MOVE 04 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "BA" OR "ba" MOVE 05 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "CE" OR "ce" MOVE 06 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "DF" OR "df" MOVE 07 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "ES" OR "es" MOVE 08 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "GO" OR "go" MOVE 09 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "MA" OR "ma" MOVE 10 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "MT" OR "mt" MOVE 11 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "MS" OR "ms" MOVE 12 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "MG" OR "mg" MOVE 13 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "PA" OR "pa" MOVE 14 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "PB" OR "pb" MOVE 15 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "PR" OR "pr" MOVE 16 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "PE" OR "pe" MOVE 17 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "PI" OR "pi" MOVE 18 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "RJ" OR "rj" MOVE 19 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "RN" OR "rn" MOVE 20 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "RS" OR "rs" MOVE 21 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "RO" OR "ro" MOVE 22 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "RR" OR "rr" MOVE 23 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "SC" OR "sc" MOVE 24 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "SP" OR "sp" MOVE 25 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "SE" OR "se" MOVE 26 TO INDICE 
                 NEXT SENTENCE
              END-IF
              IF ESTADO-ATUAL EQUAL "TO" OR "to" MOVE 27 TO INDICE 
                 NEXT SENTENCE
              END-IF.

       GERA-REL SECTION.
              PERFORM INICIO.
              PERFORM ALIMENTA-ESTADOS.
              PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
              PERFORM CABECALHO-FIM.
              PERFORM FIM.