       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOLHA.
       AUTHOR. DEPARTAMENTO-DE-TEDIO.
      *> SISTEMA DE FOLHA DE PAGAMENTO - LAYOUT DO ARQUIVO:
      *> COL 01-05 MATRICULA | 06-23 NOME | 24-30 SALARIO (V99)
      *> COL 31-33 HORAS EXTRAS
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-FUNC ASSIGN TO "FUNCIONARIOS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQ-REL ASSIGN TO "RELATORIO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-FUNC.
       01  REG-FUNC.
           05 F-MATRICULA        PIC 9(05).
           05 F-NOME             PIC X(18).
           05 F-SALARIO          PIC 9(05)V99.
           05 F-HORAS-EXTRAS     PIC 9(03).

       FD  ARQ-REL.
       01  LINHA-REL             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FIM-ARQUIVO        PIC X VALUE "N".
           88 FIM-ARQUIVO        VALUE "S".
       01  WS-CALCULOS.
           05 WS-VALOR-HORA      PIC 9(05)V99.
           05 WS-VALOR-HE        PIC 9(06)V99.
           05 WS-BRUTO           PIC 9(06)V99.
           05 WS-INSS            PIC 9(06)V99.
           05 WS-IRRF            PIC 9(06)V99.
           05 WS-LIQUIDO         PIC 9(06)V99.
       01  WS-TOTAIS.
           05 WS-QTD-FUNC        PIC 9(03) VALUE ZERO.
           05 WS-TOT-BRUTO       PIC 9(08)V99 VALUE ZERO.
           05 WS-TOT-DESC        PIC 9(08)V99 VALUE ZERO.
           05 WS-TOT-LIQ         PIC 9(08)V99 VALUE ZERO.

       01  CAB-1.
           05 FILLER PIC X(80) VALUE
              "=== RELATORIO DE FOLHA DE PAGAMENTO ===".
       01  CAB-2.
           05 FILLER PIC X(80) VALUE
           "MATR  NOME               BRUTO      INSS      IRRF   LIQUIDO".
       01  DET.
           05 D-MATRICULA        PIC 9(05).
           05 FILLER             PIC XX VALUE SPACES.
           05 D-NOME             PIC X(18).
           05 D-BRUTO            PIC ZZZ.ZZ9,99.
           05 FILLER             PIC X VALUE SPACE.
           05 D-INSS             PIC ZZ.ZZ9,99.
           05 FILLER             PIC X VALUE SPACE.
           05 D-IRRF             PIC ZZ.ZZ9,99.
           05 FILLER             PIC X VALUE SPACE.
           05 D-LIQUIDO          PIC ZZZ.ZZ9,99.
       01  TOT.
           05 FILLER             PIC X(14) VALUE "FUNCIONARIOS: ".
           05 T-QTD              PIC ZZ9.
           05 FILLER             PIC X(10) VALUE "  BRUTO: ".
           05 T-BRUTO            PIC ZZ.ZZZ.ZZ9,99.
           05 FILLER             PIC X(11) VALUE "  LIQUIDO: ".
           05 T-LIQ              PIC ZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           OPEN INPUT ARQ-FUNC OUTPUT ARQ-REL
           WRITE LINHA-REL FROM CAB-1
           WRITE LINHA-REL FROM CAB-2
           PERFORM UNTIL FIM-ARQUIVO
               READ ARQ-FUNC
                   AT END SET FIM-ARQUIVO TO TRUE
                   NOT AT END PERFORM 1000-PROCESSA
               END-READ
           END-PERFORM
           PERFORM 3000-TOTAIS
           CLOSE ARQ-FUNC ARQ-REL
           DISPLAY "FOLHA PROCESSADA. VEJA RELATORIO.TXT"
           STOP RUN.

       1000-PROCESSA.
           COMPUTE WS-VALOR-HORA ROUNDED = F-SALARIO / 220
           COMPUTE WS-VALOR-HE ROUNDED =
               WS-VALOR-HORA * 1,5 * F-HORAS-EXTRAS
           COMPUTE WS-BRUTO = F-SALARIO + WS-VALOR-HE
           PERFORM 2000-DESCONTOS
           COMPUTE WS-LIQUIDO = WS-BRUTO - WS-INSS - WS-IRRF
           ADD 1 TO WS-QTD-FUNC
           ADD WS-BRUTO TO WS-TOT-BRUTO
           ADD WS-INSS WS-IRRF TO WS-TOT-DESC
           ADD WS-LIQUIDO TO WS-TOT-LIQ
           MOVE F-MATRICULA TO D-MATRICULA
           MOVE F-NOME TO D-NOME
           MOVE WS-BRUTO TO D-BRUTO
           MOVE WS-INSS TO D-INSS
           MOVE WS-IRRF TO D-IRRF
           MOVE WS-LIQUIDO TO D-LIQUIDO
           WRITE LINHA-REL FROM DET.

       2000-DESCONTOS.
      *> TABELAS SIMPLIFICADAS (DIDATICAS, NAO USAR EM PRODUCAO!)
           EVALUATE TRUE
               WHEN WS-BRUTO <= 1500,00
                   COMPUTE WS-INSS ROUNDED = WS-BRUTO * 0,075
               WHEN WS-BRUTO <= 4000,00
                   COMPUTE WS-INSS ROUNDED = WS-BRUTO * 0,12
               WHEN OTHER
                   COMPUTE WS-INSS ROUNDED = WS-BRUTO * 0,14
           END-EVALUATE
           EVALUATE TRUE
               WHEN WS-BRUTO <= 2500,00
                   MOVE ZERO TO WS-IRRF
               WHEN WS-BRUTO <= 5000,00
                   COMPUTE WS-IRRF ROUNDED = (WS-BRUTO - WS-INSS) * 0,15
               WHEN OTHER
                   COMPUTE WS-IRRF ROUNDED = (WS-BRUTO - WS-INSS) * 0,275
           END-EVALUATE.

       3000-TOTAIS.
           MOVE WS-QTD-FUNC TO T-QTD
           MOVE WS-TOT-BRUTO TO T-BRUTO
           MOVE WS-TOT-LIQ TO T-LIQ
           WRITE LINHA-REL FROM SPACES
           WRITE LINHA-REL FROM TOT.
