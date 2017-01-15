;*========================================*
;*====         Trabalho Intel 2015/1  ====*
;*==== Levindo Gabriel Taschetto Neto ====*
;*====                00243685        ====*
;*========================================*

;||------------- Associacao de registradores de segmento com definicoes de segmento ---------------||
ASSUME CS:CODIGO,SS:PILHA,DS:DADOS

;||----------------------------------------------------- Constantes ------------------------------------------------------------||
LF EQU 	 0AH     ;        Codigo ASCII do 'line feed'
CR EQU 	 0DH     ;        Codigo ASCII do 'carriage Return' (Enter)
BKSPC EQU  08H      ;        Codigo ASCII do 'backspace'
MAXSTRING EQU  100      ; #Define MAXSTRING 100
;||--------------------------------------------------------------------------------------------------------------------------------||

;||------------------------------------------ Definicao do segmento de pilha ----------------------------------------||
PILHA SEGMENT STACK
DB 	 32 DUP ('STACK---')
PILHA ENDS
;||--------------------------------------------------------------------------------------------------------------------------------||

;||----------------------------------------- Definicao do segmento de dados ---------------------------------------||
DADOS SEGMENT
    ;--> Flags ---------------------------------------------------------------------------------------------------------------||
		AchouA   DB    0
		AchouE   DB    0
		FirstLeArq   DB    0
		flagCaracErro  DB    0     ; Flag para caso tenha achado caractere invalido no arquivo
		senaoFoi     DB 0
		
		;--> Identificacao ------------------------------------------------------------------------------------------------------
		Id_Aluno	   DB '   Levindo GT Neto -  00243685 - Para abrir o menu pressione [Enter]',0DH,0AH   
		TAM_Id      EQU $-Id_Aluno
		
    ;-->Identificacao ------------------------------------------------------------------------------------------------------
        Linha_Br    	    DB '              ',CR,LF
	    TAM_LBran			EQU $-Linha_Br
    
		Pontos	    DB '+  .  +  .  +  .  +  .  +  .  +  .  +  .  +',CR,LF
	    TAM_Pontos			EQU $-Pontos
	    
		;--> Menu --------------------------------------------------------------------------------------------------------------
        EnterFunc	    DB 'Insira a funcao que voce quer: '  
	    TAM_EF   EQU $-EnterFunc
		
		Linha_Menu	    DB '  >|      <<< Menu >>>    |<',CR,LF
	    TAM_Menu			EQU $-Linha_Menu
    
		Total_Est	    DB '  >|   [E] - Total por estante    |<   ',CR,LF
        TAM_TtEst			EQU $-Total_Est
    
		Tot_Almox	    DB '  >|   [C] - Total no Almoxarifado    |<   ',CR,LF
        TAM_TtAlm			EQU $-Tot_Almox
    
		Ler_Arqui	    DB '  >|   [A] - Ler arquivo      |<   ',CR,LF
		TAM_LeArq			EQU $-Ler_Arqui
    
		Finalizar	    DB '  >|   [F] - Finalizar programa       |<   ',CR,LF
		TAM_Final			EQU $-Finalizar
    
		Exib_Menu  	    DB '  >|   [M] - Exibir novamente o menu      |<   ',CR,LF
		TAM_EMenu			EQU $-Exib_Menu
		
    ;--> Letras 'ecoadas' na tela quando digitadas		
		a    	    DB 'a'   ; 	Para printagem da letra na hora que ela e` digitada no Menu
		TAM_a			    EQU $-a
		
		e    	    DB 'e'   ; 	Para printagem da letra na hora que ela e` digitada no Menu
		TAM_e			    EQU $-e
		
		f    	    DB 'f'   ; 	Para printagem da letra na hora que ela e` digitada no Menu
		TAM_f			    EQU $-f
		
		ce   	    DB 'c'   ; 	Para printagem da letra na hora que ela e` digitada no Menu
		TAM_ce		    EQU $-ce
		
		m    	    DB 'm'   ; 	Para printagem da letra na hora que ela e` digitada no Menu
		TAM_m			    EQU $-m
		
		;--> Funcao A -----------------------------------------------------------------------------------------------
		ErroArq	   DB 		 	'|> Ainda nao foi lido nenhum arquivo, opcoes [E] e [C] desabilitadas  <|',CR,LF 
    TAM_EA   EQU $-ErroArq
		
		ErroLinA	   DB 		 	'['   
	    TAM_ELA     EQU $-ErroLinA
		ErroLin	    DB 		 	'>>> Erro encontrado no arquivo na linha '   ; A mensagem sera' junto com o contador de linhas(linha->erro)  
	    TAM_EL   EQU $-ErroLin
		ErroLinF	   DB 		 	']!'                 ; A mensagem sera' junto com o contador de linhas(linha->erro)  
	    TAM_ELF     EQU $-ErroLinF
		
		Aviso    DB '(Nao coloque o ''.txt'')' , CR, LF
		TAM_Avi     EQU $-Aviso
		
		PegaNomeArq  DB CR,LF,'Nome do arquivo: ','$'          ; Quebra linha automaticamente
		erroArquivo   DB 'Arquivo invalido ou nao encontrado, ponha o nome de novo', '$'
	    temEstant     DB 	'>|    O almoxarifado possui ' 
   	    TAM_tEst     EQU $-temEstant
		
		wordEsts  DB 			' estante(s)       ',CR,LF
	    TAM_wEst    EQU $-wordEsts 
		
		;concatens     DB   '.txt',0
		
		;--> Funcao C -----------------------------------------------------------------------------------------------
		InsConj     DB 		CR,LF,'Ponha o conjunto desejado: ',CR,LF; Só aceita valores de 0-65435(validos: 0-999)
	    TAM_Icon     EQU $-InsConj 
		
		;--> Funcao E -----------------------------------------------------------------------------------------------
		EnterEst    DB 		'Insira a estante desejada: ',CR,LF
		TAM_EEst     EQU $-EnterEst
		
	    TAM_EEst    EQU $-EnterEst 
		NaoTemEst	    DB 'Erro!Estante nao exite no AreaAlmox ','$'
		;--> Finalizacao ---------------------------------------------------------------------------------------------
		MSG_ASTER    DB '    >|    ***************************************   |<  ',CR,LF
		TAM_ASTER EQU $-MSG_ASTER
		
		MSG_Final  DB '    >|    ** Programa encerrado com sucesso!!! **   |<  ',CR,LF
		TAM_Finals	  	 EQU $-MSG_Final
		
		MSG_ASTER2   DB '    >|    ***************************************   |<  ',CR,LF
		TAM_ASTER2	 EQU $-MSG_ASTER2
		
		PROVISORIA   DB '      >|    Funcao ainda nao implementada  |<  ',CR,LF
		TAM_PROVI		EQU $-PROVISORIA
		
		;--> Teste ---------------------------------------------------------------------------------------------------
     MensTeste     DB '    Feitos',CR,LF,'$'
		
		;--> Ponteiros e áreas da memória
		AreaEst		    DB  1048 DUP(0)
		AreaAlmox 	    DB  1048 DUP(0)
		
		;--> Variaveis -------------------------------------------------------------------------------------------------
    ContLin    DW   0        ; Contador de linhas do arquivo(Usado para apontar o erro no arquivo)
		Buffer      DB   128  DUP (?)		
		nomeArq     DB   64    DUP (?)
		ascii_conjunto DB  128   DUP(?)
		Invalido    DB 	 0        ; Caractere invalido encontrado no arquivo
    FileToSave	 DB	 10 DUP (?)
		
		;--> Flags ED -------------------------------------------------------------------------------------------------
    flagEA      DB  1   DUP(0)
		flagEst	     DB	   1   DUP(0)
		flagAlmoxarifs DB	   0 
		flagCaracINV  DB	   1   DUP(0)
		flagFirst	     DB	   1   DUP(0)
   
	   ;--> Variaveis globais
		Handler   DW ?         ; Handler dos arquivos
		conjun     DW 4 DUP(0), '$' 
		numDest	    DW	   0
		WrongLine  DW 0

		ascii2bina	    DB	   0          ; Auxiliar de conversao de ascii para binario
		totalNoConjun DW 0 
		auxDezseis DW 128 DUP(?)
		tot_pecas		DB '  pecas achadas neste conjunto!',CR, LF,'$'
		ascii2estan	    DB	   0  
				    DB	   0  
		Estnt    DB  4 DUP(0),'$'

DADOS ENDS

; definicao do segmento de codigo
CODIGO		 SEGMENT           ;       Start do codigo 

START:		
			 MOV    AX, DADOS			      ;    Inicializa segmento de dados
			 MOV    DS, AX
			 MOV    ES, AX 

		;--> Inicializando as flags
			 MOV    AchouA, 0
		 MOV	AchouE, 0
     MOV    FirstLeArq, 0
		 MOV    senaoFoi, 1
			 
			CALL LIMPA_TELA
			CALL POS_CURSOR
		
		PrintId_Aluno:		
		 ;--> Indo para o menu apos o usuario pressionar enter
		 ;--> Arrumando atributos gráficos
		 	MOV    	DH,24	;linha		
			MOV    	DL,79	;coluna     
			MOV    CH,0	  
			MOV    	CL,0		
			MOV    	BH,82H   
			MOV    	AL,0		
			MOV    	AH,6  
			INT   	10H  
	 ;--> Impressão da identificação
			 LEA    DX, Linha_Br
			 MOV    CX, TAM_LBran
			 CALL   FRASE
			 LEA    DX, Pontos
			 MOV    CX, TAM_Pontos
			 CALL   FRASE
			 CALL   FRASE
			 CALL   FRASE
		 CALL   FRASE
	    	 CALL   FRASE
			 CALL   FRASE
			 LEA    DX, Id_Aluno
			 MOV    CX, TAM_Id
			 CALL   FRASE
			 LEA    DX, Pontos
			 MOV    CX, TAM_Pontos
			 CALL   FRASE
			 CALL   FRASE
			 CALL   FRASE
		 CALL   FRASE
	    	 CALL   FRASE
			 ;------------------------------------------------------------------------------------------------------------------
			
			
			CALL EspTecla
volt_menu:  CALL LIMPA_TELA
			CALL POS_CURSOR
;||----------------------------------------------------------------------------------------------------------------------------------||
;||---------------------------------------------------------- Menu ---------------------------------------------------------||
	  ;--> Arrumando atributos gráficos
		 	 MOV    	DH,24	;linha		
			 MOV    	DL,79	;coluna     
			 MOV 	CH,0	  
			 MOV    	CL,0		
			 MOV    	BH,75H   
			 MOV    	AL,0		
			 MOV    	AH,6  
			 INT   	   10H 

			 LEA    DX, Linha_Br
			 MOV    CX, TAM_LBran
			 CALL   FRASE
			
			 LEA  	DX, Linha_Menu
			 MOV 	CX, TAM_Menu
			 CALL 	FRASE
		
			 LEA 	DX, Total_Est
			 MOV 	CX, TAM_TtEst
			 CALL 	FRASE
			
			 LEA	DX, Tot_Almox
			 MOV    CX, TAM_TtAlm
			 CALL   FRASE
			
			 LEA 	DX, Ler_Arqui
			 MOV 	CX, TAM_LeArq
			 CALL 	FRASE
			
			 LEA 	DX, Finalizar
			 MOV 	CX, TAM_Final
			 CALL 	FRASE
			
			 LEA 	DX, Exib_Menu
			 MOV 	CX, TAM_EMenu
			 CALL 	FRASE

options:	
Entre_Opcao:
FUNCAO_OPT PROC NEAR			
		    LEA 	DX, EnterFunc
		    MOV 	CX, TAM_EF
			CALL 	FRASE
			 
			CALL SWITCH_OP
			; Comparacoes no DL
			CMP DL, 'c'
			JZ tot_pec
			
			CMP DL, 'f'
			JZ 	final_prog
			
			CMP DL, 'm'
			JZ  volt_menu
			
			CMP DL, 'a'
			JZ vai_ler
			JNZ verify_outro
vai_ler: 
			CALL LE_ARQ
verify_outro:			
			CMP dl, 'e'
			JZ 	tot_pec_est
FUNCAO_OPT ENDP	
	
final_prog:	
				CALL FUNCAO_F       ; Imprime mensagem piscante de finalizacao
				CALL EXIT_INTEL          ; Volta para o Sistema Operacional
				
tot_pec:
				CMP flagEA, 0            ;Flag para saber se arquivo foi aberto ou não
				JZ erro_arq

				LEA 	DX, InsConj        ; Ponha o conjunto desejado:  
		    MOV 	CX, TAM_Icon
		    CALL 	FRASE
		
				CALL LE_CONJ		         ; Chama funcao que lê o conjunto
				CALL ASCII2BIN_CONJ		
				CALL CalcOptC	
				CALL LIMPA_TELA
				CALL POS_CURSOR
				CMP senaoFoi, 0
				JNZ  options			
erro_arq:	
				CALL POS_CURSOR
				CALL LIMPA_TELA
				
				LEA 	DX, ErroArq
		    MOV 	CX, TAM_EA
		    CALL 	FRASE
				
				JMP options							
tot_pec_est:
				CMP flagEA, 0 		    ;Flag para saber se arquivo foi aberto ou não
				JZ erro_arq		
				
		    LEA 	DX, InsConj
		    MOV 	CX, TAM_Icon
		    CALL 	FRASE	    ; Ponha o conjunto desejado: 
				
				CALL LE_CONJ
				CALL ASCII2BIN_CONJ
denovo_estante:	
				CALL POS_CURSOR		
				;CALL LIMPA_TELA
				
		    LEA 	DX, EnterEst     ; Insira a estante desejada: 
		    MOV 	CX, TAM_EEst
	  	    CALL 	FRASE	
				
				CALL PegaEst		
				CALL ASCII2BIN_EST
				CALL CalcOptE
				CALL LIMPA_TELA
				CALL POS_CURSOR
jmp_opt:    
	    JMP Options    ; Branch para ir pro Options
OutRead:	
	    CMP senaoFoi, 0
				JNZ jmp_opt
ler_arquivo:	             ; Ler arquivo trocado pelo que esta' no moodle, o que eu usava antes dava erro na ED
LE_ARQ PROC NEAR	         ; Agora e' exatamente o do moodle com algumas adaptacoes para a ED
			CALL LIMPA_TELA
			CALL Pos_Cursor
		 ;--> Arrumando atributos gráficos
		 	 MOV    	DH,24	;linha		
			 MOV    	DL,79	;coluna     
			 MOV 	CH,0	  
			 MOV    	CL,0		
			 MOV    	BH,75H   
			 MOV    	AL,0		
			 MOV    	AH,6  
			 INT   	   10H 
			 ; Insira o nome sem '.txt'
			 LEA    DX, Aviso
			 MOV    CX, TAM_Avi
			 CALL   FRASE
			 
	de_novo: 
								LEA DX, PegaNomeArq   ; Endereco da mensagem em DX
								MOV    AH, 9      ; Funcao exibir mensagem no AH
								INT   21H        ; Chamada do DOS				
								LEA  DI, nomeArq		
								LEA  SI, nomeArq
	entrada: 
								MOV    AH, 1
								INT   21H	      ; Le um caracter com eco
								CMP    AL, CR       ; compara com Enter
								JZ    continua
								
								CMP    AL, BKSPC			   ; Backspace
								JZ	    bacs
								JNZ segBuf
	bacs:			
								MOV   BL, 17
								CALL BACKSPACE	
								CMP senaoFoi, 0
								JNZ   entrada
	segBuf:   
	        MOV    [DI], AL        ; Coloca no Buffer
								INC DI
								JMP    entrada			   ; Acabou o backspace
	continua:	
								MOV AL, '.'
								MOV [DI], AL
								INC DI
								MOV AL, 't'
								MOV [DI], AL
								INC DI
								MOV AL, 'x'
								MOV [DI], AL
								INC DI
								MOV AL, 't'
								MOV [DI], AL
								INC DI
								MOV BYTE PTR [DI], 0
								
	        CMP   DI, SI
								JZ   OutRead
								MOV   BYTE PTR [DI], 0   ; Forma string ASCIIZ com o nome do arquivo
								
								MOV   DL, LF        
								MOV   AH, 2
								INT    21H		
								; fread() 
								MOV   AH, 3DH
								MOV   AL, 0
								LEA DX, nomeArq
								INT  21H
								JNC abriu_ok
								LEA DX, erroArquivo       ; Endereco da mensagem em DX
								MOV    AH, 9       ; Funcao exibir mensagem no AH
								INT  21h       ; Interrupção
								JMP    de_novo
								
	abriu_ok: 	
								MOV Handler, AX    ; Formando o Handler do arquivo
								MOV WrongLine, 1
								MOV numDest, 0
								MOV flagFirst, 0
								MOV flagAlmoxarifs, 0
					
	loopy:   	
								MOV AH, 3FH      ; Lê um caracter do arquivo
								MOV BX, Handler
								MOV CX, 1
								LEA  DX,Buffer
								INT   21H
								CMP AX, CX
								JNZ  fim
									
								MOV flagEst, 0
								CALL TEST_CARAC_ARQ
								CMP flagCaracINV, 0
								JNZ  caracter_invalido
								CMP flagEst, 0
								JNZ  FEE         ; Se achou FE (estantes++;)
								CMP flagAlmoxarifs, 0  
								JNZ  FAA        ; Se achou FA(Fim do almoxarifado)
								CALL START_STRUCT      ;Chama função que mexe na ED
								CMP senaoFoi, 0
								JNZ loopy

	FAA:
								INC    flagAlmoxarifs      ; acabouAlmoxarifado++;
								CALL START_STRUCT
								CMP  senaoFoi, 0
								JNZ   fim
								
	FEE:		 
								INC numDest	
								
								CALL START_STRUCT		
								CMP senaoFoi, 0
								JNZ  loopy
					
	caracter_invalido:	
								; Nao coloquei atributos graficos porque prejudicaria alguns JMPs
								; else caracteres_validos
								LEA 	DX, ErroLin	
								MOV 	CX, TAM_EL
								CALL 	FRASE

								LEA 	DX, ErroLinA	
								MOV 	CX, TAM_ELA
								CALL 	FRASE
								
								MOV  AX, WrongLine     ; Carrega no AX a linha onde esta' o  caractere invalido
								CALL HEXA2ASCIIi
								CALL WriteWord

								LEA 	DX, ErroLinF	
								MOV 	CX, TAM_ELF
								CALL 	FRASE
								
								CALL EspTecla     ; Funcao que espera uma tecla aleatoria do teclado
					
							   MOV flagCaracINV, 0
							   CALL LIMPA_TELA
							   CALL POS_CURSOR
							   CMP senaoFoi, 0
							   CALL FUNCAO_OPT
			
								
	fim:               ; Finalizando leitura do arquivo 	
								MOV AH, 3EH	      	  ; Fecha o arq
								MOV BX, Handler
								INT   21H			
								INC flagEA
								CALL POS_CURSOR
								CALL LIMPA_TELA
																			 
								LEA 	DX, temEstant   ; O almoxarifado possui 
								MOV 	CX, TAM_tEst
								CALL 	FRASE
								MOV AX, numDest      ; Passando parametros pra funcao que converte HEXA em ASCII
								CALL HEXA2ASCIIi
								CALL WriteWord
								LEA 	DX, wordEsts    ; X estante(s) 
								MOV 	CX, TAM_wEst
								CALL 	FRASE
								CALL EspTecla
								CALL LIMPA_TELA			
								CALL POS_CURSOR				
								CMP senaoFoi, 0
								JNZ options_fun
	options_fun:
								CALL FUNCAO_OPT
LE_ARQ ENDP
;||----------------------------------------------------------------------------------------------------------------------------||

;||---------------------------- Leitura da estante com backs e limitador -----------------------------------------||		
; Me baseei no programa Soma BCD com adaptacoes para fazer essa leitura do teclado
PegaEst PROC NEAR            ; Válidos: 0-65535
			CALL LIMPA_TELA
			CALL Pos_Cursor
		 ;--> Arrumando atributos gráficos
		 	 MOV    	DH,24	;linha		
			 MOV    	DL,79	;coluna     
			 MOV 	CH,0	  
			 MOV    	CL,0		
			 MOV    	BH,75H   
			 MOV    	AL,0		
			 MOV    	AH,6 
 
			 INT   	   10H 
			 LEA    DI, Estnt            ; Faz o DI apontar para as estantes
 			 MOV  CX, 3          ; Lê 5 caracteres
			
			 LEA 	DX, EnterEst        ; Insira a estante desejada: 
			 MOV 	CX, TAM_EEst
			 CALL	FRASE
			
			
loop_Ler:
			; Cuidando limite de tal
			CMP  DH, 5
			JA AindaNao
			
			MOV  AH, 1          ; Parametros pra ler do teclado
			INT    21H             ; Lê 1 caractere do teclado
			CMP  AL, 48
			JB  NaoEHnum         ; Não é dígito 0 a 9
			CMP AL,  57
			JA NaoEHnum          ; Não é dígito 0 a 9
			MOV [DI], AL 				        ; Coloca valor ascii no Buffer
			ADD DI, 2
			LOOP loop_Ler
	
AindaNao:	
			MOV AH, 1           ; Preparando chamada da interrupcao  
			INT   21H 
			CMP AL, BKSPC
			JZ backsEst
			CMP al, CR
			JZ   AcabouLerEst
			
			MOV AH, 2					       ; Apenas cuidando quando o caracter digitado n for numero
			MOV DL, BKSPC        ; Recua cursor para posicao do caractere invalido
			INT   21H
			MOV AH, 2
			MOV DL, ' '             ; Escreve espaco sobre caractere invalido, dando aparencia de bloquear tecla #Enjambre
			INT   21H
			MOV AH, 2
			MOV DL, BKSPC           ; Recua cursor para posicao do espaco
			INT   21H
			CMP senaoFoi, 0
			JNZ AindaNao

NaoEHnum:
     CMP AL, BKSPC          ; Verifica se foi um backspace
     JZ backsEst	
		 CMP AL, CR            ; Vê se tem Enter no AL
		 JZ AcabouLerEst
		 
		 MOV ah,2					         ; Apenas cuidando quando o caracter digitado n for numero
     MOV DL, BKSPC          ; Recua cursor para posicao do caractere invalido
     INT   21H
     MOV AH, 2
     MOV DL, ' '           ; Escreve espaco sobre caractere invalido
     INT   21H
     MOV AH, 2
     MOV DL, BKSPC         ; Recua cursor para posicao do espaco
     INT   21H
		 CMP senaoFoi, 0
		 JNZ loop_Ler
	
backsEst:	
		MOV  BL, 28
		CALL BACKSPACE
		MOV  DL, ' '            ; Coloca espaco na posicao digitada por ultimo 
		MOV  [DI+1], DL				       
		ADD  CX, 2             ; CX = CX + 2
		CMP  senaoFoi, 0
		JNZ   loop_Ler
		
AcabouLerEst:
	    MOV  DL, LF          ; Escreve LF na tela
	    MOV  BYTE PTR [DI], 0         ; Forma string com os numeros digitados para a estante
		MOV  AH, 2
		INT    21H			
			
	RET

PegaEst ENDP
;||----------------------------------------------------------------------------------------------------------------------------||

;||---------------------------- Leitura do conjunto com backs e limitador -----------------------------------------||				
; Peguei como base o programa de Soma BCD!
; A comparacao pra ver se o conjunto eh valido nao foi feito com conjun porque estava dando erro nos registradores
LE_CONJ PROC NEAR			         ; Aceitavel: 0 - 999
			

			;CALL Pos_Cursor
		 ;--> Arrumando atributos gráficos
		 	 MOV    	DH,24	;linha		
			 MOV    	DL,79	;coluna     
			 MOV 	CH,0	  
			 MOV    	CL,0		
			 MOV    	BH,75H   
			 MOV    	AL,0		
			 MOV    	AH,6	
			
;CALL LIMPA_TELA
			LEA    DI, conjun
			MOV  CX,3            ; Aceita no máx 3 caracteres
			CMP  senaoFoi, 0
			JNZ ler_outro_carac

ler_outro_carac:
			MOV AH, 1            ; Passando parametros pra interrupcao            
			INT   21H           ; Le do teclado
			CMP AL, 48
			JB    notsNumero           ; Não é dígito 0 a 9
			CMP al, 57
			JA notsNumero          ; Não é dígito 0 a 9
			MOV [DI], AL 				        ; Coloca valor ascii no Buffer
			ADD DI, 2             ; DI = DI+2;
			LOOP ler_outro_carac
	
ainda_nao:	
			MOV AH, 1
			INT   21H 
			CMP AL, BKSPC
			JZ recuou
			CMP AL, CR
			JZ fimNumero
			
			MOV  AH, 2					      ;Apenas cuidando quando o caracter digitado n for numero
			MOV  DL, BKSPC        ; Recua cursor para posicao do caractere invalido
			INT    21H
			MOV  AH, 2
			MOV  DL, ' '           ; Escreve espaco sobre caractere invalido
			INT    21H
			MOV  AH, 2
			MOV  DL, BKSPC          ; Recua cursor para posicao do espaco
			INT    21H
			CMP senaoFoi, 0
			JNZ  ainda_nao
			
notsNumero:
    CMP AL,BKSPC        ; Verifica se foi um backspace
    JZ recuou	
		    CMP AL, CR
		    JZ fimNumero
		 
		    MOV AH, 2					        ;Apenas cuidando quando o caracter digitado n for numero
    MOV DL, BKSPC          ; Recua cursor para posicao do caractere invalido
    INT   21H
    MOV AH, 2
    MOV DL, ' '           ; Escreve espaco sobre caractere invalido
    INT   21H
    MOV AH, 2
    MOV DL, BKSPC          ; Recua cursor para posicao do espaco
    INT   21H
		    CMP senaoFoi, 0
		    JNZ ler_outro_carac
	
recuou:	
		   MOV BL, 29
		   CALL BACKSPACE
		   MOV DL, ' '
		   MOV [DI+1], DL				
		   ADD CX, 2    
		   CMP senaoFoi, 0
		   JNZ ler_outro_carac
		
fimNumero: 
		   MOV BYTE PTR [DI], 0    ; Forma string ASCIIZ com o nome do arquivo
		   MOV DL, LF     ; LF: Final de Linha vai para o DL
		   MOV AH, 2
		   INT    21h

RET		

LE_CONJ ENDP
;||---------------------------------------------------------------------------------------------------------------------------||	

;||------------------------------------------ Calculando Opcao C -----------------------------------------------------||
CalcOptC PROC NEAR
		
			MOV totalNoConjun, 0   		
			; Ajustes de ponteiros (SI e DI)
			LEA DI, AreaEst         
			LEA SI, AreaAlmox 
			MOV AL, 42
			MOV BL, ascii2bina
			MOV DL, 0
			
			MOV tot_pecas + 2, 0
			MOV tot_pecas, 0
			MOV tot_pecas + 1, 0
				
comecco:			
			CMP AL, [DI] 
			JZ estante_end
			CMP [di], bl					;Compara o conjunto informado com o conjunto na estrutura de dados
			JZ foundConj
			CMP senaoFoi, 0
			JNZ notFound
					
foundConj:			
			MOV CX, [DI+2]
			ADD totalNoConjun, CX	

estante_end:			
			INC   SI
			INC   DL
			MOV CL, BYTE PTR numDest   ; Tamanho de numDest != Registrador CL
			
			CMP CL, DL					   ;Testa para ver se chegou ao fim do AreaAlmox 
			JZ endCC
			MOV DI, [SI]		       ; Faz o DI apontar para o endereco que tem no conteudo do SI
			CMP senaoFoi, 0
			JNZ  comecco
			
notFound:		
			ADD DI, 2
			ADD DI, 2
			CMP senaoFoi, 0
			JNZ comecco
			
endCC:		
			STD         ; Ajuste de Little Endian do Intel x86        
			MOV  AX,  totalNoConjun
			;ADD tot_pecas, 2
			LEA   DI, tot_pecas+3
			CALL edita
			LEA  dx, tot_pecas       ; Carrega o total de pecas no DX
			MOV AH, 9
			INT 21H
			CALL EspTecla
			
	RET
CalcOptC ENDP
;||---------------------------------------------------------------------------------------------------------------------------||

;||------------------------------------------ Calculando Opcao E -----------------------------------------------------||	

CalcOptE PROC NEAR
		; Estava dando bug na logica que eu tava usando
		MOV  AX, word ptr senaoFoi     ; Carrega no AX a linha onde esta' o  caractere invalido
		CALL HEXA2ASCIIi
		CALL WriteWord
RET
CalcOptE ENDP
;||-----------------------------------------------------------------------------------------------------------------------------||		
		
;||--------------------------------------------------------------------------------------------------------------------------------||
; subrotina para editar (converter de binário para ASCII 
; com 4 digitos e colocar na mensagem) um valor de 16 bits
; de um contador

EDITA    PROC NEAR
     MOV BX, 10            ; Divisor constante
next:
     MOV   DX, 0           ; Limpa msbits do dividendo
     DIV BX           ; Divisor de 16 bits -> dividendo de 32 bits em DX:AX
     XCHG DX, AX         ; Permuta resto (DX) e quociente (AX)
     ADD  AL, 48           ; Transforma resto (valor de 0 a 9) em ASCII
     STOSB          ; Guarda caractere na mensaqgem e DECREMENTA DI
     XCHG DX, AX        ; Devolve quociente para o AX
     TEST AX,0FFFFH          ; Testa se quociente é zero
     JNZ   next          ; Se não for, edita proximo digito
    RET     
EDITA    ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||

;||---------------------------------------------------- Opcao F -------------------------------------------------------------||
FUNCAO_F    PROC    NEAR
			 CALL   LIMPA_TELA
			 CALL   CursorGeralFim
	   ;--> Arrumando atributos gráficos
		 	 MOV    DH,24	;linha		
		 	 MOV   	DL,79	;coluna     
			 MOV   	CH,0	  
			 MOV   	CL,0		
			 MOV   	BH,82H   
		 	 MOV   	AL,0		
			 MOV   	AH,6  
			 INT   	10H 
			 LEA 	DX, MSG_ASTER
			 MOV 	CX, TAM_ASTER
			 CALL 	FRASE
			 LEA 	DX, MSG_Final
			 MOV 	CX, TAM_Finals
			 CALL	FRASE
			 LEA 	DX, MSG_ASTER2
			 MOV 	CX, TAM_ASTER2
			 CALL 		FRASE
		RET
FUNCAO_F	ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||
	
BACKSPACE PROC NEAR
							
				MOV AH, 2
				MOV DL, ' '    ; escreve espaco sobre caractere anterior
				INT   21h
				
				MOV BH, 0
				MOV AH, 03H
				INT  10H

				CMP DL, BL			
				JZ naoFaca
				
				MOV AH, 2
				MOV DL,BKSPC      ; recua cursor para posicao do espaco	
				INT    21H
				DEC  DI				
				
naoFaca:		
		    RET			
BACKSPACE endp
;||--------------------------------------------------------------------------------------------------------------------------------||		
WRITE   PROC NEAR
			mov    ah,9      
			int    21h   
			ret
WRITE   endp
;||--------------------------------------------------------------------------------------------------------------------------------||
	
EspTecla PROC NEAR
			MOV    AH, 0   ; funcao esperar tecla no AH
			INT    16H    ; chamada do DOS
	RET
EspTecla ENDP
		
		LIMPA_TELA PROC NEAR
			 MOV 	CX, 0          ;     Coluna esquerda e Linha superior
			 MOV 	DL, 79        ;       Coluna direita
		 MOV 	DH, 24        ;       Linha inferior
		 	 MOV 	AL, 0           ;      Atributo de cor
			 MOV 	AH, 6
			 MOV 	BH, 7          ;           Página
			 INT 	10H        ;       Chamada da Interrupção
		LIMPA_TELA ENDP
		
;||---------------------------------- Posicionar o cursor ----------------------------------------------|| 
POS_CURSOR   PROC   NEAR          ;Posiciona o cursor no canto superior esquerdo
			 MOV	BH, 0    
			 MOV 	DH, 0
			 MOV 	DL, 0    ; Coluna Zero
			 MOV 	AH, 2 
			 INT 	10H
	RET
POS_CURSOR 	 ENDP
		
;--> Antes fazia comparacao e testes para cada letra, daí agora ao inves disso
;--> Faz a comparacao e ve se pode imprimir ou nao
SWITCH_OP PROC NEAR
starts:				
			MOV AH, 0   ; funcao esperar tecla no AH
			INT   16H 
			
			; O 'ecuo' da tecla é feito por comparacoes, fazendo aceitar somenter enter ou backspace
			; Na versao anterior a mesma coisa da funcao era feita para cada tecla, sem receber parametro nenhum
			
			CMP AL, 'm'
			JZ     podeWrite
			
			CMP AL, 'e'
			JZ     podeWrite
		
			CMP    AL, 'c'
			JZ    podeWrite
			
			CMP   AL, 'f'
			JZ   podeWrite	

			CMP    AL, 'a'
			JZ   podeWrite

			JMP starts
			
	; Se for uma das letras aceitaveis, a escreve na tela		
	podeWrite:		
			MOV DL, AL     ; Jeito de chamar a interrupcao movendo o que tem no AL para o DL     
		    MOV AH,  2      
			INT 21H      
    naoEnter:	
			MOV  AH, 0   ; funcao esperar tecla no AH
			INT   16H
			
			;So' aceita Enter ou Backspace (Antes era um EnterOrBackspacX, sendo X a letra)
			CMP AL,BKSPC
			JZ    apags_anterior
			CMP AL, CR     ; Compara com Enter o que tem no AL(lsbAH)
			JZ    SAI

			JMP naoEnter
			
	apags_anterior:						
			MOV AH,2
			MOV DL, BKSPC      ; recua cursor para posicao do espaco	
			INT   21H
			
			MOV DL, ' '
			MOV AH, 2     ;escreve valor lido na tela
			INT   21H
			
			MOV AH, 2
			MOV DL,BKSPC      ; recua cursor para posicao do espaco	
			INT   21H
			;else         // Se nao foi digitado nem enter ou backspace
			JMP starts
	
	sai:	
			RET
SWITCH_OP endp

;||------------------------------------------------------------ ED --------------------------------------------------------------||
; Erro que tinha aqui era uma comparacao que faltava com LF e o caractere 'Enter', tambem estava dando
; problema na le_arquivo, por causa dos meus 'enjambres', daí peguei exatamente igual a que tem no moodle
; para evitar problemas		
START_STRUCT PROC NEAR

		    CMP flagFirst, 0
		    JZ  PrimeiraVez   ; primeira vez que leu arq
		    JNZ inicioStruct		
		
	PrimeiraVez:
			LEA  SI,AreaAlmox     ; Fazendo o SI apontar para a lista de estantes no almoxarifado
			LEA  DI, AreaEst  ; Fazendo o DI apontar para a lista de conjuntos/quantidade na lista de estantes 
			MOV  [SI], DI     ; Levar o que tem nas estantes para a estante correspondente no almoxarifado
			MOV flagFirst, 1
	inicioStruct:

			MOV  DH, Buffer		  ; Valor do Buffer chega certo
				
			CMP flagEst, 0
			JNZ acabouEstante
			CMP flagAlmoxarifs, 0
			JNZ acabo_almox
			
				
			CMP DH, 'F'
			JZ  endSS
		

			CMP DH, LF
			JZ  Its_coma     ; acabouConjDaEstante endss
				
			CMP DH, ','
			JZ  Its_coma
				
			CMP DH, CR
			JZ endSS
				
			;EH NUMERO DAE, PEGA E GUARDA
			SUB DH, '0'			;Converte o bit de ascii para binario
			MOV AL, 10
			MUL ascii_conjunto	
			
			MOV auxDezseis, AX
			MOV AL, BYTE PTR auxDezseis			;Aqui que faz com que só aceite conjuntos e estantes menores que 256
			MOV ascii_conjunto, AL
			
			ADD ascii_conjunto, DH
	RET
		
	acabouEstante:
				CMP senaoFoi, 0
				JNZ colocaSimb		
	colocaSimb:	MOV DH, '*'       ; Simbolo usado para FE (Fim de Estante)
				MOV [DI], DH
				ADD DI, 4 ; tava 4
				ADD SI, 2
				MOV [SI], DI
				SUB DI, 2 ; FAT AJUSTE
			RET ; pra ler outra coisa do Buffer (pegando bit a bit)
		
	Its_coma: 
				MOV DL, ascii_conjunto
				MOV [DI], DL
				add di, 2   ; DI <- DI + 2
				MOV ascii_conjunto, 0
				
		RET
	acabo_almox:
				JNZ colocaSimbA		
    colocaSimbA:MOV DH, '*' 	
				MOV DH, '%'       ; Simbolo usado para FA (Fim de almoxarifado)
				MOV [SI], DH	
	
	endSS:				
		RET
	
START_STRUCT   ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||
;||------------------------ Funcao que converte hexadecimal para ascii -----------------------------------------||
		HEXA2ASCIIi PROC NEAR
		
		PUSH SI 
		MOV CX, 0			
jmp1:       
		OR AX, AX			; Se não for 0
		JNZ	jmp4
		OR CX, CX
		JNZ	jmp0
jmp4:
		MOV DX, 0			;A = A / 10
		MOV SI,10			;DL = A % 10 + '0'
		DIV SI
		ADD DL, '0'			;Converte valor em dl para ascii
		MOV SI, CX			
		MOV	[BX+SI], DL
		INC	CX				
		JMP	jmp1
jmp0:
		MOV SI, CX			
		MOV BYTE PTR[BX+SI], 0    ;cria string
		MOV SI, BX			
		ADD BX, CX			
		DEC BX
		SAR CX, 1			
jmp3:
		OR CX, CX			;Enquanto cx não for 0
		JZ	jmp2
		MOV AL, [SI]		
		MOV	AH, [BX]
		MOV	[SI], AH
		MOV	[BX], AL
		DEC	CX				
		INC	SI				
		DEC	BX			
		JMP	jmp3
jmp2:
    POP SI 
	
	RET		

HEXA2ASCIIi endp
;||--------------------------------------------------------------------------------------------------------------------------------||

;||---------------------------------------------- Switch no arquivo --------------------------------------------------------||	
TEST_CARAC_ARQ PROC NEAR
		
			CMP Buffer, 57
			JA caracInvalido   ; não é dígito 0 a 9
			CMP Buffer, 48
			JB caracInvalido  ; não é dígito 0 a 9

			JMP retc
						
			
caracInvalido:
    CMP Buffer, 'A'
			JZ fim_almox			
			CMP Buffer, ','
			JZ retc
			CMP Buffer, 'E'
			JZ finalEst
			CMP Buffer, CR
			JZ retc
			CMP Buffer, 'F'
			JZ retc
			CMP Buffer, LF
			JZ incrementLine
			;else           // Nao eh nenhum dos caracteres validos
			JMP carac_inv

finalEst:
			INC  flagEst
			JMP retc
			
incrementLine:
			INC  WrongLine
			JMP retc
		
retc:		
			RET
			
fim_almox:
			INC flagAlmoxarifs
			JMP retc
			
carac_inv:
			INC flagCaracINV
		
TEST_CARAC_ARQ endp
;||--------------------------------------------------------------------------------------------------------------------------------||	

;||----------------------- Converte ascii para binario, se conseguir vou usar nas estantes---------------------||; ,funcao quase igual a usada nos conjuntos
			
ASCII2BIN_EST PROC NEAR
			
			CALL SETA_REGIST
			LEA DI, Estnt
			MOV ascii2estan, 0
					
	repet:	
			MOV BL, [DI]
			
			SUB BL, 48						;converte bit de ascii para binario
			
			MOV AL, 10
			MUL ascii2estan
			
			MOV WORD PTR ascii2estan, AX
			ADD ascii2estan, BL
			INC DI
			
			MOV AL, [DI]
			CMP AL, 0
			JB  repet  ; não é dígito 0 a 9
			CMP al,9
			JA  repet 
						
		RET
			
ASCII2BIN_EST ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||	

;||--------------------------- Transforma ascii para binario, vou usar nos conjuntos------------------------------||
ASCII2BIN_CONJ PROC NEAR
			
			LEA DI, conjun
			MOV ascii2bina, 0	
					
	loops:	
			MOV BL, [DI]
			
			SUB BL, 48						;converte bit de ascii para binario
			
			MOV AL, 10
			MUL ascii2bina
			
			MOV WORD PTR ascii2bina, AX
			ADD ascii2bina, BL
			INC DI
			
			MOV AL, [DI]
			CMP AL, 0
			JB  loops   ; não é dígito 0 a 9
			CMP al,9
			JA  loops  

		RET
			
ASCII2BIN_CONJ ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||

;||----------------------------------- Escreve palavra DW convertida na tela -------------------------------------||	
WriteWord	PROC	NEAR
WS_2:
		MOV		DL, [BX]		; While (*S!='\0') {
		CMP		 DL, 0
		JNZ		WS_1
		RET
WS_1:
		MOV	AH,2		; 	Int21(2)
		INT		21H
		INC		BX			; 	++S
		JMP		WS_2		; }
WriteWord	ENDP

;||------------------------------------------------------ Printf -----------------------------------------------------------------||
FRASE		 PROC   NEAR
			 MOV 	BX, 1H
			 MOV 	AH, 40H
			 INT 	21H			       ;         Escreve mensagem
    RET
FRASE		ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||	

;||------------------- Funcao que zera os principais registradores utilizados no programa -----------------------||	
SETA_REGIST PROC    NEAR
			MOV 	AX, 0
			MOV 	BX, 0
			MOV 	CX, 0
			MOV 	DX, 0
			MOV 	DI,  0
			MOV 	SI, 0
		RET
SETA_REGIST ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||	
;||---------------------- Posicional Cursor Exclusivo para a finalizacao do programa ---------------------||
CursorGeralFim   PROC    NEAR
			 MOV	BH, 0    
			 MOV 	DH, 10     ; Linha
			 MOV 	DL, 0    ; Coluna  
			 MOV 	AH, 2
			 INT 	10H
	RET
CursorGeralFim   ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||	
;||----------------------------------- Nome_do_arquivo.txt automatico --------------------------------------------------||	
;concatenaTXT proc
;   LEA DI, NomeArq  
;    arq_txt:       ; acha o final da string ASCIIZ comando
;    INC DI
;    CMP BYTE PTR [DI], 0
;    JNE arq_txt
;   
;    MOV SI, offset concatens ; concatena as duas strings (.txt)
;    MOV CX, 5   ; 5 pois sao 5 bytes: '.','t','x','t' e 0.
;    REP MOVSB      ; Movimenta 5 bytes e encerra o loop
;   
;   RET
;concatenaTXT endp
;||--------------------------------------------------------------------------------------------------------------------------------||
;||----------------------------- Chamada para o Sistema Operacional --------------------------------------------||
EXIT_INTEL PROC	NEAR
			 
			 MOV 	BH, 30         ;     Altura de cima pra baixo  
     MOV 	DH, 30         ;      Posicao da esquerda pra direita
			 CALL   Pos_Cursor
		 MOV 	AH, 8H    
			 INT 	21H							
		 	 MOV 	AH, 40H
	     MOV 	BX, 1H
			 ; Retorno para o Sistema Operacional
			 CALL LIMPA_TELA     ; Fica nas configuracoes iniciais do DosBox daí
			 MOV AX, 4C00H
			 INT 21H
RET
EXIT_INTEL ENDP
;||--------------------------------------------------------------------------------------------------------------------------------||	
		
CODIGO		ENDS  ; Finalizacao do segmento de codigo
END START 
