;;;;;;;;;;;;;;;;;;;;;LINKS DE AYUDA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;cl65 "PIA LMP.s" --verbose --target nes -o pia.nes

;https://nerdy-nights.nes.science/#main_tutorial-3
;http://www.6502.org/tutorials/6502opcodes.html#BIT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;"Declaración de variables" (realmente no existen las variables, el compilador solo reemplaza)
control1 = $0000           			;Variable que guarda que botones de P1 estan siendo presionados
is_a_pressed = $0001       			;Variable que guarda si el botón A de P1 está siendo presionado ahora
old_is_a_pressed = $0002   			;Variable que guarda el A fue presionado en el frame inmediatamente anterior
is_b_pressed = $0003       			;Variable que guarda si el botón B de P1 está siendo presionado ahora
old_is_b_pressed = $0004   			;Variable que guarda el B fue presionado en el frame inmediatamente anterior
current_slide = $0005     		   ;Variable que guarda el indice de slide que nos encontramos ahora.
ptr_select_slide_Low = $0006 	   ;Variable que va a guardar el puntero (Low byte) de la diapositiva a mostrar
ptr_select_slide_High = $0007     ;Variable que va a guardar el puntero (High Byte) de la diapositiva a mostrar
tmp_ptr_selslide_Low = $0008	   ;Variable acomuladora (Low byte), contiene el puntero a la slide actual y aumenta para cargar el 1KB
tmp_ptr_selslide_High = $0009	   ;Variable acomuladora (High byte), contiene el puntero a la slide actual y aumenta para cargar el 1KB

;;;;;;;;;;;;;;;;;REFERENTE AL ARREGLO DE SLIDES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

arreglo_de_punteros_a_slides:     			  ;Hardcoded, lo sé.
  .byte $76, $81, $76, $85, $76, $89, $76, $8D, $76, $91, $76, $95       ;Arreglo de punteros a diapositivas

head_arr_ptr_slide = $0010        			  ;Puntero al primer elemento en la lista (despues de copiarse en RAM)
length_arr_ptr_slide = $0C        			  ;Tamaño del arreglo
total_slides = $06          				  ;Constante que guarda el numero total de diapositivas


;;;;;;;;;;;;;;;;HEADER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;HEADER DEL JUEGO (formato .nes)
;Le cuenta al emulador que:

.segment "HEADER"
  ; .byte "NES", $1A        ; Algunos emuladores te requieren esta linea, no sé porque 
  .byte $4E, $45, $53, $1A  ; Esto es un juego de NES 
  .byte 2                   ; 2 x 16KB PROGRAM CODE
  .byte 1                   ; 1x  8KB CHARACTER DATA (ver final)
  .byte $01, $00            ; No bank swapping, vertical mirroring

.segment "STARTUP"  ;Requisito para el compilador

.segment "CODE"     ;Aqui empieza el PRG-ROM

;;;;;;;;;;;;;;;;;SUBRUTINAS DE INICIALIZACIÓN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Esperando a la PPU (v-blank para el NMI)
vblankwait:
  bit $2002
  bpl vblankwait
  rts

reset:
  sei		   ; No IRQ
  cld		   ; No modo decimal
  ldx #$40
  stx $4017  ; deshabilitar IRQ
  ldx #$ff   ; Inicializar Stack Counter
  txs		   ; Stack = X = $FF
  inx		   ; X = 0


jsr vblankwait

inicializar_memoria:		;Haciendo todo ceros
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne inicializar_memoria   ;Ciclando

jsr vblankwait                ;La PPU todavía no está lista

;;;;;;;;;;;;;;;;;;;;;;;;CARGANDO PALETAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

apuntar_memoria_paletas:      ;Diciendole a la PPU en donde vamos a mandar la paleta
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006

ldx #$00
cargando_paletas:        ;Estamos en un ciclo que copia los bytes a su sección de memoria correspondiente
  lda palettes, x        ;Cargamos byte por byte
  sta $2007              ;En esta dirección de memoria deben de ir, que es dónde la PPU estará "escuchando"
  inx
  cpx #$20               ;Vemos si ya copiamos 20 bytes
  bne cargando_paletas   ;Ciclamos si no.

ldx #$00

;;;;;;;;;;;;;;;;;;;;;;;INICIALIZACIÓN DE ARREGLO DE PUNTEROS DE SLIDES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

copiar_a_RAM_arr_ptr_bg:		     ;Copiando el arreglo de ROM a RAM
  lda arreglo_de_punteros_a_slides, x    ;Byte por Byte
  sta head_arr_ptr_slide, x              ;Lo ponemos en la RAM
  inx
  cpx #length_arr_ptr_slide              ;Comparamos para ver si ya copiamos todos los bytes
  bne copiar_a_RAM_arr_ptr_bg            ;Y ciclamos si no hemos terminado


;;;;;;;;;;;;;;;;;;;;;;INICIALIZANDO Y ESCRIBIENDO EL PRIMER FONDO (PARA QUE APAREZCA PRIMERO);;;;;;;;;;;;;;;;;;;;;;;;;;;;

jsr select_slide_ptr                     
jsr Apuntando_write_fondo

;;;;;;;;;;;;;;;;NMI;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

nmi:			         ;El NMI se ejecuta al antes de que la PPU comienze a dibujar cada frame
  lda #$00 	    		   ;Copiamos los 256 bytes (64 sprites) del buffer a la OAM
  sta $2003
  lda #$02
  sta $4014
  jsr configurando_PPU     ;Activamos la PPU   
  jsr prep_leer_controles  ;Leeremos los controles en cada frame

;;;;;;;;;;;;;PARA LOS BOTONES CAMBIAMOS SLIDE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

leer_AB:	;Evita bugs cuando presionas AB al mismo tiempo (mitad de la pantalla de otro fondo)
  clc
  lda control1
  and #%11000000
  cmp #%11000000
  beq nmi

leer_boton_A:	     ;Control1 guarda el byte que representa los botones presionados en el momento
  clc
  lda control1
  and #%10000000        ;Queremos ver si esta siendo presionado A, entonces lo aislaremos
  cmp #%10000000        ;Y lo comparamos para ver si sí.
  jsr seeIF_a_was_pressed  ;Si resulta ser verdad, entonces vamos a actualizar is_a_pressed

leer_boton_B:	;Control1 guarda el byte que representa los botones presionados en el momento
  clc
  lda control1
  and #%01000000        ;Queremos ver si esta siendo presionado B, entonces lo aislaremos
  cmp #%01000000        ;Y lo comparamos para ver si sí.
  jsr seeIF_b_was_pressed  ;Si resulta ser verdad, entonces vamos a actualizar is_b_pressed
  jmp nmi

seeIF_a_was_pressed:
  beq set_is_a_pressed
  ldy is_a_pressed      ;Si no, guardamos is_a_pressed a old_is_a_pressed
  sty old_is_a_pressed
  ldy #$00              ;Y actualizamos is_a_pressed
  sty is_a_pressed      
  rts

set_is_a_pressed:       ;Actualizando  is_a_pressed
  ldy is_a_pressed      ;Guardamos el valor anterior a old_is_a_pressed
  sty old_is_a_pressed
  ldy #$01              ;Hacemos 1 a is_a_pressed
  sty is_a_pressed
  lda old_is_a_pressed  ;Nos preparamos para actualizar la variable contadora de diapositivas
  eor is_a_pressed      ;Ex-OR de old_is_a_pressed con is_a_pressed (esto es para saber si se dejó de presionar el botón en el frame anterior)
  bne subir_slide       ;Si NO está siendo presionado A del frame anterior al actual, entonces subimos el contador. 
  rts

subir_slide:              ;Subirmos current_slide y verificaremos que no estamos Out of Bounds de la presentación
  inc current_slide
  lda current_slide
  cmp #total_slides       ;Comparamos con el total de diapositivas, deberia salir un numero negativo.
  bcs cap_current_slide   ;Si nos salimos del arreglo, vamonos a regresarnos
  jsr select_slide_ptr
  jsr Apuntando_write_fondo
  rts

cap_current_slide:        ;Regresamos current_slide a 0, esto es para que la presentación se cicle.
  lda #$00
  sta current_slide  
  jsr select_slide_ptr
  jsr Apuntando_write_fondo
  rts

seeIF_b_was_pressed:
  beq set_is_b_pressed
  ldy is_b_pressed      ;Si no, guardamos is_b_pressed a old_is_b_pressed
  sty old_is_b_pressed
  ldy #$00              ;Y actualizamos is_b_pressed
  sty is_b_pressed
  rts

set_is_b_pressed:       ;Actualizando  is_b_pressed
  ldy is_b_pressed      ;Guardamos el valor anterior a old_is_b_pressed
  sty old_is_b_pressed
  ldy #$01              ;Hacemos 1 a is_b_pressed
  sty is_b_pressed
  lda old_is_b_pressed  ;Nos preparamos para actualizar la variable contadora de diapositivas
  eor is_b_pressed      ;Ex-OR de old_is_b_pressed con is_b_pressed (esto es para saber si se dejó de presionar el botón en el frame anterior)
  bne bajar_slide       ;Si NO está siendo presionado B del frame anterior al actual, entonces bajaremos el contador.
  rts

bajar_slide:                  ;Bajamos current_slide y verificaremos que no estamos Out of Bounds de la "presentación"
  dec current_slide
  lda current_slide
  cmp #total_slides           ;Comparamos con el total de diapositivas, debería hacer overflow y activar carry.
  bcs ovrflow_current_slide   ;Si nos salimos del arreglo, vamonos a regresarnos
  jsr select_slide_ptr
  jsr Apuntando_write_fondo
  rts

ovrflow_current_slide:        ;Regresamos current_slide al numero total de slides y le restamos uno, esto es para que la presentación de cicle.
  lda #total_slides           ;Le restamos uno para que nos quede un puntero valido
  sbc #$01
  clc
  sta current_slide  
  jsr select_slide_ptr
  jsr Apuntando_write_fondo
  rts


;;;;;;ZONA DE SUBRUTINAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;ESCRITURA DE FONDO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Apuntando_write_fondo:
  lda #%00000000	       ; Apagando la PPU
  sta $2001

  lda $2002             ; Avisandole a la PPU que estaremos mandando bytes a la dirección de memoria $2000
  lda #$20
  sta $2006             
  lda #$00
  sta $2006 
            
  lda #$00              ;Comenzamos el ciclo de abajo en ceros
  ldy #$00              
  ldx #$00

Cargando_slide_ciclo1:
Cargando_slide_ciclo2:

  lda (tmp_ptr_selslide_Low), y	; Cargamos un byte de la slide 
  sta $2007          			; Y lo mandamos a la PPU

  iny                             ;Siguiente byte
  cpy #$00                        ;Vemos si hemos copiado los primeros 256 (FF + 1) bytes (la primera linea)
  bne Cargando_slide_ciclo2       ;Si no, ciclamos

  inc tmp_ptr_selslide_High       ;En efecto, ya terminamos de copiar la primera linea
  
  inx                             ;Aumentamos el registro X para seguir a la siguiente linea
  cpx #$04                        ;Vemos si ya copiamos $04 lineas
  bne Cargando_slide_ciclo1       ;Si no, ciclamos hasta arriba

  lda #%00011010	                 ; Encendiendo la PPU de nuevo
  sta $2001

  rts

;;;;;;;;;;CONFIGURANDO PPU;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

configurando_PPU:

  lda #%10010000					; Habilitar NMI y activando la PPU
  sta $2000

  lda #%00011010					; Configurando a la PPU
  ;Azul,Verde,Rojo,HabSprites,HabBG,DhabSangriaSprite,DhabSangriaBG,BN

  sta $2001
  lda #$00
  sta $2005
  sta $2005
  rts

;;;;;;;;;;LECTURA DE CONTROLES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

prep_leer_controles:				;Diciendole al control que nos mande los bits de todos los botones
  lda #$01
  sta $4016
  lda #$00
  sta $4016


  ldx #$08					      ;Comenzamos en el registro X = 8
leyendo_controles: 				
  lda $4016						;Leyendo el byte que nos mandó el control, el que más importa es el primer bit (menor significancia)
  lsr A					      ;Extrayendo el bit de menor significancia
  rol control1                            ;Y mandandola a control1
  ; control = A, B, SLCT, START, UP, DOWN, LFT, RIGHT   ;Orden de los bits
  dex                                     ;Vemos si ya hemos recibido los 8 bits (1 byte)
  bne leyendo_controles                   ;Si no, ciclamos
  rts                                     ;Si sí, salimos

;;;;;;;;;;SLIDE SELECT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

select_slide_ptr:			       ;Obteniendo la dirección de memoria de la diapositiva a mostrar
  clc                                ;Limpiamos carry
  lda current_slide		       ;Cargamos current_slide
  rol A				       ;Multiplicamos x2 para obtener el indice en el arreglo (hay 2 bytes por entrada)
  tax				             ;Transferimos el acomulador a X
  clc			                   ;Limpiamos carry (si hay)
  lda head_arr_ptr_slide, x	       ;Cargamos (solo) el primer byte de la slide 
  sta ptr_select_slide_Low           ;Guardamos el low byte del puntero 
  inx					       ;Incrementamos x para apuntar al segundo byte
  lda head_arr_ptr_slide, x	       ;Cargamos (solo) el segundo byte de la slide
  sta ptr_select_slide_High          ;Guardamos el high byte del puntero

  lda ptr_select_slide_Low           ;Copiar el puntero en otra "variable" temporal, primero el low byte y después el high byte
  sta tmp_ptr_selslide_Low
  lda ptr_select_slide_High
  sta tmp_ptr_selslide_High
  rts


;;;;;;;;;;;AQUI EMPIEZA LOS DATOS GRÁFICOS - NO MÁS CODIGO ::::::::::::::::::::::::::::::::::::

; ID de sprite para cada letra y simbolo disponible
_0 = $00
_1 = $01
_2 = $02
_3 = $03
_4 = $04
_5 = $05
_6 = $06
_7 = $07
_8 = $08
_9 = $09
_A = $0A
B = $0B 
C = $0C 
D = $0D 
E = $0E 
F = $0F 
G = $10 
H = $11 
I = $12 
J = $13 
K = $14 
L = $15 
M = $16 
N = $17 
N_esp = $2A
O = $18 
P = $19 
Q = $1A 
R = $1B 
S = $1C 
T = $1D 
U = $1E 
V = $1F 
W = $20 
_X = $21
_Y = $22
Z = $23
_ = $24  
guion = $28
exclamacion = $2B
cruz = $29
punto = $2C
coma = $2D
bullet = $2E
interr_apertu = $2F
interr_cierre = $3F
parent_apertu = $3D
parent_cierre = $3E

;;;;;;;;;LOS DATOS DE CADA DIAPOSITIVA, COMO PUEDEN VER USA LOS CARACTERES DECLARADOS ARRIBA;;;;;;;;;;;;;;;;;;

bg_0:
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,$A5,$A6,_,_,_,_,_,_,_,_,_,_,_,F,C,F,M,_,_,_,_,_,_,_,_,_,_,_,$A5,$A6,_
  .byte _,$A7,$A8,_,_,_,_,_,_,_,_,_,_,_,U,_A,N,L,_,_,_,_,_,_,_,_,_,_,_,$A7,$A8,_  
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,L,E,N,G,U,_A,J,E,S,_,M,O,D,E,R,N,O,S,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,D,E,_,P,R,O,G,R,_A,M,_A,C,I,O,N,_,_,_,_,_,_,_,_  
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_  
  .byte _,$B0,$B2,_,_,_,P,R,O,_Y,E,C,T,O,_,I,N,T,E,G,R,_A,D,O,R,_,_,_,_,$B0,$B2,_
  .byte _,$B1,$B3,_,_,_,_,_,_,D,E,_,_A,P,R,E,N,D,I,Z,_A,J,E,_,_,_,_,_,_,$B1,$B3,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,P,R,O,G,R,_A,M,_A,N,D,O,_,E,N,_,_A,S,S,E,M,B,L,_Y,_,_,_,_,_
  .byte _,_,_,_,_,_,_,parent_apertu,6,5,0,2,parent_cierre,_,P,_A,R,_A,_,L,_A,_,N,E,S,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,$53,$54,_,_,_,_,_,$53,$54,_,_,_,_,_,_,$4B,$4D,$4D,$4D,$4D,$50,_,_,_,_
  .byte _,_,_,_,_,_,_,$55,$56,_,_,_,_,_,$55,$56,_,_,_,_,_,_,$4C,$4E,$4F,$4F,$4E,$51,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$52,$52,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$52,$52,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$52,$52,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$52,$52,_,_,_,_,_,_
  .byte _,H,E,C,H,O,_,P,O,R,_,_,_,_,_,1,9,5,7,9,7,7,_,_,$52,$52,_,_,$36,$37,_,_
  .byte _,R,O,G,E,L,_,_A,_X,E,L,_,G,U,E,L,_,L,E,R,M,_A,_,_,$52,$52,_,$35,$25,$25,$38,_
  .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
  .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
  .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
  .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7


;La pantalla se divide en secciones de 4 x 4 tiles, luego, por cada sección volveremos a dividir en 4 subsecciones de 2 x 2
;Cada byte controla una sección de 4x4 tiles, donde cada 2 bits de este controlan la paleta de una subsección de 2x2

attribute:
  .byte %01010101, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01010101
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %01010101, %01010101, %01010101, %00000000, %11000000, %11110000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00100010, %00000000
  .byte %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %10101110, %10101111
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010

bg_1:
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,O,B,J,E,T,I,V,O,S,_,_Y,_,L,O,_,Q,U,E,_,Q,U,I,E,R,O,_,_,_,_,_
  .byte _,_,_A,P,R,E,N,D,E,R,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,_,_
  .byte _,_,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,_,_
  .byte _,_,$47,$47,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,bullet,V,E,R,_,E,L,_,F,U,N,C,I,O,N,_A,M,I,E,N,T,O,_,$47,$47,_,_
  .byte _,_,$47,$47,_,D,E,_,L,_A,S,_,C,O,M,P,U,T,_A,D,O,R,_A,S,punto,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,bullet,_A,P,R,E,N,D,E,R,_,U,N,_,N,U,E,V,O,_,L,E,N,_,$47,$47,_,_
  .byte _,_,$47,$47,_,G,U,_A,J,E,_,_Y,_,N,U,E,V,O,S,_,R,E,T,O,S,punto,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,bullet,L,O,G,R,_A,R,_,M,O,S,T,R,_A,R,_,_A,L,G,O,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,G,R,_A,F,I,C,O,_,E,N,_,L,_A,_,P,_A,N,T,_A,L,L,_A,punto,$47,$47,_,_
  .byte _,_,$47,$47,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,L,O,_,Q,U,E,_,E,S,T,_A,N,_,V,I,E,N,D,O,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,N,O,_,E,S,_,U,N,_,V,I,D,E,O,J,U,E,G,O,coma,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,N,O,_,S,E,_,E,M,O,C,I,O,N,E,N,punto,_,_,_,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$47,$47,_,_
  .byte _,_,$47,$47,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$47,$47,_,_
  .byte _,_,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,$36,$37,$36,$37,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$4B,$4D,$4D,$4D,$4D,$4D,$4D,$50,_,_
  .byte _,_,$35,$25,$25,$25,$25,$38,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$4C,$4E,$4F,$4F,$4F,$4F,$4E,$51,_,_
  .byte _,_,$39,$3A,$3B,$3A,$3B,$3C,_,_,_,_,_,$4B,$4D,$4D,$4D,$4D,$50,_,_,_,_,_,$52,$52,$52,$52,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,$4C,$4E,$4F,$4F,$4E,$51,_,_,_,_,_,$52,$52,$52,$52,_,_,_,_
  .byte $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,_,_,_,_,$52,$52,_,_,_,_,_,_,_,$52,$52,$52,$52,_,_,_,_
  .byte $C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,$C1,_,_,_,_,$52,$52,_,_,_,_,_,_,_,$52,$52,$52,$52,_,_,_,_

attribute_1:
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
  .byte %10101010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10101010
  .byte %10101010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10101010
  .byte %10101010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10101010
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
  .byte %00000000, %00000000, %00000000, %11111111, %11111111, %11111111, %10101111, %11111111
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010


bg_2:
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,L,E,N,G,U,_A,J,E,_,E,N,S,_A,M,B,L,_A,D,O,R,_,_Y,_,L,_A,_,_,_,_,_ 
  .byte _,_,N,I,N,T,E,N,D,O,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,L,_A,_,P,L,_A,T,_A,F,O,R,M,_A,_,E,S,C,O,G,I,D,_A,_,P,_A,R,_A,_,_,_
  .byte _,_,E,L,_,P,R,O,_Y,E,C,T,O,_,E,S,_,L,_A,_,N,E,S,coma,_,U,N,_A,_,_,_,_
  .byte _,_,C,O,N,S,O,L,_A,_,D,E,_,V,I,D,E,O,J,U,E,G,O,S,_,L,_A,N,guion,_,_,_
  .byte _,_,Z,_A,D,_A,_,P,O,R,_,N,I,N,T,E,N,D,O,_,E,N,_,1,9,8,5,punto,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,L,O,_,Q,U,E,_,M,_A,S,_,M,E,_,G,U,S,T,_A,_,E,S,_,L,_A,_,_,_,_,_
  .byte _,_,G,R,_A,N,_,C,_A,N,T,I,D,_A,D,_,D,E,_,I,N,F,O,R,M,_A,C,I,O,N,_,_
  .byte _,_,D,I,S,P,O,N,I,B,L,E,_,_Y,_,_A,_,L,_A,_,S,I,M,P,L,I,C,I,guion,_,_,_
  .byte _,_,D,_A,D,_,D,E,_,S,U,_,L,E,N,G,U,_A,J,E,_,E,N,S,_A,M,B,L,_A,guion,_,_
  .byte _,_,D,O,R,punto,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,E,S,T,E,_,L,E,N,G,U,_A,J,E,_,E,N,S,_A,M,B,L,_A,D,O,R,_,_,_,_,_
  .byte _,_,N,O,S,_,P,E,R,M,I,T,I,R,_A,_,O,L,V,I,D,_A,R,N,O,S,_,D,E,_,_,_
  .byte _,_,L,O,S,_,C,O,N,S,T,R,U,C,T,O,S,_,M,_A,S,_,B,_A,S,I,C,O,S,_,_,_
  .byte _,_,Q,U,E,_,C,O,N,O,C,E,M,O,S,punto,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $82,$84,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$41,$82,$84
  .byte $83,$85,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$83,$85
  .byte $82,$84,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$C6,$C7,$26,$26,$26,$26,$86,$87
  .byte $83,$85,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$C8,$C9,$26,$26,$26,$26,$8A,$8B
  .byte $82,$84,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$CA,$CB,$26,$26,$26,$26,$8E,$8F
  .byte $83,$85,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$26,$CC,$CD,$26,$26,$26,$26,$91,$92
  .byte $84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$84,$82,$26,$26,$26,$26,$82,$84
  .byte $85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$85,$83,$26,$26,$26,$26,$83,$85

attribute_2:
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00110000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %11000000
  .byte %00110011, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %11001100
  .byte %00001111, %00001111, %00001111, %00001111, %00001111, %00001111, %00000011, %00001100

bg_3:
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,E,_X,P,L,I,C,_A,C,I,O,N,_,D,E,L,_,F,U,N,C,I,O,N,_A,M,I,E,N,guion,_ 
  .byte _,_,T,O,_,D,E,_,L,_A,_,C,O,N,S,O,L,_A,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D,$5D
  .byte $5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E
  .byte $5D,$5D,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $5E,$5E,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $5D,$5D,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $5E,$5E,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,P,O,N,G,_A,N,_,_A,T,E,N,C,I,O,N,_,_A,L,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,P,I,Z,_A,R,R,O,N,punto,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$7B,$7C
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$7D,$7E
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$7F,$5D,$5D
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$7F,_,$5E,$5E
  .byte $5D,$5D,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$5D,$5D
  .byte $5E,$5E,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$79,$5E,$5E
  .byte $5D,$5D,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$5D,$5D
  .byte $5E,$5E,_,R,E,F,L,E,_X,I,O,N,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$5E,$5E
  .byte $5D,$5D,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$5D,$5D
  .byte $5E,$5E,_,interr_apertu,C,O,M,O,_,C,R,E,E,N,_,Q,U,E,_,P,O,D,_A,M,O,S,_,_,_,_,$5E,$5E
  .byte $5D,$5D,_,L,E,V,_A,N,T,_A,R,_,L,_A,_,T,E,C,N,O,L,O,G,I,_A,_,D,E,_,_,$5D,$5D
  .byte $5E,$5E,_,N,U,E,V,O,_,E,N,_,E,L,_,C,_A,S,O,_,D,E,_,U,N,_A,_,_,_,_,$5E,$5E
  .byte $5D,$5D,_,C,_A,T,_A,S,T,R,O,F,E,interr_cierre,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$5D,$5D
  .byte $5E,$5E,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$5E,$5E

attribute_3:
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00101000
  .byte %00001000, %00001010, %00001010, %00001010, %00001010, %00001010, %00001010, %00000010
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000


bg_4:
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,interr_apertu,Q,U,E,_,_A,P,R,E,N,D,I,M,O,S,interr_cierre,_,C,O,N,C,L,U,S,I,O,N,E,S,_
  .byte _,_,D,E,L,_,P,R,O,_Y,E,C,T,O,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AB,$AD
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AC,$AE
  .byte $AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,$AB,$AD,_,_,_,_,$AB,$AD
  .byte $AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,$AC,$AE,_,_,_,_,$AC,$AE
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AB,$AD
  .byte $AC,$AE,_,bullet,R,E,S,O,L,V,I,_,D,U,D,_A,S,_,M,U,_Y,_,V,I,E,J,_A,S,_,_,$AC,$AE
  .byte $AB,$AD,_,Q,U,E,_,T,E,N,I,_A,_,S,O,B,R,E,_,O,T,R,O,S,_,L,E,N,guion,_,$AB,$AD
  .byte $AC,$AE,_,G,U,_A,J,E,S,_,parent_apertu,N,O,_,S,_A,B,I,_A,_,C,O,M,O,_,U,S,_A,R,_,$AC,$AE
  .byte $AB,$AD,_,L,O,S,_,P,U,N,T,E,R,O,S,coma,_,P,O,R,_,E,J,E,M,P,L,O,parent_cierre,_,$AB,$AD
  .byte $AC,$AE,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AC,$AE
  .byte $AB,$AD,_,bullet,_A,P,R,E,N,D,I,_,U,N,_A,_,P,I,Z,C,_A,_,M,_A,S,_,D,E,L,_,$AB,$AD
  .byte $AC,$AE,_,F,U,N,C,I,O,N,_A,M,I,E,N,T,O,_,D,E,_,L,O,S,_,P,R,O,guion,_,$AC,$AE
  .byte $AB,$AD,_,C,E,S,_A,D,O,R,E,S,punto,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AB,$AD
  .byte $AC,$AE,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AC,$AE
  .byte $AB,$AD,_,bullet,_A,P,R,E,N,D,_A,N,_,D,E,_,V,E,R,D,_A,D,_,E,L,_,_,_,_,_,$AB,$AD
  .byte $AC,$AE,_,H,_A,R,D,W,_A,R,E,_,D,E,_,L,O,_,Q,U,E,_,V,_A,_Y,_A,N,_,_A,_,$AC,$AE
  .byte $AB,$AD,_,P,R,O,G,R,_A,M,_A,R,coma,_,O,_,S,I,_,N,O,coma,_,P,R,E,P,_A,guion,_,$AB,$AD
  .byte $AC,$AE,_,R,E,N,S,E,_,P,_A,R,_A,_,S,U,F,R,I,R,_,C,O,M,O,_,_Y,O,punto,_,$AC,$AE
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$AB,$AD
  .byte $AC,$AE,_,_,_,_,$60,$61,$62,$63,_,_,_,_,$60,$61,$62,$63,_,_,_,_,$60,$61,$62,$63,_,_,_,_,$AC,$AE
  .byte $AB,$AD,_,_,_,_,$64,$65,$66,$67,_,_,_,_,$64,$65,$66,$67,_,_,_,_,$64,$65,$66,$67,_,_,_,_,$AB,$AD
  .byte $AC,$AE,_,_,_,_,$68,$69,$26,$6A,_,_,_,_,$68,$69,$26,$6A,_,_,_,_,$68,$69,$26,$6A,_,_,_,_,$AC,$AE
  .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
  .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
  .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
  .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7

attribute_4:
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %00100000, %10001000
  .byte %00100010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10001000
  .byte %00100010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10001000
  .byte %00100010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %10001000
  .byte %00100010, %11000000, %00110000, %11000000, %00110000, %11000000, %00110000, %10001000
  .byte %10100010, %10101100, %10101111, %10101100, %10100011, %10101100, %10100011, %10101010
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010


bg_5:
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,G,R,_A,C,I,_A,S,_,P,O,R,_,S,U,_,_A,T,E,N,C,I,O,N,exclamacion,exclamacion,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,O,T,R,O,S,_,R,E,C,U,R,S,O,S,_,E,N,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,bullet,N,E,S,D,E,V,punto,O,R,G,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,bullet,N,E,R,D,_Y,guion,N,I,G,H,T,S,punto,N,E,S,punto,S,C,I,E,N,C,E,_,_,_,_
  .byte _,_,_,bullet,6,5,0,2,punto,O,R,G,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte _,_,_,bullet,N,E,S,H,_A,C,K,E,R,_,parent_apertu,_Y,O,U,T,U,B,E,parent_cierre,_,_,_,_,_,_,_,_,_
  .byte _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$B0,$B2,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AC,$AE,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$B1,$B3,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$A2,$A3,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AC,$AE,_,_,_,_,_,$36,$37,$36,$37,$36,$37,_,_,_,_,_,$A2,$A3,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AB,$AD,_,_,_,_,$35,$25,$25,$25,$25,$25,$25,$38,_,_,_,_,$A2,$A3,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AC,$AE,_,_,_,_,$39,$3A,$3B,$3A,$3B,$3A,$3B,$3C,_,_,_,_,$A2,$A3,_,_,_,_,_,_,_,_,_,_,_,_
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$A2,$A3,_,_,_,_,$9D,$9E,$9D,$9E,$9D,$9E,_,_
  .byte $AC,$AE,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$A2,$A3,_,_,_,_,$47,$47,$47,$47,$47,$47,_,_
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$A2,$A3,_,_,_,_,$47,$27,$47,$47,$27,$47,_,_
  .byte $AC,$AE,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$A2,$A3,_,_,_,_,$47,$27,$47,$47,$27,$47,_,_
  .byte $AB,$AD,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,$A2,$A3,_,_,$9D,$9E,$A9,$AA,$A9,$AA,$A9,$AA,$9D,$9E
  .byte $AC,$AE,_,_,_,_,_,_,_,_,$31,$32,_,_,_,_,_,_,$A2,$A3,_,_,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
  .byte $AB,$AD,_,_,_,_,_,_,_,$30,$26,$34,$33,_,_,_,_,_,$A2,$A3,_,_,$47,$47,$47,$47,$9B,$9C,$47,$47,$47,$47
  .byte $AC,$AE,_,_,_,_,_,_,$30,$26,$26,$26,$26,$33,_,_,_,_,$A2,$A3,_,_,$47,$47,$47,$47,$27,$27,$47,$47,$47,$47
  .byte $AB,$AD,_,_,_,_,_,$30,$26,$34,$26,$26,$34,$26,$33,_,_,_,$AB,$AD,_,_,$47,$47,$47,$47,$27,$27,$47,$47,$47,$47
  .byte $AC,$AE,_,_,_,_,$30,$26,$26,$26,$26,$26,$26,$26,$26,$33,_,_,$AC,$AE,_,_,$47,$47,$47,$47,$27,$27,$47,$47,$47,$47
  .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
  .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
  .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
  .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7


attribute_5:
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00100000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .byte %00100010, %00000000, %00000000, %00000000, %11111111, %00000000, %00000000, %00000000
  .byte %00100010, %00000000, %00000000, %00000000, %11111111, %10101010, %10101010, %10101010
  .byte %00100010, %00000000, %11111100, %00110000, %11111111, %10101010, %10101010, %10101010
  .byte %10100010, %10101100, %10101111, %10101111, %10101000, %10101000, %10101010, %10101010
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010




palettes:
  ;Las paletas funcionan como un libro de colorear, le podemos aplicar una paleta en específico
  ;a cada sprite/fondo.

  ; Paleta para el fondo
  .byte $21, $30, $21, $0F	;(00)  -> Texto
  .byte $31, $27, $37, $0F	;(01)  -> Monedas
  .byte $31, $17, $07, $0F	;(10)  -> Tierra y bloques
  .byte $31, $2A, $1A, $0F	;(11)  -> Arbustos

  ; Paleta para los sprites          -> Sin usar
  .byte $21, $32, $21, $22	;(00)
  .byte $33, $34, $23, $24	;(01)
  .byte $35, $36, $25, $26	;(10)
  .byte $37, $38, $27, $28	;(11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCIONAMIENTO DE SPRITES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;NOTA: EN EL JUEGO NO SE USAN LOS SPRITES, PERO HE AQUI UN EJEMPLO;;;;;;;;;;;;;;;;;;;;;;;

;hello:
  ;Tiles de la palabra "hola"
  ;Y-Coor, SpriteID, Atributos, X-Coor

;  .byte $6c, $5f, %00000000, $6c
;  .byte $6c, $4f, %00000001, $76
;  .byte $6c, $3e, %00000010, $80
;  .byte $6c, $7a, %00000011, $8A
;  .byte $6c, $2f, %00000001, $94

  ;El byte en binario son los atributos.
  ;VoltearV, VoltearH, Prioridad, S/N, S/N, S/N, PaletaH, PaletaL

;;;;;;;;;;CODIGO PARA CARGAR SPRITE, NO SE USA!!!;;;;;;;;;;;;;;;;;;

;ldx #$00
;cargar_mensaje:	
;  lda hello, x 	;Cargamos el byte de la sección hello en el acomulador
;  sta $0200, x    ;Lo pondremos en el buffer
;  inx			; x = x + 1
;  cpx #$14		;Copiar los 20 bytes
;  bne cargar_mensaje

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RESET, NMI & IRQ VECTORS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Interrupciones importantes
.segment "VECTORS"
  .addr nmi 	;El NMI es una notificación de la PPU al programa de que ya está en v-blank (ya puedes escribir)
  .addr reset	;Etiqueta cada vez que encendemos o reseteamos la consola
  .addr 0		;Otro tipo de interrupciones.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;IMPORTAR GRAFICOS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Sección donde estan los datos gráficos, me robé los sprites del mario 1
;El archivo mario.chr no está incluido, lo saque del tutorial de la página antes mencionada
.segment "CHARS"
  .incbin "mario.chr"
