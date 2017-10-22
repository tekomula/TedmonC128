;
; The complete source code for Commodore 128 TEDMON
; Extracted and disassembled by Teemu Komulainen 2017
;
; Only for personal use. Use at your own risk.
;

iexmon	= $032e
sysvec	= $0a00
chrout	= $ffd2
jprimm	= $ff7d
jjmpfar	= $ff71
mmureg	= $ff00
jsetmsg	= $ff90
jindfet	= $ff74
jindcmp	= $ff7a
jindsta	= $ff77
jbasin	= $ffcf
jopen	= $ffc0
jchkin	= $ffc6
jclose	= $ffc3
jsetnam	= $ffbd
jstop	= $ffe1
jsetlfs	= $ffba
jclrch	= $ffcc
jckout	= $ffc9
jsetbnk	= $ff68
jjsrfar	= $ff6e
jload	= $ffd5
jsave	= $ffd8

monlo	= $00
monhi	= $20

; If the memory location is changed, monlo/monhi should be changed
; accordingly.

* = $2000 ; Desired memory address where TEDMON should be located

; TEDMON must always be located at bank 0x0f in C128

jmoninit ; TEDMON cold-start entry point
	jmp moninit

jmonbrk	jmp monbrk ; Break entry point (IBRK at $0316-$0317 points here)

jimonrtn ; Re-entry point from Kernal IEXMON vector
	jmp mm3

monbrk	jsr jprimm

.byte $0d,$42,$52,$45,$41,$4b,$07,$00

	pla
	sta $02
	ldx #$05
mb1	pla
	sta $03,X
	dex
	bpl mb1
	bmi mi1

moninit	lda #$00 ; Cold-start routine
	sta mmureg
	sta $06
	sta $07
	sta $08
	sta $05
	lda #monlo
	ldy #monhi
	sta $04
	sty $03
	lda #$0f
	sta $02
	jsr jprimm

.byte $0d,$4d,$4f,$4e,$49,$54,$4f,$52,$00

mi1	cld
	tsx
	stx $09
	lda #$c0
	jsr jsetmsg
	cli

showreg jsr jprimm ; Print registers

.byte $0d,$20,$20,$20,$20,$50,$43,$20,$20,$53,$52,$20
.byte $41,$43,$20,$58,$52,$20,$59,$52,$20,$53,$50,$0d
.byte $3b,$20,$1b,$51,$00

	lda $02
	jsr cvtbyt
	txa
	jsr chrout
	lda $03
	jsr prntbyt
	ldy #$02
sreg1	lda $0002,Y
	jsr ph2
	iny
	cpy #$08
	bcc sreg1

monmain jsr curstnxt ; Main loop
	ldx #$00
	stx $7A
mm1	jsr jbasin
	sta $0200,X
	inx
	cpx #$A1
	bcs mm5
	cmp #$0D
	bne mm1
	lda #$00
	sta $01FF,X
mm2	jsr tc1
	beq monmain
	cmp #$20
	beq mm2
	jmp (iexmon)
mm3	ldx #$15
mm4	cmp comtbl,X
	beq mm6
	dex
	bpl mm4
mm5	jsr jprimm

.byte $1d,$3f,$00

	jmp monmain
mm6	cpx #$13
	bcs mm7
	cpx #$0F
	bcs mm8
	txa
	asl A
	tax
	lda exectbl+1,X
	pha
	lda exectbl,X
	pha
	jmp gp1
mm7	sta $93
	jmp monlsv
mm8	jmp numxvrt

exitmon	jmp (sysvec) ; Back to BASIC via system vector

comtbl ; Table of TEDMON commands
.byte $41,$43,$44,$46,$47,$48,$4a,$4d,$52,$54,$58,$40
.byte $2e,$3e,$3b,$24,$2b,$26,$25,$4c,$53,$56

exectbl ; Table of command execution addresses

.word assmble-1
.word cmpxfr-1
.word disassm-1
.word fillmem-1
.word gotoloc-1
.word search-1
.word jmpsub-1
.word showmem-1
.word showreg-1
.word xfr-1
.word exitmon-1
.word diskcmd-1
.word assmble-1
.word chngmem-1
.word chngreg-1

mindfet	stx $0AB2 ; Kernal INDFET call for the monitor
	ldx $68
	lda #$66
	sei
	jsr jindfet
	cli
	ldx $0AB2
	rts

mindsta	stx $0AB2 ; Kernal INDSTA call for the monitor
	ldx #$66
	stx $02B9
	ldx $68
	sei
	jsr jindsta
	cli
	ldx $0AB2
	rts

mindcmp	stx $0AB2 ; Kernal INDCMP call for the monitor
	ldx #$66
	stx $02C8
	ldx $68
	sei
	jsr jindcmp
	cli
	php
	ldx $0AB2
	plp
	rts

showmem	bcs sm1 ; Memory display
	jsr moveval
	jsr gp1
	bcc sm2
sm1	lda #$0b
	sta $60
	bne sm4
sm2	jsr calccnt
	bcc sm7
	ldx #$03
	bit $d7
	bpl sm3
	inx
sm3	lsr $62
	ror $61
	ror $60
	dex
	bne sm3
sm4	jsr jstop
	beq sm6
	jsr showlin
	lda #$08
	bit $d7
	bpl sm5
	asl A
sm5	jsr ip1
	jsr decpoco
	bcs sm4
sm6	jmp monmain
sm7	jmp mm5

chngreg jsr chngadd ; Change register
	ldy #$00
cr1	jsr gp1
	bcs cr2
	lda $60
	sta $0005,Y
	iny
	cpy #$05
	bcc cr1
cr2	jmp monmain

chngmem	bcs cm3 ; Change memory
	jsr moveval
	ldy #$00
cm1	jsr gp1
	bcs cm3
	lda $60
	jsr mindsta
	iny
	bit $D7
	bpl cm2
	cpy #$10
	bcc cm1
cm2	cpy #$08
	bcc cm1
cm3	jsr jprimm

.byte $1b,$4f,$91,$00

	jsr showlin
	jmp monmain

gotoloc	jsr chngadd ; Go routine
	ldx $09
	txs
	jmp jjmpfar

jmpsub	jsr chngadd ; Jump to subroutine
	jsr jjsrfar
	jmp monmain

showlin	jsr curstnxt ; Display a line of memory
	lda #$3E
	jsr chrout
	jsr prnthex
	ldy #$00
	beq sl2
sl1	jsr ph3
sl2	jsr mindfet
	jsr prntbyt
	iny
	cpy #$08
	bit $d7
	bpl sl3
	cpy #$10
sl3	bcc sl1
	jsr jprimm

.byte $3a,$12,$00

	ldy #$00
sl4	jsr mindfet
	pha
	and #$7F
	cmp #$20
	pla
	bcs sl5
	lda #$2E
sl5	jsr chrout
	iny
	bit $d7
	bpl sl6
	cpy #$10
	bcc sl4
sl6	cpy #$08
	bcc sl4
	rts

cmpxfr	lda #$00 ; Compare / Transfer memory
	.byte $2c
xfr	.byte $a9
	.byte $80
	sta $93
	lda #$00
	sta $0AB3
	jsr prepptr
	bcs cx1
	jsr gp1
	bcc cx2
cx1	jmp mm5
cx2	bit $93
	bpl cx4
	sec
	lda $66
	sbc $60
	lda $67
	sbc $61
	bcs cx4
	lda $63
	adc $60
	sta $60
	lda $64
	adc $61
	sta $61
	lda $65
	adc $62
	sta $62
	ldx #$02
cx3	lda $0AB7,X
	sta $66,X
	dex
	bpl cx3
	lda #$80
	sta $0AB3
cx4	jsr curstnxt
	ldy #$00
cx5	jsr jstop
	beq cx11
	jsr mindfet
	ldx #$60
	stx $02B9
	stx $02C8
	ldx $62
	sei
	bit $93
	bpl cx6
	jsr jindsta
cx6	ldx $62
	jsr jindcmp
	cli
	beq cx7
	jsr prnthex
	jsr ph3
	jsr ph3
cx7	bit $0AB3
	bmi cx8
	inc $60
	bne cx9
	inc $61
	bne cx9
	jmp mm5
cx8	jsr decpoco
	jsr decptr
	jmp cx10
cx9	jsr incptr
cx10	jsr deccnt
	bcs cx5
cx11	jmp monmain

search	jsr prepptr ; Search for pattern
	bcs sr9
	ldy #$00
	jsr tc1
	cmp #$27
	bne sr2
	jsr tc1
	cmp #$00
	beq sr9
sr1	sta $0A80,Y
	iny
	jsr tc1
	beq sr4
	cpy #$20
	bne sr1
	beq sr4
sr2	sty $0100
	jsr getparm
sr3	lda $60
	sta $0A80,Y
	iny
	jsr gp1
	bcs sr4
	cpy #$20
	bne sr3
sr4	sty $93
	jsr curstnxt
sr5	ldy #$00
sr6	jsr mindfet
	cmp $0A80,Y
	bne sr7
	iny
	cpy $93
	bne sr6
	jsr prnthex
	jsr ph3
	jsr ph3
sr7	jsr jstop
	beq sr8
	jsr incptr
	jsr deccnt
	bcs sr5
sr8	jmp monmain
sr9	jmp mm5

monlsv	ldy #$01 ; Prepare for file operations
	sty $BA
	sty $B9
	dey
	sty $C6
	sty $B7
	sty $C7
	sty $90
	lda #$0A
	sta $BC
	lda #$80
	sta $BB
mls1	jsr tc1
	beq monload
	cmp #$20
	beq mls1
	cmp #$22
	bne mls3
	ldx $7A
mls2	lda $0200,X
	beq monload
	inx
	cmp #$22
	beq mls4
	sta ($BB),Y
	inc $B7
	iny
	cpy #$11
	bcc mls2
mls3	jmp mm5
mls4	stx $7A
	jsr tc1
	beq monload
	jsr gp1
	bcs monload
	lda $60
	sta $BA
	jsr gp1
	bcs monload
	jsr moveval
	sta $C6
	jsr gp1
	bcs preplove
	jsr curstnxt
	ldx $60
	ldy $61
	lda $93
	cmp #$53
	bne mls3

monsave	lda #$00 ; Save file
	sta $B9
	lda #$66
	jsr jsave
msa1	jmp monmain

monload	lda $93 ; Load/verify file
	cmp #$56
	beq mlo1
	cmp #$4C
	bne mls3
	lda #$00
mlo1	jsr jload
	lda $90
	and #$10
	beq msa1
	lda $93
	beq mls3
	jsr jprimm

.byte $20,$45,$52,$52,$4f,$52,$00

	jmp monmain

preplove ; Prepare for load/verify
	ldx $66
	ldy $67
	lda #$00
	sta $B9
	beq monload

fillmem	jsr prepptr ; Fill memory
	bcs fm3
	lda $68
	cmp $0AB9
	bne fm3
	jsr gp1
	bcs fm3
	ldy #$00
fm1	lda $60
	jsr mindsta
	jsr jstop
	beq fm2
	jsr incptr
	jsr deccnt
	bcs fm1
fm2	jmp monmain
fm3	jmp mm5

assmble	bcs as6 ; Assemble
	jsr moveval
as1	ldx #$00
	stx $0AA1
	stx $0AB4
as2	jsr tc1
	bne as3
	cpx #$00
	bne as3
	jmp monmain
as3	cmp #$20
	beq as1
	sta $0AAC,X
	inx
	cpx #$03
	bne as2
as4	dex
	bmi as7
	lda $0AAC,X
	sec
	sbc #$3F
	ldy #$05
as5	lsr A
	ror $0AA1
	ror $0AA0
	dey
	bne as5
	beq as4
as6	jmp mm5
as7	ldx #$02
as8	lda $0AB4
	bne as13
	jsr cvtparam
	beq as12
	bcs as6
	lda #$24
	sta $0AA0,X
	inx
	lda $62
	bne as6
	ldy #$04
	lda $0AB6
	cmp #$08
	bcc as9
	cpy $0AB4
	beq as10
as9	lda $61
	bne as10
	ldy #$02
as10	lda #$30
as11	sta $0AA0,X
	inx
	dey
	bne as11
as12	dec $7A
as13	jsr tc1
	beq as14
	cmp #$20
	beq as8
	sta $0AA0,X
	inx
	cpx #$0A
	bcc as8
	bcs as6
as14	stx $63
	ldx #$00
	stx $0AB1
as15	ldx #$00
	stx $9F
	lda $0AB1
	jsr calcmn
	ldx $0AAA
	stx $64
	tax
	lda mnemtbl1,X
	jsr as30
	lda mnemtbl,X
	jsr as30
	ldx #$06
as16	cpx #$03
	bne as18
	ldy $0AAB
	beq as18
as17	lda $0AAA
	cmp #$E8
	lda #$30
	bcs as20
	jsr as29
	dey
	bne as17
as18	asl $0AAA
	bcc as19
	lda modetbl-1,X
	jsr as30
	lda modetbl+5,X
	beq as19
	jsr as30
as19	dex
	bne as16
	beq as21
as20	jsr as29
	jsr as29
as21	lda $63
	cmp $9F
	beq as22
	jmp as31
as22	ldy $0AAB
	beq as27
	lda $64
	cmp #$9D
	bne as25
	lda $60
	sbc $66
	tax
	lda $61
	sbc $67
	bcc as23
	bne as28
	cpx #$82
	bcs as28
	bcc as24
as23	tay
	iny
	bne as28
	cpx #$82
	bcc as28
as24	dex
	dex
	txa
	ldy $0AAB
	bne as26
as25	lda $005F,Y
as26	jsr mindsta
	dey
	bne as25
as27	lda $0AB1
	jsr mindsta
	jsr curstcur
	jsr jprimm

.byte $41,$20,$1b,$51,$00

	jsr da7
	inc $0AAB
	lda $0AAB
	jsr ip1
	lda #$41
	sta $034A
	lda #$20
	sta $034B
	sta $0351
	lda $68
	jsr cvtbyt
	stx $034C
	lda $67
	jsr cvtbyt
	sta $034D
	stx $034E
	lda $66
	jsr cvtbyt
	sta $034F
	stx $0350
	lda #$08
	sta $D0
	jmp monmain
as28	jmp mm5
as29	jsr as30
as30	stx $0AAF
	ldx $9F
	cmp $0AA0,X
	beq as32
	pla
	pla
as31	inc $0AB1
	beq as28
	jmp as15
as32	inc $9F
	ldx $0AAF
	rts

disassm	bcs da1 ; Disassemble
	jsr moveval
	jsr gp1
	bcc da2
da1	lda #$14
	sta $60
	bne da3
da2	jsr calccnt
	bcc da5
da3	jsr jprimm

.byte $0d,$1b,$51,$00

	jsr jstop
	beq da4
	jsr da6
	inc $0AAB
	lda $0AAB
	jsr ip1
	lda $0AAB
	jsr dp1
	bcs da3
da4	jmp monmain
da5	jmp mm5
da6	lda #$2E
	jsr chrout
	jsr ph3
da7	jsr prnthex
	jsr ph3
	ldy #$00
	jsr mindfet
	jsr calcmn
	pha
	ldx $0AAB
	inx
da8	dex
	bpl da9
	jsr jprimm

.byte $20,$20,$20,$00

	jmp da10
da9	jsr mindfet
	jsr ph2
da10	iny
	cpy #$03
	bcc da8
	pla
	ldx #$03
	jsr prntmn
	ldx #$06
da11	cpx #$03
	bne da13
	ldy $0AAB
	beq da13
da12	lda $0AAA
	cmp #$E8
	php
	jsr mindfet
	plp
	bcs da15
	jsr prntbyt
	dey
	bne da12
da13	asl $0AAA
	bcc da14
	lda modetbl-1,X
	jsr chrout
	lda modetbl+5,X
	beq da14
	jsr chrout
da14	dex
	bne da11
	rts
da15	jsr da17
	clc
	adc #$01
	bne da16
	inx
da16	jmp ph1
da17	ldx $67
	tay
	bpl da18
	dex
da18	adc $66
	bcc da19
	inx
da19	rts

calcmn	tay ; Calculates mnemonic and addressing mode
	lsr A
	bcc cmn1
	lsr A
	bcs cmn3
	cmp #$22
	beq cmn3
	and #$07
	ora #$80
cmn1	lsr A
	tax
	lda opcdtbl,X
	bcs cmn2
	lsr A
	lsr A
	lsr A
	lsr A
cmn2	and #$0F
	bne cmn4
cmn3	ldy #$80
	lda #$00
cmn4	tax
	lda indctbl,X
	sta $0AAA
	and #$03
	sta $0AAB
	tya
	and #$8F
	tax
	tya
	ldy #$03
	cpx #$8A
	beq cmn7
cmn5	lsr A
	bcc cmn7
	lsr A
cmn6	lsr A
	ora #$20
	dey
	bne cmn6
	iny
cmn7	dey
	bne cmn5
	rts

prntmn	tay ; Prints mnemonic for opcode
	lda mnemtbl,Y
	sta $63
	lda mnemtbl1,Y
	sta $64
pm1	lda #$00
	ldy #$05
pm2	asl $64
	rol $63
	rol A
	dey
	bne pm2
	adc #$3F
	jsr chrout
	dex
	bne pm1
	jmp ph3

opcdtbl ; Opcode decoding table

.byte $40,$02,$45,$03,$d0,$08,$40,$09,$30,$22,$45,$33
.byte $d0,$08,$40,$09,$40,$02,$45,$33,$d0,$08,$40,$09
.byte $40,$02,$45,$b3,$d0,$08,$40,$09,$00,$22,$44,$33
.byte $d0,$8c,$44,$00,$11,$22,$44,$33,$d0,$8c,$44,$9a
.byte $10,$22,$44,$33,$d0,$08,$40,$09,$10,$22,$44,$33
.byte $d0,$08,$40,$09,$62,$13,$78,$a9

indctbl ; Table of addressing mode indicators

.byte $00,$21,$81,$82,$00,$00,$59,$4d,$91,$92,$86,$4a
.byte $85,$9d

modetbl ; Table of mode identification characters

.byte $2c,$29,$2c,$23,$28,$24,$59,$00,$58,$24,$24,$00

mnemtbl ; Table of mnemonics in packed form (first byte)

.byte $1c,$8a,$1c,$23,$5d,$8b,$1b,$a1,$9d,$8a,$1d,$23
.byte $9d,$8b,$1d,$a1,$00,$29,$19,$ae,$69,$a8,$19,$23
.byte $24,$53,$1b,$23,$24,$53,$19,$a1,$00,$1a,$5b,$5b
.byte $a5,$69,$24,$24,$ae,$ae,$a8,$ad,$29,$00,$7c,$00
.byte $15,$9c,$6d,$9c,$a5,$69,$29,$53,$84,$13,$34,$11
.byte $a5,$69,$23,$a0

mnemtbl1 ; Table of mnemonics in packed form (second byte)

.byte $d8,$62,$5a,$48,$26,$62,$94,$88,$54,$44,$c8,$54
.byte $68,$44,$e8,$94,$00,$b4,$08,$84,$74,$b4,$28,$6e
.byte $74,$f4,$cc,$4a,$72,$f2,$a4,$8a,$00,$aa,$a2,$a2
.byte $74,$74,$74,$72,$44,$68,$b2,$32,$b2,$00,$22,$00
.byte $1a,$1a,$26,$26,$72,$72,$88,$c8,$c4,$ca,$26,$48
.byte $44,$44,$a2,$c8,$0d,$20,$20,$20

getparm	dec $7A ; Evaluates a parameter in the input buffer
gp1	jsr cvtparam
	bcs gp3
	jsr testchr
	bne gp2
	dec $7A
	lda $0AB4
	bne gp5
	beq gp4
gp2	cmp #$20
	beq gp5
	cmp #$2C
	beq gp5
gp3	pla
	pla
	jmp mm5
gp4	sec
	.byte $24
gp5	.byte $18
	lda $0AB4
	rts

cvtparam ; Transforms a numeric parameter into byte value
	lda #$00
	sta $60
	sta $61
	sta $62
	sta $0AB4
	txa
	pha
	tya
	pha
cp1	jsr tc1
	bne cp2
	jmp cp12
cp2	cmp #$20
	beq cp1
	ldx #$03
cp3	cmp comtbl+15,X
	beq cp4
	dex
	bpl cp3
	inx
	dec $7A
cp4	ldy basetbl,X
	lda basetbl+4,X
	sta $0AB6
cp5	jsr tc1
	beq cp12
	sec
	sbc #$30
	bcc cp12
	cmp #$0A
	bcc cp6
	sbc #$07
	cmp #$10
	bcs cp12
cp6	sta $0AB5
	cpy $0AB5
	bcc cp11
	beq cp11
	inc $0AB4
	cpy #$0A
	bne cp8
	ldx #$02
cp7	lda $60,X
	sta $0AB7,X
	dex
	bpl cp7
cp8 	ldx $0AB6
cp9	asl $60
	rol $61
	rol $62
	bcs cp11
	dex
	bne cp9
	cpy #$0A
	bne cp10
	asl $0AB7
	rol $0AB8
	rol $0AB9
	bcs cp11
	lda $0AB7
	adc $60
	sta $60
	lda $0AB8
	adc $61
	sta $61
	lda $0AB9
	adc $62
	sta $62
	bcs cp11
cp10	clc
	lda $0AB5
	adc $60
	sta $60
	txa
	adc $61
	sta $61
	txa
	adc $62
	sta $62
	bcs cp11
	and #$F0
	bne cp11
	beq cp5
cp11	sec
	.byte $24
cp12	.byte $18
	sty $0AB6
	pla
	tay
	pla
	tax
	lda $0AB4
	rts

basetbl ; Table of bases and bits-per-digit

.byte $10,$0a,$08,$02,$04,$03,$03,$01

prnthex	lda $68 ; Prints a hexadecimal value
	jsr cvtbyt
	txa
	jsr chrout
	lda $66
	ldx $67
ph1	pha
	txa
	jsr prntbyt
	pla
ph2	jsr prntbyt
ph3	lda #$20
	jmp chrout

curstcur ; Moves cursor to start of current line
	jsr jprimm

.byte $0d,$91,$00

	rts

curstnxt ; Moves cursor to start of next line
	lda #$0D
	jmp chrout

clrlin	jsr jprimm ; Clears a screen line

.byte $0d,$1b,$51,$20,$00

	rts

prntbyt	stx $0AAF ; Prints two ASCII characters for a byte value
	jsr cvtbyt
	jsr chrout
	txa
	ldx $0AAF
	jmp chrout

cvtbyt	pha ; Converts a byte value into two ASCII characters
	jsr cb1
	tax
	pla
	lsr A
	lsr A
	lsr A
	lsr A
cb1	and #$0F
	cmp #$0A
	bcc cb2
	adc #$06
cb2	adc #$30
	rts

testchr	dec $7A ; Tests next character in the input buffer
tc1	stx $0AAF
	ldx $7A
	lda $0200,X
	beq tc2
	cmp #$3A
	beq tc2
	cmp #$3F
tc2	php
	inc $7A
	ldx $0AAF
	plp
	rts

moveval	lda $60 ; Transfers address and bank values to working pointer
	sta $66
	lda $61
	sta $67
	lda $62
	sta $68
	rts

calccnt	sec ; Calculates number of bytes and banks to display or move
	lda $60
	sbc $66
	sta $60
	lda $61
	sbc $67
	sta $61
	lda $62
	sbc $68
	sta $62
	rts

decpoco	lda #$01 ; Decrements pointer / counter
dp1	sta $0AAF
	sec
	lda $60
	sbc $0AAF
	sta $60
	lda $61
	sbc #$00
	sta $61
	lda $62
	sbc #$00
	sta $62
	rts

deccnt	sec ; Decrements byte count
	lda $63
	sbc #$01
	sta $63
	lda $64
	sbc #$00
	sta $64
	lda $65
	sbc #$00
	sta $65
	rts

incptr	lda #$01 ; Increments address pointer
ip1	clc
	adc $66
	sta $66
	bcc ip2
	inc $67
	bne ip2
	inc $68
ip2	rts

decptr	sec ; Decrements address pointer
	lda $66
	sbc #$01
	sta $66
	lda $67
	sbc #$00
	sta $67
	lda $68
	sbc #$00
	sta $68
	rts

chngadd	bcs ca1 ; Changes bank and address
	lda $60
	ldy $61
	ldx $62
	sta $04
	sty $03
	stx $02
ca1	rts

prepptr	bcs ppex ; Prepares pointers for dual-address operations
	jsr moveval
	jsr gp1
	bcs ppex
	lda $60
	sta $0AB7
	lda $61
	sta $0AB8
	lda $62
	sta $0AB9
	jsr calccnt
	lda $60
	sta $63
	lda $61
	sta $64
	lda $62
	sta $65
	bcc ppex
	clc
	.byte $24
ppex	.byte $38
	rts

numxvrt	jsr getparm ; Performs number base conversion
	jsr clrlin
	lda #$24
	jsr chrout
	lda $62
	beq nx1
	jsr cvtbyt
	txa
	jsr chrout
nx1	lda $60
	ldx $61
	jsr ph1
	jsr clrlin
	lda #$2B
	jsr chrout
	jsr hexdec
	lda #$00
	ldx #$08
	ldy #$03
	jsr pb1
	jsr clrlin
	lda #$26
	jsr chrout
	lda #$00
	ldx #$08
	ldy #$02
	jsr pntobd
	jsr clrlin
	lda #$25
	jsr chrout
	lda #$00
	ldx #$18
	ldy #$00
	jsr pntobd
	jmp monmain

hexdec	jsr moveval ; Converts a hexadecimal value to decimal
	lda #$00
	ldx #$07
hd1	sta $0AA0,X
	dex
	bpl hd1
	inc $0AA7
	ldy #$17
	php
	sei
	sed
hd2	lsr $68
	ror $67
	ror $66
	bcc hd4
	clc
	ldx #$03
hd3	lda $0AA4,X
	adc $0AA0,X
	sta $0AA0,X
	dex
	bpl hd3
hd4	clc
	ldx #$03
hd5	lda $0AA4,X
	adc $0AA4,X
	sta $0AA4,X
	dex
	bpl hd5
	dey
	bpl hd2
	plp
	rts

pntobd	pha ; Prints octal, binary or decimal values
	lda $60
	sta $0AA2
	lda $61
	sta $0AA1
	lda $62
	sta $0AA0
	lda #$00
	sta $0AA3
	pla
pb1	sta $0AB4
	sty $0AB6
pb2	ldy $0AB6
	lda #$00
pb3	asl $0AA3
	rol $0AA2
	rol $0AA1
	rol $0AA0
	rol A
	dey
	bpl pb3
	tay
	bne pb4
	cpx #$01
	beq pb4
	ldy $0AB4
	beq pb5
pb4	inc $0AB4
	ora #$30
	jsr chrout
pb5	dex
	bne pb2
	rts

diskcmd	bne dc1 ; Handles disk commands
	ldx #$08
	.byte $2c
dc1	.byte $a6
	.byte $60
	cpx #$04
	bcc dc6
	cpx #$1F
	bcs dc6
	stx $60
	lda #$00
	sta $62
	sta $B7
	tax
	jsr jsetbnk
	jsr tc1
	dec $7A
	cmp #$24
	beq showdir
	lda #$00
	ldx $60
	ldy #$0F
	jsr jsetlfs
	jsr jopen
	bcs dc5
	ldx #$00
	jsr jckout
	bcs dc5
dc2	ldx $7A
	inc $7A
	lda $0200,X
	beq dc3
	jsr chrout
	bcc dc2
dc3	jsr jclrch
	jsr curstnxt
	ldx #$00
	jsr jchkin
	bcs dc5
dc4	jsr jbasin
	jsr chrout
	cmp #$0D
	beq dc5
	lda $90
	and #$BF
	beq dc4
dc5	jsr jclrch
	lda #$00
	sec
	jsr jclose
	jmp monmain
dc6	jmp mm5

showdir	ldy #$FF ; Displays disk directory
	ldx $7A
	dex
sd1	iny
	inx
	lda $0200,X
	bne sd1
	tya
	ldx $7A
	ldy #$02
	jsr jsetnam
	lda #$00
	ldx $60
	ldy #$60
	jsr jsetlfs
	jsr jopen
	bcs dc5
	ldx #$00
	jsr jchkin
	jsr curstnxt
	ldy #$03
sd2	sty $63
sd3	jsr jbasin
	sta $60
	lda $90
	bne dc5
	jsr jbasin
	sta $61
	lda $90
	bne dc5
	dec $63
	bne sd3
	jsr hexdec
	lda #$00
	ldx #$08
	ldy #$03
	jsr pb1
	lda #$20
	jsr chrout
sd4	jsr jbasin
	beq sd5
	ldx $90
	bne dc5
	jsr chrout
	bcc sd4
sd5	jsr curstnxt
	jsr jstop
	beq dc5
	ldy #$02
	bne sd2

; Mandatory credits follows :-)

.byte $28,$43,$29,$31,$39,$38,$36,$20,$43,$4f,$4d,$4d
.byte $4f,$44,$4f,$52,$45,$20,$45,$4c,$45,$43,$54,$52
.byte $4f,$4e,$49,$43,$53,$2c,$20,$4c,$54,$44,$2e,$20
.byte $41,$4c,$4c,$20,$52,$49,$47,$48,$54,$53,$20,$52
.byte $45,$53,$45,$52,$56,$45,$44,$2e,$ff,$ff,$ff,$ff
.byte $c8,$cd,$01,$c5

.END
