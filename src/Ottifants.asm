; This disassembly was created using Emulicious (http://www.emulicious.net)
.MEMORYMAP
SLOTSIZE $7FF0
SLOT 0 $0000
SLOTSIZE $10
SLOT 1 $7FF0
SLOTSIZE $4000
SLOT 2 $8000
DEFAULTSLOT 2
.ENDME
.ROMBANKMAP
BANKSTOTAL 16
BANKSIZE $7FF0
BANKS 1
BANKSIZE $10
BANKS 1
BANKSIZE $4000
BANKS 14
.ENDRO

.background "_spg/Ottifants.sms"

; Ports
.define Port_PSG $7F
.define Port_VDPData $BE
.define Port_VDPAddress $BF
.define _PORT_C1_ $C1

; Input Ports
.define Port_VDPStatus $BF
.define Port_IOPort1 $DC
.define Port_IOPort2 $DD

.DEF emu_page $10
.DEF decode_pix $e404
.DEF sys_page $11
.DEF tileram_adr $e408
.DEF tilemap_adr $e40a	;tilemap ram adress
.DEF Tile_page $3f
;.DEF set_tile $e40c	; set_tile_proc
.DEF update_tilemem $e416
.DEF update_2byte_tilemem $e41f
.DEF view_tilemem $e41c

.DEF cls_tileset $e419
.DEF send_palette $e40e	
.DEF key_scan $e410
.DEF send_sprites $e413

.include "WLADX/tsconfig.asm"

.BANK 0 SLOT 0
.ORG $0000
.unbackground 0 $3D5E
_LABEL_0_:
	ld sp, hl
;	ld sp, $DFEC
	nop
	nop
	nop
	jp _LABEL_275_

; Data from 7 to 7 (1 bytes)
.db $01

	di
	ld a,2
	out (Port_VDPAddress), a
	ld a, d
	jp _LABEL_91_

; Data from 10 to 15 (6 bytes)
.db $00 $00 $00 $00 $00 $00


; Data from 16 to 17 (2 bytes)
_DATA_16_:
.db $00 $00


_LABEL_18_:
	jp +++
/*
; Data from 1B to 37 (29 bytes)
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
*/
set_page2	push bc
		cp $c0
		jr c,sp21
		and $0f
		add a,$10
sp21		and $1f
		ld bc,PAGE2
		out (c),a
		ld (_RAM_FFFF_), a
		pop bc
		ret
temp0	.db 0
temp1	.db 0
temp2	.db 0
temp3	.db 0


.org $38
_LABEL_38_:
	push af
	push hl
	push de
	ld hl, (_RAM_DFEE_)
	ld a, h
	or l
	jr z, +
	ld de, ++	; Overriding return address
	push de
	jp (hl)

+:
	in a, (Port_VDPStatus)
	in a, (_PORT_C1_)
++:
	pop de
	pop hl
	pop af
	ei
	ret

_LABEL_50_:
+++:
	push af
	ld a, ($0016)
	or e
	call set_page2
	pop af
	ret

; Data from 5A to 65 (12 bytes)
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00


_LABEL_66_:
	push af
	ld a, $80
	ld (_RAM_C2CF_), a
	pop af
	retn

; Data from 6F to 6F (1 bytes)
.db $00

_LABEL_70_:
	push af
	pop hl
;	in a, (Port_IOPort2)
;	bit 4, a
;	jp z, _LABEL_0_
	push hl
	pop af
	ld hl, (_DATA_85_)
	ld sp, ($0087)
	ret
;	jp _LABEL_BD_

.org $85
; Data from 85 to 90 (12 bytes)
_DATA_85_:
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
_LABEL_91_:
	or $40
	out (Port_VDPAddress), a
	ei
	ret
/*
_LABEL_97_:
	push af
	ld a, (_RAM_FFFF_)
	ld ($00B8), a
	ld a, $CE
	call set_page2
	ld a, $93
	ld ($8003), a
	ld a, $FF
	ld ($8000), a
	ld a, $80
	ld ($8002), a
	ld a, $00
	ld ($00E1), a
	ld a, $82
	call set_page2
	pop af

_LABEL_BD_:
	di
	ld ($026F), hl
	ld ($0273), sp
	pop hl
	ld ($0271), hl
	ld sp, $026F
	push de
	push bc
	exx
	push hl
	push de
	push bc
	push ix
	push iy
	push af
	ld a, i
	ld h, a
	ld a, r
	ld l, a
	push hl
	ex af, af'
	push af
	ld d, $00
_LABEL_E2_:
	call _LABEL_20E_
	ld a, e
	cp $B4
	jr z, _LABEL_13F_
	cp $BA
	jp z, _LABEL_1D4_
	cp $B8
	jr z, _LABEL_169_
	cp $B9
	jr z, _LABEL_171_
	cp $B7
	jr z, _LABEL_15B_
	cp $B6
	jr z, _LABEL_179_
	cp $B5
	jr z, +
	cp $C9
	jr nz, _LABEL_E2_
	ld a, (_RAM_FFFF_)
	and $7F
	call set_page2
	ld hl, $0402
	ld de, _RAM_C000_
	ld bc, $1000
	ldir
	ld a, (_RAM_FFFF_)
	or $80
	call set_page2
	ld sp, $DFF0
	call _RAM_C1DB_	; Code is loaded from _LABEL_5DD_
	jp _RAM_C000_	; Possibly invalid

+:
	call _LABEL_20E_
	ld h, e
	call _LABEL_20E_
	ld l, e
	ld ($0209), hl
	ld hl, $CD00
	ld ($0207), hl
	jp _LABEL_1EA_

_LABEL_13F_:
	call _LABEL_20E_
	ld h, e
	call _LABEL_20E_
	ld l, e
	call _LABEL_20E_
	ld b, e
	call _LABEL_20E_
	ld c, e
-:
	call _LABEL_20E_
	ld (hl), e
	inc hl
	dec bc
	ld a, b
	or c
	jr nz, -
	jr _LABEL_E2_

_LABEL_15B_:
	call _LABEL_20E_
	ld a, e
	or $80
	and $BF
	call set_page2
	jp _LABEL_E2_

_LABEL_169_:
	ld hl, _DATA_25B_
	ld bc, $001A
	jr +

_LABEL_171_:
	ld hl, $025B
	ld bc, $001A
	jr -

_LABEL_179_:
	call _LABEL_20E_
	ld h, e
	call _LABEL_20E_
	ld l, e
	call _LABEL_20E_
	ld b, e
	call _LABEL_20E_
	ld c, e
+:
	ld a, (_RAM_FFFF_)
	ld ($01A1), a
	ld a, $CE
	call set_page2
-:
	ld a, (_DATA_38002_)
	xor d
	rrca
	jr c, -
	ld a, $81
	ld ($8003), a
	ld a, $00
	call set_page2
	ld a, d
	xor $41
	ld d, a
-:
	ld a, (hl)
	call _LABEL_231_
	inc hl
	dec bc
	ld a, b
	or c
	jr nz, -
	ld a, (_RAM_FFFF_)
	ld ($01CD), a
	ld a, $CE
	call set_page2
	ld a, $93
	ld ($8003), a
	ld a, d
	xor $40
	ld ($8002), a
	xor $80
	ld d, a
	ld a, $00
	call set_page2
	jp _LABEL_E2_

_LABEL_1D4_:
	call _LABEL_20E_
	ld l, e
	call _LABEL_20E_
	ld h, e
	ld ($0207), hl
	call _LABEL_20E_
	ld l, e
	call _LABEL_20E_
	ld h, e
	ld ($0209), hl
_LABEL_1EA_:
	ld a, d
	ld ($00E1), a
	pop af
	ex af, af'
	pop hl
	ld a, h
	ld i, a
	ld a, l
	ld r, a
	pop af
	pop iy
	pop ix
	pop bc
	pop de
	pop hl
	exx
	pop bc
	pop de
	pop hl
	ld sp, ($0273)
	nop
	nop
	nop
	nop
	jp _LABEL_BD_

_LABEL_20E_:
	ld a, (_RAM_FFFF_)
	ld ($022C), a
	ld a, $CE
	call set_page2
-:
	ld a, (_DATA_38002_)
	xor d
	rrca
	jr c, -
	ld a, (_DATA_38002_ - 2)
	ld e, a
	ld a, d
	ld ($8002), a
	xor $81
	ld d, a
	ld a, $86
	call set_page2
	ret

_LABEL_231_:
	ld ($0240), a
	ld a, (_RAM_FFFF_)
	ld ($0254), a
	ld a, $CE
	call set_page2
	ld a, $00
	ld ($8000), a
	ld a, d
	ld ($8002), a
	ld a, d
	xor $81
	ld d, a
-:
	ld a, (_DATA_38002_)
	xor d
	rrca
	jr nc, -
	ld a, $00
	call set_page2
	ret
*/
	jr $-1
.org $259
; Data from 259 to 25A (2 bytes)
.db $E5 $00

; Data from 25B to 26C (18 bytes)
_DATA_25B_:
.db $50 $82 $30 $00 $6A $AE $EB $9C $B8 $06 $00 $00 $46 $C3 $00 $00
.db $00 $00

; Data from 26D to 273 (7 bytes)
.db $16 $14 $2B $06 $15 $4E $EC


; Data from 274 to 274 (1 bytes)
.db $DF


_LABEL_275_:

	im 1
;	ld (_RAM_FFFC_), a
;	ld (_RAM_FFFD_), a
;	ld (_RAM_FFFE_), a
	ld a,2
	call set_page2
;	in a, (Port_VDPStatus)
	ld hl, $0000
	ld (_RAM_DFEE_), hl
;	ld a, ($0007)
;	or a
;	jp nz, _LABEL_400_

;.org $400
_LABEL_400_:
;	call _LABEL_249C_
	ld hl, _RAM_C000_
	ld de, _RAM_C000_ + 1
	ld bc, $179F
	ld (hl), $00
	ldir
	ld a, ($0016)
	ld e, a
	ld a, $0C
	or e
	call set_page2
	ld a, (_RAM_D7A0_)
	cp $AB
	jr nz, +
	ld a, (_RAM_D7A1_)
	cp $CD
	jr nz, +
	ld hl, _DATA_30028_
	ld de, _RAM_D7C0_
	ld bc, $01F1
	ldir
	jr ++

+:
	ld hl, _DATA_30008_
	ld de, _RAM_D7A0_
	ld bc, $0211
	ldir
	xor a
	ld (_RAM_D9B0_), a
++:
	call _LABEL_887_
	ld hl, _LABEL_27FB_
	ld (_RAM_DFEE_), hl
	call _LABEL_236A_
	jp _LABEL_E5D_

_LABEL_453_:		; not used??
	xor $DF
	call $2373	; Possibly invalid
	jp _LABEL_E5D_

.org $45b
; Data from 45B to 5DC (386 bytes)
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $80 $40 $C0 $20 $A0 $60 $E0 $10 $90 $50
.db $D0 $30 $B0 $70 $F0 $08 $88 $48 $C8 $28 $A8 $68 $E8 $18 $98 $58
.db $D8 $38 $B8 $78 $F8 $04 $84 $44 $C4 $24 $A4 $64 $E4 $14 $94 $54
.db $D4 $34 $B4 $74 $F4 $0C $8C $4C $CC $2C $AC $6C $EC $1C $9C $5C
.db $DC $3C $BC $7C $FC $02 $82 $42 $C2 $22 $A2 $62 $E2 $12 $92 $52
.db $D2 $32 $B2 $72 $F2 $0A $8A $4A $CA $2A $AA $6A $EA $1A $9A $5A
.db $DA $3A $BA $7A $FA $06 $86 $46 $C6 $26 $A6 $66 $E6 $16 $96 $56
.db $D6 $36 $B6 $76 $F6 $0E $8E $4E $CE $2E $AE $6E $EE $1E $9E $5E
.db $DE $3E $BE $7E $FE $01 $81 $41 $C1 $21 $A1 $61 $E1 $11 $91 $51
.db $D1 $31 $B1 $71 $F1 $09 $89 $49 $C9 $29 $A9 $69 $E9 $19 $99 $59
.db $D9 $39 $B9 $79 $F9 $05 $85 $45 $C5 $25 $A5 $65 $E5 $15 $95 $55
.db $D5 $35 $B5 $75 $F5 $0D $8D $4D $CD $2D $AD $6D $ED $1D $9D $5D
.db $DD $3D $BD $7D $FD $03 $83 $43 $C3 $23 $A3 $63 $E3 $13 $93 $53
.db $D3 $33 $B3 $73 $F3 $0B $8B $4B $CB $2B $AB $6B $EB $1B $9B $5B
.db $DB $3B

; Data from 5DD to 5FF (35 bytes)
.db $BB $7B $FB $07 $87 $47 $C7 $27 $A7 $67 $E7 $17 $97 $57 $D7 $37
.db $B7 $77 $F7 $0F $8F $4F $CF $2F $AF $6F $EF $1F $9F $5F $DF $3F
.db $BF $7F $FF


; Data from 600 to 61A (27 bytes)
_DATA_600_:
.db $00 $00 $06 $0A $00 $04 $0A $00 $06 $0E $00 $18 $00 $08 $0E $00
.db $0A $10 $00 $06 $10 $00 $0A $12 $00 $08 $12

; Data from 61B to 7C8 (430 bytes)
_DATA_61B_:
.db $00 $03 $60 $00 $03 $42 $02 $00 $00 $01 $00 $E8 $A0 $01 $01 $03
.db $03 $12 $00 $02 $FF $FF $C0 $00 $03 $10 $02 $D8 $58 $01 $00 $F4
.db $DF $01 $01 $04 $08 $02 $E4 $03 $00 $08 $08 $00 $03 $52 $02 $00
.db $00 $01 $01 $F4 $C0 $00 $01 $08 $0C $04 $00 $04 $00 $08 $08 $00
.db $03 $52 $02 $00 $00 $01 $04 $F4 $C0 $00 $01 $08 $0C $04 $00 $04
.db $8E $FF $F0 $08 $08 $01 $00 $FF $18 $01 $00 $EF $F0 $00 $01 $08
.db $08 $02 $E5 $04 $0F $FF $00 $10 $05 $01 $02 $00 $00 $01 $00 $E0
.db $EE $01 $01 $01 $04 $01 $01 $02 $00 $08 $08 $00 $03 $52 $02 $00
.db $00 $01 $07 $F4 $C0 $00 $01 $08 $0C $04 $00 $04 $00 $08 $08 $00
.db $02 $52 $02 $00 $00 $04 $0A $F4 $C0 $00 $01 $08 $0C $04 $00 $04
.db $00 $08 $08 $00 $03 $52 $02 $00 $00 $01 $0C $F4 $C0 $00 $01 $08
.db $0C $04 $00 $04 $00 $08 $08 $00 $03 $52 $02 $00 $00 $01 $0F $F4
.db $C0 $00 $01 $08 $0C $04 $00 $04 $00 $08 $08 $00 $03 $52 $02 $00
.db $00 $01 $12 $F4 $C0 $00 $01 $08 $0C $04 $00 $04 $00 $08 $08 $00
.db $03 $52 $02 $00 $00 $01 $15 $F4 $C0 $00 $01 $08 $0C $04 $00 $04
.db $00 $08 $08 $00 $03 $52 $02 $00 $00 $01 $18 $F4 $C0 $00 $01 $08
.db $0C $04 $00 $04 $00 $03 $08 $00 $02 $52 $02 $00 $00 $01 $00 $00
.db $D0 $00 $03 $10 $04 $12 $00 $02 $FF $1C $20 $01 $08 $01 $01 $00
.db $00 $03 $00 $00 $24 $10 $08 $20 $10 $01 $00 $02 $00 $18 $8F $01
.db $02 $06 $00 $00 $00 $04 $00 $00 $40 $3C $03 $10 $06 $01 $00 $02
.db $00 $5F $00 $FC $10 $01 $00 $00 $00 $01 $00 $00 $30 $20 $05 $00
.db $06 $01 $00 $02 $FF $2F $60 $FF $10 $01 $01 $00 $00 $05 $00 $00
.db $00 $10 $10 $02 $03 $01 $00 $02 $FF $0A $80 $FF $10 $01 $01 $00
.db $00 $01 $00 $00 $FF $00 $06 $0C $20 $01 $E3 $02 $00 $44 $80 $0E
.db $03 $06 $01 $00 $00 $02 $00 $00 $FF $24 $05 $10 $06 $01 $00 $02
.db $00 $28 $02 $F8 $03 $0A $02 $00 $00 $04 $00 $00 $E0 $04 $04 $02
.db $03 $01 $00 $02 $FF $08 $02 $FF $02 $05 $02 $00 $00 $02 0 0
.db $E0 $04 $04 $02 $03 $01 $00 $02 $00 $17 $85 $FE $04 $06 $01 $00
.db $00 $02 $00 $00 $FF $24 $05 $10 $06 $01 $00 $02 $FF $04 $00 $F0
.db $02 $04 $03 $00 $00 $01 $00 $00 $FF $04 $04 $02 $03 $01 $00 $02
.db $00 $44 $C0 $F8 $06 $10 $02 $00 $00 $01 $00 $00 $00 $40 $04 $00
.db $06 $01 $00 $02 $FF $F0 $00 $00 $02 $01 $01 $00 $00 $01 $00 $00
.db $D8 $00 $01 $60 $02 $00 $00 $02 $00 $03 $00 $04 $02 $06 $02 $00
.db $00 $02 $00 $00 $E0 $04 $04 $10 $06 $01 $E7 $02 $FF $18 $80 $80
.db $6A $02 $01 $00 $00 $01 $00 $00 $00 $40 $04 $09 $10 $01 $00 $02
.db $00 $08 $00 $04 $04 $05 $00 $00 $00 $03 $00 $00 $FF $04 $04 $02
.db $01 $06 $E7 $02 $FF $17 $80 $01 $01 $05 $02 $00 $00 $02 $00 $00
.db $C0 $04 $04 $02 $03 $01 $00 $02 $FF $1A $80 $03 $02 $01 $02 $00
.db $00 $04 $00 $00 $C0 $04 $04 $04 $10 $01 $00 $02


_LABEL_887_:
	ld a, ($0016)
	ld e, a
	ld a, $0C
	or e
	call set_page2
	ld hl, _RAM_C22C_
	ld de, _RAM_C22C_ + 1
	ld bc, $0095
	ld (hl), $00
	ldir
	ld ix, _DATA_326B0_
	ld de, $A6A8
	ld l, (ix+1)
	ld h, (ix+0)
	inc ix
	inc ix
	add hl, de
	ld (_RAM_C2C4_), hl
	ld ix, _DATA_326B4_
	ld iy, _RAM_C22C_
	ld bc, $0032
	ld a, $03
-:
	ld l, (ix+1)
	ld h, (ix+0)
	add hl, de
	ld (iy+38), l
	ld (iy+39), h
	ld l, (ix+17)
	ld h, (ix+16)
	add hl, de
	ld (iy+40), l
	ld (iy+41), h
	inc ix
	inc ix
	add iy, bc
	dec a
	jr nz, -
	ld ix, _DATA_326D4_
	ld l, (ix+1)
	ld h, (ix+0)
	inc ix
	inc ix
	add hl, de
	ld (_RAM_C2C6_), hl
	ld l, (ix+1)
	ld h, (ix+0)
	inc ix
	inc ix
	add hl, de
	ld (_RAM_C2C8_), hl
	xor a
	ld (_RAM_C243_), a
	ld a, $20
	ld (_RAM_C275_), a
	ld a, $40
	ld (_RAM_C2A7_), a
	ret

_LABEL_912_:
	di
	push ix
	ld c, a
	ld a, (_RAM_C2CC_)
	cp $80
	jr nc, +
	ld (_RAM_C2CE_), a
+:
	ld a, ($0016)
	ld e, a
	ld a, $0C
	or e
	call set_page2
	ld a, c
	ld (_RAM_C2CC_), a
	and $7F
	ld h, $00
	ld l, a
	xor a
	ld (_RAM_C22B_), a
	dec a
	ld (_RAM_C2C2_), a
	add hl, hl
	ld de, (_RAM_C2C4_)
	ex de, hl
	add hl, de
	inc hl
	ld a, (hl)
	ld (_RAM_C2CB_), a
	xor a
	ld (_RAM_C2CA_), a
	ld ix, _RAM_C22C_
	ld b, $03
-:
	ld (ix+34), $00
	ld l, (ix+38)
	ld h, (ix+39)
	ld a, (_RAM_C2CC_)
	add a, a
	ld e, a
	ld d, $00
	add hl, de
	ld d, (hl)
	inc hl
	ld e, (hl)
	ld h, (ix+41)
	ld l, (ix+40)
	add hl, de
	ld (ix+42), l
	ld (ix+43), h
	xor a
	ld (ix+35), a
	ld (ix+36), a
	ld (ix+44), a
	ld (ix+37), a
	ld (ix+26), a
	ld (ix+27), a
	ld de, $0032
	add ix, de
	djnz -
	ld a, $01
	ld (_RAM_C22B_), a
	pop ix
	ei
	ret

_LABEL_996_:
	ld (_RAM_C2CB_), a
	or a
	jr nz, +
	ld a, ($0016)
	ld e, a
	ld a, $0C
	or e
	call set_page2
	ld hl, (_RAM_C2C4_)
	ld a, (_RAM_C2CC_)
	and $0F
	ld e, a
	ld d, $00
	sla e
	add hl, de
	inc hl
	ld a, (hl)
	ld (_RAM_C2CB_), a
	xor a
+:
	ld (_RAM_C2CA_), a
	ret

_LABEL_9BE_:
	ld hl, _RAM_C2CA_
	dec (hl)
	jp p, _LABEL_B26_
	ld a, (_RAM_C2CB_)
	ld (_RAM_C2CA_), a
	ld ix, _RAM_C22C_
	ld a, (_RAM_C2C2_)
	ld (_RAM_C2C3_), a
	xor a
_LABEL_9D6_:
	ld (_RAM_C2CD_), a
	dec (ix+26)
	jp p, _LABEL_B18_
	ld a, (ix+27)
	ld (ix+26), a
	ld a, (ix+35)
	and a
	jp nz, _LABEL_A7A_
_LABEL_9EC_:
	ld l, (ix+42)
	ld h, (ix+43)
	ld e, (ix+34)
	ld d, $00
	add hl, de
	add hl, de
	ld d, (hl)
	inc hl
	ld e, (hl)
	ld (ix+36), e
	ld a, d
	cp $FF
	jr nz, ++
	ld a, (_RAM_C2CC_)
	and $80
	jr nz, +
	xor a
	ld (_RAM_C22B_), a
	jp _LABEL_B26_

+:
	ld a, (_RAM_C2CE_)
	call _LABEL_912_
	jp _LABEL_B26_

++:
	cp $FB
	jr nz, +
	ld a, (ix+36)
	ld (ix+44), a
	jr ++

+:
	cp $FE
	jr nz, +
--:
	ld e, (ix+36)
-:
	ld (ix+34), e
	jr _LABEL_9EC_

+:
	cp $FC
	jr nz, +
	ld a, (_RAM_C2C3_)
	cp $FF
	jr z, --
	ld a, $FF
	ld (_RAM_C2C2_), a
	jr -

+:
	cp $FD
	jr nz, +++
	ld a, (_RAM_C2C3_)
	cp $FF
	jr z, ++
	ld a, $FF
	ld (_RAM_C2C2_), hl
	jr -

++:
	inc (ix+34)
	jp _LABEL_9EC_

+++:
	ld d, $00
	ld e, a
	sla e
	rl d
	ld iy, (_RAM_C2C6_)
	add iy, de
	ld d, (iy+0)
	ld e, (iy+1)
	ld hl, (_RAM_C2C8_)
	add hl, de
	ld (ix+46), l
	ld (ix+47), h
_LABEL_A7A_:
	ld l, (ix+46)
	ld h, (ix+47)
	ld e, (ix+35)
	ld d, $00
	add hl, de
	ld a, (hl)
	or a
	jp p, _LABEL_ADB_
	cp $F9
	jr c, _LABEL_AD1_
	cp $FF
	jr nz, ++
	ld (ix+35), $00
	ld a, (ix+44)
	or a
	jr z, +
	sub $01
	ld (ix+44), a
	jp _LABEL_A7A_

+:
	inc (ix+34)
	jp _LABEL_9EC_

++:
	cp $F9
	jr nz, +
	inc (ix+35)
	jp _LABEL_B18_

+:
	cp $FE
	jr nz, _LABEL_AD1_
	ld (ix+28), a
	inc hl
	ld d, (hl)
	inc hl
	ld e, (hl)
	ld (ix+29), e
	ld (ix+30), d
	ld a, (ix+35)
	add a, $03
	ld (ix+35), a
	jp _LABEL_B18_

_LABEL_AD1_:
	and $7F
	ld (ix+45), a
	inc (ix+35)
	jr _LABEL_A7A_

_LABEL_ADB_:
	bit 6, a
	jr z, +
	and $3F
	ld (ix+27), a
	ld (ix+26), a
	inc (ix+35)
	jp _LABEL_A7A_

+:
	ld c, a
	xor a
	ld (ix+24), a
	ld a, (ix+21)
	cp $0F
	jr nc, ++
	ld a, (ix+22)
	and a
	jr nz, ++
	ld a, (ix+45)
	ld (ix+22), a
	inc c
	sub $0C
	sub $10
	add a, (ix+36)
	cp $54
	jr c, +
	ld a, c
+:
	ld (ix+25), a
++:
	inc (ix+35)
_LABEL_B18_:
	ld de, $0032
	add ix, de
	ld a, (_RAM_C2CD_)
	inc a
	cp $03
	jp nz, _LABEL_9D6_
_LABEL_B26_:
	ret


_LABEL_B27_:
	ld ix, _RAM_C22C_
	xor a
_LABEL_B2C_:
	ld (_RAM_C2CD_), a
	ld a, (ix+22)
	and a
	jp z, _LABEL_BC5_
	cp $0F
	jr c, ++
	cp (ix+21)
	jr nc, +
	ld (ix+22), $00
	jp _LABEL_BC5_

+:
	ld (ix+25), $00
++:
	dec (ix+24)
	jp p, _LABEL_BC5_
	ld (ix+24), $00
	push af
	call _LABEL_D53_
	pop af
	ld (ix+21), a
	dec a
	ld de, _DATA_61B_
	ld c, a
	add a, a
	add a, a
	add a, c
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, de
	ld bc, $0014
	push ix
	pop de
	ldir
	ld (ix+20), $01
	ld a, (ix+4)
	ld (ix+32), a
	ld a, (ix+9)
	ld (ix+33), a
	ld a, (ix+6)
	bit 0, a
	jr z, +
	ld a, (ix+0)
	ld (ix+7), a
	ld a, (ix+1)
	ld (ix+8), a
+:
	ld a, (ix+25)
	and a
	jr z, +
	add a, (ix+11)
	ld h, $00
	ld l, a
	add hl, hl
	ld de, _DATA_D6C_
	add hl, de
	ld (ix+48), l
	ld (ix+49), h
	ld a, (hl)
	ld (ix+0), a
	inc hl
	ld a, (hl)
	ld (ix+1), a
+:
	ld a, (ix+12)
	ld (ix+31), a
	ld a, (ix+18)
	and a
	jr z, +
	out (Port_PSG), a
	jr +

_LABEL_BC5_:
	ld a, (ix+21)
	and a
	jp z, _LABEL_D2A_
+:
	ld a, (ix+20)
	dec a
	jr z, +++
	dec a
	jr z, +
	dec a
	jr z, ++++
	ld c, (ix+19)
	jr ++

+:
	dec (ix+16)
	jr nz, +
	inc (ix+20)
+:
	ld c, (ix+15)
++:
	ld a, (ix+31)
	sub c
	jp c, _LABEL_D27_
	ld (ix+31), a
	jr +++++

+++:
	dec (ix+14)
	jr nz, +
	inc (ix+20)
+:
	ld a, (ix+13)
	add a, (ix+31)
	jr nc, +
	ld a, $FF
+:
	ld (ix+31), a
	jr +++++

++++:
	dec (ix+17)
	jr nz, _LABEL_C64_
	inc (ix+20)
	jr _LABEL_C64_

+++++:
	ld a, (ix+31)
	srl a
	srl a
	srl a
	srl a
	ld c, a
	ld a, (ix+18)
	and a
	jr z, +++
	and $03
	cp $03
	jr z, ++
	ld a, (ix+7)
	srl a
	srl a
	srl a
	srl a
	and $0F
	or $F0
	xor $0F
	out (Port_PSG), a
	ld a, (ix+7)
	sub (ix+8)
	jr nc, +
	xor a
	ld (ix+8), a
+:
	ld (ix+7), a
	jr +++

++:
	ld a, c
	or $F0
	xor $0F
	out (Port_PSG), a
	jr _LABEL_C64_

+++:
	ld a, c
	or $90
	or (ix+23)
	xor $0F
	out (Port_PSG), a
_LABEL_C64_:
	ld c, (ix+1)
	ld a, (ix+0)
	add a, a
	rl c
	add a, a
	rl c
	ld a, c
	and $0F
	or $80
	or (ix+23)
	out (Port_PSG), a
	ld a, (ix+1)
	ld c, a
	rra
	rra
	and $3F
	out (Port_PSG), a
	ld a, (ix+10)
	and a
	jr z, +
	dec a
	add a, (ix+4)
	ld c, a
	ld b, $00
	ld hl, _DATA_600_
	add hl, bc
	ld l, (hl)
	ld h, $00
	ld e, (ix+48)
	ld d, (ix+49)
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	jr ++

+:
	ld l, (ix+0)
	ld h, (ix+1)
++:
	ld e, (ix+29)
	ld d, (ix+30)
	add hl, de
	ld e, (ix+2)
	ld d, (ix+3)
	add hl, de
	dec (ix+33)
	jr nz, _LABEL_D2A_
	ld a, (ix+9)
	ld (ix+33), a
	ld (ix+0), l
	ld (ix+1), h
	dec (ix+4)
	jp nz, _LABEL_D2A_
	dec (ix+5)
	jr nz, +
	ld (ix+21), $00
	jp _LABEL_D27_

+:
	ld a, (ix+6)
	and a
	jr z, +++++
	bit 2, a
	jr nz, +
	bit 1, a
	jr z, +++
	jr nz, ++
+:
	ld l, (ix+7)
	ld h, (ix+8)
	ld a, (ix+0)
	ld (ix+7), a
	ld a, (ix+1)
	ld (ix+8), a
	jr ++++

++:
	ld hl, $0000
	xor a
	sbc hl, de
	ld (ix+2), l
	ld (ix+3), h
+++:
	ld a, (ix+6)
	bit 0, a
	jr z, +++++
	ld l, (ix+7)
	ld h, (ix+8)
++++:
	ld (ix+0), l
	ld (ix+1), h
+++++:
	ld a, (ix+32)
	ld (ix+4), a
	jr _LABEL_D2A_

_LABEL_D27_:
	call _LABEL_D53_
_LABEL_D2A_:
	ld bc, $0032
	add ix, bc
	ld a, (_RAM_C2CD_)
	inc a
	cp $03
	jp nz, _LABEL_B2C_
	ret

_LABEL_D39_:
	ld ix, _RAM_C22C_
	call _LABEL_D53_
	ld ix, _RAM_C25E_
	call _LABEL_D53_
	ld ix, _RAM_C290_
	call _LABEL_D53_
	ld a, $FF
	out (Port_PSG), a
	ret

_LABEL_D53_:
	ld a, (ix+18)
	and a
	jr z, +
	ld a, $FF
	out (Port_PSG), a
+:
	ld a, $9F
	or (ix+23)
	out (Port_PSG), a
	xor a
	ld (ix+21), a
	ld (ix+22), a
	ret

; Data from D6C to DFD (146 bytes)
_DATA_D6C_:
.db $00 $00 $C0 $D5 $C0 $C9 $80 $BE $C0 $B3 $C0 $A9 $40 $A0 $40 $97
.db $C0 $8E $C0 $86 $00 $7F $00 $78 $40 $71 $E0 $6A $E0 $64 $40 $5F
.db $E0 $59 $E0 $54 $20 $50 $A0 $4B $60 $47 $60 $43 $80 $3F $00 $3C
.db $A0 $38 $70 $35 $70 $32 $A0 $2F $F0 $2C $70 $2A $10 $28 $D0 $25
.db $B0 $23 $B0 $21 $C0 $1F $00 $1E $50 $1C $B8 $1A $38 $19 $D0 $17
.db $78 $16 $38 $15 $08 $14 $E8 $12 $D8 $11 $D8 $10 $E0 $0F $00 $0F
.db $28 $0E $5C $0D $9C $0C $E8 $0B $3C $0B $9C $0A $04 $0A $74 $09
.db $EC $08 $6C $08 $F0 $07 $80 $07 $14 $07 $AE $06 $4E $06 $F4 $05
.db $9E $05 $4E $05 $02 $05 $BA $04 $76 $04 $36 $04 $F8 $03 $C0 $03
.db $8A $03

_LABEL_DFE_:
	call _LABEL_1854_
	ld hl, _DATA_E0D_
	call _LABEL_3299_
	ld b, $C8
	call _LABEL_2271_
	ret

; Data from E0D to E5C (80 bytes)
_DATA_E0D_:
.db $F6 $09 $09 $20 $50 $52 $4F $47 $52 $41 $4D $4D $45 $44 $20 $42
.db $59 $F3 $4B $45 $56 $49 $4E $20 $50 $20 $48 $4F $4C $4C $4F $57
.db $41 $59 $F3 $F3 $20 $20 $47 $52 $41 $50 $48 $49 $43 $53 $20 $42
.db $59 $F3 $20 $4A $4F $48 $4E $20 $57 $20 $4C $49 $4C $4C $45 $59
.db $F3 $20 $20 $4A $4F $48 $4E $20 $4B $45 $52 $53 $48 $41 $57 $FF

; main_start
_LABEL_E5D_:
	ld sp, $DFC0
	ld a, $03
	ld (_RAM_C303_), a
/*
	ld hl, $0000
	ld bc, $3800
	call _LABEL_243C_	; clear all VDP tile gfx
	di
	ld a, $E0
	out (Port_VDPAddress), a	;       6: Enable display; no picture is shown when this bit is 0
					;      5: Enable VSync interrupt generation (IRQ)
	ld a, $81
	out (Port_VDPAddress), a	; reg 1
	ei
*/
_LABEL_E78_:
;	ld sp, $DFC0
	xor a
	call _LABEL_912_
	xor a
	ld (_RAM_C344_), a
	ld a, $03
	ld (_RAM_C303_), a
	call _LABEL_1ABD_
	jr c, +
_LABEL_E8D_:
	ld a, $C8
	ld (_RAM_C344_), a
+:
	ld a, (_RAM_C300_)
	ld h, a
	ld l, a
	ld (_RAM_C1EC_), hl
	call _LABEL_27F0_
	call _LABEL_10D5_
	ld a, (_RAM_D9AA_)
_LABEL_EA3_:
	ld (_RAM_D123_), a
	ld de, _DATA_5E2C_
	add a, a
	ld l, a
	ld h, $00
	add hl, de
	ld e, (hl)
	inc hl
	ld d, (hl)
	ld (_RAM_D125_), de
	ld a, (_RAM_D123_)
	cp $05
	jr z, +
	call _LABEL_18BB_
	ld hl, _DATA_3ABB8_
	call _LABEL_3299_
	ld a, (_RAM_D123_)
	add a, $1B
	call _LABEL_326F_
	call _LABEL_3C93_
	ld b, $C8
	call _LABEL_2271_
	call _LABEL_3C97_
+:
	ld hl, (_RAM_D125_)
	call _LABEL_28F4_
	xor a
_LABEL_EDF_:
	ld (_RAM_D127_), a
	ld a, $01
	ld (_RAM_C220_), a
	xor a
	ld (_RAM_D444_), a
_LABEL_EEB_:
	call _LABEL_27F0_
	call _LABEL_248D_
	call _LABEL_2A01_
	call _LABEL_3AEB_
	call _LABEL_2575_
	call _LABEL_5BAB_	
	call _LABEL_2A14_
	call _LABEL_27F0_
	call _LABEL_1104_
	call _LABEL_1501_	; !!!!!!!!!!!!!
	ld a, $01
	ld (_RAM_C346_), a
	call _LABEL_27F0_
	ld a, $03
	ld (_RAM_C303_), a
	call _LABEL_3C93_
	ld a, (_RAM_C344_)
	or a
	jr nz, _LABEL_F33_
	ld c, $01
	ld a, (_RAM_D446_)
	cp $01
	jr z, +
	inc c
	cp $02
	jr z, +
	ld c, $07
+:
	ld a, c
	call _LABEL_912_
_LABEL_F33_:
	xor a
	ld (_RAM_C9D8_), a
	call _LABEL_22C8_
	ld a, (_RAM_C344_)
	and a
	jr nz, +
	call _LABEL_16A5_
	jr ++

+:
	call _LABEL_234D_
	jp c, _LABEL_E78_
	jp nz, _LABEL_1050_
	ld hl, _RAM_C344_
	dec (hl)
	jr nz, ++
	ld a, (_RAM_D123_)
	cp $04
	jr z, +
	ld a, $C8
	ld (hl), a
	ld a, (_RAM_D123_)
	inc a
	jp _LABEL_EA3_

+:
	jp _LABEL_1050_

++:
	call _LABEL_1501_
	call _LABEL_106D_
	ld a, $01
	ld (_RAM_C346_), a
	ld a, (_RAM_C9D8_)
	bit 0, a
	jr nz, +
	bit 1, a
	jr nz, _LABEL_FF9_
	bit 3, a
	jp nz, _LABEL_4E94_
	jr _LABEL_F33_

+:
	ld a, (_RAM_D121_)
	or a
	jr z, +
	dec a
	ld (_RAM_D121_), a
	ld hl, $0000
	ld (_RAM_CEBC_), hl
	ld (_RAM_CEBE_), hl
	call _LABEL_5741_	; Possibly invalid
	ld hl, (_RAM_D138_)
	ld (_RAM_CEB8_), hl
	ld hl, (_RAM_D13A_)
	ld (_RAM_CEBA_), hl
	ld a, $6F
	ld (_RAM_CED7_), a
	call _LABEL_3C97_
	jp _LABEL_EEB_


+:
	xor a
	ld (_RAM_C22B_), a
	call _LABEL_105C_
	call _LABEL_16EB_
	ld a, (_RAM_D122_)
	or a
	jp z, _LABEL_1050_
	dec a
	ld (_RAM_D122_), a
	call _LABEL_1889_
	call _LABEL_3C93_
	ld a, $0A
-:
	push af
	dec a
	ld (_RAM_D7DD_), a
	ld a, $17
	call _LABEL_326F_
	ld hl, _RAM_D7DD_
	call _LABEL_3299_
	ld b, $32
	call _LABEL_2271_
	jr nz, +
	pop af
	dec a
	jr nz, -
	jp _LABEL_1050_

+:
	call _LABEL_3C97_
	call _LABEL_10D5_
	ld a, (_RAM_D123_)
	jp _LABEL_EA3_

_LABEL_FF9_:
	call _LABEL_3C97_
	ld hl, $0000
	ld (_RAM_D44A_), hl
	ld (_RAM_D44C_), hl
	xor a
	ld (_RAM_D446_), a
	ld (_RAM_D444_), a
	ld a, (_RAM_D437_)
	ld b, a
	ld a, (_RAM_D127_)
	inc a
	cp b
	jp c, _LABEL_EDF_
	ld a, (_RAM_D123_)
	cp $05
	jr z, +
	call _LABEL_1BC6_
	ld a, (_RAM_D123_)
	inc a
	jp _LABEL_EA3_

+:
	ld (_RAM_D9AA_), a
	ld a, (_RAM_C344_)
	and a
	jr nz, +
	xor a
	ld (_RAM_C22B_), a
	call _LABEL_1DE1_
	call _LABEL_16EB_
	call _LABEL_DFE_
	jr _LABEL_1050_

+:
	call _LABEL_17F6_
	ld hl, _RAM_C345_
	inc (hl)
	ld a, (hl)
	cp $05
	jp c, _LABEL_E8D_
	ld (hl), $00
_LABEL_1050_:
	call _LABEL_3C97_
	call _LABEL_2293_
	call _LABEL_D39_
	jp _LABEL_0_

_LABEL_105C_:
	call _LABEL_1889_
	ld a, $18
	call _LABEL_326F_
	call _LABEL_3C93_
	ld b, $96
	call _LABEL_2271_
	ret

_LABEL_106D_:
	ld hl, (_RAM_D44A_)
	ld bc, $0005
	ld a, h
	and a
	ld a, (_RAM_D445_)
	jr nz, +
	or a
	jr nz, ++++
	jr +++

+:
	or a
	jr nz, +
	ld a, (_RAM_D44F_)
	and a
	jr nz, +
	ld hl, $0006
	jr ++

+:
	sbc hl, bc
++:
	ld (_RAM_D44A_), hl
	jr ++++

+++:
	ld a, l
	and a
	jr z, ++++
	call _LABEL_4E45_	; Possibly invalid
	xor a
	ld (_RAM_D44A_), a
	ld c, a
	ld a, (_RAM_D44D_)
	and a
	jr z, +
	ld c, $03
+:
	ld a, c
	ld (_RAM_D446_), a
++++:
	ld hl, (_RAM_D44C_)
	ld a, h
	and a
	jr z, +
	ld bc, $FFFB
	add hl, bc
	ld (_RAM_D44C_), hl
	jr ++

+:
	ld a, l
	and a
	jr z, ++
	xor a
	ld (_RAM_D44C_), a
	ld (_RAM_D444_), a
	ld c, a
	ld a, (_RAM_D44B_)
	and a
	jr z, +
	ld c, $02
+:
	ld a, c
	ld (_RAM_D446_), a
++:
	ret

_LABEL_10D5_:
	xor a
	ld (_RAM_C9D8_), a
	ld a, $0F
	ld (_RAM_D124_), a
	ld hl, $0000
	ld (_RAM_C1F6_), hl
	ld (_RAM_D7C0_), hl
	ld (_RAM_D7C2_), hl
	ld (_RAM_D7C4_), hl
	ld (_RAM_D7C6_), hl
	ld a, $06
	ld (_RAM_C1FE_), a
	ld a, (_RAM_D9AC_)
	add a, a
	add a, $03
	ld (_RAM_D121_), a
	ld a, $02
	ld (_RAM_D122_), a
	ret

_LABEL_1104_:			; sprite to VDP
	ld hl, _DATA_2884_
	ld a, (hl)
	inc hl
	ld (_RAM_D169_), a
	di
	ld a, $00
	out (Port_VDPAddress), a
	ld a, $7F
	out (Port_VDPAddress), a
	ei
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld b, (hl)
-:
	nop
	nop
	ld a, (de)
	inc de
	out (Port_VDPData), a
	djnz -
	di
	ld a, $80
	out (Port_VDPAddress), a
	ld a, $7F
	out (Port_VDPAddress), a
	ei
	ld b, (hl)
-:
	ld a, (de)
	inc de
	nop
	out (Port_VDPData), a
	push af
	pop af
	ld a, (de)
	inc de
	out (Port_VDPData), a
	djnz -
	ld b, $00
	ld c, (hl)
	ld hl, $7F00
	add hl, bc
	ld (_RAM_D7D6_), hl
	ld hl, $7F80
	add hl, bc
	add hl, bc
	ld (_RAM_D7D8_), hl
	ld a, $01
	ld (_RAM_C9DB_), a
	ld c, $04
	ld a, $0A
	call _LABEL_14D9_
	ld hl, $FFFF
	ld (_RAM_D7C9_), hl
	ld (_RAM_D7CB_), hl
	ld (_RAM_D7CD_), hl
	ld (_RAM_D7CF_), hl
	ld a, $FF
	ld (_RAM_D7D3_), a
	ld (_RAM_D7D2_), a
	ld (_RAM_D7D4_), a
	ld (_RAM_D7D5_), a
	ret

; Data from 1177 to 13F6 (640 bytes)
_DATA_1177_:
.db $3C $00 $00 $3C $18 $66 $00 $66 $77 $66 $66 $66 $77 $66 $66 $66
.db $22 $CC $00 $CC $22 $CC $00 $CC $7E $00 $00 $78 $3C $00 $00 $00
.db $1E $00 $00 $1C $06 $38 $00 $38 $3C $18 $18 $18 $1C $18 $18 $18
.db $08 $30 $00 $30 $08 $30 $00 $30 $FC $00 $00 $FC $7E $00 $00 $00
.db $3C $00 $00 $38 $32 $4C $00 $4C $2E $0C $0C $0C $1C $18 $18 $18
.db $0C $30 $00 $30 $18 $60 $00 $60 $FC $00 $00 $FC $7E $00 $00 $00
.db $7C $00 $00 $7C $32 $0C $00 $0C $1E $18 $18 $18 $38 $30 $30 $30
.db $00 $38 $00 $38 $04 $18 $00 $18 $FC $00 $00 $F8 $7C $00 $00 $00
.db $0E $00 $00 $0E $02 $1C $00 $1C $3E $24 $24 $24 $7E $4C $4C $4C
.db $04 $F8 $00 $F8 $04 $F8 $00 $F8 $7C $00 $00 $18 $0C $00 $00 $00
.db $7C $00 $00 $7C $1C $60 $00 $60 $F0 $F0 $F0 $F0 $F8 $F8 $F8 $F8
.db $64 $18 $00 $18 $04 $38 $00 $38 $FC $00 $00 $F0 $78 $00 $00 $00
.db $38 $00 $00 $38 $1C $60 $00 $60 $78 $78 $78 $78 $FC $CC $CC $CC
.db $22 $CC $00 $CC $22 $DC $00 $DC $FE $00 $00 $F8 $7C $00 $00 $00
.db $FC $00 $00 $FC $72 $0C $00 $0C $1E $18 $18 $18 $3C $30 $30 $30
.db $18 $60 $00 $60 $10 $E0 $00 $E0 $F0 $00 $00 $C0 $60 $00 $00 $00
.db $3C $00 $00 $3C $18 $66 $00 $66 $7F $6E $6E $6E $FF $FC $FC $FC
.db $32 $CC $00 $CC $22 $CC $00 $CC $7E $00 $00 $78 $3C $00 $00 $00
.db $3C $00 $00 $3C $18 $64 $00 $64 $FE $CC $CC $CC $FE $FC $FC $FC
.db $02 $7C $00 $7C $24 $18 $00 $18 $7C $00 $00 $70 $38 $00 $00 $00
.db $00 $00 $00 $6C $00 $00 $00 $FE $01 $00 $00 $FE $01 $00 $00 $FE
.db $03 $00 $00 $7C $06 $00 $00 $38 $0C $00 $00 $10 $08 $00 $00 $00
.db $00 $00 $00 $00 $00 $78 $78 $78 $74 $F8 $F8 $FC $77 $F8 $F8 $FE
.db $77 $F8 $F8 $FC $06 $78 $78 $78 $7C $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $78 $78 $78 $04 $80 $80 $84 $07 $80 $80 $86
.db $07 $80 $80 $84 $06 $78 $78 $78 $7C $00 $00 $00 $00 $00 $00 $00
.db $28 $2C $28 $34 $3C $00 $00 $3C $24 $52 $00 $5A $E7 $91 $81 $FF
.db $66 $B5 $2C $F3 $CD $08 $0A $70 $D5 $50 $32 $0C $EE $4C $4C $F7
.db $6C $00 $6C $00 $BE $40 $FE $00 $FF $00 $FE $00 $FF $00 $FE $00
.db $7F $00 $7C $00 $3E $00 $38 $00 $1C $00 $10 $00 $08 $00 $00 $00
.db $EE $7C $6C $6C $FF $FE $FE $FE $DF $FE $DE $DE $6D $FE $6C $6C
.db $EF $FE $EE $EE $FF $FE $FE $FE $EF $7C $6C $6C $7E $00 $00 $00
.db $82 $7C $6C $6C $01 $FE $FE $FE $01 $FE $DE $DE $01 $FE $6C $6C
.db $01 $FE $EE $EE $01 $FE $FE $FE $83 $7C $6C $6C $7E $00 $00 $00
.db $1B $00 $00 $13 $09 $76 $00 $76 $36 $24 $24 $24 $36 $24 $24 $24
.db $13 $24 $00 $24 $12 $25 $00 $25 $F7 $00 $00 $F7 $7B $00 $00 $00
.db $9C $00 $00 $9C $08 $B4 $00 $B4 $F6 $A4 $A4 $A4 $F6 $A4 $A4 $A4
.db $18 $A4 $00 $A4 $10 $AC $00 $AC $BC $00 $00 $38 $9C $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00


_LABEL_13F7_:
	ld a, (_RAM_C9DB_)
	or a
	ret z
	ld de, _RAM_D7C1_
	ld hl, _RAM_D7CA_
	ld c, $00
-:
	ld a, (de)
	cp (hl)
	jr z, +
	ld (hl), a
	push hl
	push de
	push bc
	call _LABEL_14D9_
	pop bc
	pop de
	pop hl
+:
	inc de
	inc hl
	inc c
	ld a, c
	cp $04
	jr nz, -
	ld a, (_RAM_D121_)
	ld hl, _RAM_D7D2_
	cp (hl)
	jr z, +
	ld (hl), a
	ld c, $05
	call _LABEL_14D9_
+:
	ld hl, _RAM_D447_
	ld a, (_RAM_D446_)
	or a
	jr nz, ++
	ld a, (_RAM_D44E_)
	or a
	jr z, ++
	ld a, (_RAM_C300_)
	and $08
	jr z, +
	ld a, (hl)
	jr +++

+:
	ld a, $13
	ld (_RAM_D82E_), a
	ld (_RAM_D82F_), a
	jr ++++

++:
	sla a
	ld d, $00
	ld e, a
	add hl, de
	ld a, (hl)
	ld hl, _RAM_D7D5_
	cp (hl)
	jr z, +++++
	ld (hl), a
+++:
	cp $64
	jr c, ++
	cp $80
	jr c, +
	xor a
	jr ++

+:
	ld a, $11
	ld (_RAM_D82E_), a
	ld a, $12
	ld (_RAM_D82F_), a
	jr ++++

++:
	ld l, a
	ld h, $00
	call _LABEL_1987_
++++:
	ld c, $06
	ld a, (_RAM_D82E_)
	cp $5C
	jr nz, +
	xor a
+:
	call _LABEL_14D9_
	ld c, $07
	ld a, (_RAM_D82F_)
	call _LABEL_14D9_
+++++:
	ld a, (_RAM_D446_)
	ld hl, _RAM_D7D4_
	cp (hl)
	jr z, +
	ld (hl), a
	ld c, $0E
	add a, $0D
	call _LABEL_14D9_
+:
	ld a, (_RAM_CED7_)
	srl a
	srl a
	srl a
	srl a
	cp $07
	jr c, +
	xor a
+:
	ld hl, _RAM_D7D3_
	cp (hl)
	jr z, +
	ld (hl), a
	ld b, a
	ld c, $08
-:
	ld a, c
	sub $08
	cp b
	jr z, _LABEL_14C7_
	push bc
	ld a, $0B
	call _LABEL_14D9_
	pop bc
	inc c
	jr -

_LABEL_14C7_:
	ld a, c
	sub $08
	cp $06
	jr z, +
	push bc
	ld a, $0C
	call _LABEL_14D9_
	pop bc
	inc c
	jr _LABEL_14C7_

+:
	ret

_LABEL_14D9_:
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, _DATA_1177_
	add hl, de
	ld de, $2000
	push hl
	ld h, c
	ld l, $00
	sra h
	rr l
	sra h
	rr l
	sra h
	rr l
	add hl, de
	pop de
	ld bc, $0020
	call _LABEL_2451_
	ret

_LABEL_1501_:
	ld a, ($0016)
	ld e, a
	ld a, $06
	or e
	call set_page2
	xor a
	ld (_RAM_C1D0_), a
	ld hl, _RAM_D12A_
	inc (hl)
	ld hl, (_RAM_D3F7_)
	ld (_RAM_D405_), hl
	ld hl, (_RAM_D3F9_)
	ld (_RAM_D407_), hl
	ld a, $FF
	ld (_RAM_C100_), a
	ld a, $01
	ld (_RAM_C1CF_), a
	ld hl, _RAM_C9DC_
	ld bc, $002E
_LABEL_152F_:
	ld a, (hl)
	cp $FF
	jp z, _LABEL_15C8_
	or a
	jr nz, +
	add hl, bc
	jr _LABEL_152F_

+:
	ld (_RAM_D11F_), hl
	push hl
	pop ix
	inc hl
	inc hl
	ld de, _RAM_D10F_
	ld bc, $0010
	ldir
	xor a
	ld (_RAM_C2D0_), a
	bit 5, (ix+20)
	call nz, _LABEL_25B2_
	call _LABEL_1639_
	ld a, (ix+1)
	and a
	jr z, +
	ld hl, _RAM_C1D0_
	inc (hl)
	ld a, (hl)
	and $03
	ld c, a
	ld a, (_RAM_D12A_)
	and $03
	cp c
	jr nz, ++
+:
	bit 1, (ix+20)
	call nz, _LABEL_26B6_
++:
	bit 2, (ix+20)
	jr z, ++
	ld b, (ix+35)
	ld hl, _RAM_C100_
-:
	ld c, l
	ld l, (hl)
	inc l
	jr z, +
	ld a, (hl)
	dec l
	cp b
	jr nc, -
+:
	ld l, c
	ld c, (hl)
	ld a, (_RAM_C1CF_)
	ld (hl), a
	ld l, a
	add a, $04
	ld (_RAM_C1CF_), a
	ld (hl), c
	inc l
	ld (hl), b
	inc l
	ld de, (_RAM_D11F_)
	ld (hl), e
	inc l
	ld (hl), d
++:
	ld a, (ix+1)
	and a
	jr z, +
	bit 5, (ix+20)
	call nz, _LABEL_2623_
+:
	ld hl, _RAM_D10F_
	ld de, (_RAM_D11F_)
	inc de
	inc de
	ld bc, $0010
	ldir
	ld hl, (_RAM_D11F_)
	ld bc, $002E
	add hl, bc
	jp _LABEL_152F_

_LABEL_15C8_:
	ld a, (_RAM_C346_)
	and a
	jr z, +
	call _LABEL_27F0_
+:
	call _LABEL_3CC5_
	call _LABEL_13F7_
	ld hl, (_RAM_D3F7_)
	ld (_RAM_D3FB_), hl
	ld hl, (_RAM_D3F9_)
	ld (_RAM_D3FD_), hl
	ld a, (_RAM_D417_)
	ld (_RAM_D418_), a
	ld hl, (_RAM_D40E_)
	ld (_RAM_D411_), hl
	ld a, (_RAM_D9A0_)
	ld (_RAM_D413_), a
	ld a, (_RAM_D99F_)
	ld (_RAM_D40C_), a
	ld hl, _RAM_C8D6_
	ld (_RAM_C8D2_), hl
	ld (hl), $D0
	ld hl, _RAM_C92A_
	ld (_RAM_C8D4_), hl
	ld hl, _RAM_C100_
-:
	ld a, (hl)
	cp $FF
	jr z, +
	ld l, a
	push hl
	inc l
	inc l
	ld e, (hl)
	inc l
	ld d, (hl)
	push de
	pop ix
	ld e, (ix+42)
	ld d, $00
	ld hl, _DATA_162F_
	add hl, de
	ld e, (hl)
	inc hl
	ld d, (hl)
	ex de, hl
	call _LABEL_16A4_
	pop hl
	jr -

+:
	ret

; Jump Table from 162F to 1638 (5 entries, indexed by  unknown)
_DATA_162F_:
.dw _LABEL_3471_ _LABEL_3716_ _LABEL_3588_ _LABEL_36D9_ _LABEL_3552_

_LABEL_1639_:
	res 7, (ix+20)
	ld iy, (_RAM_D11B_)
-:
	ld a, (iy+0)
	inc iy
	cp $00
	jr z, +
	ld e, (iy+0)
	ld d, (iy+1)
	ld c, a
	ld b, $00
	ld hl, $3B42	; !!!!!
	add hl, bc
	add hl, bc
	ld c, (hl)
	inc hl
	ld a, (hl)
	and $1F
	ld b, a
	xor (hl)
	ld hl, $3B42	; ??????
	add hl, bc
	rlca
	rlca
	rlca
	push af
	call _LABEL_16A4_
	pop af
	ld e, a
	ld d, $00
	add iy, de
	jr -

+:
	push iy
	pop hl
	ld a, (ix+33)
	or a
	jp p, +
	ld (ix+33), $00
	ld a, (ix+32)
	or a
	jr nz, ++
	ld hl, _DATA_19D30_
	jr +++

+:
	push hl
	call _LABEL_39D0_
	pop hl
	jr nz, +++
	ld a, (ix+41)
	or a
	jr nz, ++
	ld hl, _DATA_19D30_
	jr +++

++:
	call _LABEL_24E4_
+++:
	ld (_RAM_D11B_), hl
	ret

_LABEL_16A4_:
	jp (hl)

_LABEL_16A5_:
	ld a, (_RAM_C1E9_)
	and a
	ret nz
	call +
	ret z
	ld e, $17
	call _LABEL_24FE_
	ret c
	ld a, $01
	ld (_RAM_C1E9_), a
	xor a
	ld (_RAM_C22B_), a
	ret

-:
	call _LABEL_27F0_
	call +
	jr z, -8
	ret

_LABEL_16C7_:
+:
	ld a, (_RAM_C2CF_)
	ld e, a
	xor a
	ld (_RAM_C2CF_), a
	ld a, (_RAM_C1EB_)
	xor $80
	ld d, a
	ld a, e
	ld (_RAM_C1EB_), a
	and d
	ld (_RAM_C1EA_), a
	ret


_LABEL_16DE_:
	xor a
	ld (_RAM_C2CF_), a
	ld a, (_RAM_C1D8_)
	ld e, a
	ld a, (_RAM_C1DC_)
	or e
	ret

_LABEL_16EB_:
	call _LABEL_19C5_
	ld hl, _RAM_D7BB_
	ld b, $06
	ld de, $0005
-:
	push hl
	pop ix
	ld a, (_RAM_C1F7_)
	cp (ix+4)
	jr c, ++
	jr nz, +
	ld a, (_RAM_C1F6_)
	cp (ix+3)
	jr c, ++
	jr z, ++
+:
	and a
	sbc hl, de
	djnz -
++:
	ld a, b
	cp $06
	ret z
	ld (_RAM_D7F4_), a
	add hl, de
	push hl
	pop ix
	ld de, $D7BB
	ex de, hl
	and a
	sbc hl, de
	jr z, +
	ld b, h
	ld c, l
	ld hl, _RAM_D7BA_
	ld de, _RAM_D7BF_
	lddr
+:
	ld a, (_RAM_C1F7_)
	ld (ix+4), a
	ld a, (_RAM_C1F6_)
	ld (ix+3), a
	push ix
	call +
_LABEL_1741_:
	pop ix
	ld hl, $D80C
	ld b, $03
-:
	inc hl
	inc hl
	ld a, (hl)
	inc hl
	ld (ix+0), a
	inc ix
	djnz -
	call _LABEL_17F6_
	ret

+:
	ld a, $06
	call _LABEL_912_
	call _LABEL_1889_
	call _LABEL_3C93_
	ld a, $1A
	call _LABEL_326F_
	ld hl, _RAM_D807_
	ld b, $03
	call _LABEL_1770_
	ret

_LABEL_1770_:
	ld c, b
	push ix
	ld (_RAM_C1EE_), hl
	ld de, $0005
	add hl, de
	push hl
	pop ix
_LABEL_177D_:
	push bc
	ld (ix+1), $08
	ld hl, (_RAM_C1EE_)
	call _LABEL_3299_
	call _LABEL_27F0_
	call _LABEL_27F0_
	call _LABEL_27F0_
	call _LABEL_27F0_
	ld (ix+1), $00
	ld hl, (_RAM_C1EE_)
	call _LABEL_3299_
	call _LABEL_27F0_
	call _LABEL_27F0_
	call _LABEL_22C8_
	pop bc
	ld de, $0003
	ld a, (_RAM_C1D8_)
	ld l, a
	ld a, (_RAM_C1DC_)
	or l
	jr nz, ++++
	ld a, (_RAM_C1D6_)
	and a
	jp p, +
	ld a, b
	cp c
	jr z, +
	dec ix
	dec ix
	dec ix
	inc b
	jr +++

+:
	ld a, (_RAM_C1D5_)
	and a
	jr z, +++
	add a, (ix+2)
	cp $41
	jr nc, +
	ld a, $5C
	jr ++

+:
	cp $5D
	jr c, ++
	ld a, $41
++:
	ld (ix+2), a
	ld a, $0B
	ld (_RAM_C242_), a
+++:
	jr _LABEL_177D_

++++:
	add ix, de
	ld a, $16
	ld (_RAM_C242_), a
	djnz _LABEL_177D_
	pop ix
	ret

_LABEL_17F6_:
	call _LABEL_3C97_
	call _LABEL_2293_
	ld a, $19
	call _LABEL_326F_
	call _LABEL_3C93_
	ld b, $06
	ld ix, _RAM_D7A2_

-:
	push bc
	ld a, $07
	sub b
	ld (_RAM_D7FB_), a
	add a, a
	add a, $0A
	ld (_RAM_D7F8_), a
	push ix
	pop hl
	ld de, $0000
	add hl, de
	ld de, _RAM_D7FF_
	ld bc, $0003
	ldir
	ld hl, _RAM_D7F6_
	call _LABEL_3299_
	ld l, (ix+3)
	ld h, (ix+4)
	call _LABEL_197D_
	ld b, $05
	call _LABEL_2271_
	pop bc
	jr nz, +
	ld de, $0005
	add ix, de
	push bc
	call _LABEL_27F0_
	pop bc
	djnz -
	ld b, $FA
	call _LABEL_2271_
	ld hl, _RAM_C22B_
	ld (hl), $00
+:
	ret

_LABEL_1854_:
	call _LABEL_3C97_
	call _LABEL_2293_
	ld a, $01
	call _LABEL_24BB_
	ld a, ($0016)
	ld e, a
	ld a, $08
	or e
	call set_page2
	ld hl, $0000
	ld de, _DATA_22DBD_
	ld bc, $03E0
	ld (_RAM_C2FD_), bc
	call _LABEL_2451_
	ld de, _DATA_22D72_
	ld bc, $040A
	call _LABEL_190D_
	call _LABEL_340A_
	call _LABEL_3C93_
	ret


_LABEL_1889_:
	call _LABEL_3C97_
	call _LABEL_2293_	; cls
	ld a, $12
	call _LABEL_24BB_
	ld a, ($0016)
	ld e, a
	ld a, $08
	or e
	call set_page2
	ld hl, $0000
	ld de, _DATA_2326B_
	ld bc, $0BE0
	ld (_RAM_C2FD_), bc
	call _LABEL_2451_	; send gfx
	ld de, _DATA_2319D_
	ld bc, $0307
	call _LABEL_190D_
	call _LABEL_340A_
	ret

_LABEL_18BB_:
	call _LABEL_3C97_
	call _LABEL_2293_
	ld a, $12
	call _LABEL_24BB_
	ld a, ($0016)
	ld e, a
	ld a, $0F
	or e
	call set_page2
	ld hl, $0000
	ld de, _DATA_3C822_
	ld bc, $2240
	ld (_RAM_C2FD_), bc
	call _LABEL_2451_
	ld de, _DATA_3C5C8_
	ld bc, $0305
	call _LABEL_190D_
	ld a, ($0016)
	ld e, a
	ld a, $0E
	or e
	call set_page2
	call _LABEL_340A_
	ret

_LABEL_18F7_:
	call _LABEL_3C97_
	call _LABEL_2293_	;clear screens
	ld a, $12
	call _LABEL_24BB_
	ld bc, $0020
	ld (_RAM_C2FD_), bc
	call _LABEL_340A_	; send gfx to vdp
	ret

_LABEL_190D_:
	push ix
	ld h, $00
	ld l, b
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld b, $3C
	add hl, bc
	add hl, hl
	push de
	pop ix
	ld e, (ix+0)
	ld a, e
	ld (_RAM_C1FA_), a
	inc ix
	ld d, (ix+0)
	inc ix

	ld a,l
	ld (tilemap_adr),a
	ld a,h
	add a,$80		; = #f8
	ld (tilemap_adr+1),a
/*
	di
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	out (Port_VDPAddress), a
	ei
*/
--:
	ld b, $01
	ld a, (ix+0)
	inc ix
	cp $80
	jr c, +
	and $7F
	ld b, a
	ld a, (ix+0)
	inc ix
+:
	ld c, a
-:
	ld a, (ix+0)
;	out (Port_VDPData), a
	call update_tilemem
;	push af
;	pop af
	ld a, c
;	out (Port_VDPData), a
	call update_tilemem
	dec e
	jr nz, +
	dec d
	jr z, ++
	ld a, d
	ld de, $0040
	add hl, de
	ld d, a
	ld a, (_RAM_C1FA_)
	ld e, a
/*
	di
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	out (Port_VDPAddress), a
	ei
*/
	ld a,l
	ld (tilemap_adr),a
	ld a,h
	add a,$80		; = #f8
	ld (tilemap_adr+1),a
+:
	djnz -
	inc ix
	jr --

++:
	pop ix
	ret

; Data from 1973 to 197C (10 bytes)
_DATA_1973_:
.db $10 $27 $E8 $03 $64 $00 $0A $00 $01 $00


_LABEL_197D_:
	call _LABEL_1987_
	ld hl, _RAM_D82B_
	call _LABEL_3299_
	ret

_LABEL_1987_:
	push ix
	push iy
	ld ix, _RAM_D82B_
	ld iy, _DATA_1973_
	ld b, $05
--:
	ld e, (iy+0)
	ld d, (iy+1)
	ld c, $00
-:
	xor a
	sbc hl, de
	jr c, +
	inc c
	jr -

+:
	add hl, de
	inc iy
	inc iy
	ld (ix+0), c
	inc ix
	djnz --
	ld b, $04
	ld hl, _RAM_D82B_
	ld a, $00
-:
	cp (hl)
	jr nz, +
	ld (hl), $5C
	inc hl
	djnz -
+:
	pop iy
	pop ix
	ret

_LABEL_19C5_:
	ld iy, _RAM_D7C0_
	ld ix, _DATA_1973_
	ld hl, $0000
	ld b, $05
--:
	ld e, (ix+0)
	ld d, (ix+1)
	ld a, (iy+0)
	or a
	jr z, +
-:
	add hl, de
	dec a
	jr nz, -
+:
	inc iy
	inc ix
	inc ix
	djnz --
	ld de, (_RAM_C1F6_)
	add hl, de
	ld (_RAM_C1F6_), hl
	ld ix, _RAM_D7C0_
	ld iy, _DATA_1973_
	ld b, $05
--:
	ld e, (iy+0)
	ld d, (iy+1)
	ld c, $00
-:
	xor a
	sbc hl, de
	jr c, +
	inc c
	jr -

+:
	add hl, de
	inc iy
	inc iy
	ld (ix+0), c
	inc ix
	djnz --
	ret

_DATA_1a19_:
; Data from 1A19 to 1A3D (37 bytes)
.db $F6 $06 $03 $20 $20 $4F $54 $54 $49 $46 $41 $4E $54 $20 $41 $4E
.db $44 $20 $41 $4C $4C $F3 $52 $45 $4C $41 $54 $45 $44 $20 $45 $4C
.db $45 $4D $45 $4E $54

; Data from 1A3E to 1A81 (68 bytes)
.db $53 $20 $41 $52 $45 $F3 $20 $20 $54 $48 $45 $20 $50 $52 $4F $50
.db $45 $52 $54 $59 $20 $4F $46 $F3 $20 $20 $20 $20 $20 $4F $54 $54
.db $49 $46 $41 $4E $54 $F3 $20 $20 $50 $52 $4F $44 $55 $43 $54 $49
.db $4F $4E $53 $20 $49 $4E $43 $5B $F3 $20 $20 $5D $20 $31 $39 $39
.db $33 $20 $4F $54

; Data from 1A82 to 1ABC (59 bytes)
.db $54 $49 $46 $41 $4E $54 $F3 $20 $20 $20 $20 $20 $50 $52 $4F $44
.db $5B $20 $49 $4E $43 $5B $F3 $20 $20 $20 $20 $5D $20 $31 $39 $39
.db $33 $20 $53 $45 $47 $41 $F3 $41 $4C $4C $20 $52 $49 $47 $48 $54
.db $53 $20 $52 $45 $53 $45 $52 $56 $45 $44 $FF


_LABEL_1ABD_:			; $1926 - intro text
	xor a
	ld (_RAM_D9AA_), a
	call _LABEL_18F7_	; send gfx to vdp
	ld hl, _DATA_1a19_
	call _LABEL_3299_	; print text
	call _LABEL_3C93_
	ld b, $C8
	call _LABEL_2269_	; wait
	ld a, (_RAM_D9B0_)
	and a
	jr nz, _LABEL_1AF1_
	call _LABEL_1889_
	ld a, $FE
	ld hl, _DATA_1D51_
	call _LABEL_1C01_
	jr nc, +
	ld a, $FF
	ld (_RAM_D9B0_), a
+:
	ld a, (_RAM_D9A7_)
	add a, a
	ld (_RAM_D9A8_), a
_LABEL_1AF1_:
	call _LABEL_1889_
	ld a, $C8
	ld hl, _DATA_1D70_
	call _LABEL_1C01_
	jp nc, _LABEL_1B8B_
	ld a, (_RAM_D9A1_)
	dec a
	jr z, ++
	dec a
	jr nz, _LABEL_1B83_
	call _LABEL_1889_
	ld a, $FF
	ld hl, _DATA_1D83_
	call _LABEL_1C01_
	ld a, (_RAM_D9A9_)
	and a
	jr z, +
	xor a
	ld (_RAM_D9AA_), a
	jr _LABEL_1AF1_

+:
	ld a, $FF
	ld hl, _DATA_1DC8_
	call _LABEL_1BED_
	ld a, (_RAM_D9A1_)
	ld (_RAM_D9AA_), a
	jr _LABEL_1AF1_

++:
	call _LABEL_1889_
	ld a, $21
	call _LABEL_326F_
	call _LABEL_3C93_
	ld b, $04
	ld hl, _RAM_D816_
	call _LABEL_1770_
	ld ix, _DATA_1BAD_
	ld b, $05
-:
	ld a, (_RAM_D81D_)
	cp (ix+0)
	jr nz, +
	ld a, (_RAM_D820_)
	cp (ix+1)
	jr nz, +
	ld a, (_RAM_D823_)
	cp (ix+2)
	jr nz, +
	ld a, (_RAM_D826_)
	cp (ix+3)
	jr z, ++
+:
	ld de, $0005
	add ix, de
	djnz -
	jr _LABEL_1AF1_

++:
	ld a, $06
	sub b
	cp $05
	jr z, +
	ld (_RAM_D9AA_), a
	ld a, $17
	call _LABEL_4053_	; Possibly invalid
	jp _LABEL_1AF1_

_LABEL_1B83_:
	ld hl, _DATA_1D4B_
	call _LABEL_3299_
	scf
	ret

_LABEL_1B8B_:
	xor a
	ret

+:
	ld hl, _RAM_D7DF_
	call _LABEL_3299_
	ld a, $01
	ld (_RAM_D12E_), a
	ld c, $03
--:
	ld a, $15
	ld (_RAM_C242_), a
	ld e, $14
-:
	call _LABEL_27F0_
	dec e
	jr nz, -
	dec c
	jr nz, --
	jp _LABEL_1AF1_

_DATA_1BAD_
; Data from 1BAD to 1BC5 (25 bytes)
.db $4A $59 $53 $46 $FF $41 $4F $48 $54 $FF $50 $49 $48 $45 $FF $4E
.db $52 $43 $46 $FF $4E $49 $44 $54 $FF


_LABEL_1BC6_:
	ld a, (_RAM_D123_)
	cp $04
	ret nc
	ld c, a
	add a, a
	add a, a
	add a, c
	ld e, a
	ld d, $00
	ld hl, _DATA_1BAD_
	add hl, de
	push hl
	call _LABEL_18BB_
	ld a, $22
	call _LABEL_326F_
	pop hl
	call _LABEL_3299_
	call _LABEL_3C93_
	ld b, $C8
	call _LABEL_2271_
	ret

_LABEL_1BED_:
	ld (_RAM_D12A_), a
	ld (_RAM_D9A2_), hl
	ld (_RAM_D9A4_), hl
	call _LABEL_1889_
	ld a, $25
	call _LABEL_326F_
	jp +

_LABEL_1C01_:
	ld (_RAM_D12A_), a
	ld (_RAM_D9A2_), hl
	ld (_RAM_D9A4_), hl
+:
	call _LABEL_3C93_
	xor a
	ld (_RAM_D9A1_), a
_LABEL_1C11_:
	call _LABEL_27F0_
	call _LABEL_27F0_
	ld hl, _RAM_D12A_
	ld a, (hl)
	cp $FF
	jr z, +
	dec (hl)
	jp z, _LABEL_1D11_
+:
	call _LABEL_22C8_
	ld a, (_RAM_C1D3_)
	ld b, a
	ld a, (_RAM_C1D5_)
	ld c, a
	ld a, (_RAM_C1D6_)
	or c
	ld (_RAM_C1D3_), a
	ld a, b
	ld (_RAM_C1D4_), a
	and a
	jr nz, ++
	ld a, (_RAM_C1D5_)
	and a
	jr z, ++
	ld c, a
	ld hl, (_RAM_D9A2_)
	ld de, $0006
	cp $80
	jr c, +
	ld a, (hl)
	and a
	jr z, ++
	ld de, $FFFA
+:
	add hl, de
	ld a, (hl)
	cp $FF
	jr z, ++
	ld (_RAM_D9A2_), hl
	ld a, (_RAM_D9A1_)
	add a, c
	ld (_RAM_D9A1_), a
	ld e, $1E
	call _LABEL_4053_	; Possibly invalid
++:
	ld iy, (_RAM_D9A4_)
_LABEL_1C6D_:
	ld hl, _DATA_1D4B_
	ld a, (_RAM_D9A1_)
	cp (iy+0)
	jr nz, +
	ld hl, _DATA_1D4E_
+:
	call _LABEL_3299_
	ld a, (iy+1)
	call _LABEL_326F_
	ld e, (iy+2)
	ld d, $00
	ld hl, _RAM_D9A6_
	add hl, de
	ex de, hl
	ld l, (iy+4)
	ld h, (iy+5)
	ld a, l
	or h
	jr z, _LABEL_1CD8_
	ld a, (_RAM_D9A1_)
	cp (iy+0)
	jr nz, +
	ld a, (_RAM_C1D4_)
	and a
	jr nz, +
	ld a, (_RAM_C1D6_)
	and a
	jr z, +
	ld b, a
	ld a, (de)
	add a, b
	cp (hl)
	jr nc, +
	ld (de), a
	push de
	ld e, $1E
	call _LABEL_4053_	; Possibly invalid
	pop de
+:
	inc hl
	ld a, (hl)
	cp $FF
	jr nz, +
	ld a, (de)
	ld l, a
	ld h, $00
	call _LABEL_1987_
	ld hl, _RAM_D82E_
	call _LABEL_3299_
	jr _LABEL_1CD8_

+:
	ld a, (de)
	ld c, a
	ld b, $00
	add hl, bc
	ld a, (hl)
	call _LABEL_326F_
_LABEL_1CD8_:
	ld a, (_RAM_D9A1_)
	cp (iy+0)
	jr nz, +
	call _LABEL_16DE_
	jr z, +
	ld e, (iy+2)
	ld d, $00
	ld hl, $D9A6
	add hl, de
	ex de, hl
	ld c, (iy+3)
	ld hl, _data_1D14_
	ld b, $00
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	call +++
	jr c, ++
+:
	ld de, $0006
	add iy, de
	ld a, (iy+0)
	cp $FF
	jp nz, _LABEL_1C6D_
	jp _LABEL_1C11_

_LABEL_1D11_:
	ccf
++:
	ret

+++:
	jp (hl)

_data_1D14_:
; Data from_data_1D14 to 1D23 (16 bytes)_
.db $2F $1D $20 $1D $24 $1D $2B $1D $31 $1D $2F $1D $3A $A1 $D9 $12

+:
	ld e, $1F
	call _LABEL_4053_	; Possibly invalid
	scf
	ret

_LABEL_1D2B_:
	ld a, (de)
	call _LABEL_912_
	xor a
	ret

_LABEL_1D31_:
	ld a, (de)
	add a, $0F
	cp $13
	jr z, +
	cp $1B
	jr z, +
	cp $1D
	jr nz, ++
+:
	add a, $80
++:
	ld e, a
	call _LABEL_4053_	; Possibly invalid
	xor a
	ld (_RAM_C22B_), a
	ret


; Data from 1D4B to 1D4D (3 bytes)
_DATA_1D4B_:
.db $F4 $00 $FF

; Data from 1D4E to 1D50 (3 bytes)
_DATA_1D4E_:
.db $F4 $08 $FF

; Data from 1D51 to 1D6F (31 bytes)
_DATA_1D51_:
.db $00 $00 $01 $02 $00 $00 $01 $01 $01 $02 $00 $00 $02 $02 $01 $02
.db $00 $00 $03 $03 $01 $02 $00 $00 $04 $04 $01 $02 $00 $00 $FF

; Data from 1D70 to 1D82 (19 bytes)
_DATA_1D70_:
.db $00 $05 $00 $04 $00 $00 $01 $06 $00 $04 $00 $00 $02 $07 $00 $04
.db $00 $00 $FF

; Data from 1D83 to 1DBD (59 bytes)
_DATA_1D83_:
.db $00 $08 $06 $04 $AD $1D $01 $09 $07 $06 $B2 $1D $02 $0A $08 $08
.db $B4 $1D $03 $0B $05 $04 $B6 $1D $04 $0C $03 $04 $A8 $1D $05 $29
.db $09 $04 $B9 $1D $FF $04 $0D $0E $0F $10 $04 $11 $12 $13 $14 $08
.db $FF $11 $FF $02 $15 $16 $04 $2A $2B $2C $2D

; Pointer Table from 1DBE to 1DC7 (5 entries, indexed by $D9A8)
_DATA_1DBE_:
.dw _DATA_3ABBC_ _DATA_3AEBF_ _DATA_3B151_ _DATA_3B3F9_ _DATA_3B6C5_

_DATA_1DC8_:
; Data from 1DC8 to 1DE0 (25 bytes)
.db $00 $26 $00 $04 $DB $1D $01 $27 $00 $04 $DD $1D $02 $28 $00 $04
.db $DF $1D $FF $01 $1B $01 $1C $01 $1D


_LABEL_1DE1_:
	call _LABEL_18F7_
	ld a, (_RAM_D124_)
	or a
	jr z, +
	ld a, $23
	jr ++

+:
	ld a, $24
++:
	call _LABEL_326F_
	call _LABEL_3C93_
	ld b, $C8
	call _LABEL_2271_
	ret

_LABEL_1DFC_:
	ld a, (_RAM_D438_)
	cp $02
	jr z, _LABEL_1E42_
	cp $07
	jr z, _LABEL_1E42_
	ld a, (_RAM_D156_)
	bit 0, a
	jr z, +
	ld hl, (_RAM_D15B_)
	ld (_RAM_D111_), hl
	ld hl, (_RAM_D113_)
	ld bc, (_RAM_D157_)
	add hl, bc
	ld (_RAM_D41A_), hl
	ld hl, (_RAM_D115_)
	ld (_RAM_D41C_), hl
	jr ++

+:
	bit 1, a
	jr z, _LABEL_1E42_
	ld hl, (_RAM_D15D_)
	ld (_RAM_D10F_), hl
	ld hl, (_RAM_D15B_)
	ld (_RAM_D111_), hl
	ld hl, $0000
	ld (_RAM_D41A_), hl
	ld (_RAM_D41C_), hl
	jr ++

_LABEL_1E42_:
	ld hl, (_RAM_D113_)
	ld (_RAM_D41A_), hl
	ld hl, (_RAM_D115_)
	ld (_RAM_D41C_), hl
++:
	ld hl, $0000
	ld a, (_RAM_C1FF_)
	and a
	jr z, +
	ld hl, $0100
+:
	ld de, $0280
	ld bc, $017E

_LABEL_1E60_:
	xor a
	ld (_RAM_C1FB_), a
	ld (_RAM_D14A_), bc
	ld (_RAM_D14C_), de
	ld (_RAM_D152_), hl
	ld hl, (_RAM_D111_)
	ld (_RAM_D150_), hl
	ld bc, (_RAM_D41C_)
	add hl, bc
	ld (_RAM_D111_), hl
	ld hl, (_RAM_D10F_)
	ld bc, (_RAM_D41A_)
	ld (_RAM_D14E_), hl
	add hl, bc
	ld (_RAM_D10F_), hl
	ld a, c
	or b
	jr z, +++
	ld a, b
	cp $80
	jr nc, +
	ld bc, (_RAM_D14A_)
	add hl, bc
	ld a, (_RAM_D453_)
	cp h
	jr nc, +++
	jp ++

+:
	ld a, h
	cp $01
	jr nc, +++
	jr z, +++
++:
	ld hl, (_RAM_D14E_)
	ld (_RAM_D10F_), hl
+++:
	ex de, hl
	ld hl, (_RAM_D111_)
	ld a, (_RAM_D41D_)
	cp $80
	jp c, _LABEL_1F43_
	ld a, h
	cp $00
	jp m, +
	ld bc, $0040
	add hl, bc
	ex de, hl
	ld hl, (_RAM_D10F_)
	ld bc, (_RAM_D14A_)
	srl b
	rr c
	add hl, bc
	ex de, hl
	call _LABEL_21E5_
	and $01
	jr z, ++
+:
	ld a, $08
	ld (_RAM_C1FB_), a
	ld hl, (_RAM_D150_)
	ld (_RAM_D111_), hl
	ld bc, $0040
	add hl, bc
++:
	ex de, hl
	ld hl, (_RAM_D41A_)
	ld a, l
	or h
	ret z
	ld a, h
	ld hl, (_RAM_D10F_)
	cp $80
	jr nc, +
	ld bc, (_RAM_D14A_)
	add hl, bc
	ld (_RAM_D154_), hl
	ld bc, (_RAM_D152_)
	add hl, bc
	jr ++

+:
	ld bc, (_RAM_D152_)
	ld (_RAM_D154_), hl
	sbc hl, bc
++:
	ex de, hl
	call _LABEL_21E5_
	ld de, (_RAM_D154_)
	and $01
	jr nz, ++
	ld hl, (_RAM_D111_)
	ld bc, (_RAM_D14C_)
	add hl, bc
	call _LABEL_21E5_
	ld c, a
	and $E0
	jr z, +
	call _LABEL_20DA_
	jr nz, ++
+:
	ld a, (_RAM_D149_)
	and $01
	ret z
++:
	ld a, $04
	ld (_RAM_C1FB_), a
	ld hl, (_RAM_D14E_)
	ld (_RAM_D10F_), hl
	ret

_LABEL_1F43_:
	ld bc, (_RAM_D41A_)
	ld a, c
	or b
	jr z, _LABEL_1F9C_
	ld hl, (_RAM_D10F_)
	ld a, b
	cp $80
	jr nc, +
	ld bc, (_RAM_D14A_)
	add hl, bc
	ld (_RAM_D154_), hl
	ld bc, (_RAM_D152_)
	add hl, bc
	jr ++

+:
	ld bc, (_RAM_D152_)
	ld (_RAM_D154_), hl
	sbc hl, bc
++:
	ex de, hl
	ld hl, (_RAM_D111_)
	call _LABEL_21E5_
	and $01
	jr nz, +
	ld de, (_RAM_D154_)
	ld bc, (_RAM_D14C_)
	add hl, bc
	ld bc, $FF80
	add hl, bc
	call _LABEL_21E5_
	and $08
	jr nz, _LABEL_1F9C_
	ld a, (_RAM_D149_)
	bit 0, a
	jr z, _LABEL_1F9C_
+:
	ld a, $04
	ld (_RAM_C1FB_), a
	ld hl, (_RAM_D14E_)
	ld (_RAM_D10F_), hl
_LABEL_1F9C_:
	ld hl, (_RAM_D41C_)
	ld a, l
	or h
	ret z
	ld hl, (_RAM_D111_)
	ex de, hl
	ld hl, (_RAM_D10F_)
	ld bc, (_RAM_D14A_)
	srl b
	rr c
	add hl, bc
	ex de, hl
	ld bc, (_RAM_D14C_)
	add hl, bc
	call _LABEL_21E5_
	and $E0
	ret z
	ld a, $0C
	ld (_RAM_C1FB_), a
	ret

_LABEL_1FC4_:
	ld a, $10
	ld (_RAM_C1FB_), a
	ret

_LABEL_1FCA_:
	ld a, (_RAM_D156_)
	bit 0, a
	ret nz
	ld hl, (_RAM_D10F_)
	ld bc, (_RAM_D144_)
	srl b
	rr c
	ld a, c
	ld (_RAM_D146_), a
	add hl, bc
	ex de, hl
	ld hl, (_RAM_D111_)
	ld a, l
	and $80
	ld l, a
	ld bc, (_RAM_D142_)
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jp nz, _LABEL_207C_
	and $E0
	jr z, +
	ld bc, $FF80
	add hl, bc
	jr _LABEL_207C_

+:
	ld bc, $0080
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jr nz, _LABEL_207C_
	ld hl, (_RAM_D144_)
	ld bc, $0060
	sbc hl, bc
	ld a, l
	ld de, (_RAM_D10F_)
	ld a, e
	neg
	ld (_RAM_D146_), a
	add hl, de
	ex de, hl
	ld hl, (_RAM_D111_)
	ld a, l
	and $80
	ld l, a
	ld bc, (_RAM_D142_)
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jr nz, _LABEL_207C_
	and $E0
	jr z, +
	ld bc, $FF80
	add hl, bc
	jr _LABEL_207C_

+:
	ld bc, $0080
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jr nz, _LABEL_207C_
	ld bc, $0060
	ld hl, (_RAM_D10F_)
	ld a, $7F
	sub l
	ld (_RAM_D146_), a
	add hl, bc
	ex de, hl
	ld hl, (_RAM_D111_)
	ld a, l
	and $80
	ld l, a
	ld bc, (_RAM_D142_)
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jr nz, _LABEL_207C_
	and $E0
	jr z, +
	ld bc, $FF80
	add hl, bc
	jr _LABEL_207C_

+:
	ld bc, $0080
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	ret z
_LABEL_207C_:
	ld de, (_RAM_D142_)
	and a
	sbc hl, de
	ld (_RAM_D111_), hl
	ld a, (_RAM_D146_)
	ld b, a
	call _LABEL_2126_
	ld hl, (_RAM_D12F_)
	ld (_RAM_D133_), hl
	ld hl, (_RAM_D131_)
	ld (_RAM_D135_), hl
	ret

_LABEL_209A_:
	ld a, c
	ld (_RAM_D146_), a
	ex de, hl
	add hl, bc
	ex de, hl
	ld bc, (_RAM_D142_)
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jr nz, ++
	and $E0
	jr z, +
	ld bc, $FF80
	add hl, bc
	jr ++

+:
	ld bc, $0080
	add hl, bc
	call _LABEL_21E5_
	bit 3, a
	jr z, +++
++:
	ld de, (_RAM_D142_)
	and a
	sbc hl, de
	ld (_RAM_D111_), hl
	ld a, (_RAM_D146_)
	ld b, a
	call _LABEL_2126_
	ld hl, (_RAM_D12F_)
	ld (_RAM_D117_), hl
+++:
	ret


_LABEL_20DA_:
	and $E0
	ld d, a
	ld a, (_RAM_D116_)
	cp $80
	jr c, +++
	ld a, (_RAM_D113_)
	cp $80
	jr c, +
	neg
+:
	srl a
	ld b, a
	ld a, (_RAM_D115_)
	add a, b
	jr nc, +
	ld e, $04
	jr ++

+:
	add a, b
	jr nc, ++++
	ld e, $02
++:
	srl d
	srl d
	srl d
	srl d
	srl d
	ld a, (_RAM_D114_)
	cp $80
	ld a, d
	jr c, +
	sub $08
	neg
+:
	cp e
	jr nc, ++++
+++:
	ld a, $01
	and a
	ret

++++:
	xor a
	ret

_LABEL_211E_:
	ld a, (_RAM_D156_)
	bit 0, a
	ret nz
	ld b, $C0
_LABEL_2126_:
	ld a, (_RAM_D149_)
	srl a
	and $70
	add a, $65
	ld l, a
	ld a, $21
	adc a, $00
	ld h, a
	ld a, (hl)
	ld (_RAM_D12F_), a
	add a, a
	sbc a, a
	ld (_RAM_D130_), a
	inc hl
	ld a, (hl)
	ld (_RAM_D131_), a
	add a, a
	sbc a, a
	ld (_RAM_D132_), a
	inc hl
	ld a, (_RAM_D10F_)
	add a, b
	and $70
	rrca
	rrca
	rrca
	rrca
	ld e, a
	ld d, $00
	add hl, de
	ld b, (hl)
	ld a, (_RAM_D111_)
	and $8F
	or b
	ld (_RAM_D111_), a
	ld a, $01
	or a
	ret

; Data from 2165 to 21E4 (128 bytes)
.db $00 $00 $70 $70 $70 $70 $70 $70 $70 $70 $00 $00 $00 $00 $00 $00
.db $FC $E0 $70 $60 $50 $40 $30 $20 $10 $00 $00 $00 $00 $00 $00 $00
.db $FE $F0 $70 $70 $60 $60 $50 $50 $40 $40 $00 $00 $00 $00 $00 $00
.db $FE $F0 $30 $30 $20 $20 $10 $10 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $02 $10 $00 $00 $10 $10 $20 $20 $30 $30 $00 $00 $00 $00 $00 $00
.db $02 $10 $40 $40 $50 $50 $60 $60 $70 $70 $00 $00 $00 $00 $00 $00
.db $04 $20 $00 $10 $20 $30 $40 $50 $60 $70 $00 $00 $00 $00 $00 $00


_LABEL_21E5_:
	ld a, (_RAM_FFFF_)
	push af
	ld c, e
	ld a, ($0016)
	ld e, a
	ld a, (_RAM_D41F_)
	or e
	call set_page2
	ld e, c
	push hl
	push hl
	ld a, h
	and $FE
	ld h, $00
	ld l, a
	ld bc, _RAM_D16B_
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld a, d
	srl a
	add a, l
	ld l, a
	ld a, h
	adc a, $00
	ld h, a
	ld l, (hl)
	ld h, $00
	add hl, hl
	ld bc, _RAM_D1F7_
	add hl, bc
	ld c, (hl)
	inc hl
	ld b, (hl)
	pop hl
	add hl, hl
	ld a, h
	add a, a
	add a, a
	add a, a
	and $18
	add a, c
	ld c, a
	ld a, e
	add a, a
	ld a, d
	rla
	add a, a
	and $06
	add a, c
	ld c, a
	ld a, (bc)
	ld (_RAM_D148_), a
	inc bc
	ld a, (bc)
	ld (_RAM_D149_), a
	pop hl
	pop bc
	ld c, e
	ld a, ($0016)
	ld e, a
	ld a, b
	or e
	call set_page2
	ld e, c
	ld a, (_RAM_D149_)
	ret


_LABEL_2247_:
	push hl
	push de
	ld de, (_RAM_C1EC_)
	ld h, e
	ld l, $FD
	ld a, d
	or d
	sbc hl, de
	sbc a, $00
	sbc hl, de
	sbc a, $00
	ld e, a
	ld d, $00
	sbc hl, de
	jr nc, +
	inc hl
+:
	ld (_RAM_C1EC_), hl
	ld a, l
	pop de
	pop hl
	ret

_LABEL_2269_:
	push bc
	call _LABEL_27F0_
	pop bc
	djnz _LABEL_2269_
	ret

_LABEL_2271_:
	push bc
	call _LABEL_16DE_
	call _LABEL_22C8_
	pop bc
	ld a, (_RAM_C1DB_)
	and a
	jr nz, +
	ld a, (_RAM_C1D8_)
	and a
	jr nz, ++
	ld a, b
	and a
	ret z
	dec b
	push bc
	call _LABEL_27F0_
	pop bc
	jr _LABEL_2271_

+:
	ret

++:
	scf
	ret

_LABEL_2293_:
;	call _LABEL_2479_	; clear palette 
;	call +
	ld hl,0
	call cls_tileset
/*
	di			; reg 8,9: vert/horiz offsets = 0
	xor a
	out (Port_VDPAddress), a
	ld a, $88
	out (Port_VDPAddress), a
	xor a
	out (Port_VDPAddress), a
	ld a, $89
	out (Port_VDPAddress), a
	ei
*/
	call _LABEL_248D_
	ret
/*
+:
	ld hl, $3800
	ld bc, $0700
	call _LABEL_243C_
	ld hl, $0000
	ld bc, $0020
	call _LABEL_243C_
	ret
*/
_LABEL_22C0_:
	call _LABEL_22C8_
	ld a, (_RAM_C1D8_)
	and a
	ret

_LABEL_22C8_:
;	in a, (Port_IOPort1)
	call key_scan
	cpl
	ld e, a
	ld bc, _DATA_2340_
	ld h, $00
	and $0C
	ld l, a
	add hl, bc
	ld a, (hl)
	ld (_RAM_C1D6_), a
	ld h, $00
	ld a, e
	and $03
	ld l, a
	add hl, bc
	ld a, (hl)
	ld (_RAM_C1D5_), a
	ld c, e
	ld hl, _RAM_C1D7_
	ld de, $C1DB
	ld a, (_RAM_D9AB_)
	and a
	jr z, +
	ex de, hl
+:
	ld a, (hl)
	cpl
	ld b, a
	ld a, c
	and $10
	ld (hl), a
	and b
	inc hl
	ld (hl), a
	inc hl
	ld a, c
	or b
	and $10
	xor $10
	ld (hl), a
	ld a, (de)
	cpl
	ld b, a
	ld a, c
	and $20
	ld (de), a
	and b
	inc de
	ld (de), a
	inc de
	ld a, c
	or b
	and $20
	xor $20
	ld (de), a
	ld a, (_RAM_C344_)
	or a
	ret nz
	ld a, (_RAM_C1D7_)
	and a
	jr z, +
	ld a, (_RAM_C1DA_)
	inc a
	jr z, +++
	jr nz, ++
+:
	xor a
++:
	ld (_RAM_C1DA_), a
+++:
	ld a, (_RAM_C1DB_)
	and a
	jr z, +
	ld a, (_RAM_C1DE_)
	inc a
	jr z, +++
	jr nz, ++
+:
	xor a
++:
	ld (_RAM_C1DE_), a
+++:
	ret

; Data from 2340 to 234C (13 bytes)
_DATA_2340_:
.db $00 $FF $01 $00 $FF $00 $00 $00 $01 $00 $00 $00 $00


_LABEL_234D_:
	ld b, $00
	call _LABEL_2271_
	ret nz
	ret c
	ld a, $01
	ld (_RAM_C1D6_), a
	dec a
	ld (_RAM_C1D5_), a
	ld (_RAM_C1D8_), a
	ld (_RAM_C1DC_), a
	ld (_RAM_C1D7_), a
	ld (_RAM_C1DB_), a
	ret

_LABEL_236A_:
	ld hl, _RAM_D900_
	ld (_RAM_D12B_), hl
	ld iy, _DATA_23D3_
_LABEL_2373_
	ld a, $09
_LABEL_2376_:
	ld (_RAM_D12A_), a
	ld e, (iy+0)
	ld d, (iy+1)
	ld ix, _DATA_23E5_
	ld a, $07
_LABEL_2385_:
	ex af, af'
	ld c, (ix+0)
	ld a, (ix+1)
	ld b, $10
	ld hl, $0000
-:
	add hl, hl
	jr c, ++
	rl c
	rla
	jr nc, +
	add hl, de
	jr c, ++
+:
	djnz -
++:
	ld a, l
	srl h
	rra
	srl h
	rra
	srl h
	rra
	srl h
	rra
	srl h
	rra
	srl h
	rra
	ld c, a
	ld a, h
	and $03
	ld b, a
	ld hl, (_RAM_D12B_)
	ld (hl), c
	inc l
	ld (hl), b
	inc l
	ld (_RAM_D12B_), hl
	inc ix
	inc ix
	ex af, af'
	dec a
	jr nz, _LABEL_2385_
	inc iy
	inc iy
	ld a, (_RAM_D12A_)
	dec a
	jr nz, _LABEL_2376_
	ret

; Data from 23D3 to 23E4 (18 bytes)
_DATA_23D3_:
.db $00 $00 $32 $00 $62 $00 $8E $00 $B5 $00 $D5 $00 $ED $00 $FB $00
.db $00 $01

; Data from 23E5 to 23F2 (14 bytes)
_DATA_23E5_:
.db $01 $00 $03 $00 $05 $00 $08 $00 $0C $00 $10 $00 $18 $00


_LABEL_23F3_:
	push iy
	ld iy, _RAM_CEB6_
	ld b, $00
	ld a, (iy+3)
	sub (ix+3)
	jr nc, +
	neg
	ld b, $0F
+:
	ld e, a
	ld a, (iy+5)
	sub (ix+5)
	ld d, a
	jr nc, +
	neg
	ld d, a
	ld a, b
	xor $1F
	ld b, a
+:
	ld a, e
	cp d
	jr z, +
	jr nc, ++

+:
	ld e, d
	ld d, a
	ld a, b
	xor $07
	ld b, a
++:
	ld c, $04
	ld a, e
	add a, a
	add a, a
	sla d
	sla d
-:
	dec c
	jr z, +
	sub e
	cp d
	jr nc, -
+:
	ld a, c
	xor b
	add a, a
	add a, a
	add a, a
	pop iy
	ret

_LABEL_243C_:		; fill VDP memory
	ld e, $00
	ld a, l
	di
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	ei
-:
	ld a, e
	out (Port_VDPData), a
	dec bc
	ld a, b
	or c
	jr nz, -
	ret

_LABEL_2451_:
/*
	ld a, l
	di
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	ei
*/
	ld (tileram_adr),hl
	srl b
	rr c
	srl b
	rr c

-:
	ld	a, (de)
	ld (temp0),a
	inc	de
	ld	a, (de)
	ld (temp1),a
	inc	de
	ld	a, (de)
	ld (temp2),a
	inc	de
	ld	a, (de)
	ld (temp3),a
	inc	de
	push hl
	ld hl,temp3
	call decode_pix
	pop hl

/*
	ld a, (de)
	inc de
	out (Port_VDPData), a
*/
	dec bc
	ld a, b
	or c
	jr nz, -
	ret

_LABEL_2465_:
	ld a, l
	di
	out (Port_VDPAddress), a
	ld a, h
	out (Port_VDPAddress), a
	ei
	push af
	pop af
-:
	in a, (Port_VDPData)
	ld (de), a
	inc de
	dec bc
	ld a, b
	or c
	jr nz, -
	ret

_LABEL_2479_:			; palette clear
	ld a, $00
	di
	out (Port_VDPAddress), a
	ld a, $C0
	out (Port_VDPAddress), a
	ei
	ld b, $20
-:
	xor a
	out (Port_VDPData), a
	push af
	pop af
	djnz -
	ret

_LABEL_248D_:			; send $d0 to #3f00. sprites?
	ld a, $00
	di
	out (Port_VDPAddress), a
	ld a, $7F
	out (Port_VDPAddress), a
	ld a, $D0
	out (Port_VDPData), a
	ei
	ret

/*
_LABEL_249C_:
	ld hl, _DATA_24B0_	; initial vdp programming
	ld b, $0B
	ld c, $80
-:
	ld a, (hl)
	inc hl
	di
	out (Port_VDPAddress), a
	ld a, c
	inc c
	out (Port_VDPAddress), a
	ei
	djnz -
	ret

; Data from 24B0 to 24BA (11 bytes)
_DATA_24B0_:
.db $26 $A0 $FF $FF $FF $FF $FF $F0 $00 $00 $00
*/
_LABEL_24BB_:
	push af
	and $0F
	ld h, $00
	ld l, a
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld bc, _DATA_3A3B_
	add hl, bc
	ld de, _RAM_C304_
	ld bc, $0010
	ldir
	pop af
	and $F0
	ld l, a
	ld h, $00
	ld bc, _DATA_3ACB_
	add hl, bc
	ld de, _RAM_C314_
	ld bc, $0010
	ldir
	ret

_LABEL_24E4_:
	ld c, a
	ld a, ($0016)
	ld e, a
	ld a, $06
	or e
	call set_page2
	ld hl, _DATA_19BA4_
	ld d, $00
	ld e, c
	ex de, hl
	add hl, hl
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	add hl, de
	ret

_LABEL_24FE_:
	push de
	ld a, e
	ld c, a
	call _LABEL_24E4_
	ld e, (hl)
	inc hl
	push hl
	ld iy, _DATA_3B30_
	ld d, $00
	add iy, de
	ld l, (iy+0)
	ld h, (iy+1)
	ld b, (iy+2)
	ld de, $002E
-:
	ld a, (hl)
	and a
	jr z, +
	add hl, de
	djnz -
	scf
	pop hl
	pop de
	ret

+:
	push hl
	pop iy
	ld (hl), c
	ld bc, $002B
	inc hl
	inc hl
	ld (hl), $00
	ld e, l
	ld d, h
	inc de
	ldir
	pop hl
	ld (iy+14), l
	ld (iy+15), h
	ld a, $32
	ld (iy+22), a
	ld (iy+23), a
	pop de
	and a
	ret

_LABEL_2548_:
	ld a, (ix+27)
	add a, d
	ld (iy+27), a
	add a, $40
	and $80
	rlca
	ld (iy+20), a
	ld hl, (_RAM_D10F_)
	ld (iy+3), h
	ld (iy+2), l
	ld hl, (_RAM_D111_)
	ld (iy+5), h
	ld (iy+4), l
	push ix
	push iy
	pop ix
	call _LABEL_46B4_	; Possibly invalid
	pop ix
	ret


_LABEL_2575_:
	ld b, $1B
	ld hl, _RAM_C9DC_
	ld de, $002E
-:
	ld (hl), $00
	add hl, de
	djnz -
	ld b, $08
	ld a, $01
	ld hl, _RAM_CEB6_
	ld de, $002D
-:
	ld (hl), $00
	inc hl
	ld (hl), a
	add hl, de
	add a, a
	djnz -
	ld b, $05
	ld de, $002E
-:
	ld (hl), $00
	add hl, de
	djnz -
	ld a, $FF
	ld (_RAM_D10C_), a
	ret

_LABEL_25A4_:
	ld hl, _RAM_C000_
	ld bc, $00FF
	ld e, l
	ld d, h
	inc de
	ld (hl), $00
	ldir
	ret


_LABEL_25B2_:
	ld hl, (_RAM_D405_)
	ld bc, (_RAM_D407_)
	call _LABEL_2663_
	ret c
	ld a, (ix+31)
	ld e, a
	ld d, $00
	ld iy, _DATA_4385_
	add iy, de
	ld h, $C0
	ld a, b
	add a, (iy+1)
	add a, a
	ld l, a
	ld b, (iy+0)
	ld a, (ix+1)
	cpl
	ld e, a
-:
	ld a, e
	and (hl)
	ld (hl), a
	or d
	ld d, a
	inc l
	inc l
	djnz -
	ld h, $C0
	ld a, c
	add a, (iy+3)
	add a, a
	ld l, a
	inc l
	ld c, d
	ld b, (iy+2)
	ld d, $00
-:
	ld a, e
	and (hl)
	ld (hl), a
	or d
	ld d, a
	inc l
	inc l
	djnz -
	and c
	jr z, ++
	ld c, a
	ld hl, _RAM_CEDA_
	ld de, $002E
	ld b, (ix+30)
-:
	srl c
	jr c, +
	ret z
	add hl, de
	jr -

+:
	ld a, b
	and (hl)
	jr nz, +
	add hl, de
	jr -

+:
	ld (_RAM_C2D0_), a
	ld de, $0024
	and a
	sbc hl, de
	ld (_RAM_C2D1_), hl
++:
	ret

_LABEL_2623_:
	ld hl, (_RAM_D3F7_)
	ld bc, (_RAM_D3F9_)
	call _LABEL_2663_
	ret c
	ld a, (ix+31)
	ld e, a
	ld d, $00
	ld iy, _DATA_4385_
	add iy, de
	ld h, $C0
	ld a, b
	add a, (iy+1)
	add a, a
	ld l, a
	ld b, (iy+0)
	ld e, (ix+1)
-:
	ld a, e
	or (hl)
	ld (hl), a
	inc l
	inc l
	djnz -
	ld h, $C0
	ld a, c
	add a, (iy+3)
	add a, a
	ld l, a
	inc l
	ld b, (iy+2)
-:
	ld a, e
	or (hl)
	ld (hl), a
	inc l
	inc l
	djnz -
	ret

_LABEL_2663_:
	ld de, $0300
	and a
	sbc hl, de
	jr nc, +
	ld hl, $0000
+:
	ex de, hl
	ld hl, (_RAM_D10F_)
	and a
	sbc hl, de
	jr c, ++
	ld a, h
	cp $16
	jr nc, ++
	ld h, b
	ld l, c
	ld de, $0180
	and a
	sbc hl, de
	jr nc, +
	ld hl, $0000
+:
	ex de, hl
	ld hl, (_RAM_D111_)
	and a
	sbc hl, de
	jr c, ++
	ld a, h
	cp $0F
	jr nc, ++
	ld de, $0040
	ld hl, (_RAM_D111_)
	add hl, de
	add hl, hl
	ld c, h
	ld hl, (_RAM_D10F_)
	add hl, de
	add hl, hl
	ld b, h
	res 3, (ix+20)
	and a
	ret

++:
	set 3, (ix+20)
	xor a
	ld (_RAM_C2D0_), a
	scf
	ret

_LABEL_26B6_:
	ld a, (_RAM_D444_)
	dec a
	jr nz, +
	ld a, (ix+1)
	and a
	ret nz
+:
	dec (ix+24)
	ret nz
_LABEL_26C5_:
	ld de, (_RAM_D11D_)
	ld l, (ix+25)
	ld h, $00
	add hl, de
	ex de, hl
	ld hl, _DATA_5BFE_
	add hl, de
	ld a, (hl)
	and a
	jp m, +
	add a, (ix+21)
	ld (ix+26), a
	inc hl
	ld a, (hl)
	ld (ix+24), a
	set 4, (ix+20)
	inc (ix+25)
	inc (ix+25)
	ret

+:
	and $7F
	jr z, +
	inc hl
	ld a, (hl)
	ld (_RAM_D11D_), a
	inc hl
	ld a, (hl)
	ld (_RAM_D11E_), a
	ld (ix+25), $00
	ld (ix+24), $01
	jr _LABEL_26C5_

+:
	ld (ix+25), $00
	jr _LABEL_26C5_

_LABEL_270D_:
	dec (ix+24)
	ret nz
_LABEL_2711_:
	ld de, (_RAM_D11D_)
	ld l, (ix+25)
	ld h, $00
	add hl, de
	ex de, hl
	ld hl, _DATA_5BFE_
	add hl, de
	ld a, (hl)
	and a
	jp m, +
	ld (ix+26), a
	inc hl
	ld a, (hl)
	ld (_RAM_C2D4_), a
	inc hl
	ld a, (hl)
	ld (_RAM_C2D5_), a
	inc hl
	ld a, (hl)
	ld (ix+24), a
	ld a, (ix+25)
	add a, $04
	ld (ix+25), a
	ret

+:
	and $7F
	jr z, +
	inc hl
	ld a, (hl)
	ld (_RAM_D11D_), a
	inc hl
	ld a, (hl)
	ld (_RAM_D11E_), a
	ld (ix+25), $00
	ld (ix+24), $01
	jr _LABEL_2711_

+:
	ld (ix+25), $00
	jr _LABEL_2711_

_LABEL_275E_:
	ld hl, (_RAM_C9D9_)
	ld a, h
	or l
	jr z, ++
	ld a, (_RAM_C2D5_)
	or $02
	jr z, +
	ld de, (_RAM_C2D8_)
	or a
	sbc hl, de
	jr c, +
	ld hl, (_RAM_C9D9_)
	ld (_RAM_C2D8_), hl
	xor a
	ld (_RAM_C2DA_), a
	inc a
	ld (_RAM_C2DB_), a
+:
	ld hl, $0000
	ld (_RAM_C9D9_), hl
++:
	ld a, (_RAM_C2DB_)
	cp $FF
	jr z, _LABEL_27EF_
	dec a
	ld (_RAM_C2DB_), a
	jr nz, _LABEL_27EF_
_LABEL_2796_:
	ld de, (_RAM_C2D8_)
	ld a, d
	or e
	jr z, _LABEL_27EF_
	ld a, (_RAM_C2DA_)
	ld l, a
	ld h, $00
	add hl, de
	ex de, hl
	ld hl, _DATA_5BFE_
	add hl, de
	ld a, (hl)
	ld e, a
	and $80
	ld a, e
	jr nz, +
	ld a, (_RAM_C2D6_)
	add a, e
	ld (_RAM_C2D7_), a
	inc hl
	ld a, (hl)
	ld (_RAM_C2DB_), a
	ld a, (_RAM_C2DA_)
	add a, $02
	ld (_RAM_C2DA_), a
	ret

+:
	cp $80
	jr z, +
	cp $82
	jr z, ++
	inc hl
	ld a, (hl)
	ld (_RAM_C2D8_), a
	inc hl
	ld a, (hl)
	ld (_RAM_C2D9_), a
	xor a
	ld (_RAM_C2DA_), a
	inc a
	ld (_RAM_C2DB_), a
	jr _LABEL_2796_

+:
	xor a
	ld (_RAM_C2DA_), a
	jr _LABEL_2796_

++:
	xor a
	ld (_RAM_C2D8_), a
	ld (_RAM_C2D9_), a
_LABEL_27EF_:
	ret


_LABEL_27F0_:
	ld a, (_RAM_C300_)
	ld b, a
-:
	ld a, (_RAM_C300_)
	cp b
	jr z, -
	ret

_LABEL_27FB_:			; intro interrupt
	push ix
	push iy
	push bc
;	in a, (Port_VDPStatus)
xypos	ld bc,0
	call view_tilemem

	ld a, (_RAM_C301_)
	and a
	jr nz, _LABEL_2879_
	inc a
	ld (_RAM_C301_), a
	ld a, (_RAM_FFFF_)
	push af
	ld hl, _RAM_C300_
	inc (hl)
	ld a, (_RAM_C346_)
	and a
	jr z,_LABEL_284E_

	xor a
	ld (_RAM_C346_), a
	ld a, (_RAM_D40C_)
	neg
/*
	out (Port_VDPAddress), a
	ld a, $88			; horizontal
	out (Port_VDPAddress), a
*/
	ld (xypos+2),a
	call _LABEL_3155_
	ld a, ($0016)
	ld e, a
	ld a, (_RAM_D41F_)
	or e
	call set_page2
	call _LABEL_28B5_
	ld a, (_RAM_D7F5_)
	ld c, a
	ld a, (_RAM_D413_)
	add a, c
/*	
	out (Port_VDPAddress), a
	ld a, $89			; vertical
	out (Port_VDPAddress), a
*/
	ld (xypos+1),a
	call _LABEL_2E3C_
	call _LABEL_31F4_

_LABEL_284E_:
	ld a, ($0016)
	ld e, a
	ld a, $0C
	or e
	call set_page2
	ld a, (_RAM_C22B_)
	and a
	jr z, +
	ld a, (_RAM_D9AF_)
	and $01
	jr z, +
	call _LABEL_9BE_	
+:
	call _LABEL_B27_	; sound?
;	in a, (Port_IOPort2)
;	bit 4, a
;	jp z, _LABEL_0_
	pop af
	ld e, a
	rst $18	; _LABEL_18_
	xor a
	ld (_RAM_C301_), a

_LABEL_2879_:
	pop bc
	pop iy
	pop ix
	pop de
	pop de
	pop hl
	pop af
	ei
	ret

_DATA_2884_:
; Data from 2884 to 28B3 (48 bytes)
.db $31 $88 $28 $0F $00 $00 $00 $00 $00 $00 $00 $00 $08 $08 $08 $08
.db $08 $08 $08 $70 $00 $78 $01 $80 $02 $88 $03 $F0 $04 $F8 $05 $08
.db $06 $10 $07 $68 $08 $70 $09 $78 $0A $80 $0B $88 $0C $90 $0D $08
.db $0E

_LABEL_28B5_:
	ld a, (_RAM_D7D6_)
	out (Port_VDPAddress), a
	ld a, (_RAM_D7D7_)
	out (Port_VDPAddress), a
	ld hl, (_RAM_C8D2_)
	ld de, _RAM_C8D6_
	and a
	sbc hl, de
	jr z, ++
	ex de, hl
	ld a, e
	cp $31
	jr c, +
	ld a, $31
+:
	ld b, a
	ld e, a
	ld c, Port_VDPData
	otir
	ld a, $D0
	out (Port_VDPData), a
	ld a, (_RAM_D7D8_)
	out (Port_VDPAddress), a
	ld a, (_RAM_D7D9_)
	out (Port_VDPAddress), a
	ld a, e
	add a, a
	ld b, a
	ld hl, _RAM_C92A_
	otir
	ret

++:
	ld a, $D0
	out (Port_VDPData), a
	ret


_LABEL_28F4_:
/*
	di
	ld a, $A0			; enable vsync, disable screen
	out (Port_VDPAddress), a
	ld a, $81			; reg 1
	out (Port_VDPAddress), a
	ei
*/
	ld de, _RAM_D41E_
	ld bc, $001A
	ldir
	ld a, (_RAM_D425_)
	call _LABEL_24BB_
	ld a, ($0016)
	ld e, a
	ld a, (_RAM_D41E_)
	or e
	call set_page2
	ld hl, $0000
	ld de, (_RAM_D423_)
	ld bc, $2000
	call _LABEL_2451_		; depack game	 gfx
	call +				; init objects ?
/*	di
	ld a, $E0			; enable screen
	out (Port_VDPAddress), a
	ld a, $81
	out (Port_VDPAddress), a
	ei
*/
	ret

+:
	ld a, ($0016)
	ld e, a
	ld a, $0B
	or e
	call set_page2
	call ++
	ld hl, _RAM_C5EF_
	ld (_RAM_C34A_), hl
	ld hl, _RAM_C36F_
	ld (_RAM_C34C_), hl
	xor a
	ld (_RAM_C34E_), a
	ld hl, _RAM_D429_
-:
	ld c, (hl)
	inc hl
	ld b, (hl)
	inc hl
	ld a, c
	or b
	jr z, +
	push hl
	call _LABEL_29C2_
	pop hl
	jr -

+:
	ld hl, (_RAM_C34A_)
	xor a
	ld (hl), a
	inc hl
	ld (hl), a
	ret

++:
	ld hl, _RAM_C34F_
	ld de, _RAM_C34F_ + 1
	ld bc, $001F
	ld (hl), $FF
	ldir
	ld hl, (_RAM_D429_)
	ld bc, $0006
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	push ix
	push hl
	pop ix
	ld de, $C34F
-:
	ld a, (ix+5)
	or a
	jp m, +
	ld bc, $0008
	add ix, bc
	jr -

+:
	ld a, (_RAM_D9A9_)
	cp $03
	jr nz, ++
	ld de, _RAM_C34F_
-:
	ld a, (de)
	cp $FF
	jr z, ++
	call _LABEL_2247_
	and $0F
	ld hl, _RAM_C34F_
	ld b, $00
	ld c, a
	add hl, bc
	ld a, (hl)
	cp $FF
	jr z, +
	ex de, hl
	ld b, (hl)
	ex de, hl
	ld (de), a
	ld (hl), b
+:
	inc de
	jr -

++:
	pop ix
	ret

_LABEL_29C2_:
	push ix
	ld hl, $0006
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	push hl
	pop ix
	ld de, (_RAM_C34C_)
	ld hl, (_RAM_C34A_)
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld (_RAM_C34A_), hl
	ld a, (_RAM_C34E_)
	inc a
	ld (_RAM_C34E_), a
	ld bc, $0008
	ld hl, (_RAM_C34C_)
	ld de, $C34F
-:
	ld a, (ix+5)
	or a
	jp m, +
	xor a
	ld (hl), a
	inc hl
	add ix, bc
	jr -

+:
	ld (_RAM_C34C_), hl
	pop ix
	ret

_LABEL_2A01_:
	ld a, (_RAM_D127_)
	ld hl, _RAM_C5EF_
	add a, a
	ld c, a
	ld b, $00
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld (_RAM_C34C_), hl
	ret

_LABEL_2A14_:
	ld a, (_RAM_D127_)
	add a, a
	ld hl, _RAM_D429_
	ld b, $00
	ld c, a
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld de, _RAM_D439_
	ld bc, $0009
	ldir
	ld a, (_RAM_C220_)
	and a
	jr z, +
	ld hl, (_RAM_D439_)
	ld (_RAM_C20F_), hl
	ld hl, (_RAM_D43B_)
	ld (_RAM_C211_), hl
	ld a, $FF
	ld (_RAM_D7DC_), a
	xor a
	ld (_RAM_C20B_), a
	ld (_RAM_C220_), a
	ld (_RAM_D44E_), a
	ld (_RAM_D447_), a
	jr ++

+:
	ld hl, (_RAM_D138_)
	ld (_RAM_C20F_), hl
	ld hl, (_RAM_D13A_)
	ld (_RAM_C211_), hl
	ld a, (_RAM_D446_)
	cp $01
	jr nz, ++
	call _LABEL_3DE9_
	inc a
	ld (_RAM_D449_), a
++:
	ld a, ($0016)
	ld e, a
	ld a, $0B
	or e
	call set_page2
	call _LABEL_2B24_
	ld a, ($0016)
	ld e, a
	ld a, (_RAM_D41F_)
	or e
	call set_page2
	ld hl, (_RAM_D43D_)
	ld a, (hl)
	ld (_RAM_D450_), a
	sub $08
	add a, a
	dec a
	ld (_RAM_D451_), a
	add a, $10
	ld (_RAM_D453_), a
	inc hl
	ld a, (hl)
	sub $06
	add a, a
	dec a
	ld (_RAM_D452_), a
	add a, $09
	ld (_RAM_D454_), a
	inc hl
	ld (_RAM_D43D_), hl
	ld hl, $0000
	xor a
	ld (_RAM_D40E_), hl
	ld (_RAM_D9A0_), a
	ld de, _RAM_D16B_
	ld hl, (_RAM_D43D_)
	ld a, (_RAM_D450_)
	ld c, a
	ld b, $00
	ld a, $46
-:
	ex af, af'
	ld a, l
	ld (de), a
	inc de
	ld a, h
	ld (de), a
	inc de
	add hl, bc
	ex af, af'
	dec a
	jr nz, -
	ld de, _RAM_D1F7_
	ld hl, (_RAM_D427_)
	ld bc, $0020
	xor a
-:
	ex af, af'
	ld a, l
	ld (de), a
	inc de
	ld a, h
	ld (de), a
	inc de
	add hl, bc
	ex af, af'
	dec a
	jr nz, -
	call _LABEL_2BA4_
	ld hl, (_RAM_D3F7_)
	ld (_RAM_D3FB_), hl
	ld hl, (_RAM_D3F9_)
	ld (_RAM_D3FD_), hl
	ld hl, (_RAM_C20F_)
	ld (_RAM_CEB8_), hl
	ld (_RAM_C213_), hl
	ld (_RAM_D138_), hl
	ld hl, (_RAM_C211_)
	ld (_RAM_CEBA_), hl
	ld (_RAM_C215_), hl
	ld (_RAM_D13A_), hl
	ld a, ($0016)
	ld e, a
	ld a, $0B
	or e
	call set_page2
	di
	call _LABEL_2C79_
	ei
	ld a, ($0016)
	ld e, a
	ld a, (_RAM_D41F_)
	or e
	call set_page2
	ret

_LABEL_2B24_:
	ld b, $00
	ld ix, (_RAM_D43F_)
	ld iy, _RAM_D456_
-:
	ld a, (ix+1)
	ld (iy+1), a
	ld a, (ix+2)
	ld (iy+2), a
	ld a, (ix+3)
	ld (iy+3), a
	ld a, (ix+4)
	ld (iy+4), a
	ld a, (ix+0)
	ld (iy+0), a
	cp $FF
	jr z, +
	ld de, $0008
	add ix, de
	ld de, $0005
	add iy, de
	inc b
	jr -

+:
	dec b
	jr z, _LABEL_2BA3_
_LABEL_2B60_:
	push bc
	ld hl, _RAM_D45A_
	ld de, _RAM_D45F_
--:
	push bc
	ld b, (hl)
	ld a, (de)
	cp b
	jr c, +
	jr nz, ++
	dec hl
	dec de
	ld b, (hl)
	ld a, (de)
	inc hl
	inc de
	cp b
	jr nc, ++
+:
	ld bc, $0004
	or a
	sbc hl, bc
	ex de, hl
	sbc hl, bc
	ex de, hl
	ld b, $05
-:
	ld c, (hl)
	ld a, (de)
	ld (hl), a
	ld a, c
	ld (de), a
	inc hl
	inc de
	djnz -
	ld bc, $0004
	add hl, bc
	ex de, hl
	add hl, bc
	ex de, hl
	jr +++

++:
	ld bc, $0005
	add hl, bc
	ex de, hl
	add hl, bc
	ex de, hl
+++:
	pop bc
	djnz --
	pop bc
	djnz _LABEL_2B60_
_LABEL_2BA3_:
	ret

_LABEL_2BA4_:
	ld bc, $0800
	ld hl, (_RAM_C20F_)
	sbc hl, bc
	jr nc, +
	ld hl, $0000
+:
	ld a, (_RAM_D451_)
	cp h
	jr nc, +
	ld l, $FF
	ld h, a
+:
	ld (_RAM_D3F7_), hl
	ld (_RAM_D3FB_), hl
	ld (_RAM_D3FF_), hl
	ld (_RAM_D403_), hl
	ld a, l
	ld (_RAM_D40B_), a
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	ld a, l
	ld (_RAM_D99F_), a
	ld (_RAM_D40C_), a
	ld (_RAM_D40D_), a
	or a
	ld bc, $0600
	ld hl, (_RAM_C211_)
	sbc hl, bc
	jr nc, +
	ld hl, $0000
+:
	ld a, (_RAM_D452_)
	cp h
	jr nc, +
	ld l, $00
	inc a
	ld h, a
+:
	ld de, $0E00
	or a
	sbc hl, de
	ld (_RAM_D3F9_), hl
	ld (_RAM_D401_), hl
	add hl, de
	ex de, hl
	ld hl, $0000
	ld bc, $0080
-:
	ld a, d
	or e
	jr z, ++
	add hl, bc
	add a, $80
	ld a, h
	cp $0E
	jr c, +
	sub $0E
	ld h, a
+:
	ld a, e
	sub $80
	ld e, a
	ld a, d
	sbc a, $00
	ld d, a
	jr -

++:
	ld (_RAM_D40E_), hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_D9A0_), a
	ld b, $1C
_LABEL_2C36_:
	push bc
	ld de, (_RAM_D3F9_)
	ld hl, $0080
	add hl, de
	ld (_RAM_D3F9_), hl
	ld hl, $0080
	ld de, (_RAM_D40E_)
	add hl, de
	ld a, h
	cp $0E
	jr c, +
	sub $0E
+:
	ld h, a
	ld (_RAM_D40E_), hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_D9A0_), a
	ld (_RAM_D413_), a
	ld a, (_RAM_D99F_)
	ld (_RAM_D40C_), a
	call _LABEL_2EE2_
	ld hl, (_RAM_D3F9_)
	ld (_RAM_D401_), hl
	ld a, (_RAM_D9A0_)
	ld (_RAM_D410_), a
	pop bc
	djnz _LABEL_2C36_
	ret

_LABEL_2C79_:
	ld ix, (_RAM_D43F_)
	ld bc, $0008
-:
	ld e, (ix+9)
	ld d, (ix+10)
	ld hl, (_RAM_D3F7_)
	or a
	sbc hl, de
	jr c, +
	add ix, bc
	jr -

+:
	ld (_RAM_D15F_), ix
	ld ix, _RAM_D456_
	ld bc, $0005
-:
	ld e, (ix+8)
	ld d, (ix+9)
	ld hl, (_RAM_D3F9_)
	or a
	sbc hl, de
	jr c, +
	add ix, bc
	jr -

+:
	ld (_RAM_D161_), ix
	ld a, $01
	ld (_RAM_D41A_), a
	call +
	call _LABEL_4EE5_	; Possibly invalid
	ret

+:
	ld hl, _RAM_C36F_
	ld bc, $0280
-:
	res 7, (hl)
	inc hl
	dec bc
	ld a, b
	or c
	jr nz, -
	ret

_LABEL_2CCE_:
	xor a
	ld (_RAM_D417_), a
	ld de, (_RAM_D10F_)
	ld hl, (_RAM_D113_)
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	cp $80
	jr nc, +
	cp $05
	jr c, ++
	ld a, $04
	ld h, a
	jr ++

+:
	cp $FC
	jr nc, ++
	ld a, $FC
	ld h, a
++:
	rlca
	add hl, de
	adc a, $00
	and $01
	jr z, +
	ex de, hl
+:
	ld de, (_RAM_D3F7_)
	or a
	sbc hl, de
	jr c, +++
	ld bc, $0680
	sbc hl, bc
	jr c, +++
	ld bc, $0200
	sbc hl, bc
	jr c, _LABEL_2D51_
	jr z, _LABEL_2D51_
	ld a, h
	ld h, $00
	or a
	jr nz, +
	ld a, l
	cp $81
	jr c, ++
+:
	ld l, $80
++:
	add hl, de
	ld a, (_RAM_D451_)
	cp h
	jr nc, +
	ld l, $FF
	ld h, a
+:
	ld (_RAM_D3F7_), hl
	ld a, $01
	ld (_RAM_D417_), a
	jr _LABEL_2D51_

+++:
	ld a, h
	ld h, $FF
	cp $FF
	jr nz, +
	ld a, l
	cp $80
	jr nc, ++
+:
	ld l, $80
++:
	add hl, de
	jr c, +
	ld hl, $0000
+:
	ld (_RAM_D3F7_), hl
	ld a, $02
	ld (_RAM_D417_), a
_LABEL_2D51_:
	ld de, (_RAM_D111_)
	ld hl, (_RAM_C21C_)
	ld a, (_RAM_C1D5_)
	and a
	jr nz, +
	ld a, h
	or l
	jr z, +++
	ld a, h
	cpl
+:
	ld bc, $0020
	cp $80
	jr c, +
	ld bc, $FFE0
+:
	add hl, bc
	ld a, h
	cp $03
	jr c, +
	cp $FE
	jr c, ++
+:
	ld (_RAM_C21C_), hl
++:
	ld a, h
	rlca
	add hl, de
	adc a, $00
	and $01
	jr z, ++++
	ld hl, (_RAM_D111_)
+++:
	ld hl, $0000
	ld (_RAM_C21C_), hl
	ex de, hl
++++:
	ld de, (_RAM_D3F9_)
	or a
	sbc hl, de
	jr c, _LABEL_2DE6_
	ld bc, $0400
	sbc hl, bc
	jr c, _LABEL_2DE6_
	ld bc, $0200
	sbc hl, bc
	jp c, _LABEL_2E30_
	jp z, _LABEL_2E30_
	ld a, (_RAM_D417_)
	or $04
	ld (_RAM_D417_), a
	ld a, h
	ld h, $00
	or a
	jr nz, +
	ld a, l
	cp $81
	jr c, ++
+:
	ld l, $80
++:
	ld c, l
	ld b, h
	add hl, de
	ld a, (_RAM_D452_)
	cp h
	jr c, _LABEL_2E30_
	ld (_RAM_D3F9_), hl
	ld l, c
	ld h, b
	ld de, (_RAM_D40E_)
	add hl, de
	ld a, h
	cp $0E
	jr c, +
	sub $0E
+:
	ld h, a
	ld (_RAM_D40E_), hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_D9A0_), a
	jr _LABEL_2E30_

_LABEL_2DE6_:
	ld a, (_RAM_D417_)
	or $08
	ld (_RAM_D417_), a
	ld a, h
	ld h, $FF
	cp h
	jr nz, +
	ld a, l
	cp $80
	jr nc, ++
+:
	ld l, $80
++:
	ld c, l
	ld b, h
	add hl, de
	jr c, +
	ld hl, $0000
	ld (_RAM_D3F9_), hl
	ld (_RAM_D40E_), hl
	xor a
	ld (_RAM_D9A0_), a
	jr _LABEL_2E30_

+:
	ld (_RAM_D3F9_), hl
	ld l, c
	ld h, b
	or a
	ld de, (_RAM_D40E_)
	add hl, de
	ld (_RAM_D40E_), hl
	ld a, h
	cp $0E
	jr c, +
	add a, $0E
+:
	ld h, a
	ld (_RAM_D40E_), hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_D9A0_), a
_LABEL_2E30_:
	ld hl, (_RAM_D3F7_)
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_D99F_), a
	ret


_LABEL_2E3C_:
	ld a, (_RAM_D40B_)
	and $80
	ld b, a
	ld a, (_RAM_D3FB_)
	and $80
	cp b
	jr z, +
	ld (_RAM_D40B_), a
	call ++
+:
	ld a, (_RAM_D410_)
	and $F8
	ld b, a
	ld a, (_RAM_D413_)
	and $F8
	cp b
	ret z
	ld a, (_RAM_D418_)
	and $08
	jr nz, +
	call _LABEL_2EE2_
	ld hl, (_RAM_D3FD_)
	ld (_RAM_D401_), hl
	ld a, (_RAM_D413_)
	ld (_RAM_D410_), a
	ret

+:
	call _LABEL_2F22_
	ld hl, (_RAM_D3FD_)
	ld (_RAM_D401_), hl
	ld a, (_RAM_D413_)
	ld (_RAM_D410_), a
	ret

++:
	ld bc, (_RAM_D3FB_)
	ld (_RAM_D3FF_), bc
	ld (_RAM_D409_), bc
	ld a, (_RAM_D40C_)
	ld (_RAM_D40D_), a
	and $F8
	ld c, a
	ld a, (_RAM_D418_)
	and $01
	jr z, +
	ld a, (_RAM_D413_)
	and $F8
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, c
	rrca
	rrca
	ld e, a
	ld d, $00
	add hl, de
	ex de, hl
	ld a, (_RAM_D40A_)
	add a, $10
	ld (_RAM_D40A_), a
	call _LABEL_2F64_
	ret

+:
	ld a, (_RAM_D413_)
	and $F8
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, c
	add a, $08
	rrca
	rrca
	ld e, a
	ld d, $00
	add hl, de
	ex de, hl
	ld hl, (_RAM_D409_)
	ld bc, $0080
	add hl, bc
	ld (_RAM_D409_), hl
	call _LABEL_2F64_
	ret


_LABEL_2EE2_:
	ld a, (_RAM_D40C_)
	add a, $08
	ld (_RAM_D40D_), a
	ld hl, (_RAM_D3FB_)
	ld bc, $0080
	add hl, bc
	ld (_RAM_D3FF_), hl
	ld bc, $0D80
	ld hl, (_RAM_D401_)
	add hl, bc
	ld (_RAM_D409_), hl
	ld a, (_RAM_D410_)
	and $F8
	cp $08
	jr nc, +
	add a, $E0
+:
	sub $08
	ld h, $00
	ld l, a
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, (_RAM_D40D_)
	and $F8
	rrca
	rrca
	ld e, a
	ld d, $00
	add hl, de
	ex de, hl
	rrca
	call _LABEL_3073_
	ret

_LABEL_2F22_:
	ld a, (_RAM_D40C_)
	add a, $08
	ld (_RAM_D40D_), a
	ld hl, (_RAM_D3FB_)
	ld bc, $0080
	add hl, bc
	ld (_RAM_D3FF_), hl
	ld bc, $0080
	or a
	ld hl, (_RAM_D401_)
	sbc hl, bc
	ld (_RAM_D409_), hl
	ld a, (_RAM_D410_)
	and $F8
	cp $08
	jr nc, +
	add a, $E0
+:
	sub $08
	ld h, $00
	ld l, a
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, (_RAM_D40D_)
	and $F8
	rrca
	rrca
	ld e, a
	ld d, $00
	add hl, de
	ex de, hl
	rrca
	call _LABEL_3073_
	ret

_LABEL_2F64_:
	ld a, (_RAM_D413_)
	and $F8
	rrca
	rrca
	rrca
	ld (_RAM_D415_), a
	neg
	add a, $1C
	ld (_RAM_D414_), a
	ld h, $00
	ld a, (_RAM_D3FE_)
	and $FE
	ld l, a
	ld bc, _RAM_D16B_
	add hl, bc
	ld c, (hl)
	inc hl
	ld b, (hl)
	ld h, $00
	ld a, (_RAM_D40A_)
	srl a
	ld l, a
	add hl, bc
	push hl
	pop ix
	ld hl, (_RAM_D409_)
	add hl, hl
	ld a, h
	add a, a
	and $06
	ld b, a
	ld a, (_RAM_D413_)
	and $18
	add a, b
	ld (_RAM_D416_), a
	ld hl, $3800
	add hl, de
	ld a, (ix+0)
	add a, a
	ld c, a
	ld a, $00
	adc a, $00
	ld b, a
	ld a, c
	add a, $F7
	ld c, a
	ld a, b
	adc a, $D1
	ld b, a
	ld a, (bc)
	ld e, a
	ld a, (_RAM_D416_)
	add a, e
	ld e, a
	inc bc
	ld a, (bc)
	adc a, $00
	ld d, a
	ld a, (_RAM_D416_)
	and $07
	ld (_RAM_D416_), a
	ld a, (_RAM_D414_)
	ld b, a
_LABEL_2FD1_:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	ld a, (de)
	out (Port_VDPData), a
	inc de
	ld a, (de)
	and $F6
	out (Port_VDPData), a
	ld a, e
	add a, $07
	ld e, a
	and $18
	jr nz, +
	push bc
	ld a, (_RAM_D450_)
	ld c, a
	ld b, $00
	add ix, bc
	ld a, (ix+0)
	add a, a
	ld c, a
	ld a, $00
	adc a, $00
	ld b, a
	ld a, c
	add a, $F7
	ld c, a
	ld a, b
	adc a, $D1
	ld b, a
	ld a, (bc)
	ld e, a
	ld a, (_RAM_D416_)
	add a, e
	ld e, a
	inc bc
	ld a, (bc)
	adc a, $00
	ld d, a
	pop bc
+:
	ld a, $40
	add a, l
	ld l, a
	ld a, h
	adc a, $00
	ld h, a
	djnz _LABEL_2FD1_
	ld a, (_RAM_D415_)
	or a
	ret z
	ld bc, $0700
	sbc hl, bc
	ld b, a
_LABEL_3027_:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	ld a, (de)
	out (Port_VDPData), a
	inc de
	ld a, (de)
	and $F6
	out (Port_VDPData), a
	ld a, e
	add a, $07
	ld e, a
	and $18
	jr nz, +
	push bc
	ld a, (_RAM_D450_)
	ld c, a
	ld b, $00
	add ix, bc
	ld a, (ix+0)
	add a, a
	ld c, a
	ld a, $00
	adc a, $00
	ld b, a
	ld a, c
	add a, $F7
	ld c, a
	ld a, b
	adc a, $D1
	ld b, a
	ld a, (bc)
	ld e, a
	ld a, (_RAM_D416_)
	add a, e
	ld e, a
	inc bc
	ld a, (bc)
	adc a, $00
	ld d, a
	pop bc
+:
	ld a, $40
	add a, l
	ld l, a
	ld a, h
	adc a, $00
	ld h, a
	djnz _LABEL_3027_
	ret


_LABEL_3073_:
	ld (_RAM_D415_), a
	neg
	add a, $20
	ld (_RAM_D414_), a
	ld h, $00
	ld a, (_RAM_D40A_)
	and $FE
	ld l, a
	ld bc, _RAM_D16B_
	add hl, bc
	ld c, (hl)
	inc hl
	ld b, (hl)
	ld h, $00
	ld a, (_RAM_D400_)
	srl a
	ld l, a
	add hl, bc
	push hl
	pop ix
	ld a, (_RAM_D40D_)
	rrca
	rrca
	and $06
	ld c, a
	ld a, (_RAM_D40A_)
	ld b, a
	ld a, (_RAM_D409_)
	srl b
	rra
	rrca
	rrca
	rrca
	and $18
	add a, c
	ld (_RAM_D416_), a
	ld hl, $3800			; vdp tiles
	add hl, de
	push hl
/*
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
*/
	ld a,h
	add a,$c0
	ld h,a
	ld (tilemap_adr),hl

	ld l, (ix+0)
	ld h, $00
	inc ix
	add hl, hl
	ld a, l
	add a, $F7
	ld l, a
	ld a, h
	adc a, $D1
	ld h, a
	ld a, (_RAM_D416_)
	add a, (hl)
	ld e, a
	inc hl
	ld a, (hl)
	adc a, $00
	ld d, a
	ld a, (_RAM_D416_)
	and $F8
	ld (_RAM_D416_), a
	ld a, (_RAM_D414_)
	ld b, a
-:
	ld a, (de)
	call update_tilemem
;	out (Port_VDPData), a
	inc de
	ld a, (de)
	and $F6
	inc e
	call update_tilemem
;	out (Port_VDPData), a
	ld a, e
	and $07
	jr nz, +
	ld l, (ix+0)
	ld h, $00
	inc ix
	add hl, hl
	ld a, l
	add a, $F7
	ld l, a
	ld a, h
	adc a, $D1
	ld h, a
	ld a, (_RAM_D416_)
	add a, (hl)
	ld e, a
	inc hl
	ld a, (hl)
	adc a, $00
	ld d, a
+:
	djnz -
	pop hl
	ld a, (_RAM_D415_)
	or a
	ret z
	add a, a
	ld c, a
	ld b, $00
	sbc hl, bc
	ld a, (_RAM_D415_)
	ld b, a

	ld a,h
	add a,$c0
	ld h,a
	ld (tilemap_adr),hl
/*
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
*/
-:
	ld a, (de)
;	out (Port_VDPData), a
	call update_tilemem
	inc de
	ld a, (de)
	inc e
	and $F6
;	out (Port_VDPData), a
	call update_tilemem
	ld a, e
	and $07
	jr nz, +
	ld l, (ix+0)
	ld h, $00
	inc ix
	add hl, hl
	ld a, l
	add a, $F7
	ld l, a
	ld a, h
	adc a, $D1
	ld h, a
	ld a, (_RAM_D416_)
	add a, (hl)
	ld e, a
	inc hl
	ld a, (hl)
	adc a, $00
	ld d, a
+:
	djnz -
	ret

_LABEL_3155_:
	ld hl, (_RAM_C78E_)
	ld de, _RAM_C792_
	ld (_RAM_C78E_), de
	and a
	sbc hl, de
	jr z, +
	ld a, l
	rr h
	rra
	rr h
	rra
	ex de, hl
;	ld c, Port_VDPAddress
-:
/*
	outi
	outi
	dec c
	outi
	outi
	inc c
*/
	push af
	ld e,(hl)
	inc hl
	ld a,(hl)
	add a,$80
	ld d,a
	inc hl
	ld (tilemap_adr),de
	ld a,(hl)
	call update_tilemem
	inc hl
	ld a,(hl)
	call update_tilemem
	inc hl
	pop af
	dec a
	jr nz, -
+:
	ld hl, (_RAM_C790_)
	ld de, $C892
	ld (_RAM_C790_), de
	and a
	sbc hl, de
	jr z, _LABEL_31F3_
	push hl
	ld a, (_RAM_D420_)
	ld a, ($0016)
	ld e, a
	ld a, a
	or e
	call set_page2
	pop hl
	ld a, l
	rr h
	rra
	rr h
	rra
	ld hl, _RAM_C892_
	ld c, Port_VDPAddress
_LABEL_31A4_:
	outi
	outi
	dec c
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ex de, hl
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	outi
	ex de, hl
	inc c
	dec a
	jr nz, _LABEL_31A4_
_LABEL_31F3_:
	ret


_LABEL_31F4_:
	ld hl, _RAM_C5FC_
_LABEL_31F7_:
	ld a, (hl)
	and a
	jp z, _LABEL_3266_
	ld b, a
	inc hl
	ld a, (_RAM_FFFF_)
	ld c, a
	and $0F
	cp (hl)
	jr z, +
	ld a, c
	and $F0
	or (hl)
	call set_page2
+:
	inc hl
	ld a, (hl)
	out (Port_VDPAddress), a
	inc hl
	ld a, (hl)
	ld a, (hl)
	inc hl
	out (Port_VDPAddress), a
	ld a, b
	add a, a
	jp c, +
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	push hl
	ex de, hl
	ld d, $05
--:
	ld c, b
	ld b, $10
-:
	ld a, (hl)
	inc hl
	out (Port_VDPData), a
	ld e, (hl)
	ld a, (de)
	inc hl
	nop
	nop
	out (Port_VDPData), a
	djnz -
	ld b, c
	djnz --
	pop hl
	jp _LABEL_31F7_

+:
	res 7, b
	ld c, b
	ld b, (hl)
	inc hl
---:
	push bc
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	push hl
	ex de, hl
	ld d, $05
--:
	ld c, b
	ld b, $10
-:
	ld e, (hl)
	ld a, (de)
	out (Port_VDPData), a
	inc hl
	ld a, (hl)
	inc hl
	and $FF
	out (Port_VDPData), a
	djnz -
	ld b, c
	djnz --
	pop hl
	pop bc
	dec c
	jp nz, ---
	jp _LABEL_31F7_


_LABEL_3266_:
	ld hl, _RAM_C5FC_
	ld (_RAM_C78C_), hl
	ld (hl), $00
	ret


_LABEL_326F_:
	ld c, a
	ld a, ($0016)
	ld e, a
	ld a, $0E
	or e
	call set_page2
	ld a, (_RAM_D9A8_)
	ld e, a
	ld d, $00
	ld hl, _DATA_1DBE_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld a, c
	and a
	jr z, +
-:
	ld a, (hl)
	inc hl
	cp $FF
	jr nz, -
	dec c
	jr nz, -
+:
	call _LABEL_3299_
	ret

_LABEL_3299_:
	ld a, ($0016)
	ld e, a
	ld a, $0E
	or e
	call set_page2
_LABEL_32A3_:
	ld a, (hl)
	inc hl
	ld (_RAM_C1DF_), hl
	cp $FF
	jp z, _LABEL_3375_
	cp $F5
	jp z, _LABEL_336F_
	cp $F6
	jr z, ++++
	cp $F4
	jr z, +++
	cp $F3
	jr z, +
	cp $F2
	jr nz, _LABEL_3309_
	ld a, $20
	jr ++

+:
	ld a, $40
++:
	ld de, (_RAM_C1E7_)
	add a, e
	ld e, a
	ld a, d
	adc a, $00
	ld d, a
	ld (_RAM_C1E7_), de
	ld (_RAM_C1E1_), de
	jp _LABEL_336F_

+++:
	ld a, (hl)
	ld (_RAM_C1E6_), a
	inc hl
	ld (_RAM_C1DF_), hl
	jp _LABEL_336F_

++++:
	ld e, (hl)
	inc hl
	ld a, (hl)
	cp $1C
	jr c, +
	sub $1C
+:
	ld d, a
	inc hl
	ld (_RAM_C1DF_), hl
	ld l, d
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld d, $00
	add hl, de
	ld (_RAM_C1E7_), hl
	ld (_RAM_C1E1_), hl
	jr _LABEL_336F_

_LABEL_3309_:
	ld c, a
	cp $20
	jr z, ++
	jr nc, +
	add a, $5E
	jr ++

+:
	cp $3A
	jr nc, ++
	add a, $2E
++:
	ld c, a
	ld a, (_RAM_C1E3_)
	and a
	jr z, +
	ld hl, (_RAM_C1E1_)
	add hl, hl
/*	ld a, l
	di
	out (Port_VDPAddress), a	; read VDP tilemem 
	ld a, h
	or $38
	out (Port_VDPAddress), a
	ei
	in a, (Port_VDPData)
	ld l, a
	push de
	nop
	in a, (Port_VDPData)
	and $01
	ld h, a
*/
	push de
	ld a,h
	or $f8
	ld h,a
	ld l,(hl)
	ld a,(hl)
	and 1
	ld h,a

	call +++
	pop de
	jr ++

+:
	ld a, c
	cp $20
	jr nz, +
	ld c, $5C
+:
	ld b, $00
	ld hl, (_RAM_C1E4_)
	add hl, bc
	ld c, l
	ld a, (_RAM_C1E6_)
	or h
	ld b, a
++:
	ld hl, (_RAM_C1E1_)
	add hl, hl
/*	ld a, l			; write VDP tilemem
	di
	out (Port_VDPAddress), a
	ld a, h
	or $78
	out (Port_VDPAddress), a
	ei
	ld a, c
	out (Port_VDPData), a
	ld de, (_RAM_C1E1_)
	inc de
	ld (_RAM_C1E1_), de
	ld a, b
	out (Port_VDPData), a
*/
	ld a,h
	or $f8
	ld h,a
	ld (hl),c
	inc hl
	ld (hl),b

	ld de, (_RAM_C1E1_)
	inc de
	ld (_RAM_C1E1_), de

_LABEL_336F_:
	ld hl, (_RAM_C1DF_)
	jp _LABEL_32A3_

_LABEL_3375_:
	ret

+++:
	ld a, c
	cp $20
	jr nz, +
	ld b, h
	ld c, l
	ret

+:
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, l
	di
	out (Port_VDPAddress), a
	ld a, h
	out (Port_VDPAddress), a
	ei
	ld a, c
	sub $41
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, _DATA_3A8E8_
	add hl, de
	ld de, _RAM_C2DD_
	ld a, $08
-:
	ld (_RAM_C2FF_), a
	ld a, (hl)
	inc hl
	or (hl)
	cpl
	ld c, a
	ld a, (_RAM_C1E6_)
	ld b, a
	in a, (Port_VDPData)
	and c
	srl b
	jr nc, +
	or (hl)
+:
	dec hl
	ld (de), a
	inc de
	in a, (Port_VDPData)
	and c
	srl b
	jr nc, +
	or (hl)
+:
	ld (de), a
	inc de
	in a, (Port_VDPData)
	and c
	srl b
	jr nc, +
	inc hl
	or (hl)
	dec hl
+:
	ld (de), a
	inc de
	in a, (Port_VDPData)
	and c
	srl b
	jr nc, +
	or (hl)
+:
	inc hl
	inc hl
	ld (de), a
	inc de
	ld a, (_RAM_C2FF_)
	dec a
	jr nz, -
	ld hl, (_RAM_C2FD_)
	ld de, _RAM_C2DD_
	ld bc, $0020
	call _LABEL_2451_
	ld hl, (_RAM_C2FD_)
	ld b, h
	ld c, l
	ld de, $0020
	add hl, de
	ld (_RAM_C2FD_), hl
	srl b
	rr c
	srl b
	rr c
	srl b
	rr c
	srl b
	rr c
	srl b
	rr c
	ret

_LABEL_340A_:
	ld a, ($0016)
	ld e, a
	ld a, $0E
	or e
	call set_page2
	xor a
	ld (_RAM_C1E3_), a
	ld hl, (_RAM_C2FD_)
	ld a, l
	srl h
	rra
	srl h
	rra
	srl h
	rra
	srl h
	rra
	srl h
	rra
	ld l, a
	ld bc, $FFBF
	add hl, bc
	ld (_RAM_C1E4_), hl
	ld hl, (_RAM_C2FD_)
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	rl h
	rl h
	rl h
	ld (tileram_adr),hl

	ld hl, _DATA_3A8E8_
	ld bc, $02D0
	xor a
	ld (temp2),a
	ld (temp3),a
/*
	ld a, e
	di
	out (Port_VDPAddress), a
	ld a, d
	or $40
	out (Port_VDPAddress), a
	ei
*/
-:
	ld a, (hl)
	ld (temp0),a
;	out (Port_VDPData), a
	inc hl
	dec bc
	ld a, (hl)
	ld (temp1),a
	inc hl
	dec bc
;	out (Port_VDPData), a
/*	push af
	pop af
	xor a
	out (Port_VDPData), a
	push af
	pop af
	xor a
	out (Port_VDPData), a
*/
	push hl
	ld hl,temp3
	call decode_pix
	pop hl

	ld a, c
	or b
	jr nz, -
	ld hl, (_RAM_C2FD_)
	ld de, $02D0
	add hl, de
	add hl, de
	ld (_RAM_C2FD_), hl
	ret

.org $346b
_LABEL_346b_:
	ld a,1
	ld (_RAM_C1E3_),a
	ret


_LABEL_3471_:
	ld a, (ix+26)
	cp $55
	jr nc, +
	ld de, _DATA_3ECA2_
	ld a, $09
	ld (_RAM_C9D7_), a
	ld a, (ix+26)
	ld (_RAM_C8B4_), a
	jr ++

+:
	ld de, (_RAM_D421_)
	ld a, (_RAM_D420_)
	ld (_RAM_C9D7_), a
	ld a, (ix+26)
	ld (_RAM_C8B4_), a
	sub $55
++:
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, de
	push hl
	ld a, ($0016)
	ld e, a
	ld a, $0F
	or e
	call set_page2
	pop iy
	ld d, (ix+3)
	ld e, (ix+2)
	ld a, (iy+7)
	bit 0, (ix+20)
	jr z, +
	neg
+:
	add a, a
	add a, a
	ld l, a
	sbc a, a
	ld h, a
	add hl, hl
	add hl, hl
	add hl, de
	ld bc, (_RAM_D3F7_)
	or a
	sbc hl, bc
	ld a, h
	rra
	inc a
	cp $09
	jp nc, _LABEL_3548_
	ld c, h
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_C8C9_), a
	rl c
	rra
	sra a
	sra a
	ld (_RAM_C8CB_), a
	ld bc, (_RAM_D3F9_)
	ld h, (ix+5)
	ld l, (ix+4)
	or a
	sbc hl, bc
	ld a, h
	rra
	add a, $02
	cp $09
	jr nc, _LABEL_3548_
	ld a, (iy+6)
	ex de, hl
	add a, a
	add a, a
	ld l, a
	sbc a, a
	ld h, a
	add hl, hl
	add hl, hl
	add hl, de
	ld c, h
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_C8CA_), a
	rl c
	rra
	sra a
	sra a
	ld (_RAM_C8CC_), a
	push iy
	pop hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld (_RAM_C9D3_), de
	ld a, (hl)
	inc hl
	inc hl
	ld (_RAM_C9D5_), a
	push hl
	ld hl, _RAM_C19D_
	ld c, $0A
	call _LABEL_3865_
	pop hl
	ld a, (ix+1)
	and a
	jr z, +
	ld a, (ix+40)
	and $02
	jr nz, _LABEL_3548_
+:
	call _LABEL_390C_
_LABEL_3548_:
	ld a, (ix+40)
	or a
	ret z
	dec a
	ld (ix+40), a
	ret

_LABEL_3552_:
	ld l, (ix+2)
	ld h, (ix+3)
	push hl
	ld de, (_RAM_D3F7_)
	add hl, de
	ld (ix+2), l
	ld (ix+3), h
	ld l, (ix+4)
	ld h, (ix+5)
	push hl
	ld de, (_RAM_D3F9_)
	add hl, de
	ld (ix+4), l
	ld (ix+5), h
	call _LABEL_3471_
	pop hl
	ld (ix+4), l
	ld (ix+5), h
	pop hl
	ld (ix+2), l
	ld (ix+3), h
	ret

_LABEL_3588_:
	ld a, (ix+20)
	ld (_RAM_C8BF_), a
	set 4, (ix+20)
	ld de, _DATA_1B37A_
	ld l, (ix+26)
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, de
	push hl
	pop iy
	xor a
	ld (_RAM_C8BE_), a
	ld (_RAM_C8BB_), a
	ld b, $00
	ld c, (iy+0)
	ld de, (_RAM_D3F7_)
	ld hl, $007F
	add hl, de
	ex de, hl
	ld a, e
	and $80
	ld e, a
	ld h, (ix+3)
	ld l, (ix+2)
	or a
	sbc hl, de
	add hl, hl
	ld a, h
	jr z, +
	cp $80
	jr c, ++
	add a, c
	ret nc
	ret z
	ld (_RAM_C8B9_), a
	sub c
	neg
	ld b, a
	add a, a
	ld (_RAM_C8BB_), a
	ex de, hl
	add hl, hl
	jr ++++

+:
	ex de, hl
	add hl, hl
	jr +++

++:
	add hl, de
	add hl, de
	add a, c
	sub $20
	jr z, +++
	jr nc, +
	ld a, $01
	ld (_RAM_C8BE_), a
	jr +++

+:
	ld b, a
	ld a, c
	sub b
	ret c
	ret z
	ld c, a
+++:
	ld a, c
	ld (_RAM_C8B9_), a
	ld a, b
	add a, a
++++:
	ld (_RAM_C8BC_), a
	ld a, h
	ld (_RAM_C8BA_), a
	ld b, $00
	ld c, (iy+1)
	ld de, (_RAM_D3F9_)
	ld a, e
	and $80
	ld e, a
	ld h, (ix+5)
	ld l, (ix+4)
	or a
	sbc hl, de
	add hl, hl
	ld a, h
	jr z, ++
	jr c, +++
	add a, c
	sub $1A
	jr nc, +
	ld a, (_RAM_C8BE_)
	and a
	jr z, ++++
	res 4, (ix+20)
	ld a, (_RAM_C8BF_)
	and $10
	ret z
	jr ++++

+:
	sub c
	ret nc
	neg
	ld c, a
	jr ++++

++:
	ld hl, $0000
	jr ++++

+++:
	add a, c
	ret nc
	ret z
	ld hl, $0000
	ld b, c
	ld c, a
	sub b
	neg
	ld b, a
++++:
	ld a, c
	ld (_RAM_C8B8_), a
	ld a, (_RAM_C8BA_)
	and $1F
	add a, a
	ld c, a
	srl h
	ld l, $00
	rr l
	ld de, (_RAM_D40E_)
	add hl, de
	srl h
	ld a, l
	rra
	and $C0
	add a, c
	ld l, a
	ld de, $3800
	add hl, de
	ex de, hl
	ld l, (iy+2)
	ld h, (iy+3)
	ld a, (iy+0)
	add a, a
	ld c, a
	ld a, b
	and a
	jr z, +
	ld b, $00
-:
	add hl, bc
	dec a
	jr nz, -
+:
	ld a, (_RAM_C8BB_)
	ld c, a
	add hl, bc
	ld b, d
	ld c, e
	ld a, (_RAM_C8B8_)
	ld de, (_RAM_C78E_)
--:
	ld (_RAM_C8B8_), a
	ld a, b
	cp $3F
	jr c, +
	sub $07
	ld b, a
+:
	push bc
	ld a, (_RAM_C8B9_)
-:
	ex af, af'
	ld a, c
	ld (de), a
	inc de
	ld a, b
	or $40
	ld (de), a
	inc de
	ld a, c
	ld c, $03
	ldi
	ldi
	add a, $02
	ld c, a
	and $3F
	jr nz, +
	xor c
	ld c, a
+:
	ex af, af'
	dec a
	jr nz, -
	ld bc, (_RAM_C8BC_)
	add hl, bc
	pop bc
	ld a, c
	add a, $40
	ld c, a
	jr nc, +
	inc b
+:
	ld a, (_RAM_C8B8_)
	dec a
	jr nz, --
	ld (_RAM_C78E_), de
	ret

_LABEL_36D9_:
	ld a, ($0016)
	ld e, a
	ld a, $0F
	or e
	call set_page2
	bit 4, (ix+20)
	ret z
	res 4, (ix+20)
	ld de, $B37A
	ld l, (ix+26)
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, de
	inc hl
	inc hl
	inc hl
	inc hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	ex de, hl
	ld a, (hl)
	inc hl
	ld de, (_RAM_C790_)
-:
	ldi
	ldi
	ldi
	ldi
	dec a
	jr nz, -
	ld (_RAM_C790_), de
	ret

_LABEL_3716_:
	ld a, ($0016)
	ld e, a
	ld a, $0F
	or e
	call set_page2
	ld iy, _DATA_3EA62_
	ld a, (ix+26)
	call _LABEL_37E7_
	xor a
	ld (_RAM_C8B2_), a
	ld a, (iy+6)
	ld (_RAM_C8B3_), a
	ld a, $0D
	ld (_RAM_C9D7_), a
	ld a, $0F
	ld (_RAM_C8CF_), a
	call _LABEL_37F1_
	jr c, ++
	ld a, (ix+26)
	ld a, (_RAM_C8B6_)
	ld c, a
	ld a, (ix+26)
	bit 0, (ix+20)
	jr z, +
	or $80
+:
	cp c
	jr z, +
	ld (_RAM_C8B6_), a
	push hl
	ld de, $61E0
	call _LABEL_38AE_
	pop hl
+:
	ld a, (_RAM_C8C8_)
	and $01
	jr nz, ++
	call _LABEL_390C_
++:
	ld a, (iy+7)
	bit 0, (ix+20)
	jr z, ++
	cp $F0
	jr nz, +
	sub $08
+:
	neg
++:
	ld (_RAM_C8B2_), a
	ld a, (_RAM_C2D5_)
	bit 0, a
	jr z, _LABEL_37E6_
	ld bc, (_RAM_C2D8_)
	ld a, c
	or b
	jr nz, +
	ld a, (_RAM_C2D4_)
	jr ++

+:
	ld a, (_RAM_C2D7_)
++:
	ld (_RAM_C8B4_), a
	ld (_RAM_C8B5_), a
	ld iy, _DATA_3EB7A_
	call _LABEL_37E7_
	ld a, (_RAM_C8B3_)
	add a, (iy+6)
	sub $10
	ld (_RAM_C8B3_), a
	ld a, $0E
	ld (_RAM_C9D7_), a
	ld a, $1B
	ld (_RAM_C8CF_), a
	call _LABEL_37F1_
	jr c, _LABEL_37E6_
	ld a, (_RAM_C8B7_)
	ld c, a
	ld a, (_RAM_C8B5_)
	bit 0, (ix+20)
	jr z, +
	or $80
+:
	cp c
	jr z, +
	ld (_RAM_C8B7_), a
	push hl
	ld de, $6360
	call _LABEL_38AE_
	pop hl
+:
	ld a, (_RAM_C8C8_)
	and $01
	jr nz, _LABEL_37E6_
	call _LABEL_390C_
_LABEL_37E6_:
	ret

_LABEL_37E7_:
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	ex de, hl
	add iy, de
	ret

_LABEL_37F1_:
	ld a, (_RAM_C8B2_)
	add a, a
	ld l, a
	sbc a, a
	ld h, a
	add hl, hl
	add hl, hl
	add hl, hl
	ld d, (ix+3)
	ld e, (ix+2)
	add hl, de
	ld bc, (_RAM_D3F7_)
	and a
	sbc hl, bc
	ld a, h
	rra
	inc a
	cp $09
	jr nc, _LABEL_3861_
	ld c, h
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	ld (_RAM_C8C9_), a
	rl c
	rra
	sra a
	sra a
	ld (_RAM_C8CB_), a
	ld bc, (_RAM_D3F9_)
	ld h, (ix+5)
	ld l, (ix+4)
	and a
	sbc hl, bc
	ld a, h
	rra
	add a, $02
	cp $09
	jr nc, _LABEL_3861_
	ld a, (_RAM_C8B3_)
	ld c, h
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add a, h
	ld (_RAM_C8CA_), a
	rl c
	rra
	sra a
	sra a
	ld (_RAM_C8CC_), a
	push iy
	pop hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld (_RAM_C9D3_), de
	ld a, (hl)
	inc hl
	inc hl
	ld (_RAM_C9D5_), a
	xor a
	ret

_LABEL_3861_:
	scf
	ld a, $01
	ret

_LABEL_3865_:
	ld a, (ix+20)
	rrca
	and $80
	ld b, a
	ld a, (_RAM_C8B4_)
	or b
	ld (_RAM_C8CF_), a
	ld b, c
	ld a, (_RAM_D12A_)
	ld d, a
	ld c, $00
	ld e, l
	inc e
-:
	ld a, (_RAM_C8CF_)
	cp (hl)
	jr z, ++
	inc l
	ld a, d
	sub (hl)
	cp c
	jr c, +
	ld c, a
	ld e, l
+:
	inc l
	inc l
	inc l
	inc l
	djnz -
	dec e
	ld l, e
	ld a, (_RAM_C8CF_)
	ld (hl), a
	inc l
	ld (hl), d
	inc l
	ld a, (hl)
	ld (_RAM_C8CF_), a
	inc l
	ld e, (hl)
	inc l
	ld d, (hl)
	call _LABEL_38AE_
	ret

++:
	inc l
	ld a, d
	ld (hl), a
	inc l
	ld a, (hl)
	ld (_RAM_C8CF_), a
	ret

_LABEL_38AE_:
	bit 0, (ix+20)
	jr nz, +
	ld a, (_RAM_C9D5_)
	ld hl, (_RAM_C78C_)
	ld (hl), a
	inc hl
	ld a, (_RAM_C9D7_)
	ld (hl), a
	inc hl
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld de, (_RAM_C9D3_)
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld (hl), $00
	ld (_RAM_C78C_), hl
	ret

+:
	ld hl, (_RAM_C78C_)
	ld a, (iy+4)
	ld c, a
	or $80
	ld (hl), a
	inc hl
	ld a, (_RAM_C9D7_)
	ld (hl), a
	inc hl
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld a, (iy+5)
	ld (hl), a
	inc hl
	ld b, $00
	sla c
	add hl, bc
	ld (_RAM_C78C_), hl
	ld (hl), b
	dec hl
	rrca
	rrca
	rrca
	ld c, a
	ld de, (_RAM_C9D3_)
	ld a, (iy+4)
-:
	ld (hl), d
	dec hl
	ld (hl), e
	dec hl
	ex de, hl
	add hl, bc
	ex de, hl
	dec a
	jr nz, -
	ret

_LABEL_390C_:
	ld a, (_RAM_C8CB_)
	and a
	jp p, +
	add a, (hl)
	ret nc
	ret z
	ld d, a
	ld a, (hl)
	inc hl
	ld c, (hl)
	sub d
	ld b, a
	jr z, +++
	ld a, (_RAM_C8C9_)
	ld e, a
	ld a, b
	add a, a
	add a, a
	add a, a
	add a, e
	ld (_RAM_C8C9_), a
	ld a, (_RAM_C8CF_)
-:
	add a, c
	djnz -
	ld (_RAM_C8CF_), a
	jr +++

+:
	add a, (hl)
	sub $20
	jr nc, +
	ld d, (hl)
	jr ++

+:
	ld e, a
	ld a, (hl)
	sub e
	ret c
	ret z
	ld d, a
++:
	inc hl
	ld c, (hl)
+++:
	ld a, (_RAM_C8CC_)
	inc a
	jp m, ++
	add a, c
	sub $19
	jr c, +
	ld e, a
	ld a, c
	sub e
	ret c
	ret z
	ld b, a
	jr +++

+:
	ld b, c
	ld e, $00
	jr +++

++:
	add a, c
	ret nc
	ret z
	ld b, a
	ld a, c
	sub b
	ld e, a
	jr z, +++
	ld a, (_RAM_C8CF_)
	add a, e
	ld (_RAM_C8CF_), a
	ld a, e
	add a, a
	add a, a
	add a, a
	ld c, a
	ld a, (_RAM_C8CA_)
	add a, c
	ld (_RAM_C8CA_), a
+++:
	push ix
	push iy
	ld iy, (_RAM_C8D2_)
	ld ix, (_RAM_C8D4_)
	push iy
	pop hl
	push bc
	ld bc, $C916
	sbc hl, bc
	pop bc
	jr nc, +
	ld a, (_RAM_C8CF_)
	ld h, a
	ld a, (_RAM_C8C9_)
	ld l, a
	ld a, (_RAM_C8CA_)
	ld c, a
--:
	push bc
	ld a, c
-:
	ld (iy+0), a
	add a, $08
	inc iy
	ld (ix+0), l
	inc ix
	ld (ix+0), h
	inc h
	inc ix
	djnz -
	pop bc
	ld a, l
	add a, $08
	ld l, a
	ld a, h
	add a, e
	ld h, a
	dec d
	jr nz, --
	ld (iy+0), $D0
	ld (_RAM_C8D2_), iy
	ld (_RAM_C8D4_), ix
+:
	pop iy
	pop ix
	ret

_LABEL_39D0_:
	ld a, (ix+22)
	cp $FF
	jr z, _LABEL_3A29_
	ld hl, (_RAM_D3F7_)
	ld bc, $0300
	or a
	sbc hl, bc
	jr c, +
	ld de, (_RAM_D10F_)
	or a
	sbc hl, de
	jr nc, _LABEL_3A2B_
+:
	ld hl, (_RAM_D3F7_)
	ld de, $1300
	add hl, de
	ld de, (_RAM_D10F_)
	or a
	sbc hl, de
	jr nc, +
	jr _LABEL_3A2B_

+:
	ld hl, (_RAM_D3F9_)
	ld bc, $0480
	or a
	sbc hl, bc
	jr c, +
	ld de, (_RAM_D111_)
	or a
	sbc hl, de
	jr nc, _LABEL_3A2B_
+:
	ld hl, (_RAM_D3F9_)
	ld de, $1080
	add hl, de
	ld de, (_RAM_D111_)
	or a
	sbc hl, de
	jr nc, +
	jr _LABEL_3A2B_

+:
	ld a, (ix+22)
	ld (ix+23), a
_LABEL_3A29_:
	and a
	ret

_LABEL_3A2B_:
	ld a, (_RAM_D454_)
	ld b, a
	ld a, (_RAM_D112_)
	cp b
	jr nc, +
	dec (ix+23)
	ret

+:
	xor a
	ret

.org $3a3b
; Data from 3A3B to 3ACA (144 bytes)
_DATA_3A3B_:
.db $00 $00 $10 $10 $20 $20 $30 $30 $34 $34 $38 $38 $3E $3E $15 $3F
.db $10 $2A $0F $05 $20 $24 $35 $39 $3E $00 $01 $02 $07 $10 $1E $3B
.db $10 $3F $3B $26 $15 $2A $01 $02 $03 $07 $0F $20 $24 $39 $3E $00
.db $20 $00 $24 $39 $3D $10 $11 $27 $04 $09 $0E $02 $03 $0B $0F $3F
.db $00 $10 $00 $1F $01 $02 $07 $1B $11 $04 $08 $0E $20 $34 $39 $3F
.db $20 $04 $14 $19 $29 $2E $01 $06 $0B $0F $11 $26 $3B $03 $00 $3F
.db $10 $24 $29 $04 $19 $2E $01 $06 $0B $0F $11 $26 $3B $03 $00 $3F
.db $05 $00 $01 $06 $03 $07 $1B $2F $04 $09 $0E $22 $37 $24 $20 $3F
.db $14 $00 $11 $2A $15 $09 $0E $01 $02 $07 $0F $20 $34 $39 $3E $3F

; Data from 3ACB to 3AEA (32 bytes)
_DATA_3ACB_:
.db $14 $00 $15 $2A $11 $09 $0E $01 $02 $07 $0F $20 $34 $39 $3E $3F
.db $10 $2E $0C $08 $15 $2A $01 $02 $03 $07 $0F $20 $24 $39 $3E $00

_LABEL_3AEB_:
	ld hl, _RAM_C5FC_
	ld (_RAM_C78C_), hl
	ld (hl), $00
	ld a, $D0
	ld (_RAM_C8D6_), a
	ld hl, $C792
	ld (_RAM_C78E_), hl
	ld hl, $C892
	ld (_RAM_C790_), hl
	ld hl, _RAM_C19D_
	ld c, $27
	ld b, $0A
-:
	ld (hl), $FF
	inc l
	ld (hl), $FF
	inc l
	ld (hl), c
	inc l
	call +
	ld a, c
	add a, $0C
	ld c, a
	djnz -
	ret

+:
	ex de, hl
	ld l, c
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld a, h
	add a, $60
	ld h, a
	ex de, hl
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ret

.org $3b30
; Data from 3B30 to 3B41 (18 bytes)
_DATA_3B30_:
.db $B6 $CE $01 $DE $D0 $01 $E4 $CE $07 $26 $D0 $04 $DC $C9 $07 $1E
.db $CB $14 $50 $01 $B4 $68 $D7 $28 $E2 $68 $F3 $08 $DA $28 $F8 $28
.db $FE $28 $06 $29 $0D $29 $14 $09 $6A $49 $96 $49 $1C $29 $1F $49
.db $33 $69 $28 $49 $3F $69 $4B $69 $25 $44 $AE $24 $BE $44 $C8 $44
.db $3B $2E $50 $69 $60 $89 $E0 $49 $58 $2A $47 $2A $EC $09 $06 $0A
.db $18 $0A $25 $0A $6F $0A $DA $6E $9C $2E $B6 $04 $F4 $2A $08 $2B
.db $12 $2B $72 $0B $22 $4B $65 $0B $45 $2B $3C $2B $FD $0B $C5 $0B
.db $DE $0B $AA $2C $C8 $2C $4E $42 $EB $4C $F0 $4C $F5 $4C $01 $4D
.db $45 $0E $67 $0E $55 $0E $19 $06 $35 $11 $6A $52 $61 $12 $3C $52
.db $C0 $91 $09 $12 $24 $0F $65 $0F $A6 $91 $3C $11 $B5 $32 $FA $4A
.db $0B $4D $C3 $12 $CD $12 $DF $12 $D5 $12 $9B $08 $4C $11 $0F $13
.db $CB $08 $CD $04 $28 $0D $2B $0D $36 $0D $41 $0D $47 $0D $4D $0D
.db $55 $0D $1F $08 $31 $08 $04 $0E $77 $0D $64 $0D $9A $0D $A3 $0D
.db $26 $2E $14 $2E $1D $4E $34 $0E $0B $4E $06 $0D $F0 $0A $1E $2B
.db $00 $90 $D8 $8F $BD $0F $7E $0E $D5 $18 $5D $13 $74 $13 $11 $25
.db $BA $0D $8C $2E $01 $6F $9F $30 $AE $10 $2F $86 $52 $0C $43 $0C
.db $35 $0C $95 $0C $87 $0C $7F $4C $77 $4C $71 $0C $0B $46 $F6 $45
.db $E5 $05 $C6 $05 $97 $46 $CC $86 $63 $07 $D2 $96 $03 $17 $0D $17
.db $28 $17 $14 $17 $B7 $16 $90 $16 $F8 $56 $3D $17 $31 $18 $33 $45
.db $6E $05 $BF $05 $6C $17 $87 $17 $9E $24 $8F $04 $70 $24 $03 $13
.db $F8 $22 $00 $03 $3A $03 $9F $11 $5D $11 $CE $02 $C6 $02 $B6 $02
.db $99 $02 $68 $02 $60 $02 $58 $02 $E1 $07 $BF $27 $FC $07 $3C $02
.db $C6 $2D $C9

_LABEL_3C93_:
	ld a, $06
	jr +

_LABEL_3C97_:
	ld a, $00
+:
	ld (_RAM_C22A_), a
	ld hl, _DATA_3D5E_
	ld (_RAM_C227_), hl
-:
	call _LABEL_27F0_
	call +
	jr c, -
	ret

_LABEL_3CAB_:
	ld a, e
	ld (_RAM_C22A_), a
	ld a, d
	and a
	jr z, +
	ld hl, _DATA_3D6E_
	jr ++

+:
	ld hl, _DATA_3D5E_
++:
	ld (_RAM_C227_), hl
	ld a, (iy+2)
	ld (_RAM_C303_), a
	ret

+:
_LABEL_3CC5_:
	ld a, (_RAM_C22A_)
	ld hl, _RAM_C229_
	cp (hl)
	ret z
	jr c, +
	inc (hl)
	jr ++

+:
	dec (hl)
++:
	ld a, (hl)
	srl a
	ret c
	ld c, a
	ld a, (_RAM_C303_)
	bit 0, a
	jr z, ++
	bit 1, a
	jr z, +
	ld ix, _RAM_C304_
	ld b, $20
	jr _LABEL_3CF9_

+:
	ld ix, _RAM_C304_
	ld b, $10
	jr _LABEL_3CF9_

++:
	ld ix, _RAM_C314_
	ld b, $10
_LABEL_3CF9_:
	push bc
	call ++
	pop bc
	djnz _LABEL_3CF9_
	call +
	scf
	ret

+:
	; send palette
	ld hl, _RAM_C324_
/*
	xor a
	di
	out (Port_VDPAddress), a
	ld a, $C0
	out (Port_VDPAddress), a
	ei
	ld b, $20
*/
	ld bc,$2000
	jp send_palette
/*
-:
	ld a, (hl)
	inc hl
	out (Port_VDPData), a
	djnz -
	ret
*/
++:
	ld b, $00
	ld d, $00
	ld a, (ix+0)
	and $30
	rrca
	rrca
	ld e, a
	ld hl, (_RAM_C227_)
	add hl, de
	add hl, bc
	ld a, (hl)
	add a, a
	add a, a
	add a, a
	add a, a
	ld (ix+32), a
	ld a, (ix+0)
	and $0C
	ld e, a
	ld hl, (_RAM_C227_)
	add hl, de
	add hl, bc
	ld a, (hl)
	add a, a
	add a, a
	or (ix+32)
	ld (ix+32), a
	ld a, (ix+0)
	and $03
	add a, a
	add a, a
	ld e, a
	ld hl, (_RAM_C227_)
	add hl, de
	add hl, bc
	ld a, (hl)
	or (ix+32)
	ld (ix+32), a
	inc ix
	ret

.org $3d5e
; Data from 3D5E to 3D6D (16 bytes)
_DATA_3D5E_:					; palette fade in
.db $00 $00 $00 $00 $00 $00 $01 $01 $00 $01 $01 $02 $00 $01 $02 $03

; Data from 3D6E to 3D7D (16 bytes)
_DATA_3D6E_:
.db $03 $02 $01 $00 $03 $02 $02 $01 $03 $03 $02 $02 $03 $03 $03 $03


_LABEL_3D7E_:
	ld hl, $016C
	ld (_RAM_CEC6_), hl
	xor a
	ld (_RAM_CECF_), a
	ld (_RAM_C1FF_), a
	inc a
	ld (_RAM_CECE_), a
	ret

_LABEL_3D90_:
	ld hl, (_RAM_D113_)
	call _LABEL_5B91_	; Possibly invalid
	ld (_RAM_D113_), hl
	ret

_LABEL_3D9A_:
	ld hl, (_RAM_D115_)
	add hl, hl
	ld (_RAM_D115_), hl
	ret

_LABEL_3DA2_:
	ld hl, (_RAM_D113_)
	add hl, hl
	ld (_RAM_D113_), hl
	ret

_LABEL_3DAA_:
	ld hl, $0F80
	call _LABEL_2247_
	ld c, a
	bit 0, a
	ld a, (ix+20)
	and $FE
	bit 0, c
	jr nz, +
	ld bc, $04E0
	sbc hl, bc
	or $01
+:
	ld (_RAM_D10F_), hl
	ld (ix+20), a
	ld hl, $0B00
	call _LABEL_2247_
	bit 0, a
	jr nz, +
	ld bc, $0258
	add hl, bc
+:
	ld (_RAM_D111_), hl
	ret

_LABEL_3DDB_:
	ld a, (_RAM_D44F_)
	and a
	ret z
	dec a
	ld (_RAM_D44F_), a
	ret

_DATA_3DE5_:
; Data from 3DE5 to 3DE8 (4 bytes)
.db $31 $3B $4F $63


_LABEL_3DE9_:
	ld hl, _DATA_3DE5_
	ld a, (_RAM_D9A9_)
	ld c, a
	xor a
	ld (_RAM_C20D_), a
	ld b, a
	add hl, bc
	ld a, (hl)
	ret

_LABEL_3DF8_:
	call _LABEL_3DE9_
	ld (ix+33), a
	ld (ix+44), $33
	ld a, $01
	ld (_RAM_D446_), a
	ret

_LABEL_3E08_:
	ld a, (_RAM_D122_)
	inc a
	ld (_RAM_D122_), a
	ret

_LABEL_3E10_:
	ld a, (_RAM_C20D_)
	or a
	jr nz, +
	ld hl, $000C
	ld (_RAM_CEC6_), hl
	xor a
	ld (_RAM_CECF_), a
	inc a
	ld (_RAM_CECE_), a
	ld (_RAM_C223_), a
	ld (_RAM_C222_), a
	ld a, $00
	ld (_RAM_D438_), a
	ld (_RAM_D455_), a
	ret

+:
	ld hl, $01A0
	ld (_RAM_C9D9_), hl
	ret

_LABEL_3E3A_:
	ld a, (ix+43)
	add a, e
	ld (ix+26), a
	ret

_LABEL_3E42_:
	ld hl, _RAM_D7DA_
	ld d, $00
	ld a, (_RAM_C20D_)
	ld e, a
	inc a
	ld (_RAM_C20D_), a
	add hl, de
	ld a, (hl)
	add a, $2C
	ld e, a
	call _LABEL_441C_	; Possibly invalid
	ret

_DATA_3E58_:
; Data from 3E58 to 3E7B (36 bytes)
.db $00 $00 $00 $00 $02 $02 $02 $00 $01 $01 $01 $00 $00 $01 $02 $00
.db $01 $00 $01 $00 $02 $00 $01 $00 $01 $02 $02 $00 $00 $01 $00 $00
.db $02 $01 $00 $00

_LABEL_3E7C_:
	ld a, (_RAM_D7DB_)
	ld (_RAM_D7DA_), a
	ld a, (_RAM_D7DC_)
	ld (_RAM_D7DB_), a
	ld a, (ix+43)
	ld (_RAM_D7DC_), a
	ld b, $08

-:
	ld hl, _DATA_3E58_
	ld a, b
	add a, a
	add a, a
	ld e, a
	ld d, $00
	add hl, de
	ld a, (_RAM_D7DA_)
	cp (hl)
	jr nz, +
	inc hl
	ld a, (_RAM_D7DB_)
	cp (hl)
	jr nz, +
	inc hl
	ld a, (_RAM_D7DC_)
	cp (hl)
	jr z, ++
+:
	dec b
	jp p, -
	ret

++:
	push bc
	ld d, $00
	ld e, $6C
	push iy
	call _LABEL_24FE_
	pop iy
	ld a, (_RAM_D9A9_)
	add a, a
	add a, a
	ld h, a
	ld a, $18
	sub h
	ld h, a
	ld l, $00
	ld (_RAM_D44C_), hl
	ld a, $03
	ld (_RAM_D446_), a
	pop af
	inc a
	ld (_RAM_D444_), a
	cp $04
	jr z, _LABEL_3F1A_
	cp $08
	jr z, ++
	cp $06
	jr z, +
	cp $03
	jr nz, _LABEL_3F14_
	ld a, (_RAM_D44D_)
	add a, a
	ld (_RAM_D44D_), a
	jr _LABEL_3F14_

+:
	ld a, (_RAM_D9A9_)
	ld h, a
	ld a, $06
	sub h
	ld h, a
	ld l, $00
	ld (_RAM_D44C_), hl
	ld e, $9D
	call _LABEL_4053_	; Possibly invalid
	ret

++:
	ld a, (_RAM_D9A9_)
	ld c, a
	ld a, $08
	sub c
	add a, a
	add a, a
	add a, a
	add a, a
	dec a
	add a, a
	ld (_RAM_C8C8_), a
_LABEL_3F14_:
	ld e, $19
	call _LABEL_4053_	; Possibly invalid
	ret

_LABEL_3F1A_:
	ld a, (_RAM_D122_)
	inc a
	ld (_RAM_D122_), a
	ld hl, $0105
	ld (_RAM_D44C_), hl
	jr _LABEL_3F14_

_LABEL_3F29_:
	ld e, $15
	call _LABEL_4053_	; Possibly invalid
	ret

_LABEL_3F2F_:
	ld hl, (_RAM_D111_)
	ld a, (_RAM_D116_)
	cp $80
	jr nc, +
	ld a, (ix+39)
	cp h
	jr c, ++++
	jr z, ++++
	jr ++

+:
	ld a, h
	cp $01
	jr c, +++
	ld a, (ix+38)
	cp h
	jr z, ++
	jr nc, +++
++:
	ld a, $01
	or a
	ret

+++:
	res 2, (ix+40)
	jr +++++

++++:
	set 2, (ix+40)
+++++:
	ld (_RAM_D112_), a
	xor a
	ld (_RAM_D111_), a
	xor a
	ret

_LABEL_3F67_:
	res 7, (ix+20)
	call -
	jr nz, +++
	ld hl, (_RAM_D116_)
	jp m, +
	bit 2, (ix+40)
	ret nz
	jr ++

+:
	bit 2, (ix+40)
	ret z
++:
	set 7, (ix+20)
	ret

+++:
	ld b, (iy+1)
	ld c, (iy+0)
	ld hl, (_RAM_D119_)
	ld de, $0004
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld a, (_RAM_D116_)
	or a
	jp p, +
	or a
	ld de, (_RAM_D111_)
	sbc hl, de
	jr c, +
	or a
	sbc hl, bc
	jr nc, ++
+:
	set 7, (ix+20)
++:
	ret

_LABEL_3FB2_:
	ld a, e
	cp $80
	ld a, (_RAM_CED7_)
	jr c, ++
	add a, e
	jr nc, +
	cp $6F
	jr c, +++
+:
	xor a
	jr +++

++:
	add a, e
	jr c, +
	cp $6F
	jr c, +++
+:
	ld a, $6F
+++:
	ld (_RAM_CED7_), a
	ret

_LABEL_3FD1_:
	ld h, (ix+19)
	ld l, $00
	ld (_RAM_D111_), hl
	ld h, (ix+18)
	ld (_RAM_D10F_), hl
	ret

_LABEL_3FE0_:
	res 7, (ix+20)
	ld a, (_RAM_C300_)
	and e
	and $FE
	ret nz
	set 7, (ix+20)
	ret

_LABEL_3FF0_:
	ld a, (ix+33)
	sub e
	ld (ix+33), a
	ret

_LABEL_3FF8_:
	ld a, (ix+33)
	inc a
	ld (_RAM_D449_), a
	ret

.org $4000
_LABEL_4000_:
	ld a, (de)
	and a
	jr z, +
	dec a
	ld (de), a
+:
	ld (_RAM_C1FB_), a
	ret

_LABEL_400A_:
	ld a, (de)
	ld (_RAM_C1FB_), a
	ret

_LABEL_400F_:
	ld a, (_RAM_D447_)
	ld e, a
	ld a, (_RAM_D441_)
	add a, e
	ld (_RAM_D447_), a
	cp $64
	jp nc, _LABEL_4930_
	ld c, a
	ld a, (_RAM_D44E_)
	and a
	ret nz
	ld a, (_RAM_D9A9_)
	ld b, a
	dec b
	ret c
	jr nz, +
	ld a, $19
	jr ++

+:
	dec b
	jr nz, +
	ld a, $32
	jr ++

+:
	ld a, $4B
++:
	cp c
	jr z, +
	ret nc
+:
	ld a, $01
	ld (_RAM_D44E_), a
	ld a, $84
	call _LABEL_912_
	ld a, ($0016)
	ld e, a
	ld a, $06
	or e
	call set_page2
	ret

_LABEL_4053_:
	ld a, (_RAM_D9AF_)
	and $02
	ret z
	ld a, e
	and $80
	jr nz, +
	ld a, (_RAM_C242_)
	cp e
	ret nc
	ld a, e
	ld (_RAM_C242_), a
	ret

+:
	ld a, e
	ld a, (_RAM_C2A6_)
	cp e
	ret nc
	ld a, e
	and $7F
	ld (_RAM_C2A6_), a
	ret

	
_LABEL_4075_:	
		res 7, (ix+20)
		ld a, (_RAM_C203_)
		cp $23
		jr c, ++
		ld a, (_RAM_C1FF_)
		and a
		jr nz, ++
		ld a, (_RAM_D110_)
		ld c, a
		ld a, (_RAM_D0E1_)
		sub c
		ld c, a
		ld a, (_RAM_CECA_)
		bit 0, a
		ld a, c
		jr nz, +
		neg
+:	
		cp e
		jr nc, ++
		ld a, (_RAM_D112_)
		ld c, a
		ld a, (_RAM_D0E3_)
		sub c
		jr nc, +
		neg
+:	
		cp d
		jr nc, ++
		set 7, (ix+20)
++:	
		ret
		call _LABEL_23F3_
		or $07
		ld d, a
		ld e, $0A
		push iy
		push de
		call _LABEL_24FE_
		pop de
		jr nc, +
		ld a, $04
		pop iy
		jr ++
	
+:	
		ld hl, (_RAM_D10F_)
		ld (iy+2), l
		ld (iy+3), h
		ld hl, (_RAM_D111_)
		ld (iy+4), l
		ld (iy+5), h
		ld a, d
		ld (iy+27), a
		call _LABEL_46C2_
		ld (iy+6), l
		ld (iy+7), h
		ld (iy+8), e
		ld (iy+9), d
		push ix
		pop hl
		ld (iy+13), h
		ld (iy+12), l
		ld a, $00
		ld (_RAM_D119_), a
		pop iy
		xor a
++:	
		ld (_RAM_C1FB_), a
		ret
	
_LABEL_4101_:	
		ld a, (_RAM_D119_)
		ld (_RAM_C1FB_), a
		ret
	
_LABEL_4108_:	
		ld a, (ix+43)
		ld c, a
		srl a
		srl a
		srl a
		srl a
		ld e, a
		ld a, $0F
		and c
		ld d, a
		ld a, (_RAM_D112_)
		ld b, a
		sub e
		ld (ix+38), a
		ld a, d
		add a, e
		ld (ix+39), a
		ret
	
_LABEL_4127_:	
		ld a, (ix+43)
		ld c, a
		srl a
		srl a
		srl a
		srl a
		ld e, a
		ld a, $0F
		and c
		ld d, a
		ld a, (_RAM_D110_)
		ld b, a
		sub e
		ld (ix+38), a
		add a, e
		add a, d
		ld (ix+39), a
		xor a
		ld (_RAM_D117_), a
		ld (_RAM_D118_), a
		ret
	
_LABEL_414D_:	
		ld a, (_RAM_D112_)
		ld b, a
		sub e
		ld (ix+38), a
		add a, e
		add a, d
		ld (ix+39), a
		ret
	
_LABEL_415B_:	
		res 7, (ix+20)
		ld hl, (_RAM_D10F_)
		ld bc, (_RAM_D113_)
		add hl, bc
		ld a, (_RAM_D453_)
		cp h
		ret nc
		set 7, (ix+20)
		ret
	
_LABEL_4171_:	
		set 7, (ix+20)
		ld a, (ix+1)
		and a
		jr z, +
		ld a, (_RAM_D444_)
		cp $01
		ret z
		cp $06
		jr nz, +
		ld (ix+33), $FF
+:	
		ld b, (iy+2)
		ld c, $00
		srl b
		rr c
		ld (_RAM_D142_), bc
		ld bc, (_RAM_D117_)
		ld hl, (_RAM_D113_)
		ld a, h
		cp $80
		jr nc, +
		add hl, bc
		ld a, h
		cp $80
		jr c, ++
		ld a, (iy+3)
		ld h, $00
		ld l, a
		jr ++
	
+:	
		add hl, bc
		ld a, h
		cp $80
		jr nc, ++
		ld a, (iy+3)
		ld h, $FF
		neg
		ld l, a
++:	
		ld (_RAM_D113_), hl
		ld bc, (_RAM_D10F_)
		add hl, bc
		ld (_RAM_D10F_), hl
		push hl
		push de
		pop bc
		pop de
		ld hl, (_RAM_D111_)
		call _LABEL_209A_
		ret nz
		res 7, (ix+20)
		ret
	
_LABEL_41D9_:	
		res 7, (ix+20)
		call +
		ret nz
		set 7, (ix+20)
		ret
	
+:	
		ld hl, (_RAM_D10F_)
		ld a, (_RAM_D114_)
		cp $80
		jr nc, +
		add hl, de
		ld a, (ix+39)
		cp h
		jr c, +++
		jr z, +++
		jr ++
	
+:	
		ld a, h
		cp $01
		jr c, +++
		ld a, (ix+38)
		cp h
		jr z, ++
		jr nc, +++
++:	
		ld a, $01
		or a
		ret
	
+++:	
		xor a
		ret
	
_LABEL_420E_:	
		ld a, (_RAM_C203_)
		and a
		jp z, _LABEL_429C_
		xor a
		ld (_RAM_C204_), a
		call _LABEL_4D02_
		bit 7, (ix+20)
		jr nz, _LABEL_4285_
		ld a, (_RAM_CECA_)
		bit 0, a
		jr z, +
		ld hl, (_RAM_D113_)
		ld de, $0002
		add hl, de
		push hl
		ld de, $0040
		sbc hl, de
		pop hl
		jr nc, +++
		jr ++
	
+:	
		and a
		ld hl, (_RAM_D113_)
		ld de, $0002
		sbc hl, de
		push hl
		ld de, $0040
		add hl, de
		pop hl
		jr nc, +++
++:	
		ld (_RAM_D113_), hl
+++:	
		ld hl, (_RAM_D111_)
		add hl, hl
		add hl, hl
		ld c, h
		ld hl, (_RAM_D0E2_)
		ld de, $0040
		add hl, de
		add hl, hl
		add hl, hl
		ld a, h
		ld de, $0010
		ld hl, (_RAM_D115_)
		sub c
		jr nc, +
		and a
		sbc hl, de
		jr ++
	
+:	
		add hl, de
++:	
		ld c, a
		ld de, $0040
		push hl
		ld a, h
		cp $80
		jr c, +
		add hl, de
		jr ++
	
+:	
		sbc hl, de
++:	
		pop hl
		jr nc, +
		ld (_RAM_D115_), hl
+:	
		jr +
	
_LABEL_4285_:	
		xor a
		ld (_RAM_C203_), a
		ld a, (ix+0)
		ld (_RAM_C1FF_), a
		ld hl, $0000
		ld (_RAM_D113_), hl
		ld (_RAM_D115_), hl
		ld a, $08
		jr ++
	
_LABEL_429C_:	
		ld a, $04
		jr ++
	
+:	
		xor a
++:	
		ld (_RAM_C1FB_), a
		ret
	
_LABEL_42A5_:	
		xor a
		ld (_RAM_C1FB_), a
		ld a, (_RAM_C203_)
		and a
		jr nz, +
		ld a, $04
		ld (_RAM_C1FB_), a
		ret
	
+:	
		ld a, (_RAM_CECA_)
		bit 0, a
		jr z, +
		ld hl, (_RAM_D10F_)
		ex de, hl
		ld hl, (_RAM_CEB8_)
		sbc hl, de
		ld de, $0200
		sbc hl, de
		ld a, h
		cp $80
		jr nc, ++
		ld hl, (_RAM_D113_)
		inc hl
		inc hl
		ld a, l
		cp $20
		ret nc
		jr +++
	
+:	
		ld hl, (_RAM_D10F_)
		ex de, hl
		ld hl, (_RAM_D0E0_)
		sbc hl, de
		ld de, $0180
		add hl, de
		ld a, h
		cp $80
		jr c, ++
		ld hl, (_RAM_D113_)
		dec hl
		dec hl
		push hl
		ld bc, $0020
		add hl, bc
		pop hl
		ret nc
		jr +++
	
++:	
		ld hl, $0000
+++:	
		ld (_RAM_D113_), hl
		ret
	
_LABEL_4301_:	
		push de
		ld hl, $CEE4
		ld de, $002E
		ld b, $07
		xor a
		ld c, a
-:	
		cp (hl)
		jr z, +
		inc c
+:	
		add hl, de
		djnz -
		pop de
		ld a, c
		cp e
		jr nc, +
		res 7, (ix+20)
		jr ++
	
+:	
		set 7, (ix+20)
++:	
		ret
	
_LABEL_4323_:	
		call _LABEL_2247_
		and $07
		ld e, a
		ld d, $00
		ld hl, _DATA_4336_
		add hl, de
		ld e, (hl)
		ld d, $00
		call _LABEL_44D8_
		ret
	
; Pointer Table from 4336 to 433D (4 entries, indexed by unknown)	
_DATA_4336_:	
	.dw $7376 $7472 $7372 $7674
	
_LABEL_433E_:	
		ld a, (_RAM_D9A9_)
		ld c, a
		ld a, (ix+43)
		dec a
		jr z, +
		dec a
		jr z, ++
		ld a, $05
		sub c
		add a, a
		add a, a
		add a, a
		add a, a
		ld e, a
		call _LABEL_3FB2_
		jr +++
	
+:	
		jp _LABEL_4930_
	
++:	
		ld e, $35
		jp _LABEL_4908_
	
+++:	
		ret
	
_LABEL_4361_:	
		ld a, (_RAM_C1FB_)
		cp (iy+2)
		jr nz, +
		add iy, de
		ret
	
+:	
		inc iy
		inc iy
		inc iy
		ret
	
_LABEL_4373_:	
		ld a, (_RAM_C1FB_)
		cp (iy+2)
		jr z, +
		add iy, de
		ret
	
+:	
		inc iy
		inc iy
		inc iy
		ret
	
; Pointer Table from 4385 to 43DC (44 entries, indexed by $C9FB)	
_DATA_4385_:	
	.dw $0001 $0001 $0002 $0002 $0003 $0003 $0003 $0105
	.dw $0003 $FE0A $0004 $0005 $0003 $0006 $0004 $0004
	.dw $0004 $0002 $0001 $0004 $0001 $0002 $0002 $0001
	.dw $0003 $0004 $0004 $0001 $0002 $0004 $0003 $0002
	.dw $0004 $0003 $0002 $0003 $0003 $0005 $0003 $0104
	.dw $0005 $0004 $0002 $0002
	
	
_LABEL_43DD_:	
		res 7, (ix+20)
		call _LABEL_22C8_
		call _LABEL_16C7_
		ret z
		ld a, $01
		ld (_RAM_C22B_), a
		set 7, (ix+20)
		xor a
		ld (_RAM_C1E9_), a
		ret
	
_LABEL_43F6_:	
		ld a, e
		ld (_RAM_D11D_), a
		ld a, d
		ld (_RAM_D11E_), a
		ld (ix+24), $01
		ld a, (iy+2)
		ld (ix+25), a
		set 1, (ix+20)
		ret
	
_LABEL_440D_:	
		res 1, (ix+20)
		ld a, (_RAM_D436_)
		ld e, a
		call +
		ret
	
+:	
		ld (ix+21), e
_LABEL_441C_:	
		ld (ix+26), e
		set 2, (ix+20)
		ret
	
_LABEL_4424_:	
		ld (ix+30), e
		ld (ix+31), d
		ld a, (iy+2)
		ld (ix+32), a
		set 5, (ix+20)
		ret
	
_LABEL_4435_:	
		res 5, (ix+20)
		ret
		ld a, e
		or (ix+20)
		jr +
	
_LABEL_4440_:	
		ld a, e
		and (ix+20)
+:	
		ld (ix+20), a
		ret
	
_LABEL_4448_:	
		ld a, e
		ld hl, _RAM_C9D8_
		or (hl)
		jr +
	
_LABEL_444F_:	
		ld a, e
		ld hl, _RAM_C9D8_
		and (hl)
+:	
		ld (hl), a
		ret
	
_LABEL_4456_:	
		bit 3, (ix+20)
		ret z
		jp _LABEL_48DC_
	
_LABEL_445E_:	
		ld d, (ix+34)
		ld a, d
		push ix
		pop hl
		ld d, $00
		add hl, de
		ld (hl), a
		ret
	
_LABEL_446A_:	
		ld c, d
		push ix
		pop hl
		ld d, $00
		add hl, de
		ld a, (hl)
		add a, c
		ld (hl), a
		ret
	
_LABEL_4475_:	
		ld a, (de)
		ld b, $00
		ld c, (iy+2)
		push ix
		pop hl
		add hl, bc
		ld (hl), a
		ret
	
_LABEL_4481_:	
		ld b, $00
		ld c, (iy+2)
		push ix
		pop hl
		add hl, bc
		ld a, (hl)
		ld (de), a
		ret
	
_LABEL_448D_:	
		ld a, (iy+2)
		ld (de), a
		ret
	
_LABEL_4492_:	
		ld a, (de)
		ld b, $00
		ld c, (iy+2)
		ld hl, $D10D
		add hl, bc
		ld (hl), a
		inc de
		inc hl
		ld a, (de)
		ld (hl), a
		ret
	
_LABEL_44A2_:	
		ld a, (iy+2)
		ld (de), a
		inc de
		ld a, (iy+3)
		ld (de), a
		ret
	
_LABEL_44AC_:	
		res 7, (ix+20)
		push iy
		push de
		ld d, $00
		call _LABEL_24FE_
		pop de
		jr c, +
		set 7, (ix+20)
		xor a
		ld (iy+20), a
		ld hl, (_RAM_D10F_)
		ld (iy+3), h
		ld (iy+2), l
		ld hl, (_RAM_D111_)
		ld (iy+5), h
		ld (iy+4), l
+:	
		pop iy
		ret
	
	
_LABEL_44D8_:	
		res 7, (ix+20)
		push iy
		push de
		ld d, $00
		call _LABEL_24FE_
		pop de
		jr c, ++
		set 7, (ix+20)
		ld (_RAM_D119_), iy
		xor a
		ld (iy+20), a
		ld hl, (_RAM_D10F_)
		ld (iy+3), h
		ld (iy+2), l
		ld (iy+18), h
		ld hl, (_RAM_D111_)
		ld (iy+5), h
		ld (iy+4), l
		ld (iy+19), h
		ld hl, $0000
		bit 0, d
		jr z, +
		ld hl, (_RAM_D11F_)
+:	
		ld (iy+13), h
		ld (iy+12), l
		ld (iy+43), $88
++:	
		pop iy
		ret
	
_LABEL_4522_:	
		ld hl, (_RAM_D119_)
		ld a, l
		or h
		ret z
		ld a, d
		ld d, $00
		add hl, de
		ld (hl), a
		ret
	
_LABEL_452E_:	
		ld hl, (_RAM_D119_)
		ld a, l
		or h
		ret z
		ld bc, $0002
		add hl, bc
		ld de, (_RAM_D10F_)
		ld (hl), e
		inc hl
		ld (hl), d
		inc hl
		ld de, (_RAM_D111_)
		ld (hl), e
		inc hl
		ld (hl), d
		ret
	
_LABEL_4548_:	
		ld hl, (_RAM_D119_)
		ld a, l
		or h
		ret z
		ld bc, $0004
		add hl, bc
		ld de, (_RAM_D111_)
		ld (hl), e
		inc hl
		ld (hl), d
		ret
	
_LABEL_455A_:	
		ld hl, (_RAM_C218_)
		ld (_RAM_C21A_), hl
		ld hl, (_RAM_D10F_)
		ld (_RAM_C218_), hl
		ret
	
_LABEL_4567_:	
		ld a, (ix+38)
		ld e, a
		ld a, (ix+39)
		ld d, a
		ld hl, (_RAM_D119_)
		ld a, h
		or l
		jr z, +
		ld de, $0002
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
+:	
		ld hl, (_RAM_C21A_)
		add hl, de
		srl h
		rr l
		ld (_RAM_D10F_), hl
		ret
	
_LABEL_4589_:	
		ld hl, (_RAM_D119_)
		ld a, l
		or h
		ret z
		ld d, $00
		add hl, de
		ex de, hl
		push ix
		pop bc
		add hl, bc
		ld a, (hl)
		ld (de), a
		ret
	
_LABEL_459A_:	
		ld hl, (_RAM_D119_)
		ld a, l
		or h
		ret z
		ld a, e
		cpl
		ld b, a
		ld a, (ix+20)
		and e
		ld c, a
		ld de, $0014
		add hl, de
		ld a, (hl)
		and b
		or c
		ld (hl), a
		ret
	
_LABEL_45B1_:	
		ld a, (_RAM_C300_)
		and $03
		add a, $03
		ld e, a
		call _LABEL_441C_
		ld bc, $0030
		ld de, $0180
		ld hl, (_RAM_CEB8_)
		ld a, (_RAM_CECA_)
		bit 0, a
		ld a, (_RAM_C8B2_)
		jr z, +
		ld bc, $FFD0
		ld de, $0000
		neg
+:	
		add hl, de
		ld e, a
		add a, a
		sbc a, a
		ld d, a
		add hl, de
		ld (_RAM_D10F_), hl
		ld hl, _DATA_5363_
		ld a, (_RAM_D438_)
		add a, a
		ld e, a
		ld d, $00
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ld hl, (_RAM_CEBA_)
		add hl, de
		ld (_RAM_D111_), hl
		ld hl, (_RAM_CEBC_)
		add hl, bc
		ld a, (_RAM_D444_)
		cp $07
		jr nz, +
		add hl, bc
+:	
		ld (_RAM_D113_), hl
		ld hl, (_RAM_CEBE_)
		ld bc, $000F
		sbc hl, bc
		ld a, (_RAM_C1D5_)
		cp $FF
		jr nz, +
		ld bc, $FFE1
		add hl, bc
+:	
		ld (_RAM_D115_), hl
		add hl, hl
		ld a, h
		and a
		jr z, ++
		cp $FF
		jr z, ++
		add a, a
		jr c, +
		ld hl, $0080
		jr ++
	
+:	
		ld hl, $FF80
		ld (_RAM_D115_), hl
++:	
		ret
	
_LABEL_4632_:	
		ld a, (ix+34)
		ld e, a
		ld (ix+27), e
		jp _LABEL_46B4_
	
_LABEL_463C_:	
		ld a, e
		bit 0, (ix+20)
		jr z, +
		ld a, d
+:	
		ld (ix+27), a
		jp _LABEL_46B4_
	
_LABEL_464A_:	
		ld a, e
		add a, (ix+27)
		ld (ix+27), a
		jp _LABEL_46B4_
	
_LABEL_4654_:	
		ld a, (ix+27)
		and $F8
		or e
		ld (ix+27), a
		jp _LABEL_46B4_
	
_LABEL_4660_:	
		ld d, e
		ld e, (ix+34)
		ld a, (ix+27)
		ld c, a
		and $F8
		sub e
		ret z
		ld a, c
		jp p, +
		add a, d
		ld (ix+27), a
		jp _LABEL_46B4_
	
+:	
		sub d
		ld (ix+27), a
		jp _LABEL_46B4_
		push de
		call _LABEL_23F3_
		pop de
		add a, $40
		jr +
	
_LABEL_4687_:	
		push de
		call _LABEL_23F3_
		pop de
+:	
		ld b, a
		ld a, (ix+27)
		ld c, a
		and $F8
		sub b
		ret z
		ld a, c
		jp p, +
		add a, e
		ld (ix+27), a
		jp _LABEL_46B4_
	
+:	
		sub e
		ld (ix+27), a
		jp _LABEL_46B4_
	
_LABEL_46A7_:	
		call _LABEL_23F3_
		ld b, a
		ld a, (ix+27)
		and $07
		or b
		ld (ix+27), a
	
_LABEL_46B4_:	
		ld a, (ix+27)
		call _LABEL_46C2_
		ld (_RAM_D113_), hl
		ld (_RAM_D115_), de
		ret
	
_LABEL_46C2_:	
		ld b, a
		and $07
		jr z, ++
		dec a
		ld c, a
		ld a, b
		rra
		rra
		rra
		and $1F
		ld b, a
		add a, $7E
		ld l, a
		ld h, $D9
		ld a, (hl)
		add a, c
		add a, a
		ld l, a
		ld e, (hl)
		inc l
		ld d, (hl)
		jr nc, +
		and a
		ld hl, $0000
		sbc hl, de
		ex de, hl
+:	
		push de
		ld a, b
		add a, $08
		and $1F
		add a, $7E
		ld l, a
		ld h, $D9
		ld a, (hl)
		add a, c
		add a, a
		ld l, a
		ld e, (hl)
		inc l
		ld d, (hl)
		jr c, +
		ld hl, $0000
		sbc hl, de
		ex de, hl
+:	
		pop hl
		ret
	
++:	
		ld hl, $0000
		ld d, h
		ld e, l
		ret
	
_LABEL_4707_:	
		ld hl, (_RAM_CEB8_)
		ld de, (_RAM_D10F_)
		xor a
		sbc hl, de
		sbc a, a
		ld c, a
		and $01
		ld b, a
		ld a, (ix+20)
		and $FE
		or b
		ld (ix+20), a
		ret
	
_LABEL_4720_:	
		res 7, (ix+20)
		ld hl, (_RAM_CEB8_)
		ld de, (_RAM_D10F_)
		xor a
		sbc hl, de
		sbc a, a
		ld c, a
		and $01
		ld b, a
		ld a, (ix+20)
		and $01
		xor b
		ret nz
		set 7, (ix+20)
		ret
	
_LABEL_473F_:	
		res 7, (ix+20)
		call -
		ld de, (_RAM_D113_)
		ld a, d
		xor c
		jr z, +
		ld hl, $0000
		and a
		sbc hl, de
		ld (_RAM_D113_), hl
		set 7, (ix+20)
+:	
		ld a, (ix+27)
		ld b, a
		and $F8
		add a, $40
		add a, a
		sbc a, a
		xor c
		ret z
		ld a, b
		and $F8
		ld c, a
		ld a, $80
		sub c
		ld c, a
		ld a, b
		and $07
		or c
		ld (ix+27), a
		ret

_LABEL_4777_:
-:	
		ld bc, (_RAM_D113_)
		ld hl, $0000
		and a
		sbc hl, bc
		ld (_RAM_D113_), hl
		ret
	
_LABEL_4785_:	
		ld hl, (_RAM_D113_)
		ld b, h
		ld c, l
		sra h
		rr l
		sbc hl, bc
		ld (_RAM_D113_), hl
		ret
	
_LABEL_4794_:	
		call -
		ld a, (ix+20)
		xor $01
		ld (ix+20), a
		ld a, (ix+27)
		ld b, a
		and $F8
		ld c, a
		ld a, $80
		sub c
		ld c, a
		ld a, b
		and $07
		or c
		ld (ix+27), a
		ex de, hl
		ret
	
_LABEL_47B3_:	
		ld a, (ix+43)
		ld d, a
		ld e, $00
		ld hl, (_RAM_D10F_)
		add hl, de
		ld (_RAM_D10F_), hl
		ret
	
_LABEL_47C1_:	
		ld hl, (_RAM_D111_)
		add hl, de
		ld (_RAM_D111_), hl
		ret
	
_LABEL_47C9_:	
		ld bc, (_RAM_D115_)
		ld hl, $0000
		and a
		sbc hl, bc
		ld (_RAM_D115_), hl
		ret
	
_LABEL_47D7_:	
		call -
		ld a, (ix+27)
		ld c, a
		neg
		and $F8
		ld b, a
		ld a, c
		and $07
		or b
		ld (ix+27), a
		ex de, hl
		ret
	
_LABEL_47EC_:	
		res 7, (ix+20)
		ld b, e
		ld hl, (_RAM_CEB8_)
		ld de, $00C0
		add hl, de
		ld de, (_RAM_D10F_)
		sbc hl, de
		ld a, h
		jr nc, +
		neg
+:	
		cp b
		ret nc
		set 7, (ix+20)
		ret
	
_LABEL_480A_:	
		res 7, (ix+20)
		ld b, e
		ld hl, (_RAM_CEB8_)
		ld de, $00C0
		add hl, de
		ld de, (_RAM_D10F_)
		ld a, (ix+20)
		and $01
		jr z, +
		ex de, hl
+:	
		sbc hl, de
		ret c
		ld a, h
		cp b
		ret nc
		set 7, (ix+20)
		ret
	
_LABEL_482D_:	
		ld (_RAM_D113_), de
		ret
	
_LABEL_4832_:	
		ld (_RAM_D117_), de
		ret
	
_LABEL_4837_:	
		call -
		bit 0, (ix+20)
		ret z
		call _LABEL_4777_
		ret
	
	
-:	
		ld (_RAM_D115_), de
		ret
	
_LABEL_4848_:	
		ld d, $A0
		ld e, (ix+34)
_LABEL_484D_:	
		ld hl, (_RAM_D115_)
		ld c, e
		ld b, $00
		add hl, bc
		ld (_RAM_C1FC_), bc
		ex de, hl
		bit 7, d
		jr nz, -
		ld b, $00
		ld c, h
		ld h, d
		ld l, e
		sbc hl, bc
		jr c, -
		ld e, c
		ld d, b
		jr -
	
_LABEL_486A_:	
		add iy, de
		ret
	
_LABEL_486D_:	
		bit 7, (ix+20)
		jr nz, -
		inc iy
		inc iy
		ret
	
_LABEL_4878_:	
		bit 7, (ix+20)
		jr z, -
		inc iy
		inc iy
		ret
	
_LABEL_4883_:	
		ld (ix+28), e
		inc iy
		ret
	
_LABEL_4889_:	
		ld (ix+29), e
		inc iy
		ret
		dec (ix+28)
		jr nz, +
		inc iy
		ret
	
_LABEL_4897_:	
		dec (ix+29)
		jr nz, +
		inc iy
		ret
	
+:	
		ld a, e
		add a, a
		sbc a, a
		ld d, a
		add iy, de
		ret
	
_LABEL_48A6_:	
		ld h, $00
		ld a, (ix+0)
		cp $61
		ret nc
		ld l, (ix+37)
		ld bc, (_RAM_C34C_)
		add hl, bc
		set 6, (hl)
		ret
	
_LABEL_48B9_:	
		ld a, (ix+18)
		add a, $02
		ld b, a
		ld a, (_RAM_D3F8_)
		sub b
		jr nc, +
		add a, $13
		jr nc, +
		ld a, (ix+19)
		add a, $02
		ld b, a
		ld a, (_RAM_D3FA_)
		sub b
		jr nc, +
		add a, $0D
		ret c
+:	
		call +
		ret
	
	
_LABEL_48DC_:	
		ld (ix+0), $00
		res 5, (ix+20)
		ret
	
+:	
		ld h, $00
		ld a, (ix+0)
		cp $61
		jr nc, +
		ld l, (ix+37)
		ld bc, (_RAM_C34C_)
		add hl, bc
		res 7, (hl)
+:	
		call _LABEL_48DC_
		ret
	
_LABEL_48FC_:	
		call +
		ld a, (_RAM_D444_)
		cp $05
		ret nz
+:	
		ld e, (ix+44)
	

_LABEL_4908_:
--:
	ld hl, $D7C0
	ld a, e
	and $0F
	ld c, a
	srl e
	srl e
	srl e
	srl e
	ld d, $00
	add hl, de
	ld a, (_RAM_D7C1_)
	ld e, a
-:
	ld a, (hl)
	add a, c
	ld (hl), a
	sub $0A
	jr c, +
	ld (hl), a
	dec hl
	ld c, $01
	djnz -
+:
	ld a, (_RAM_D7C1_)
	cp e
	ret z
_LABEL_4930_:
	ld e, $15
	call _LABEL_4053_
	ld a, (_RAM_D121_)
	cp $09
	jr z, +
	inc a
	ld (_RAM_D121_), a
	ret

+:
	ld e, $35
	jp --

_LABEL_4946_:
	ld a, (ix+43)
	ld (ix+29), a
	ret

_LABEL_494D_:
	call _LABEL_2247_
	and e
	add a, d
	ld (ix+34), a
	ret

_LABEL_4956_:
	call _LABEL_2247_
	and e
	inc a
	ld (ix+28), a
	ret

_LABEL_495F_:
	call -
	cp d
	ret nc
	ld (ix+28), d
	ret

_LABEL_4968_:
	res 7, (ix+20)
	call _LABEL_2247_
	cp e
	ret nc
	set 7, (ix+20)
	ret

_LABEL_4976_:
	ld a, (ix+34)
	ld (ix+28), a
	ret

_LABEL_497D_:
	push de
	call _LABEL_2247_
	pop de
	and e
	ld (_RAM_C1FB_), a
	ret
	ld hl, (_RAM_D113_)
	ld bc, $0001
	call _LABEL_5B5A_
	ld (_RAM_D113_), hl
	call +
	ret

_LABEL_4997_:
	ld a, (_RAM_D444_)
	cp $01
	ret z
	cp $06
	jr nz, +
	ld (ix+33), $FF
+:
	call +
	ret

+:
	ld de, (_RAM_D115_)
	ld hl, (_RAM_D111_)
	add hl, de
	ld (_RAM_D111_), hl
+:
	ld de, (_RAM_D113_)
	ld hl, (_RAM_D10F_)
	add hl, de
	ld (_RAM_D10F_), hl
	ret

_LABEL_49C0_:	
		res 7, (ix+20)
		bit 0, (ix+20)
		ret z
		set 7, (ix+20)
		ret
	
_LABEL_49CE_:	
		res 7, (ix+20)
		ld a, (_RAM_C2D0_)
		or a
		ret z
		and e
		ret z
		set 7, (ix+20)
		ret
	
_LABEL_49DE_:	
		res 7, (ix+20)
		ld a, (_RAM_C2D0_)
		or a
		ret z
		ld c, a
		ld a, $20
		and c
		ret z
		ld hl, (_RAM_C2D1_)
		ld b, $00
		ld c, $21
		push hl
		add hl, bc
		ld a, e
		add a, (hl)
		ld (hl), a
		pop hl
		ld c, $28
		add hl, bc
		cp $05
		jr c, ++
		cp $80
		jr c, +
		xor a
		jr ++
	
+:	
		ld a, $05
++:	
		sla a
		sla a
		ld c, a
		ld a, $18
		sbc a, c
		ld (hl), a
		ld e, $1A
		call _LABEL_4053_
		set 7, (ix+20)
		ret
	
_LABEL_4A1C_:	
		res 7, (ix+20)
		ld a, (_RAM_C8C8_)
		or a
		ret nz
		call ++
		bit 7, (ix+20)
		ret z
		ld hl, (_RAM_CEB8_)
		ld bc, (_RAM_D10F_)
		sbc hl, bc
		ld a, h
		cp $80
		ld a, $2C
		jr c, +
		neg
+:	
		ld (_RAM_C225_), a
		ret
	
++:	
		res 7, (ix+20)
		ld a, (_RAM_C2D0_)
		or a
		ret z
		ld c, a
		ld a, (iy+2)
		and c
		ret z
		ld hl, (_RAM_C2D1_)
		ld b, $00
		ld c, e
		add hl, bc
		ld a, d
		add a, (hl)
		ld (hl), a
		ld e, $14
		call _LABEL_4053_
		set 7, (ix+20)
		ret
	
_LABEL_4A66_:	
		ld a, (_RAM_D438_)
		cp $02
		jr z, +
		cp $07
		jr nz, ++
+:	
		set 7, (ix+20)
		ret
	
++:	
		ld hl, $018C
		ld (_RAM_CEC6_), hl
		xor a
		ld (_RAM_CECF_), a
		inc a
		ld (_RAM_CECE_), a
		ld a, (ix+43)
		ld (_RAM_C222_), a
		ld e, a
		and $F0
		ld b, $00
		cp $80
		jr c, +
		dec b
+:	
		ld c, a
		ld (_RAM_D113_), bc
		ld d, $00
		sla e
		sla e
		sla e
		sla e
		ld (_RAM_D115_), de
		res 7, (ix+20)
		ld hl, (_RAM_D10F_)
		ld a, (_RAM_CECA_)
		bit 0, a
		jr nz, +
		ld bc, $FF00
		add hl, bc
+:	
		ld (_RAM_D15D_), hl
		ld hl, (_RAM_D111_)
		ld bc, $0100
		add hl, bc
		ld (_RAM_D15B_), hl
		ld a, $02
		ld (_RAM_D156_), a
		ld a, (_RAM_C1D8_)
		and a
		jr z, +
		jr ++
	
+:	
		ld a, (_RAM_C221_)
		and a
		jr nz, +++
++:	
		xor a
		ld (_RAM_D156_), a
		ld (_RAM_C221_), a
		ld (_RAM_C222_), a
		ret
	
+++:	
		cp $02
		jr nz, +
		call _LABEL_4794_
		ld hl, (_RAM_D111_)
		ld bc, $0160
		add hl, bc
		ld (_RAM_D111_), hl
		ld a, $01
		ld (_RAM_C221_), a
+:	
		set 7, (ix+20)
		ret
	
_LABEL_4AFF_:	
		res 7, (ix+20)
		ld de, (_RAM_D111_)
		ld hl, (_RAM_D10F_)
		ld bc, $0040
		add hl, bc
		ex de, hl
		call _LABEL_21E5_
		and $01
		ret z
		set 7, (ix+20)
		ret
	
_LABEL_4B1A_:	
		ld b, (iy+3)
		ld c, (iy+2)
		res 7, (ix+20)
		ld hl, (_RAM_D111_)
		add hl, de
		ex de, hl
		ld hl, (_RAM_D10F_)
		ld a, (_RAM_D114_)
		cp $80
		jr nc, +
		add hl, bc
		jr +
	
+:	
		ex de, hl
		call _LABEL_21E5_
		and $09
		ret z
		set 7, (ix+20)
		ret
	
_LABEL_4B42_:	
		ld a, (ix+1)
		and a
		jr z, ++
		ld a, (_RAM_D444_)
		cp $01
		jr z, +
		cp $06
		jr nz, ++
		ld (ix+33), $FF
		jr ++
	
+:	
		xor a
		ld (_RAM_C1FB_), a
		ret
	
++:	
		ld (_RAM_D142_), de
		ld c, (iy+2)
		ld b, (iy+3)
		ld (_RAM_D144_), bc
		ld hl, (_RAM_D113_)
		ld (_RAM_D41A_), hl
		ld hl, (_RAM_D115_)
		ld (_RAM_D41C_), hl
		ld hl, $0000
		call _LABEL_1E60_
		ld a, (_RAM_C1FB_)
		cp $0C
		jr nz, _LABEL_4BCC_
		ld a, (_RAM_D149_)
		and $E0
		cp $80
		jr z, +
		ld bc, (_RAM_D113_)
		xor b
		bit 7, a
		jr nz, +
		call _LABEL_4785_
+:	
		ld de, (_RAM_D115_)
		ld h, d
		ld l, e
		sra h
		rr l
		and a
		sbc hl, de
		ld (_RAM_D115_), hl
		ld bc, (_RAM_C1FC_)
		add hl, bc
		jr c, +
		add hl, bc
		jr nc, ++
+:	
		ld a, $10
		ld (_RAM_C1FB_), a
++:	
		ld hl, (_RAM_D111_)
		ld de, (_RAM_D10F_)
		ld bc, (_RAM_D144_)
		srl b
		rr c
		call _LABEL_209A_
		ret
	
_LABEL_4BCC_:	
		cp $04
		jr nz, +
		jp _LABEL_4785_
	
+:	
		cp $08
		jr nz, +
		ld hl, $0000
		ld (_RAM_D115_), hl
+:	
		ret
	
_LABEL_4BDE_:	
		ret
	
_LABEL_4BDF_:	
		ret
	
_LABEL_4BE0_:	
		ret
	
_LABEL_4BE1_:	
		ld a, e
		call _LABEL_912_
		ld a, ($0016)
		ld e, a
		ld a, $06
		or e
		call set_page2
		ret
	
_LABEL_4BF0_:	
		nop
		ret
	
_LABEL_4BF2_:	
		ld hl, (_RAM_D113_)
		ld a, h
		or a
		jp m, ++
		ld e, (iy+3)
		ld d, $00
		push hl
		or a
		sbc hl, de
		jr nc, +
		ld de, $0001
		ld bc, $0000
		jr +++
	
+:	
		pop hl
		push hl
		ld e, (iy+4)
		inc de
		or a
		sbc hl, de
		jr c, ++++
		ld de, $FFFF
		ld bc, $0000
		jr +++
	
++:	
		ld e, (iy+3)
		ld d, $00
		push hl
		inc hl
		add hl, de
		jr nc, +
		ld de, $FFFF
		ld bc, $0000
		jr +++
	
+:	
		pop hl
		push hl
		ld e, (iy+4)
		add hl, de
		jr c, ++++
		ld de, $0001
		ld bc, $0000
		jr +++
	
+++:	
		pop hl
		add hl, de
		ld (_RAM_D113_), hl
		ld (_RAM_D117_), bc
		jr +++++
	
++++:	
		pop hl
+++++:	
		ld bc, $0001
		ld hl, (_RAM_D115_)
		ld a, l
		or h
		jr z, ++
		ld a, h
		or a
		jp m, +
		sbc hl, bc
		jr c, ++
		ld (_RAM_D115_), bc
		jr ++
	
+:	
		ex de, hl
		ld hl, $0000
		sbc hl, bc
		or a
		sbc hl, de
		jr c, ++
		add hl, de
		ld (_RAM_D115_), hl
++:	
		ret
	
_LABEL_4C77_:	
		ld hl, $01F4
		ld (_RAM_C207_), hl
		ret
	
_LABEL_4C7E_:	
		ld a, $01
		ld (_RAM_C209_), a
		ld hl, (_RAM_D15B_)
		ld bc, $0080
		add hl, bc
		ld (_RAM_D15B_), hl
		ret
	
_LABEL_4C8E_:	
		ld hl, (_RAM_D10F_)
		ld (_RAM_D138_), hl
		ld hl, (_RAM_D111_)
		ld bc, $FF00
		add hl, bc
		ld (_RAM_D13A_), hl
		ret
	
_LABEL_4C9F_:	
		ld de, (_RAM_D119_)
		ld a, d
		neg
		ld hl, (_RAM_D117_)
		ld c, a
		add a, a
		sbc a, a
		ld b, a
		add hl, bc
		ld (_RAM_D117_), hl
		ld a, $20
		add a, d
		sra a
		sra a
		sra a
		sra a
		add a, $2F
		ld (ix+26), a
		ex de, hl
		add hl, de
		ld (_RAM_D119_), hl
		ex de, hl
		add hl, hl
		ex de, hl
		ld a, d
		cp $80
		jr c, +
		neg
		ld d, a
		ld a, h
		jr ++
	
+:	
		ld d, a
		ld a, h
		add a, $80
++:	
		and $F8
		or d
		ld (ix+27), a
		jp _LABEL_46B4_
	
_LABEL_4CE1_:	
		ld hl, $2000
		ld (_RAM_D119_), hl
		ret
	
_LABEL_4CE8_:	
		ld a, (ix+43)
		ld d, a
		and $0F
		ld (iy+3), a
		xor a
		ld (iy+2), a
		ld e, a
		srl d
		srl d
		srl d
		srl d
		call +
		ret
	
+:	
_LABEL_4D02_:
		res 7, (ix+20)
		ld b, d
		ld c, e
		ld hl, (_RAM_D10F_)
		ld de, $0180
		sbc hl, de
		ex de, hl
		ld hl, (_RAM_CEB8_)
		sbc hl, de
		ret c
		ld de, $0180
		sbc hl, de
		jr c, +
		sbc hl, bc
		ret nc
+:	
		ld b, (iy+3)
		ld c, (iy+2)
		ld hl, (_RAM_D111_)
		ld de, $0280
		sbc hl, de
		ex de, hl
		ld hl, (_RAM_CEBA_)
		jr nc, +
		sbc hl, de
		jr ++
	
+:	
		sbc hl, de
		ret c
++:	
		ld de, $0280
		sbc hl, de
		jr c, +
		sbc hl, bc
		ret nc
+:	
		set 7, (ix+20)
		ret
	
_LABEL_4D4B_:	
		res 7, (ix+20)
		ld a, (_RAM_D110_)
		sub $01
		ld b, a
		ld a, (_RAM_CEB9_)
		sub b
		ret c
		sub $01
		jr c, +
		sub $01
		ret nc
+:	
		ld a, (_RAM_D112_)
		sub $02
		ld b, a
		ld a, (_RAM_CEBB_)
		jr nc, +
		sub b
		jr ++
	
+:	
		sub b
		ret c
++:	
		ld b, $02
		sub b
		jr c, +
		sub $01
		ret nc
+:	
		set 7, (ix+20)
		ret
	
_LABEL_4D7E_:	
		res 7, (ix+20)
		ld a, (_RAM_C8C8_)
		and a
		jr z, +
		ld a, (_RAM_D438_)
		cp $02
		ret z
+:	
		call +
		bit 7, (ix+20)
		ret z
		ld a, (ix+45)
		dec a
		ret z
		ld (ix+45), a
		res 7, (ix+20)
		ret
	
_LABEL_4DA3_:	
		ld d, (ix+43)
		ld e, $00
		call +
		ret
	
+:	
		res 7, (ix+20)
		ld a, (_RAM_CEBF_)
		or a
		ret m
		ld b, d
		ld c, e
		ld hl, (_RAM_D10F_)
		ld de, $00C0
		sbc hl, de
		ex de, hl
		ld hl, (_RAM_CEB8_)
		sbc hl, de
		ret c
		sbc hl, bc
		ret nc
		ld hl, (_RAM_CEBA_)
		ld bc, $0200
		add hl, bc
		ex de, hl
		ld hl, (_RAM_D111_)
		sbc hl, de
		ret c
		ld bc, $0100
		sbc hl, bc
		ret nc
		ld a, $01
		ld (_RAM_D156_), a
		ld hl, (_RAM_D111_)
		ld bc, $FD80
		add hl, bc
		ld (_RAM_D15B_), hl
		ld hl, (_RAM_D113_)
		ld (_RAM_D157_), hl
		set 7, (ix+20)
		ret
	
_LABEL_4DF7_:	
		res 7, (ix+20)
		ld a, (_RAM_C205_)
		cp e
		ret c
		set 7, (ix+20)
		ret
	
_LABEL_4E05_:	
		ld a, (_RAM_C20B_)
		cpl
		or (ix+43)
		cpl
		jr +
	
_LABEL_4E0F_:	
		ld a, (_RAM_C20B_)
		or (ix+43)
		jr +
	
_LABEL_4E17_:	
		ld a, (_RAM_C20B_)
		xor (ix+43)
	
+:	
		ld (_RAM_C20B_), a
		ret
	
_LABEL_4E21_:	
		res 7, (ix+20)
		ld a, (_RAM_C20B_)
		and (ix+43)
		ret z
		set 7, (ix+20)
		ret
	
_LABEL_4E31_:	
		add a, a
		add a, a
		ld c, a
		ld b, $00
		ld a, ($0016)
		ld e, a
		ld a, $04
		or e
		call set_page2
		ld hl, $BB10
		add hl, bc
		ret
_LABEL_4E45_:	
		ld a, (_RAM_C20E_)
		call -
		ld bc, $0005
		add hl, bc
		jr +
	
_LABEL_4E51_:	
		ld a, (ix+43)
		ld (_RAM_C20E_), a
		call -
		xor a
		ld (_RAM_D44A_), a
		ld a, (hl)
		ld (_RAM_D44B_), a
		ld a, $02
		ld (_RAM_D446_), a
		inc hl
		ld a, (hl)
		ld (_RAM_D44F_), a
		inc hl
+:	
		ld b, (hl)
		ld c, $00
		sla b
		inc hl
		ld (_RAM_D138_), bc
		ld b, (hl)
		sla b
		inc hl
		ld (_RAM_D13A_), bc
		ld a, (hl)
		inc hl
		ld (_RAM_D127_), a
		ld hl, _RAM_C9D8_
		set 3, (hl)
		ld a, ($0016)
		ld e, a
		ld a, $06
		or e
		jp set_page2
		ret

_LABEL_4E94_:
		call _LABEL_3C97_
		ld hl, _RAM_C9D8_
		res 3, (hl)
		jp _LABEL_EEB_
_LABEL_4E9F_:		
		ld hl, (_RAM_CEB8_)
		ld (_RAM_D10F_), hl
		ld hl, (_RAM_CEBA_)
		ld (_RAM_D111_), hl
		push iy
		call _LABEL_50C8_
		pop iy
		call _LABEL_275E_
		ret
	
_LABEL_4EB6_:	
		push ix
		push iy
		ld ix, (_RAM_D15F_)
		ld iy, (_RAM_D161_)
		ld a, (_RAM_FFFF_)
		push af
		ld a, ($0016)
		ld e, a
		ld a, $0B
		or e
		call set_page2
		call _LABEL_4F17_
		call _LABEL_4FBA_
		pop bc
		ld a, ($0016)
		ld e, a
		ld a, b
		or e
		call set_page2
		pop iy
		pop ix
		ret
	
_LABEL_4EE5_:	
		push ix
		push iy
		ld a, (_RAM_FFFF_)
		push af
		ld a, ($0016)
		ld e, a
		ld a, $0B
		or e
		call set_page2
		ld ix, (_RAM_D15F_)
		ld bc, $0008
		ld iy, (_RAM_D161_)
		call _LABEL_4F22_
		call _LABEL_4FC6_
		pop bc
		ld a, ($0016)
		ld e, a
		ld a, b
		or e
		call set_page2
		pop iy
		pop ix
		ret
	
_LABEL_4F17_:	
		ld a, (_RAM_D417_)
		and $03
		jp z, +++
		dec a
		jr nz, _LABEL_4F48_

_LABEL_4F22_:
-:	
		ld hl, (_RAM_D3F7_)
		ld bc, $1200
		add hl, bc
		ld e, (ix+1)
		ld d, (ix+2)
		or a
		sbc hl, de
		jr c, ++
		call ++++
		jr nc, +
		call _LABEL_4F9A_
+:	
		ld bc, $0008
		add ix, bc
		jr -
	
++:	
		ld (_RAM_D15F_), ix
+++:	
		ret
	
_LABEL_4F48_:	
		ld hl, (_RAM_D3F7_)
		ld bc, $0200
		or a
		sbc hl, bc
		jr c, ++
		ld e, (ix+1)
		ld d, (ix+2)
		or a
		sbc hl, de
		jr nc, ++
		call ++++
		jr nc, +
		call _LABEL_4F9A_
+:	
		ld bc, _RAM_FFF8_
		add ix, bc
		jr _LABEL_4F48_
	
++:	
		ld (_RAM_D15F_), ix
		ret
	
++++:	
		ld hl, (_RAM_D3F9_)
		ld de, $0300
		or a
		sbc hl, de
		ld e, (ix+3)
		ld d, (ix+4)
		jr c, +
		or a
		sbc hl, de
		jr z, +
		jr nc, ++
+:	
		ld a, e
		or d
		jr z, ++
		ld hl, (_RAM_D3F9_)
		ld bc, $0F00
		add hl, bc
		or a
		sbc hl, de
		ccf
++:	
		ret
	
_LABEL_4F9A_:	
		ld a, (ix+0)
		ld (_RAM_D164_), a
		ld l, (ix+1)
		ld h, (ix+2)
		ld (_RAM_D165_), hl
		ld l, (ix+3)
		ld h, (ix+4)
		ld (_RAM_D167_), hl
		push iy
		call _LABEL_505E_
		pop iy
		ret
	
_LABEL_4FBA_:	
		ld a, (_RAM_D417_)
		and $0C
		jp z, +++
		cp $08
		jr z, _LABEL_4FEC_
-:	
_LABEL_4FC6_:
		ld hl, (_RAM_D3F9_)
		ld bc, $0F00
		add hl, bc
		ld e, (iy+3)
		ld d, (iy+4)
		or a
		sbc hl, de
		jr c, ++
		call ++++
		jr nc, +
		call _LABEL_503E_
+:	
		ld bc, $0005
		add iy, bc
		jr -
	
++:	
		ld (_RAM_D161_), iy
+++:	
		ret
	
_LABEL_4FEC_:	
		ld hl, (_RAM_D3F9_)
		ld bc, $0300
		or a
		sbc hl, bc
		jr c, ++
		ld e, (iy+3)
		ld d, (iy+4)
		or a
		sbc hl, de
		jr nc, ++
		call ++++
		jr nc, +
		call _LABEL_503E_
+:	
		ld bc, _RAM_FFFB_
		add iy, bc
		jr _LABEL_4FEC_
	
++:	
		ld (_RAM_D161_), iy
		ret
	
++++:	
		ld hl, (_RAM_D3F7_)
		ld de, $0200
		or a
		sbc hl, de
		ld e, (iy+1)
		ld d, (iy+2)
		jr c, +
		or a
		sbc hl, de
		jr z, +
		jr nc, ++
+:	
		ld a, d
		or e
		jr z, ++
		ld hl, (_RAM_D3F7_)
		ld bc, $1200
		add hl, bc
		or a
		sbc hl, de
		ccf
++:	
		ret
	
_LABEL_503E_:	
		ld a, (iy+0)
		ld (_RAM_D164_), a
		ld l, (iy+1)
		ld h, (iy+2)
		ld (_RAM_D165_), hl
		ld l, (iy+3)
		ld h, (iy+4)
		ld (_RAM_D167_), hl
		push iy
		call _LABEL_505E_
		pop iy
		ret
	
_LABEL_505E_:	
		push ix
		ld c, a
		ld b, $00
		ld hl, (_RAM_C34C_)
		add hl, bc
		ld a, (hl)
		bit 6, a
		jr nz, _LABEL_50C5_
		bit 7, a
		jr nz, _LABEL_50C5_
		push hl
		ld ix, (_RAM_D43F_)
		ld h, b
		ld l, c
		add hl, hl
		add hl, hl
		add hl, hl
		ld b, h
		ld c, l
		add ix, bc
		ld e, (ix+5)
		ld a, (_RAM_FFFF_)
		push af
		call _LABEL_24FE_
		pop bc
		push af
		ld a, ($0016)
		ld e, a
		ld a, b
		or e
		call set_page2
		pop af
		pop hl
		jr c, _LABEL_50C5_
		set 7, (hl)
		ld a, (ix+6)
		ld (iy+43), a
		ld a, (_RAM_D164_)
		ld (iy+37), a
		ld (iy+32), $08
		ld (iy+41), $07
		ld hl, (_RAM_D165_)
		ld (iy+2), l
		ld (iy+3), h
		ld (iy+18), h
		ld hl, (_RAM_D167_)
		ld (iy+4), l
		ld (iy+5), h
		ld (iy+19), h
_LABEL_50C5_:	
		pop ix
		ret
	
	
_LABEL_50C8_:	
		ld hl, (_RAM_C200_)
		jp (hl)
	
_LABEL_50CC_:	
		ld a, (_RAM_C1DB_)
		and a
		ret z
		ld a, (_RAM_C222_)
		and a
		ret nz
		ld a, $01
		ld (_RAM_C203_), a
		ld a, (_RAM_D442_)
		cp $D0
		jr nc, +
		xor a
		ld (_RAM_D442_), a
+:	
		ld hl, $01CA
		ld (_RAM_C9D9_), hl
		ld hl, +
		ld (_RAM_C200_), hl
		ret
	
+:	
		ld a, (_RAM_C1DB_)
		and a
		jp z, ++
		ld a, (_RAM_D442_)
		cp $D0
		jr c, +
		ld hl, $0000
		ld (_RAM_CEC6_), hl
		ld hl, $0000
		ld (_RAM_CECF_), hl
		inc hl
		ld (_RAM_CECE_), hl
+:	
		xor a
		ld (_RAM_D442_), a
		ld a, (_RAM_C203_)
		inc a
		ld (_RAM_C203_), a
		cp $23
		ret c
		ld e, $12
		call _LABEL_4053_
		ld hl, $01CD
		ld (_RAM_C9D9_), hl
		ld hl, +++
		ld (_RAM_C200_), hl
		ret
	
++:	
		xor a
		ld (_RAM_C203_), a
		ld d, $00
		ld e, $04
		call _LABEL_24FE_
		ret c
		ld hl, $01DA
		ld (_RAM_C9D9_), hl
		ld e, $10
		call _LABEL_4053_
		ld hl, _LABEL_50CC_
		ld (_RAM_C200_), hl
		ret
	
+++:	
		ld a, (_RAM_C1FF_)
		and a
		jr z, +
		ld hl, +++
		ld (_RAM_C200_), hl
		ld hl, $01D7
		ld (_RAM_C9D9_), hl
		ret
	
+:	
		ld a, (_RAM_C1DB_)
		and a
		jr z, ++
		ld a, (_RAM_C203_)
		inc a
		jr c, +
		ld (_RAM_C203_), a
		sub $C8
		ret c
+:	
		ld (_RAM_C202_), a
		ret
	
++:	
		ld hl, $01D7
		ld (_RAM_C9D9_), hl
		xor a
		ld (_RAM_C203_), a
		ld hl, _LABEL_50CC_
		ld (_RAM_C200_), hl
		ret
	
+++:	
		ld a, (_RAM_C222_)
		and a
		ret nz
		ld a, (_RAM_C1DC_)
		and a
		ret z
		ld de, $01DF
		ld (_RAM_C9D9_), de
		ld a, $01
		ld (_RAM_C203_), a
		ld hl, +
		ld (_RAM_C200_), hl
		ret
	
+:	
		ld hl, _RAM_C203_
		ld a, (_RAM_C1DD_)
		and a
		jr nz, +
		ld a, (hl)
		cp $20
		jr nc, +
		inc (hl)
		ret
	
+:	
		xor a
		ld (_RAM_C1FF_), a
		ld a, (hl)
		ld (_RAM_C204_), a
		ld (hl), $00
		ld e, $10
		call _LABEL_4053_
		ld hl, $01DA
		ld (_RAM_C9D9_), hl
		ld hl, _LABEL_50CC_
		ld (_RAM_C200_), hl
		ret
	
_LABEL_51D2_:	
		res 7, (ix+20)
		ld a, (_RAM_D112_)
		sub (ix+38)
		jr c, +
		sub (ix+39)
		ret c
		ld a, (ix+39)
		add a, (ix+38)
		jr ++
	
+:	
		ld a, (ix+38)
++:	
		ld (_RAM_D112_), a
		xor a
		ld (_RAM_D111_), a
		set 7, (ix+20)
		ret
	
_LABEL_51F9_:	
		xor a
		ld hl, (_RAM_D10F_)
		ld c, (ix+38)
		ld b, (ix+39)
		sbc hl, bc
		jr c, +
		ld bc, $0700
		sbc hl, bc
		jr c, ++
+:	
		ld a, $04
++:	
		ld (_RAM_C1FB_), a
		ret
	
_LABEL_5214_:	
		xor a
		ld (_RAM_C1FB_), a
		ld hl, (_RAM_D10F_)
		ld a, (_RAM_D114_)
		cp $80
		jr nc, +
		add hl, de
+:	
		ex de, hl
		ld hl, (_RAM_D111_)
		ld b, (iy+3)
		ld c, (iy+2)
		add hl, bc
		call _LABEL_21E5_
		and $01
		ret z
		ld a, $04
		ld (_RAM_C1FB_), a
		ret
	
_LABEL_523A_:	
		ld hl, (_RAM_D10F_)
		add hl, de
		ld (ix+38), l
		ld (ix+39), h
		ret
	
_LABEL_5245_:	
		ld hl, (_RAM_D111_)
		ld (ix+38), l
		ld (ix+39), h
		ret
	
_LABEL_524F_:	
		ld hl, (_RAM_D111_)
		ld (_RAM_D119_), hl
		ret
	
_LABEL_5256_:	
		ld a, (_RAM_D112_)
		ld d, a
		ld a, (_RAM_D11A_)
		sub d
		ld e, a
		add a, a
		sbc a, a
		ld d, a
		ld hl, (_RAM_D115_)
		add hl, de
		ld (_RAM_D115_), hl
		ret
	
_LABEL_526A_:	
		ld a, (_RAM_D110_)
		ld d, a
		ld a, (ix+39)
		sub d
		ld e, a
		add a, a
		sbc a, a
		ld d, a
		ld hl, (_RAM_D113_)
		add hl, de
		add hl, de
		ld (_RAM_D113_), hl
		ret
	
_LABEL_527F_:	
		ld hl, (_RAM_D10F_)
		ld bc, $0020
		add hl, bc
		ex de, hl
		ld l, (ix+38)
		ld h, (ix+39)
		sbc hl, de
		jr nc, ++
		add hl, bc
		jr c, +
		add hl, bc
		jr c, +
		ld bc, $FFF8
		jr ++
	
+:	
		ld bc, $0000
		ld l, (ix+38)
		ld h, (ix+39)
		ld (_RAM_D10F_), hl
++:	
		ld l, c
		ld h, b
		ld (_RAM_D113_), hl
		ret
	
_LABEL_52AE_:	
		res 7, (ix+20)
		ld a, (_RAM_C2D0_)
		or a
		ret z
		ld a, $01
		push iy
		ld iy, (_RAM_D119_)
		ld (iy+12), a
		pop iy
		set 7, (ix+20)
		ret
	
_LABEL_52C9_:	
		push iy
		ld iy, (_RAM_D119_)
		ld a, $02
		ld (iy+12), a
		pop iy
		ret
	
_DATA_52D7_:	
	.dw $0140 $0020 $0140 $0030 $0150 $0040 $0140 $0000
	.dw $0160 $0010 $0140 $0010 $0130 _LABEL_50_ $0140 _LABEL_50_
	.dw $0140 $0030 $0140 $0000 $0160
	
	
; Data from 5301 to 5362 (98 bytes)	
	.db $C0 $FF $60 $01 $10 $00 $A0 $00 $10 $00 $50 $01 $10 $00 $30 $01
	.db $10 $00 $10 $01 $10 $00 $40 $01 $10 $00 $50 $01 $10 $00 $80 $01
	.db $10 $00 $E0 $00 $10 $00 $60 $01 $10 $00 $30 $01 $10 $00 $50 $01
	.db $50 $00 $50 $01 $50 $00 $40 $01 $50 $00 $30 $01 $60 $00 $30 $01
	.db $90 $00 $30 $01 $50 $00 $30 $01 $60 $00 $40 $01 $60 $00 $50 $01
	.db $50 $00 $50 $01 $50 $00 $30 $01 $50 $00 $30 $01 $50 $00 $C0 $00
	.db $50 $00
	
	
; Data from 5363 to 5372 (16 bytes)	
_DATA_5363_:	
	.db $80 $00 $80 $00 $80 $00 $80 $00 $80 $00 $E0 $00 $80 $00 $80 $00
	
_LABEL_5373_:	
		xor a
		ld (_RAM_C1FB_), a
		ld a, (_RAM_D438_)
		cp $07
		jr nz, +
		res 2, (ix+20)
		ret
	
+:	
		set 2, (ix+20)
		ld a, (_RAM_C204_)
		and a
		jr nz, _LABEL_53D5_
		ld a, (_RAM_C1FF_)
		and a
		jp z, _LABEL_540D_
		ld a, (_RAM_C8B5_)
		sub $00
		add a, a
		add a, a
		ld c, a
		ld b, $00
		ld hl, _DATA_52D7_
		add hl, bc
		ld e, (hl)
		inc hl
		ld d, (hl)
		inc hl
		ld c, (hl)
		inc hl
		ld b, (hl)
		ld hl, (_RAM_CEB8_)
		ld a, (_RAM_CECA_)
		bit 0, a
		ld a, (_RAM_C8B2_)
		jr z, +
		sbc hl, de
		ld de, $0080
		add hl, de
		jr ++
	
+:	
		add hl, de
++:	
		ld e, a
		add a, a
		sbc a, a
		ld d, a
		ex de, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, hl
		add hl, de
		ld (_RAM_D10F_), hl
		ld hl, (_RAM_D0E2_)
		add hl, bc
		ld (_RAM_D111_), hl
		jr _LABEL_5416_
	
_LABEL_53D5_:	
		ld hl, (_RAM_D0E0_)
		ld a, (_RAM_CECA_)
		bit 0, a
		jr nz, +
		ld de, $0200
		jr ++
	
+:	
		ld de, $FF80
++:	
		add hl, de
		ld (_RAM_D10F_), hl
		ld a, (_RAM_C204_)
		sla a
		ld l, a
		ld h, $00
		ex de, hl
		ld hl, (_RAM_CEBC_)
		ld a, (_RAM_CECA_)
		bit 0, a
		jr z, +
		and a
		sbc hl, de
		jr ++
	
+:	
		add hl, de
++:	
		ld (_RAM_D113_), hl
		ld hl, $FFC0
		ld (_RAM_D115_), hl
_LABEL_540D_:	
		xor a
		ld (_RAM_C1FF_), a
		ld a, $04
		ld (_RAM_C1FB_), a
_LABEL_5416_:	
		ret
	
_LABEL_5417_:	
		ld a, (_RAM_C1D1_)
		and a
		ret nz
		push iy
		ld a, (_RAM_D12E_)
		or a
		jr z, +
		ld a, (_RAM_C1DA_)
		cp $64
		jr c, +
		ld a, (_RAM_C1DE_)
		cp $64
		jr c, +
		ld a, $02
		ld (_RAM_C9D8_), a
+:	
		ld hl, $0280
		ld (_RAM_D142_), hl
		ld hl, $0180
		ld (_RAM_D144_), hl
		ld a, (_RAM_D438_)
		add a, a
		ld e, a
		ld d, $00
		ld hl, _DATA_55E3_
		add hl, de
		ld e, (hl)
		inc hl
		ld d, (hl)
		ex de, hl
		ld a, (_RAM_C1D2_)
		and a
		jr nz, +
		call _LABEL_553A_
		xor a
		ld (_RAM_D156_), a
+:	
		ld a, (_RAM_C209_)
		and a
		jr z, +++
		cp $07
		jr z, +
		inc a
		jr ++
	
+:	
		xor a
++:	
		ld (_RAM_C209_), a
+++:	
		ld a, (_RAM_D438_)
		cp $02
		jr z, ++
		ld a, (_RAM_C225_)
		and a
		jr z, +
		ld a, $02
		call _LABEL_55F3_
+:	
		ld a, (_RAM_C9D8_)
		bit 2, a
		jr z, ++
		ld e, $1C
		call _LABEL_4053_
		ld a, $01
		call _LABEL_55F3_
++:	
		ld a, (_RAM_D438_)
		ld e, a
		ld a, (_RAM_D455_)
		cp e
		jr z, _LABEL_5506_
		add a, a
		add a, a
		add a, a
		add a, a
		ld c, a
		ld b, $00
		ld hl, _DATA_553B_
		add hl, bc
		ld c, e
		add hl, bc
		add hl, bc
		ld c, (hl)
		inc hl
		ld b, (hl)
		ld hl, $0126
		and a
		sbc hl, bc
		jr nz, ++
		ld de, (_RAM_D113_)
		ld a, e
		rl d
		jr nc, +
		neg
+:	
		cp $41
		jr nc, +
		cp $38
		jr c, ++
		ld a, (_RAM_D12A_)
		and $01
		jr nz, ++
+:	
		ld bc, $0221
++:	
		ld a, b
		or c
		jr z, +
		ld a, b
		ld (_RAM_D11E_), a
		ld a, c
		ld (_RAM_D11D_), a
		ld (ix+25), $00
		ld (ix+24), $01
+:	
		ld a, (_RAM_D438_)
		ld (_RAM_D455_), a
		add a, $BB
		ld l, a
		ld a, $00
		adc a, $55
		ld h, a
		ld a, (hl)
		ld (ix+31), a
		xor a
		ld (_RAM_D442_), a
		ld (_RAM_C205_), a
		ld (_RAM_C206_), a
		ld (_RAM_D443_), a
_LABEL_5506_:	
		ld a, (_RAM_C223_)
		and a
		jr nz, +
		ld a, (_RAM_C1D6_)
		or a
		jr z, +
		inc a
		xor $02
		rrca
		ld b, a
		ld a, (ix+20)
		and $FE
		or b
		ld (ix+20), a
+:	
		call _LABEL_2CCE_
		call _LABEL_270D_
		ld a, (_RAM_D12A_)
		and $03
		jr nz, +
		ld a, (_RAM_C8C8_)
		or a
		jr z, +
		dec a
		ld (_RAM_C8C8_), a
+:	
		pop iy
		ret
	
_LABEL_553A_:	
		jp (hl)
	
; Data from 553B to 55E2 (168 bytes)	
_DATA_553B_:	
	.db $00 $00 $BF $00 $4C $01 $E0 $00 $26 $01 $39 $01 $39 $01 $9F $00
	.db $B6 $00 $00 $00 $4C $01 $E0 $00 $26 $01 $39 $01 $39 $01 $9F $00
	.db $45 $01 $00 $00 $FB $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
	.db $1B $01 $BF $00 $4C $01 $00 $00 $26 $01 $00 $00 $00 $00 $9F $00
	.db $32 $01 $BF $00 $4C $01 $E0 $00 $EC $00 $39 $01 $39 $01 $9F $00
	.db $45 $01 $BF $00 $4C $01 $E0 $00 $00 $00 $00 $00 $00 $00 $9F $00
	.db $45 $01 $BF $00 $4C $01 $E0 $00 $00 $00 $00 $00 $00 $00 $9F $00
	.db $B3 $00 $B3 $00 $4C $01 $B3 $00 $B3 $00 $B3 $00 $B3 $00 $B3 $00
	.db $48 $48 $48 $48 $48 $4C $4C $50 $00 $00 $00 $00 $38 $00 $00 $00
	.db $38 $00 $70 $00 $38 $00 $70 $00 $38 $00 $00 $00 $38 $00 $70 $00
	.db $38 $00 $00 $00 $38 $00 $38 $00
	
; Jump Table from 55E3 to 55F2 (8 entries, indexed by  $D438)	
_DATA_55E3_:	
	.dw _LABEL_57FC_ _LABEL_58DA_ _LABEL_5638_ _LABEL_5938_ _LABEL_5883_ _LABEL_5A8F_ _LABEL_5ACC_ _LABEL_575E_
	
	
_LABEL_55F3_:	
		ld (_RAM_C226_), a
		cp $01
		jr nz, +
		ld a, $50
		jr ++
	
+:	
		cp $02
		jr nz, +
		ld a, (_RAM_C225_)
		ld l, a
		rl a
		sbc a, a
		ld h, a
		ld (_RAM_D113_), hl
		ld hl, $FFA0
		ld (_RAM_D115_), hl
		ld a, $25
		ld (_RAM_C8C8_), a
		xor a
		ld (_RAM_C225_), a
		jr ++
	
+:	
		ld a, $02
		ld (_RAM_D455_), a
		ld a, $14
++:	
		ld (_RAM_C20C_), a
		ld a, $02
		ld (_RAM_D438_), a
		ld (_RAM_C222_), a
		ld (_RAM_C223_), a
		xor a
		ld (_RAM_C1FF_), a
		ret
	
_LABEL_5638_:	
		ld a, (_RAM_C226_)
		cp $01
		jr nz, ++
		ld a, (_RAM_C20C_)
		dec a
		ld (_RAM_C20C_), a
		jr nz, +
		ld hl, _RAM_C9D8_
		set 0, (hl)
		res 2, (hl)
		ret
	
+:	
		cp $46
		ret nz
		ld hl, $0151
		ld (_RAM_D11D_), hl
		xor a
		ld (_RAM_CECF_), a
		inc a
		ld (_RAM_CECE_), a
		ret
	
++:	
		cp $02
		jp nz, _LABEL_56F6_
		ld bc, $0001
		ld hl, (_RAM_D113_)
		call _LABEL_5B5A_
		ld (_RAM_D113_), hl
		ld a, (_RAM_C1FE_)
		ld c, a
		ld hl, (_RAM_D115_)
		ld a, h
		or a
		jp m, _LABEL_56D5_
		ld b, $00
		add hl, bc
		ld de, $0070
		call _LABEL_5B91_
		ld (_RAM_D115_), hl
		call _LABEL_1DFC_
		ld a, (_RAM_C1FB_)
		cp $04
		jr nz, +
		call _LABEL_4785_
+:	
		call _LABEL_1FCA_
		ret z
		ld e, $1A
		call _LABEL_4053_
		ld hl, $01E6
		ld (_RAM_C9D9_), hl
		ld a, (_RAM_CED7_)
		sub $10
		cp $80
		jr c, +
		ld e, $03
		call _LABEL_4BE1_
		ld a, $01
		jp _LABEL_55F3_
	
+:	
		xor a
		ld (_RAM_C222_), a
		ld (_RAM_C223_), a
		ld a, $03
		ld (_RAM_C226_), a
		ld a, $3C
		ld (_RAM_C20C_), a
		ld hl, $0000
		ld (_RAM_D113_), hl
		ld (_RAM_D115_), hl
		ret
	
_LABEL_56D5_:	
		ld b, $00
		add hl, bc
		ld (_RAM_D115_), hl
		call _LABEL_1DFC_
		ld a, (_RAM_C1FB_)
		cp $04
		jr nz, +
		ld hl, $0000
		ld (_RAM_D113_), hl
		ret
	
+:	
		cp $08
		ret nz
		ld hl, $0000
		ld (_RAM_D115_), hl
		ret
	
_LABEL_56F6_:	
		cp $03
		jr nz, ++
		ld a, (_RAM_C1D5_)
		ld c, a
		ld a, (_RAM_C1D6_)
		or c
		ld c, a
		ld a, (_RAM_C1D7_)
		or c
		ld c, a
		ld a, (_RAM_C1DB_)
		or c
		jr z, +
		ld a, $05
		ld (_RAM_D438_), a
		ret
	
+:	
		call _LABEL_5B0E_
		ret nz
		ld hl, _RAM_C20C_
		dec (hl)
		ret nz
		call _LABEL_4BDE_
		ld a, $00
		ld (_RAM_D438_), a
		ret
	
++:	
		ld a, (_RAM_C20C_)
		dec a
		ld (_RAM_C20C_), a
		jr z, _LABEL_5741_
		cp $13
		ret nz
		ld hl, $00FB
		ld (_RAM_D11D_), hl
		xor a
		ld (_RAM_CECF_), a
		inc a
		ld (_RAM_CECE_), a
		ret
	
_LABEL_5741_:	
		ld a, $03
		ld (_RAM_D438_), a
		xor a
		ld (_RAM_C222_), a
		ld (_RAM_C223_), a
		ret
_LABEL_574E_:	
		ld a, $04
		ld (_RAM_D438_), a
		ld hl, $0000
		ld (_RAM_D115_), hl
		xor a
		ld (_RAM_C222_), a
		ret
	
_LABEL_575E_:	
		ld a, (_RAM_D444_)
		cp $03
		jr z, ++
		ld a, (_RAM_D443_)
		cp $FF
		jr nz, +
		ld a, $06
+:	
		inc a
		ld (_RAM_D443_), a
		ld e, $07
		call _LABEL_4BE1_
		call _LABEL_5741_
		ret
	
++:	
		ld hl, (_RAM_D113_)
		ld a, (_RAM_C1D6_)
		and a
		jr z, +++
		ld de, $0070
		cp $01
		jr nz, +
		ld bc, $0008
		jr ++
	
+:	
		ld bc, $FFF8
++:	
		add hl, bc
		call _LABEL_5B91_
		jr ++++
	
+++:	
		ld bc, $0004
		call _LABEL_5B5A_
++++:	
		ld (_RAM_D113_), hl
		ld (_RAM_D41A_), hl
		ld de, $FF03
		call _LABEL_484D_
		ld a, (_RAM_C1D5_)
		cp $01
		ld hl, $0000
		jr nz, +
		ld hl, $0030
		jr ++
	
+:	
		cp $FF
		jr nz, ++
		ld hl, $FFD0
++:	
		ld bc, (_RAM_D115_)
		sbc hl, bc
		sra h
		rr l
		sra h
		rr l
		sra h
		rr l
		add hl, bc
		ld (_RAM_D115_), hl
		ld (_RAM_D41C_), hl
		ld de, $0280
		ld (_RAM_D142_), de
		ld bc, $0180
		ld (_RAM_D144_), bc
		ld hl, $0000
		call _LABEL_1E60_
		ld a, (_RAM_D116_)
		cp $80
		ret nc
		call _LABEL_1FCA_
		ret z
		call _LABEL_574E_
		ret
	
_LABEL_57FC_:	
		ld a, (_RAM_C223_)
		and a
		ret nz
		ld a, (_RAM_D156_)
		bit 0, a
		jr z, +
		call _LABEL_1DFC_
+:	
		ld a, (_RAM_C203_)
		and a
		jr nz, +
		ld hl, _RAM_D442_
		inc (hl)
		ld a, (hl)
		cp $D0
		jr c, +
		ld (hl), $00
		call +++
+:	
		call _LABEL_1FCA_
		jr nz, +
		call _LABEL_5741_
		ret
	
+:	
		call _LABEL_5B0E_
		ret nz
		ld a, (_RAM_C1D6_)
		and a
		jr nz, ++
		ld a, (_RAM_D426_)
		inc a
		ld c, a
		ld a, (_RAM_D133_)
		or a
		jp p, +
		neg
+:	
		cp c
		jr c, +
		call _LABEL_574E_
		ret
	
+:	
		ld a, (_RAM_C1D5_)
		cp $01
		ret nz
		ld a, $05
		ld (_RAM_D438_), a
		ret
	
++:	
		ld a, $01
		ld (_RAM_D438_), a
		ret
	
+++:	
		call _LABEL_2247_
		and $03
		jr nz, +
		ld hl, $0043
		jr ++
	
+:	
		dec a
		jr nz, +
		ld hl, $0018
		jr ++
	
; Data from 586D to 586D (1 bytes)	
	.db $C9
	
+:	
		ld hl, $0195
		ld (_RAM_C9D9_), hl
		ld hl, $0092
++:	
		ld (_RAM_D11D_), hl
		ld (ix+25), $00
		ld (ix+24), $01
		ret
	
_LABEL_5883_:	
		ld hl, (_RAM_D113_)
		ld a, l
		or h
		jr nz, +
		ld a, $00
		ld (_RAM_D438_), a
		jp _LABEL_57FC_
	
+:	
		ld a, (_RAM_D426_)
		ld c, a
		ld b, $00
		ld de, (_RAM_D133_)
		add hl, de
		call _LABEL_5B5A_
		ld (_RAM_D113_), hl
		call _LABEL_5B91_
		ld a, (_RAM_C300_)
		and $03
		jr nz, +
		ld e, $0F
		call _LABEL_4053_
+:	
		call _LABEL_1DFC_
		call _LABEL_1FCA_
		jr nz, +
		call _LABEL_5741_
		ret
	
+:	
		call _LABEL_5B0E_
		ret nz
		ld a, (_RAM_C1D6_)
		and a
		jr z, +
		ld a, $01
		ld (_RAM_D438_), a
		ret
	
+:	
		ld a, (_RAM_C1D5_)
		cp $01
		ret nz
		ld a, $06
		ld (_RAM_D438_), a
		ret
	
_LABEL_58DA_:	
		ld bc, $0006
		ld a, (_RAM_C1D6_)
		and a
		jr z, ++
		jp p, +
		ld bc, $FFFA
+:	
		ld de, (_RAM_D133_)
		ld hl, (_RAM_D113_)
		add hl, de
		add hl, bc
		ld de, $0038
		ld a, (_RAM_D444_)
		cp $02
		jr nz, +
		ld de, $0060
+:	
		call _LABEL_5B6C_
		ld (_RAM_D113_), hl
++:	
		call _LABEL_1DFC_
		ld a, (_RAM_C1FB_)
		cp $04
		jr nz, +
		ld hl, $01AF
		ld (_RAM_C9D9_), hl
+:	
		call _LABEL_1FCA_
		jr z, ++
		call _LABEL_5B0E_
		ret nz
		ld a, (_RAM_C1D5_)
		cp $01
		jr z, +
		ld a, (_RAM_C1D6_)
		or a
		ret nz
		call _LABEL_574E_
		ret
	
+:	
		ld a, $06
		ld (_RAM_D438_), a
		ret
	
++:	
		call _LABEL_574E_
		ret
	
_LABEL_5938_:	
		ld a, (_RAM_C1D6_)
		add a, a
		jr z, +
		add a, a
		ld c, a
		add a, a
		sbc a, a
		ld b, a
		ld hl, (_RAM_D113_)
		add hl, bc
		ld de, $0020
		call _LABEL_5B91_
		ld (_RAM_D113_), hl
		jr ++
	
+:	
		ld bc, $0001
		ld hl, (_RAM_D113_)
		call _LABEL_5B5A_
		ld (_RAM_D113_), hl
++:	
		ld a, (_RAM_C1FE_)
		ld c, a
		ld hl, (_RAM_D115_)
		ld a, (_RAM_D444_)
		cp $09
		jr nz, +
		ld a, (_RAM_C1D8_)
		and a
		jr z, +
		ld a, (_RAM_C20A_)
		and a
		jr nz, +
		inc a
		ld (_RAM_C20A_), a
		ld hl, $FFB0
		jr _LABEL_59D6_
	
+:	
		ld a, h
		or a
		jp m, _LABEL_5A29_
		xor a
		ld (_RAM_C206_), a
		ld a, (_RAM_D156_)
		bit 1, a
		jr nz, ++
		ld a, (_RAM_C205_)
		and a
		jr nz, +
		push hl
		ld hl, $00EC
		ld (_RAM_D11D_), hl
		ld hl, $0000
		ld (_RAM_CECF_), hl
		inc hl
		ld (_RAM_CECE_), hl
		pop hl
+:	
		inc a
		ld (_RAM_C205_), a
++:	
		ld a, (_RAM_C1D5_)
		cp $FF
		jr z, +
		cp $01
		jr nz, +
		ld a, c
		add a, a
		ld c, a
+:	
		ld a, (_RAM_C209_)
		and a
		jr z, +
		ld b, h
		ld c, l
		srl h
		rr l
		srl h
		rr l
		srl h
		rr l
		ld a, l
		cp $06
		jr c, _LABEL_5A16_
		sbc hl, bc
_LABEL_59D6_:	
		ld e, $93
		call _LABEL_4053_
		jr ++
	
+:	
		ld b, $00
		add hl, bc
		ld de, $0070
		call _LABEL_5B91_
++:	
		ld (_RAM_D115_), hl
		call _LABEL_1DFC_
		ld a, (_RAM_C1FB_)
		cp $04
		jr nz, +
		call _LABEL_4785_
		ld hl, $01AF
		ld (_RAM_C9D9_), hl
+:	
		ld a, (_RAM_C209_)
		and a
		jr z, +
		ld hl, (_RAM_D150_)
		ld (_RAM_D111_), hl
		ret
	
+:	
		call _LABEL_1FCA_
		ret z
		xor a
		ld (_RAM_C20A_), a
		ld e, $1A
		call _LABEL_4053_
_LABEL_5A16_:	
		ld a, $00
		ld (_RAM_D438_), a
		xor a
		ld (_RAM_C205_), a
		ld hl, $0000
		ld (_RAM_D113_), hl
		ld (_RAM_D115_), hl
		ret
	
_LABEL_5A29_:	
		xor a
		ld (_RAM_C205_), a
		ld a, (_RAM_C206_)
		and a
		jr nz, +
		push hl
		ld hl, $00E0
		ld (_RAM_D11D_), hl
		ld hl, $0000
		ld (_RAM_CECF_), hl
		inc hl
		ld (_RAM_CECE_), hl
		pop hl
+:	
		inc a
		ld (_RAM_C206_), a
		ld a, (_RAM_C1D5_)
		cp $FF
		jr z, +
		ld a, (_RAM_C1D7_)
		and a
		jr z, ++
+:	
		dec c
		dec c
		dec c
		ld a, (_RAM_C209_)
		and a
		jr z, ++
		dec c
		dec c
		dec c
++:	
		ld b, $00
		add hl, bc
		ld (_RAM_D115_), hl
		call _LABEL_1DFC_
		ld a, (_RAM_C1FB_)
		cp $04
		jr nz, +
		ld hl, $0000
		ld (_RAM_D113_), hl
		ld hl, $01AF
		ld (_RAM_C9D9_), hl
		ret
	
+:	
		cp $08
		ret nz
		ld hl, $0000
		ld (_RAM_D115_), hl
		ld hl, $01AF
		ld (_RAM_C9D9_), hl
		ret
	
_LABEL_5A8F_:	
		ld a, (_RAM_D156_)
		bit 0, a
		jr z, +
		call _LABEL_1DFC_
+:	
		call _LABEL_1FCA_
		jr nz, +
		call _LABEL_5741_
		ret
	
+:	
		call _LABEL_5B0E_
		ret nz
		ld a, (_RAM_C1D5_)
		cp $01
		jr z, +
		ld a, $00
		ld (_RAM_D438_), a
		ret
	
+:	
		ld a, (_RAM_D426_)
		inc a
		ld c, a
		ld a, (_RAM_D133_)
		or a
		jp p, +
		neg
+:	
		cp c
		ret c
		ld a, $06
		ld (_RAM_D438_), a
		ld (_RAM_D137_), a
		ret
	
_LABEL_5ACC_:	
		ld hl, (_RAM_D113_)
		ld a, l
		or h
		jr nz, +
		ld a, (_RAM_D137_)
		or a
		jr nz, +
		ld a, $05
		ld (_RAM_D438_), a
		ret
	
+:	
		ld a, l
		ld (_RAM_D137_), a
		ld a, (_RAM_D426_)
		ld c, a
		ld b, $00
		ld de, (_RAM_D133_)
		add hl, de
		call _LABEL_5B5A_
		ld (_RAM_D113_), hl
		call _LABEL_1DFC_
		call _LABEL_1FCA_
		jr nz, +
		call _LABEL_5741_
		ret
	
+:	
		call _LABEL_5B0E_
		ret nz
		ld a, (_RAM_C1D5_)
		cp $01
		ret z
		call _LABEL_574E_
		ret
	
_LABEL_5B0E_:	
		ld a, (_RAM_C1D8_)
		or a
		ret z
		ld hl, $FFB0
		ld a, (_RAM_D156_)
		bit 0, a
		jr z, +
		ld bc, (_RAM_D159_)
		add hl, bc
		ld bc, (_RAM_D157_)
		ld (_RAM_D113_), bc
+:	
		ld (_RAM_D115_), hl
		ld e, $11
		call _LABEL_4053_
		ld a, (_RAM_D444_)
		cp $03
		ld a, $03
		jr nz, +
		ld e, $85
		call _LABEL_4BE1_
		ld hl, (_RAM_D111_)
		ld bc, $FF80
		add hl, bc
		ld (_RAM_D111_), hl
		ld hl, $01FA
		ld (_RAM_C9D9_), hl
		ld a, $07
		ld (_RAM_C222_), a
+:	
		ld (_RAM_D438_), a
		and a
		ret
_LABEL_5B5A_:
	ld a, h
	or a
	jp p, +
	add hl, bc
	ret nc
	ld hl, $0000
	ret

+:
	sbc hl, bc
	ret nc
	ld hl, $0000
	ret

_LABEL_5B6C_:
	ld a, h
	or a
	jp m, +
	or l
	ret z
	ex de, hl
	ld bc, (_RAM_D135_)
	add hl, bc
	ex de, hl
	or a
	sbc hl, de
	add hl, de
	ret c
	ex de, hl
	ret

+:
	ex de, hl
	ld b, h
	ld c, l
	ld hl, (_RAM_D135_)
	sbc hl, bc
	ex de, hl
	and a
	sbc hl, de
	add hl, de
	ret nc
	ex de, hl
	ret

_LABEL_5B91_:
	ld a, l
	or h
	ret z
	ld a, h
	or a
	jp m, +
	sbc hl, de
	add hl, de
	ret c
	ex de, hl
	ret

+:
	add hl, de
	or a
	sbc hl, de
	ret c
	and a
	ld hl, $0000
	sbc hl, de
	ret

_LABEL_5BAB_:
	ld e, $01
	call _LABEL_24FE_
	ld (iy+22), $FF
	ld e, $02
	call _LABEL_24FE_
	ld (iy+22), $FF
	xor a
	ld (_RAM_C20A_), a
	ld (_RAM_C204_), a
	ld (_RAM_C1FF_), a
	ld (_RAM_C203_), a
	ld (_RAM_C202_), a
	ld (_RAM_C8C7_), a
	ld (_RAM_C8C8_), a
	ld (_RAM_C1D1_), a
	ld (_RAM_C1D2_), a
	ld (_RAM_C225_), a
	ld (_RAM_D445_), a
	dec a
	ld (_RAM_C8B7_), a
	ld a, $00
	call _LABEL_55F3_
	add a, $BB
	ld l, a
	ld a, $00
	adc a, $55
	ld h, a
	ld a, (hl)
	ld (_RAM_CED5_), a
	ld hl, $50CC
	ld (_RAM_C200_), hl
	call _LABEL_25A4_
	ret

.org $5BFE
; Data from_DATA_5BFE to 5DC2 (453 bytes)_
_DATA_5BFE_:
.db $03 $01 $03 $04 $81 $07 $00 $02 $00 $03 $40 $80 $0E $13 $01 $14
.db $81 $13 $00 $0F $1F $01 $40 $80 $1D $06 $03 $08 $1E $06 $03 $08
.db $1D $19 $03 $08 $1E $06 $03 $08 $1D $06 $03 $08 $1E $06 $03 $08
.db $1D $06 $03 $08 $1E $06 $03 $08 $1D $19 $03 $08 $1E $06 $03 $08
.db $81 $07 $00 $02 $01 $03 $08 $1B $22 $03 $08 $1C $22 $03 $08 $1B
.db $22 $03 $08 $1C $22 $03 $08 $1B $22 $03 $08 $1C $22 $03 $08 $1B
.db $22 $03 $08 $1C $22 $03 $08 $1B $22 $03 $08 $1C $22 $03 $08 $1B
.db $22 $03 $08 $1C $22 $03 $1C $1B $22 $03 $08 $1C $22 $03 $08 $1B
.db $22 $03 $08 $1C $22 $03 $08 $1B $22 $03 $08 $02 $01 $03 $08 $81
.db $07 $00 $0C $16 $03 $10 $0D $16 $03 $08 $0D $19 $03 $08 $80 $07
.db $08 $01 $06 $00 $23 $01 $06 $81 $AA $00 $00 $23 $01 $0C $01 $24
.db $01 $0C $80 $81 $1B $01 $10 $04 $03 $04 $11 $05 $03 $04 $80 $02
.db $01 $03 $04 $03 $01 $03 $04 $04 $02 $03 $04 $03 $02 $03 $04 $02
.db $01 $03 $04 $06 $01 $03 $04 $05 $00 $03 $04 $06 $00 $03 $04 $80
.db $07 $08 $03 $04 $81 $E7 $00 $08 $09 $03 $40 $80 $07 $0A $03 $04
.db $09 $0A $03 $04 $0A $0A $03 $04 $81 $16 $01 $1F $00 $02 $01 $18
.db $00 $02 $04 $17 $00 $02 $04 $16 $0A $02 $04 $15 $0A $03 $04 $14
.db $0A $03 $04 $81 $16 $01 $0B $0A $03 $40 $80 $13 $0B $03 $08 $0A
.db $0B $03 $08 $81 $07 $00 $10 $03 $03 $04 $81 $2D $01 $11 $04 $03
.db $40 $80 $11 $01 $03 $08 $81 $07 $00 $12 $00 $03 $08 $81 $40 $01
.db $13 $1A $03 $40 $80 $12 $01 $03 $08 $81 $07 $00 $13 $15 $03 $80
.db $80 $13 $13 $03 $14 $14 $13 $03 $08 $15 $13 $03 $08 $16 $00 $02
.db $08 $17 $00 $02 $08 $18 $00 $02 $08 $81 $87 $01 $12 $00 $01 $04
.db $14 $00 $01 $04 $15 $00 $01 $04 $16 $00 $00 $04 $17 $00 $00 $04
.db $18 $00 $00 $04 $81 $87 $01 $1F $00 $00 $7F $80 $09 $0C $01 $04
.db $0A $0C $01 $04 $80 $07 $20 $19 $08 $07 $20 $06 $10 $07 $14 $82
.db $1E $28 $19 $07 $80 $14 $0C $81 $C1 $01 $15 $0C $81 $C1 $01 $15
.db $0C $82 $17 $0C $16 $0C $17 $0C $16 $0C $17 $0C $16 $0C $81 $C1
.db $01 $17 $0C $16 $0C

; Data from 5DC3 to 5E00 (62 bytes)
.db $17 $0C $16 $0C $82 $10 $06 $80 $0D $06 $0E $06 $81 $D4 $01 $0F
.db $06 $80 $00 $01 $82 $11 $06 $12 $06 $82 $10 $06 $11 $06 $12 $06
.db $82 $20 $08 $21 $08 $20 $08 $21 $08 $20 $08 $21 $08 $20 $08 $21
.db $08 $82 $13 $23 $82 $00 $01 $82 $00 $01 $01 $01 $80 $00

; Data from 5E01 to 5E2B (43 bytes)
.db $02 $01 $02 $02 $02 $80 $00 $02 $01 $02 $02 $02 $01 $02 $80 $00
.db $03 $01 $03 $02 $03 $03 $03 $04 $03 $05 $03 $06 $03 $80 $20 $03
.db $03 $04 $21 $03 $03 $04 $22 $03 $03 $04 $80

_DATA_5E2C_:
; Pointer Table from 5E2C to 5E37 (6 entries, indexed by $D9AA)
.dw $5E38 $5E76 $5EB4 $5EF2 $5F30 $5F6E

; Data from 5E38 to 5E98 (97 bytes)
.db $02 $04 $08 $4A $AF $08 $80 $03 $05 $20 $80 $52 $5E $5B $5E $64
.db $5E $6D $5E $00 $00 $00 $00 $02 $01 $04 $00 $0C $00 $1B $E0 $92
.db $CB $9A $05 $00 $08 $00 $5C $92 $97 $33 $9C $04 $00 $04 $00 $1A
.db $E0 $9B $A3 $9D $02 $00 $04 $00 $0F $B2 $A3 $A3 $A0 $00 $02 $05
.db $0A $22 $B0 $E8 $9F $04 $05 $20 $80 $90 $5E $99 $5E $A2 $5E $AB
.db $5E $00 $00 $00 $00 $03 $02 $04 $00 $08 $00 $04 $E0 $8D $C3 $A0
.db $03

; Data from 5E99 to 7FEF (8535 bytes)
.incbin "Ottifants_DATA_5E99_.inc"


.BANK 1 SLOT 1
.ORG $0000

; Data from 7FF0 to 7FFF (16 bytes)
.db $54 $4D $52 $20 $53 $45 $47 $41 $FF $FF $90 $5C $20 $71 $00 $40

.BANK 2
.ORG $0000

; Data from 8000 to BFFF (16384 bytes)
.incbin "Ottifants_DATA_8000_.inc"

.BANK 3
.ORG $0000

; Data from C000 to FFFF (16384 bytes)
.incbin "Ottifants_DATA_C000_.inc"

.BANK 4
.ORG $0000

; Data from 10000 to 13FFF (16384 bytes)
.incbin "Ottifants_DATA_10000_.inc"

.BANK 5
.ORG $0000

; Data from 14000 to 17FFF (16384 bytes)
.incbin "Ottifants_DATA_14000_.inc"

.BANK 6
.ORG $0000

; Data from 18000 to 19BA3 (7076 bytes)
.incbin "Ottifants_DATA_18000_.inc"

; Data from 19BA4 to 19D2F (396 bytes)
_DATA_19BA4_:
.db $00 $00 $F4 $00 $3A $01 $31 $01 $4A $01 $B2 $15 $90 $01 $AD $01
.db $A7 $01 $8C $01 $B4 $01 $FB $13 $90 $03 $2B $16 $27 $03 $40 $03
.db $0A $03 $F3 $02 $D2 $15 $58 $02 $81 $02 $9A $0A $58 $08 $53 $04
.db $5D $03 $CB $13 $3F $02 $6C $0D $22 $14 $C8 $04 $FB $04 $E7 $03
.db $E1 $03 $23 $02 $93 $04 $54 $05 $BD $05 $35 $05 $D2 $0A $97 $05
.db $DC $01 $FE $01 $11 $02 $BB $06 $0F $06 $F0 $06 $3E $07 $BC $0D
.db $C9 $07 $75 $07 $4D $0A $01 $08 $C9 $08 $B3 $08 $D5 $07 $60 $0B
.db $E2 $10 $1C $11 $D6 $15 $EB $15 $7C $03 $68 $03 $87 $04 $8D $0B
.db $D6 $10 $77 $0D $C2 $02 $E3 $0D $02 $0E $2F $0E $26 $0F $45 $0E
.db $B7 $0E $E0 $0E $BC $0D $CC $0B $10 $0C $4A $0C $1A $0E $34 $12
.db $58 $11 $2B $04 $7D $13 $C2 $0A $DF $0C $98 $02 $2D $15 $7C $12
.db $28 $09 $AE $0F $32 $0A $66 $14 $50 $16 $A2 $18 $58 $15 $61 $15
.db $8C $15 $C9 $11 $36 $0D $50 $0D $43 $13 $F7 $12 $C7 $09 $FF $09
.db $62 $10 $2A $10 $24 $0B $34 $0B $CB $14 $18 $15 $AA $16 $CB $10
.db $88 $13 $A9 $13 $57 $17 $C5 $17 $09 $18 $D9 $18 $18 $17 $81 $16
.db $35 $04 $8B $14 $00 $0E $2A $02 $02 $00 $12 $D6 $C2 $00 $12 $D7
.db $C2 $00 $19 $D8 $C2 $00 $00 $12 $DA $C2 $00 $12 $DB $C2 $FF $12
.db $D3 $C2 $00 $12 $D4 $C2 $00 $01 $00 $00 $00 $07 $FD $0E $24 $08
.db $0E $20 $03 $06 $20 $0E $23 $64 $0E $21 $6F $6B $6D $00 $51 $FC
.db $FF $08 $01 $00 $6B $6D $00 $51 $FC $FF $03 $07 $FD $07 $FB $0E
.db $20 $03 $0E $23 $64 $6C $00 $51 $FD $FF $09 $02 $03 $0E $23 $66
.db $03 $20 $00 $09 $0E $24 $10 $07 $DB $54 $04 $00 $56 $FE $06 $24
.db $21 $54 $0F $00 $23 $FF $52 $25 $00 $69 $52 $21 $00 $47 $04 $70
.db $38 $56 $F1 $54 $0F $00 $23 $FF $52 $13 $00 $68 $80 $00 $80 $00
.db $52 $0B $00 $47 $04 $70 $38 $56 $ED $51 $02 $00


; Data from 19D30 to 1B379 (5706 bytes)
_DATA_19D30_:
.incbin "Ottifants_DATA_19D30_.inc"




; Pointer Table from 1B37A to 1B381 (4 entries, indexed by unknown)
_DATA_1B37A_:
.dw $240E $0020 $160E $0EFF


; Data from 1B382 to 1BFFF (3198 bytes)
.incbin "Ottifants_DATA_1B382_.inc"


.BANK 7
.ORG $0000

; Data from 1C000 to 1FFFF (16384 bytes)
.incbin "Ottifants_DATA_1C000_.inc"

.BANK 8
.ORG $0000

; Data from 20000 to 22D71 (11634 bytes)
.incbin "Ottifants_DATA_20000_.inc"

; Data from 22D72 to 22DBC (75 bytes)
_DATA_22D72_:
.db $0D $04 $00 $01 $00 $02 $00 $03 $00 $04 $00 $05 $00 $06 $00 $07
.db $00 $08 $00 $09 $00 $0A $00 $0B $00 $0C $00 $0D $00 $0E $00 $0F
.db $00 $10 $00 $11 $00 $12 $00 $13 $00 $00 $00 $14 $00 $15 $00 $16
.db $00 $17 $00 $18 $00 $19 $00 $00 $00 $1A $00 $1B $85 $00 $00 $00
.db $1C $85 $00 $00 $00 $1D $00 $1E $8A $00 $00


; Data from 22DBD to 2319C (992 bytes)
_DATA_22DBD_:
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $2F $1F $3F $00
.db $00 $00 $00 $00 $02 $01 $03 $00 $03 $07 $07 $00 $0F $0E $0E $01
.db $1F $1C $1C $03 $3E $38 $38 $06 $3C $78 $78 $04 $FF $FF $FF $00
.db $BC $7E $FE $00 $FF $FE $FE $01 $FF $FE $FE $01 $FF $00 $00 $FF
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $FA $FC $FC $02
.db $00 $00 $00 $00 $7D $7E $7E $01 $7E $63 $63 $1C $72 $63 $63 $10
.db $73 $62 $63 $10 $75 $64 $66 $11 $7B $78 $7C $03 $7F $68 $7C $03
.db $00 $00 $00 $00 $00 $00 $00 $00 $80 $00 $00 $80 $80 $00 $00 $80
.db $80 $00 $00 $80 $83 $07 $07 $80 $07 $0F $0F $00 $05 $04 $06 $01
.db $00 $00 $00 $00 $33 $23 $33 $00 $67 $66 $66 $01 $77 $66 $66 $11
.db $F7 $A6 $A6 $51 $FF $EF $EF $10 $FF $FF $FF $00 $FF $66 $66 $99
.db $00 $00 $00 $00 $FF $FF $FF $00 $FF $04 $04 $FB $84 $00 $00 $84
.db $00 $00 $00 $00 $D0 $E0 $E0 $10 $B0 $C0 $C0 $30 $E0 $00 $00 $E0
.db $00 $00 $00 $00 $FE $FC $FC $02 $FC $C8 $C8 $34 $E8 $C0 $C0 $28
.db $E0 $C0 $C0 $20 $EF $CF $CF $20 $FF $DF $DF $20 $EF $C0 $C0 $2F
.db $00 $00 $00 $00 $1F $1F $1F $00 $3F $3F $3F $00 $7F $60 $60 $1F
.db $70 $60 $60 $10 $FE $FE $FE $00 $FF $FE $FE $01 $FF $C6 $C6 $39
.db $00 $00 $00 $00 $1F $0E $1F $00 $BF $1F $3F $80 $FF $21 $73 $8C
.db $71 $41 $61 $10 $E1 $41 $C1 $20 $E1 $C1 $C1 $20 $E1 $C1 $C1 $20
.db $00 $00 $00 $00 $10 $00 $10 $00 $98 $10 $98 $00 $DC $98 $98 $44
.db $DC $98 $98 $44 $DC $98 $98 $44 $DC $18 $98 $44 $DC $18 $18 $C4
.db $00 $00 $00 $00 $3F $3F $3F $00 $3F $00 $00 $3F $38 $00 $10 $28
.db $38 $10 $30 $08 $38 $30 $30 $08 $38 $30 $30 $08 $39 $31 $31 $08
.db $00 $00 $00 $00 $C0 $80 $C0 $00 $E0 $C0 $E0 $00 $70 $60 $60 $10
.db $70 $60 $60 $10 $70 $40 $60 $10 $F0 $80 $C0 $30 $E0 $00 $80 $60
.db $5F $3F $7F $00 $BF $7F $FF $00 $7F $00 $00 $7F $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $FF $FF $FF $00 $FF $FF $FF $00 $6F $F0 $F0 $0F $F8 $E0 $E0 $18
.db $F0 $E0 $E0 $10 $F0 $E0 $E0 $10 $F0 $E0 $E0 $10 $F0 $E0 $E1 $10
.db $FE $F8 $FC $02 $F4 $F8 $F8 $04 $FC $30 $38 $C4 $38 $30 $30 $08
.db $38 $30 $70 $08 $78 $30 $70 $08 $38 $70 $F0 $08 $F8 $70 $F0 $08
.db $7E $6C $6C $12 $7E $66 $6E $10 $77 $63 $67 $10 $73 $61 $63 $10
.db $71 $40 $41 $30 $20 $00 $00 $20 $00 $00 $00 $00 $00 $00 $00 $00
.db $0B $08 $1C $03 $3E $10 $38 $06 $2C $70 $70 $0C $F8 $E0 $E0 $18
.db $F0 $C0 $C0 $30 $F0 $40 $E0 $10 $60 $00 $00 $60 $00 $00 $00 $00
.db $77 $66 $66 $11 $77 $66 $66 $11 $77 $66 $66 $11 $77 $66 $66 $11
.db $77 $66 $66 $11 $73 $42 $42 $31 $21 $00 $00 $21 $00 $00 $00 $00
.db $E0 $C0 $C0 $20 $E0 $C0 $C0 $20 $E0 $C0 $C0 $20 $E0 $C0 $C0 $20
.db $E0 $C0 $C0 $20 $E0 $80 $80 $60 $40 $01 $01 $40 $00 $01 $01 $00
.db $E7 $C6 $C6 $21 $EF $CE $CE $21 $FF $DE $DE $21 $FD $F6 $F6 $09
.db $7F $64 $66 $19 $35 $04 $06 $31 $8E $08 $8C $02 $FE $F0 $F8 $06
.db $E3 $C2 $C3 $20 $E7 $C4 $C6 $21 $EF $C8 $EC $03 $FE $F0 $F8 $06
.db $7C $60 $70 $0C $38 $00 $00 $38 $00 $00 $00 $00 $00 $00 $00 $00
.db $9C $18 $18 $84 $9C $18 $18 $84 $1C $18 $18 $04 $1F $18 $1F $00
.db $1F $1F $1F $00 $0F $00 $00 $0F $00 $00 $00 $00 $00 $00 $00 $00
.db $3B $32 $33 $08 $3F $34 $36 $09 $3F $38 $3C $03 $BE $B0 $B8 $06
.db $FC $20 $30 $CC $98 $00 $00 $98 $00 $00 $00 $00 $00 $00 $00 $00
.db $C0 $00 $00 $C0 $80 $00 $00 $80 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $F1 $E0 $E3 $10 $77 $FB $FF $00 $FF $7E $FE $01 $3F $7C $7C $03
.db $16 $38 $38 $06 $1C $00 $00 $1C $00 $00 $00 $00 $00 $00 $00 $00
.db $F8 $F0 $F0 $08 $F8 $70 $70 $88 $F8 $70 $70 $88 $78 $70 $70 $08
.db $68 $70 $70 $08 $70 $60 $60 $10 $70 $60 $60 $10 $50 $60 $60 $10
.db $AC $C0 $F0 $0C $70 $00 $00 $70 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $00 $00 $00 $00 $2F $1F $3F $00 $5D $3E $7E $01 $B7 $78 $F8 $07
.db $3C $00 $00 $3C $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $A0 $C0 $C0 $20 $60 $80 $80 $60 $C0 $00 $00 $C0 $00 $00 $00 $00
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00



; Data from 2319D to 2326A (206 bytes)
_DATA_2319D_:
.db $12 $07 $00 $00 $00 $01 $00 $02 $00 $03 $00 $04 $00 $05 $8D $00
.db $00 $00 $06 $00 $07 $00 $08 $00 $09 $00 $0A $00 $0B $8B $00 $00
.db $00 $0C $00 $0D $00 $0E $00 $0F $00 $10 $00 $11 $00 $12 $00 $13
.db $00 $14 $00 $15 $84 $00 $00 $00 $16 $00 $17 $82 $00 $00 $00 $18
.db $00 $19 $00 $1A $00 $1B $00 $1C $00 $1D $00 $1E $00 $1F $00 $20
.db $00 $21 $00 $22 $00 $23 $00 $24 $00 $25 $00 $26 $00 $27 $00 $28
.db $00 $29 $00 $2A $00 $2B $00 $2C $00 $2D $00 $2E $00 $2F $00 $30
.db $00 $31 $00 $32 $00 $33 $00 $34 $00 $35 $00 $36 $00 $37 $00 $38
.db $00 $39 $00 $3A $00 $3B $00 $3C $00 $3D $00 $3E $00 $3F $00 $40
.db $00 $41 $00 $42 $00 $43 $00 $44 $00 $45 $00 $46 $00 $47 $00 $48
.db $00 $49 $00 $4A $00 $4B $00 $4C $00 $4D $00 $4E $06 $22 $00 $4F
.db $00 $50 $00 $51 $00 $52 $00 $53 $00 $54 $00 $55 $00 $56 $00 $57
.db $00 $58 $00 $59 $00 $5A $00 $5B $00 $5C $00 $5D $00 $5E

; Data from 2326B to 23FFF (3477 bytes)
_DATA_2326B_:
.incbin "Ottifants_DATA_2326B_.inc"


.BANK 9
.ORG $0000

; Data from 24000 to 27FFF (16384 bytes)
.incbin "Ottifants_DATA_24000_.inc"

.BANK 10
.ORG $0000

; Data from 28000 to 2BFFF (16384 bytes)
.incbin "Ottifants_DATA_28000_.inc"

.BANK 11
.ORG $0000

; Data from 2C000 to 2FFFF (16384 bytes)
.incbin "Ottifants_DATA_2C000_.inc"

.BANK 12
.ORG $0000

; Data from 30000 to 30007 (8 bytes)
.db $0C $00 $00 $00 $00 $00 $00 $00

; Data from 30008 to 30027 (32 bytes)
_DATA_30008_:
.db $AB $CD $4B $50 $48 $B8 $0B $53 $52 $54 $D0 $07 $4A $4C $20 $E8
.db $03 $44 $45 $20 $F4 $01 $4A $41 $50 $FA $00 $4A $4B $20 $64 $00

; Data from 30028 to 326AF (9864 bytes)
_DATA_30028_:
.incbin "Ottifants_DATA_30028_.inc"


; Data from 326B0 to 326B3 (4 bytes)
_DATA_326B0_:
.db $11 $36 $11 $58

; Data from 326B4 to 326D3 (32 bytes)
_DATA_326B4_:
.db $02 $7A $03 $50 $04 $26 $04 $FC $05 $5E $05 $A0 $05 $E2 $05 $F2
.db $02 $9C $03 $72 $04 $48 $05 $1E $05 $80 $05 $C2 $05 $EE $05 $FA

; Data from 326D4 to 33FFF (6444 bytes)
_DATA_326D4_:
.incbin "Ottifants_DATA_326D4_.inc"


.BANK 13
.ORG $0000

; Data from 34000 to 37FFF (16384 bytes)
.incbin "Ottifants_DATA_34000_.inc"

.BANK 14
.ORG $0000

; Data from 38000 to 38001 (2 bytes)
.db $0E $00


; Data from 38002 to 3A8E7 (10470 bytes)
_DATA_38002_:
.incbin "Ottifants_DATA_38002_.inc"



; Data from 3A8E8 to 3ABB7 (720 bytes)
_DATA_3A8E8_:
.db $30 $30 $00 $30 $00 $78 $48 $00 $FC $00 $00 $CC $00 $CC $CC $CC
.db $FC $FC $00 $66 $00 $66 $7C $00 $66 $00 $00 $66 $00 $66 $FC $FC
.db $3E $3E $00 $66 $00 $C0 $C0 $00 $C0 $00 $00 $C0 $00 $66 $3C $3C
.db $F8 $F8 $00 $6C $00 $66 $66 $00 $66 $00 $00 $66 $00 $6C $F8 $F8
.db $FE $FE $00 $62 $00 $68 $78 $00 $68 $00 $00 $60 $04 $66 $FE $FE
.db $FE $FE $00 $62 $00 $68 $78 $00 $68 $00 $00 $60 $00 $60 $F8 $F8
.db $3E $3E $00 $62 $00 $C0 $C0 $00 $DE $00 $00 $C6 $00 $66 $3E $3E
.db $CE $CE $00 $CC $00 $CC $FC $00 $CC $00 $00 $CC $00 $CC $CE $CE
.db $FC $FC $00 $30 $00 $30 $30 $00 $30 $00 $00 $30 $00 $30 $FC $FC
.db $3E $3E $00 $0C $00 $0C $0C $00 $0C $00 $00 $CC $00 $CC $78 $78
.db $EE $EE $00 $6C $00 $78 $70 $00 $70 $00 $00 $78 $00 $6C $EE $EE
.db $F0 $F0 $00 $60 $00 $60 $60 $00 $60 $00 $00 $66 $00 $66 $FC $FE
.db $CE $CE $00 $FC $00 $FC $CC $00 $CC $00 $00 $CC $00 $CC $EE $EE
.db $E6 $E6 $00 $66 $00 $76 $7E $00 $7E $00 $00 $6E $00 $66 $F6 $F6
.db $38 $38 $00 $6C $00 $C6 $C6 $00 $C6 $00 $00 $C6 $00 $6C $38 $38
.db $FC $FC $00 $66 $00 $66 $66 $00 $7C $00 $00 $60 $00 $60 $F8 $F8
.db $38 $38 $00 $6C $00 $C6 $C6 $00 $C6 $00 $00 $C6 $00 $7C $36 $36
.db $F8 $F8 $00 $CC $00 $CC $CC $00 $F8 $00 $00 $F0 $00 $D8 $CE $CE
.db $78 $78 $00 $CC $00 $C0 $78 $00 $0C $00 $00 $0C $00 $CC $78 $78
.db $FC $FC $00 $B4 $00 $30 $30 $00 $30 $00 $00 $30 $00 $30 $78 $78
.db $DC $DC $00 $CC $00 $CC $CC $00 $CC $00 $00 $CC $00 $CC $78 $78
.db $DC $DC $00 $CC $00 $CC $48 $00 $48 $00 $48 $30 $00 $30 $30 $30
.db $CE $CE $00 $C6 $00 $C6 $C6 $00 $D6 $00 $00 $FE $00 $C6 $82 $82
.db $CC $CC $00 $CC $00 $78 $30 $00 $30 $00 $00 $78 $00 $CC $CE $CE
.db $CE $CE $00 $CC $00 $78 $30 $00 $30 $00 $00 $30 $00 $30 $78 $78
.db $FE $FE $00 $C6 $00 $0C $18 $00 $30 $00 $00 $60 $00 $C6 $FE $FE
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $28 $38 $18 $28 $38 $38
.db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
.db $3C $3C $00 $66 $00 $DB $B3 $00 $B3 $00 $00 $DB $00 $66 $3C $3C
.db $78 $78 $00 $CC $00 $CC $CC $00 $CC $00 $00 $CC $00 $CC $78 $78
.db $30 $30 $00 $70 $00 $30 $30 $00 $30 $00 $00 $30 $00 $30 $78 $78
.db $78 $78 $00 $CC $00 $0C $18 $00 $30 $00 $00 $60 $00 $CC $FC $FC
.db $78 $78 $00 $CC $00 $0C $38 $00 $0C $00 $00 $0C $00 $CC $78 $78
.db $18 $18 $00 $38 $00 $68 $C8 $00 $FC $00 $00 $18 $00 $18 $3C $3C
.db $FC $FC $00 $C0 $00 $C0 $F8 $00 $0C $00 $00 $0C $00 $CC $78 $78
.db $38 $38 $00 $60 $00 $C0 $F8 $00 $CC $00 $00 $CC $00 $CC $78 $78
.db $FC $FC $00 $CC $00 $0C $18 $00 $18 $00 $00 $30 $00 $30 $30 $30
.db $78 $78 $00 $CC $00 $CC $78 $00 $CC $00 $00 $CC $00 $CC $78 $78
.db $78 $78 $00 $CC $00 $CC $CC $00 $7C $00 $00 $0C $00 $18 $70 $70
.db $3C $3C $00 $66 $00 $40 $66 $00 $3C $00 $00 $18 $04 $0C $18 $18
.db $CC $CC $00 $CC $00 $00 $CC $00 $CC $00 $00 $CC $00 $CC $78 $78
.db $5A $5A $00 $24 $00 $00 $66 $00 $76 $00 $40 $3E $00 $6E $F6 $F6
.db $00 $00 $00 $38 $00 $6C $38 $00 $76 $00 $00 $DC $00 $CC $76 $76
.db $00 $00 $00 $00 $00 $00 $00 $00 $14 $1C $0C $14 $04 $0C $0C $0C
.db $00 $00 $00 $00 $28 $38 $18 $28 $28 $38 $18 $18 $00 $00 $00 $00

; Data from 3ABB8 to 3ABBB (4 bytes)
_DATA_3ABB8_:
.db $F6 $06 $14 $FF

; 1st entry of Pointer Table from 1DBE (indexed by _RAM_D9A8_)
; Data from 3ABBC to 3AEBE (771 bytes)
_DATA_3ABBC_:
.db $F6 $0C $0C $44 $45 $55 $54 $53 $43 $48 $FF $F6 $0C $0E $45 $4E
.db $47 $4C $49 $53 $48 $FF $F6 $0C $10 $46 $52 $41 $4E $68 $41 $49
.db $53 $FF $F6 $0C $12 $49 $54 $41 $4C $49 $41 $4E $4F $FF $F6 $0C
.db $14 $45 $53 $50 $41 $6A $4F $4C $FF $F6 $09 $0D $53 $50 $49 $45
.db $4C $20 $53 $54 $41 $52 $54 $45 $4E $FF $F6 $09 $0F $50 $41 $53
.db $53 $57 $4F $52 $54 $FF $F6 $09 $11 $4F $50 $54 $49 $4F $4E $45
.db $4E $FF $F6 $06 $0B $4C $45 $42 $45 $4E $F6 $0F $0B $FF $F6 $06
.db $0D $4D $55 $53 $49 $4B $F6 $0E $0D $FF $F6 $06 $0F $53 $46 $58
.db $F6 $0E $0F $FF $F6 $06 $11 $4A $4F $59 $50 $41 $44 $F6 $0F $11
.db $FF $F6 $06 $13 $45 $42 $45 $4E $45 $F6 $0F $13 $FF $54 $52 $41
.db $49 $4E $49 $4E $47 $20 $FF $45 $49 $4E $46 $41 $43 $48 $20 $20
.db $FF $4E $4F $52 $4D $41 $4C $20 $20 $20 $FF $53 $43 $48 $57 $49
.db $45 $52 $45 $47 $FF $03 $FF $05 $FF $07 $FF $09 $FF $31 $20 $53
.db $50 $52 $49 $4E $47 $45 $4E $FF $32 $20 $53 $50 $52 $49 $4E $47
.db $45 $4E $FF $F6 $07 $12 $57 $45 $49 $54 $45 $52 $20 $53 $54 $41
.db $52 $54 $20 $5B $5B $5B $20 $20 $FF $F6 $09 $0F $53 $50 $49 $45
.db $4C $20 $42 $45 $45 $4E $44 $45 $54 $FF $F6 $0B $07 $F4 $08 $48
.db $49 $47 $48 $20 $53 $43 $4F $52 $45 $53 $FF $F4 $00 $F6 $0A $0C
.db $FF $20 $20 $20 $20 $20 $20 $44 $41 $53 $20 $48 $41 $55 $53 $20
.db $20 $20 $20 $20 $20 $FF $20 $20 $20 $20 $20 $44 $45 $52 $20 $4B
.db $45 $4C $4C $45 $52 $20 $20 $20 $20 $20 $FF $20 $20 $20 $44 $49
.db $45 $20 $42 $41 $55 $53 $54 $45 $4C $4C $45 $20 $20 $20 $20 $FF
.db $20 $20 $20 $20 $20 $20 $44 $41 $53 $20 $42 $69 $52 $4F $20 $20
.db $20 $20 $20 $20 $20 $FF $20 $20 $20 $44 $45 $52 $20 $44 $53 $43
.db $48 $55 $4E $47 $45 $4C $20 $20 $20 $FF $F6 $06 $14 $FF $F6 $0A
.db $0C $45 $4E $54 $45 $52 $20 $44 $52 $69 $43 $4B $45 $4E $F3 $20
.db $20 $50 $41 $53 $53 $57 $4F $52 $44 $FF $F6 $08 $14 $50 $41 $53
.db $53 $57 $4F $52 $54 $20 $20 $20 $20 $FF $F6 $06 $06 $4F $48 $20
.db $4A $45 $20 $44 $55 $20 $4D $55 $53 $53 $54 $20 $41 $4C $4C $45
.db $F3 $56 $45 $52 $4C $4F $52 $45 $4E $20 $47 $45 $47 $41 $4E $47
.db $45 $4E $45 $4E $F3 $20 $50 $41 $50 $49 $45 $52 $45 $20 $56 $4F
.db $4E $20 $50 $41 $50 $41 $F3 $50 $41 $55 $4C $20 $5A $55 $53 $41
.db $4D $4D $45 $4E $53 $41 $4D $4D $45 $4C $4E $F3 $44 $41 $4D $49
.db $54 $20 $45 $52 $20 $4E $41 $43 $48 $20 $48 $41 $55 $53 $45 $F3
.db $20 $20 $20 $20 $4B $4F $4D $4D $45 $4E $20 $4B $41 $4E $4E $FF
.db $F6 $06 $05 $20 $20 $20 $20 $48 $45 $52 $5A $4C $49 $43 $48 $45
.db $4E $F3 $20 $20 $20 $20 $47 $4C $69 $43 $4B $57 $55 $4E $53 $43
.db $48 $F3 $44 $55 $20 $48 $41 $53 $54 $20 $41 $4C $4C $45 $20 $50
.db $41 $50 $49 $45 $52 $45 $F3 $20 $20 $20 $56 $4F $4E $20 $50 $41
.db $50 $41 $20 $50 $41 $55 $4C $F3 $5A $55 $53 $41 $4D $4D $45 $4E
.db $20 $55 $4E $44 $20 $45 $52 $20 $4B $41 $4E $4E $F3 $20 $45 $4E
.db $44 $4C $49 $43 $48 $20 $4E $41 $43 $48 $20 $48 $41 $55 $53 $45
.db $F3 $20 $20 $20 $20 $20 $20 $20 $20 $4B $4F $4D $4D $45 $4E $FF
.db $F6 $0B $0B $57 $41 $48 $4C $45 $20 $4C $45 $56 $45 $4C $FF $F6
.db $07 $0F $FF $F6 $07 $11 $FF $F6 $07 $13 $FF $F6 $06 $15 $53 $4F
.db $55 $4E $44 $F6 $0F $15 $FF $20 $20 $20 $20 $20 $20 $20 $20 $20
.db $20 $FF $4D $55 $53 $49 $4B $20 $20 $20 $20 $20 $FF $53 $4F $55
.db $4E $44 $20 $20 $20 $20 $20 $FF $46 $58 $20 $6B $20 $4D $55 $53
.db $49 $4B $FF

; 2nd entry of Pointer Table from 1DBE (indexed by _RAM_D9A8_)
; Data from 3AEBF to 3B150 (658 bytes)
_DATA_3AEBF_:
.db $FF $FF $FF $FF $FF $F6 $0C $0D $53 $54 $41 $52 $54 $FF $F6 $0C
.db $0F $50 $41 $53 $53 $57 $4F $52 $44 $FF $F6 $0C $11 $4F $50 $54
.db $49 $4F $4E $FF $F6 $06 $0B $4C $49 $56 $45 $53 $F6 $0F $0B $FF
.db $F6 $06 $0D $54 $55 $4E $45 $F6 $0E $0D $FF $F6 $06 $0F $53 $46
.db $58 $F6 $0E $0F $FF $F6 $06 $11 $4A $4F $59 $50 $41 $44 $F6 $0F
.db $11 $FF $F6 $06 $13 $4C $45 $56 $45 $4C $F6 $0F $13 $FF $54 $52
.db $41 $49 $4E $49 $4E $47 $FF $45 $41 $53 $59 $20 $20 $20 $20 $FF
.db $4E $4F $52 $4D $41 $4C $20 $20 $FF $48 $41 $52 $44 $20 $20 $20
.db $20 $FF $03 $FF $05 $FF $07 $FF $09 $FF $31 $20 $4A $55 $4D $50
.db $FF $32 $20 $4A $55 $4D $50 $FF $F6 $09 $12 $43 $4F $4E $54 $49
.db $4E $55 $45 $20 $5B $5B $5B $20 $20 $FF $F6 $07 $0F $20 $20 $20
.db $20 $20 $47 $41 $4D $45 $20 $4F $56 $45 $52 $FF $F6 $0B $07 $F4
.db $08 $48 $49 $47 $48 $20 $53 $43 $4F $52 $45 $53 $FF $F4 $00 $F6
.db $0A $0C $50 $4C $45 $41 $53 $45 $20 $45 $4E $54 $45 $52 $F3 $59
.db $4F $55 $52 $20 $49 $4E $49 $54 $49 $41 $4C $53 $FF $20 $20 $20
.db $20 $20 $54 $48 $45 $20 $48 $4F $55 $53 $45 $20 $20 $20 $20 $20
.db $FF $20 $20 $20 $54 $48 $45 $20 $42 $41 $53 $45 $4D $45 $4E $54
.db $20 $20 $20 $20 $FF $20 $54 $48 $45 $20 $42 $55 $49 $4C $44 $49
.db $4E $47 $20 $53 $49 $54 $45 $20 $FF $20 $20 $20 $20 $54 $48 $45
.db $20 $4F $46 $46 $49 $43 $45 $20 $20 $20 $20 $20 $FF $20 $20 $20
.db $20 $54 $48 $45 $20 $4A $55 $4E $47 $4C $45 $20 $20 $20 $20 $20
.db $FF $F6 $06 $14 $FF $F6 $0A $0C $50 $4C $45 $41 $53 $45 $20 $45
.db $4E $54 $45 $52 $F3 $20 $20 $50 $41 $53 $53 $57 $4F $52 $44 $FF
.db $F6 $08 $14 $50 $41 $53 $53 $57 $4F $52 $44 $20 $20 $20 $20 $FF
.db $F6 $06 $07 $20 $4F $48 $20 $44 $45 $41 $52 $20 $20 $59 $4F $55
.db $20 $4D $55 $53 $54 $F3 $20 $43 $4F $4C $4C $45 $43 $54 $20 $41
.db $4C $4C $20 $44 $41 $44 $44 $59 $F3 $20 $20 $20 $4F $54 $54 $49
.db $46 $41 $4E $54 $53 $20 $4C $4F $53 $54 $F3 $20 $20 $50 $41 $50
.db $45 $52 $53 $20 $53 $4F $20 $48 $45 $20 $43 $41 $4E $F3 $20 $20
.db $20 $20 $20 $20 $47 $4F $20 $48 $4F $4D $45 $FF $F6 $06 $07 $20
.db $20 $43 $4F $4E $47 $52 $41 $54 $55 $4C $41 $54 $49 $4F $4E $53
.db $F3 $59 $4F $55 $20 $48 $41 $56 $45 $20 $43 $4F $4C $4C $45 $43
.db $54 $45 $44 $F3 $41 $4C $4C $20 $44 $41 $44 $44 $59 $20 $4F $54
.db $54 $49 $46 $41 $4E $54 $53 $F3 $20 $50 $41 $50 $45 $52 $53 $20
.db $41 $4E $44 $20 $48 $45 $20 $43 $41 $4E $F3 $20 $43 $4F $4D $45
.db $20 $48 $4F $4D $45 $20 $41 $54 $20 $4C $41 $53 $54 $FF $F6 $0A
.db $0B $53 $45 $4C $45 $43 $54 $20 $4C $45 $56 $45 $4C $FF $F6 $06
.db $0F $FF $F6 $06 $11 $FF $F6 $06 $13 $FF $F6 $06 $15 $53 $4F $55
.db $4E $44 $F6 $0F $15 $FF $4E $4F $4E $45 $20 $20 $20 $20 $20 $20
.db $FF $4D $55 $53 $49 $43 $20 $4F $4E $4C $59 $FF $46 $58 $20 $4F
.db $4E $4C $59 $20 $20 $20 $FF $4D $55 $53 $49 $43 $20 $6B $20 $46
.db $58 $FF

; 3rd entry of Pointer Table from 1DBE (indexed by _RAM_D9A8_)
; Data from 3B151 to 3B3F8 (680 bytes)
_DATA_3B151_:
.db $FF $FF $FF $FF $FF $F6 $0A $0D $44 $45 $4D $41 $52 $52 $45 $52
.db $20 $4A $45 $55 $FF $F6 $0A $0F $4D $4F $54 $20 $44 $45 $20 $50
.db $41 $53 $53 $45 $FF $F6 $0D $11 $4F $50 $54 $49 $4F $4E $FF $F6
.db $06 $0B $56 $49 $45 $F6 $0F $0B $FF $F6 $06 $0D $4D $55 $53 $49
.db $51 $55 $45 $F6 $0E $0D $FF $F6 $06 $0F $53 $4F $4E $20 $46 $58
.db $F6 $0E $0F $FF $F6 $06 $11 $4A $4F $59 $50 $41 $44 $F6 $0F $11
.db $FF $F6 $06 $13 $4E $49 $56 $45 $41 $55 $F6 $0F $13 $FF $41 $50
.db $50 $52 $45 $4E $54 $49 $53 $FF $46 $41 $43 $49 $4C $45 $20 $20
.db $20 $FF $4E $4F $52 $4D $41 $4C $20 $20 $20 $FF $44 $49 $46 $46
.db $49 $43 $49 $4C $45 $FF $03 $FF $05 $FF $07 $FF $09 $FF $31 $20
.db $53 $41 $55 $54 $45 $52 $FF $32 $20 $53 $41 $55 $54 $45 $52 $FF
.db $F6 $09 $12 $43 $4F $4E $54 $49 $4E $55 $45 $52 $20 $5B $5B $5B
.db $20 $20 $FF $F6 $07 $0F $20 $20 $20 $20 $20 $47 $41 $4D $45 $20
.db $4F $56 $45 $52 $FF $F6 $0B $09 $F4 $08 $48 $49 $47 $48 $20 $53
.db $43 $4F $52 $45 $53 $FF $F4 $00 $F6 $0A $0C $FF $20 $20 $20 $20
.db $20 $4C $41 $20 $4D $41 $49 $53 $4F $4E $FF $20 $20 $20 $20 $20
.db $20 $4C $41 $20 $43 $41 $56 $45 $20 $FF $20 $20 $20 $20 $4C $45
.db $20 $43 $48 $41 $54 $49 $45 $52 $FF $20 $20 $20 $20 $20 $4C $45
.db $20 $42 $55 $52 $45 $41 $55 $FF $20 $20 $20 $20 $20 $4C $41 $20
.db $4A $55 $4E $47 $4C $45 $FF $F6 $06 $14 $FF $F6 $09 $0C $F3 $4D
.db $4F $54 $20 $44 $45 $20 $50 $41 $53 $53 $45 $FF $F6 $07 $14 $4D
.db $4F $54 $20 $44 $45 $20 $50 $41 $53 $53 $45 $20 $20 $FF $F6 $06
.db $06 $20 $20 $20 $50 $41 $55 $56 $52 $45 $20 $44 $45 $20 $54 $4F
.db $49 $F3 $20 $54 $55 $20 $44 $4F $49 $53 $20 $52 $41 $53 $53 $45
.db $4D $42 $4C $45 $52 $F3 $20 $20 $54 $4F $55 $53 $20 $4C $45 $53
.db $20 $50 $41 $50 $49 $45 $52 $53 $F3 $50 $45 $52 $44 $55 $53 $20
.db $50 $41 $52 $20 $50 $41 $50 $41 $20 $50 $41 $55 $4C $F3 $20 $50
.db $4F $55 $52 $20 $51 $55 $6D $49 $4C $20 $50 $55 $49 $53 $53 $45
.db $F3 $52 $45 $4E $54 $52 $45 $52 $20 $41 $20 $4C $41 $20 $4D $41
.db $49 $53 $4F $4E $FF $F6 $06 $06 $20 $20 $20 $46 $45 $4C $49 $43
.db $49 $54 $41 $54 $49 $4F $4E $53 $F3 $20 $20 $54 $55 $20 $41 $53
.db $20 $52 $41 $53 $53 $45 $4D $42 $4C $45 $F3 $20 $54 $4F $55 $53
.db $20 $4C $45 $53 $20 $50 $41 $50 $49 $45 $52 $53 $20 $44 $45 $F3
.db $50 $41 $50 $41 $20 $50 $41 $55 $4C $20 $45 $54 $20 $49 $4C $20
.db $50 $45 $55 $54 $F3 $20 $20 $20 $45 $4E $46 $49 $4E $20 $52 $45
.db $4E $54 $52 $45 $52 $F3 $20 $20 $20 $20 $20 $20 $43 $48 $45 $5A
.db $20 $53 $4F $49 $FF $F6 $06 $0B $4E $49 $56 $45 $41 $55 $20 $44
.db $45 $20 $53 $45 $4C $45 $43 $54 $49 $4F $4E $FF $F6 $07 $0F $FF
.db $F6 $07 $11 $FF $F6 $07 $13 $FF $F6 $06 $15 $53 $4F $4E $F6 $0F
.db $15 $FF $50 $41 $53 $20 $44 $45 $20 $53 $4F $4E $20 $20 $20 $FF
.db $4D $55 $53 $49 $51 $55 $45 $20 $20 $20 $20 $20 $20 $FF $53 $4F
.db $4E $20 $46 $58 $20 $20 $20 $20 $20 $20 $20 $FF $4D $55 $53 $49
.db $51 $55 $45 $20 $6B $46 $58 $FF

; 4th entry of Pointer Table from 1DBE (indexed by _RAM_D9A8_)
; Data from 3B3F9 to 3B6C4 (716 bytes)
_DATA_3B3F9_:
.db $FF $FF $FF $FF $FF $F6 $09 $0D $43 $4F $4D $49 $4E $43 $2E $20
.db $47 $49 $4F $43 $4F $FF $F6 $09 $0F $50 $41 $52 $4F $4C $41 $20
.db $43 $48 $49 $41 $56 $45 $FF $F6 $0C $11 $4F $50 $5A $49 $4F $4E
.db $49 $FF $F6 $06 $0B $56 $49 $54 $45 $F6 $0F $0B $FF $F6 $06 $0D
.db $4D $55 $53 $49 $43 $41 $F6 $0E $0D $FF $F6 $06 $0F $53 $46 $58
.db $F6 $0E $0F $FF $F6 $06 $11 $4A $4F $59 $50 $41 $44 $F6 $0F $11
.db $FF $F6 $06 $13 $50 $49 $41 $4E $4F $F6 $0F $13 $FF $54 $52 $41
.db $49 $4E $49 $4E $47 $FF $46 $41 $43 $49 $4C $45 $FF $4E $4F $52
.db $4D $41 $4C $45 $FF $44 $49 $46 $46 $49 $43 $49 $4C $45 $FF $03
.db $FF $05 $FF $07 $FF $09 $FF $31 $20 $53 $41 $4C $54 $41 $52 $45
.db $FF $32 $20 $53 $41 $4C $54 $41 $52 $45 $FF $F6 $09 $12 $41 $56
.db $41 $4E $54 $49 $20 $5B $5B $5B $20 $20 $FF $F6 $0A $0F $47 $49
.db $4F $43 $4F $20 $46 $49 $4E $49 $54 $4F $FF $F6 $0A $07 $F4 $08
.db $50 $55 $4E $54 $45 $47 $47 $49 $20 $4D $41 $58 $FF $F4 $00 $F6
.db $0A $0C $FF $20 $20 $20 $20 $20 $20 $4C $41 $20 $43 $41 $53 $41
.db $20 $20 $20 $20 $20 $FF $20 $20 $20 $20 $20 $4C $41 $20 $43 $41
.db $4E $54 $49 $4E $41 $20 $20 $20 $FF $20 $20 $20 $20 $20 $49 $4C
.db $20 $43 $41 $4E $54 $49 $45 $52 $45 $20 $20 $FF $20 $20 $20 $20
.db $20 $20 $4C $27 $55 $46 $46 $49 $43 $49 $4F $20 $20 $20 $FF $20
.db $50 $41 $45 $53 $41 $47 $47 $49 $4F $20 $47 $49 $55 $4E $47 $4C
.db $41 $FF $F6 $06 $14 $FF $F6 $0A $0C $50 $52 $45 $4D $2E $20 $45
.db $4E $54 $45 $52 $F3 $50 $41 $52 $4F $4C $41 $20 $43 $48 $49 $41
.db $56 $45 $FF $F6 $08 $14 $50 $41 $52 $4F $4C $41 $20 $43 $48 $49
.db $41 $56 $45 $FF $F6 $06 $06 $20 $20 $20 $41 $48 $49 $4D $45 $6C
.db $20 $44 $45 $56 $49 $F3 $52 $41 $43 $43 $4F $47 $4C $49 $45 $52
.db $45 $20 $54 $55 $54 $54 $49 $20 $49 $F3 $20 $44 $4F $43 $55 $4D
.db $45 $4E $54 $49 $20 $50 $45 $52 $44 $55 $54 $49 $F3 $44 $45 $4C
.db $20 $50 $41 $50 $41 $20 $50 $41 $55 $4C $20 $50 $52 $49 $4D $41
.db $F3 $20 $20 $20 $43 $48 $45 $20 $45 $47 $4C $49 $20 $50 $4F $53
.db $53 $41 $F3 $20 $20 $20 $54 $4F $52 $4E $41 $52 $45 $20 $41 $20
.db $43 $41 $53 $41 $FF $F6 $06 $05 $20 $20 $20 $43 $4F $4E $47 $52
.db $41 $54 $55 $4C $41 $5A $49 $4F $4E $49 $6C $F3 $20 $20 $20 $53
.db $45 $49 $20 $46 $49 $4E $41 $4C $4D $45 $4E $54 $45 $F3 $20 $20
.db $20 $52 $49 $55 $53 $43 $49 $54 $4F $20 $41 $F3 $20 $20 $52 $41
.db $43 $43 $4F $47 $4C $49 $45 $52 $45 $20 $54 $55 $54 $54 $49 $20
.db $49 $F3 $44 $4F $43 $55 $4D $45 $4E $54 $49 $20 $44 $49 $20 $50
.db $41 $50 $41 $20 $50 $41 $55 $4C $F3 $20 $45 $47 $4C $49 $20 $50
.db $55 $4F $20 $46 $49 $4E $41 $4C $4D $45 $4E $54 $45 $F3 $20 $20
.db $20 $20 $54 $4F $52 $4E $41 $52 $45 $20 $41 $20 $43 $41 $53 $41
.db $FF $F6 $08 $0B $47 $52 $41 $44 $4F $20 $44 $49 $20 $53 $43 $45
.db $4C $54 $41 $FF $F6 $06 $0F $FF $F6 $06 $11 $FF $F6 $06 $13 $FF
.db $F6 $06 $15 $53 $4F $55 $4E $4F $F6 $0F $15 $FF $20 $20 $20 $20
.db $20 $20 $20 $20 $20 $20 $20 $FF $4D $55 $53 $49 $43 $41 $20 $20
.db $20 $20 $20 $FF $53 $4F $55 $4E $4F $20 $46 $58 $20 $20 $20 $FF
.db $4D $55 $53 $49 $43 $41 $20 $6B $20 $46 $58 $FF

; 5th entry of Pointer Table from 1DBE (indexed by _RAM_D9A8_)
; Data from 3B6C5 to 3BFFF (2363 bytes)
_DATA_3B6C5_:
.incbin "Ottifants_DATA_3B6C5_.inc"


.BANK 15
.ORG $0000

; Data from 3C000 to 3C5C7 (1480 bytes)
.incbin "Ottifants_DATA_3C000_.inc"

; Data from 3C5C8 to 3C821 (602 bytes)
_DATA_3C5C8_:
.db $14 $12 $00 $00 $00 $01 $00 $02 $00 $03 $00 $04 $00 $05 $00 $06
.db $00 $07 $00 $08 $00 $09 $82 $00 $00 $00 $0A $00 $0B $00 $0C $00
.db $0D $84 $00 $00 $00 $0E $00 $0F $00 $10 $00 $11 $00 $12 $00 $13
.db $00 $14 $00 $15 $00 $16 $00 $17 $00 $18 $00 $19 $00 $1A $00 $1B
.db $00 $1C $00 $1D $00 $1E $83 $00 $00 $00 $1F $00 $20 $00 $21 $00
.db $22 $00 $23 $00 $24 $00 $25 $00 $26 $00 $27 $00 $28 $00 $29 $00
.db $2A $00 $2B $00 $2C $00 $2D $00 $2E $00 $2F $83 $00 $00 $00 $30
.db $00 $31 $00 $32 $00 $33 $00 $34 $00 $35 $00 $36 $00 $37 $00 $38
.db $00 $39 $00 $3A $00 $3B $00 $3C $00 $3D $00 $3E $00 $3F $00 $40
.db $83 $00 $00 $00 $41 $00 $42 $00 $43 $00 $44 $00 $45 $00 $46 $00
.db $47 $00 $48 $00 $49 $00 $4A $00 $4B $00 $4C $00 $4D $00 $4E $00
.db $4F $00 $50 $00 $51 $84 $00 $00 $00 $52 $00 $53 $00 $54 $00 $55
.db $00 $56 $00 $57 $00 $58 $00 $59 $00 $5A $00 $5B $00 $5C $00 $5D
.db $00 $5E $00 $5F $00 $60 $00 $61 $84 $00 $00 $00 $62 $00 $63 $00
.db $64 $00 $65 $00 $66 $00 $67 $00 $68 $00 $69 $00 $6A $00 $6B $00
.db $6C $00 $6D $00 $6E $00 $6F $00 $70 $00 $71 $00 $72 $00 $73 $00
.db $74 $00 $00 $00 $75 $00 $76 $00 $77 $00 $78 $00 $79 $00 $7A $00
.db $7B $00 $7C $00 $7D $00 $7E $00 $7F $00 $80 $00 $81 $00 $82 $00
.db $83 $00 $84 $00 $85 $00 $86 $00 $87 $00 $88 $00 $89 $00 $8A $00
.db $8B $00 $8C $00 $8D $00 $8E $00 $8F $00 $90 $00 $91 $00 $92 $00
.db $93 $00 $94 $00 $95 $00 $96 $00 $97 $00 $98 $00 $99 $00 $9A $00
.db $9B $00 $9C $00 $9D $00 $9E $00 $9F $00 $A0 $00 $A1 $00 $A2 $00
.db $A3 $00 $A4 $00 $A5 $00 $A6 $00 $A7 $00 $A8 $00 $A9 $00 $AA $00
.db $AB $00 $AC $00 $AD $00 $AE $00 $AF $00 $B0 $00 $B1 $00 $B2 $00
.db $B3 $00 $B4 $00 $B5 $00 $B6 $00 $B7 $00 $B8 $00 $B9 $00 $BA $00
.db $BB $00 $BC $00 $BD $00 $BE $00 $BF $00 $C0 $00 $C1 $00 $C2 $00
.db $00 $00 $C3 $00 $C4 $00 $C5 $00 $C6 $00 $C7 $00 $C8 $00 $C9 $00
.db $CA $00 $CB $00 $CC $00 $CD $00 $CE $00 $CF $00 $D0 $00 $D1 $00
.db $D2 $00 $D3 $00 $D4 $00 $D5 $00 $D6 $00 $00 $00 $D7 $00 $D8 $00
.db $D9 $00 $DA $00 $DB $00 $DC $00 $DD $00 $DE $00 $DF $00 $E0 $00
.db $E1 $00 $E2 $00 $E3 $00 $E4 $00 $E5 $00 $E6 $00 $E7 $00 $E8 $00
.db $E9 $83 $00 $00 $00 $EA $00 $EB $00 $00 $00 $EC $00 $ED $00 $EE
.db $00 $EF $00 $F0 $00 $F1 $00 $F2 $00 $F3 $00 $F4 $00 $F5 $00 $F6
.db $00 $F7 $00 $F8 $00 $F9 $83 $00 $00 $00 $FA $02 $FA $83 $00 $00
.db $00 $FB $00 $FC $00 $FD $00 $FE $00 $FF $01 $00 $01 $01 $01 $02
.db $01 $03 $01 $04 $8B $00 $00 $82 $01 $05 $01 $06 $01 $07 $01 $08
.db $01 $09 $01 $0A $01 $0B $01 $0C $01 $0D $8F $00 $00 $01 $0E $01
.db $0F $01 $10 $01 $11 $04 $09 $95 $00 $00

; Data from 3C822 to 3EA61 (8768 bytes)
_DATA_3C822_:
.incbin "Ottifants_DATA_3C822_.inc"


; Data from 3EA62 to 3EB79 (280 bytes)
_DATA_3EA62_:
.db $08 $80 $09 $00 $03 $03 $00 $F0 $28 $81 $09 $00 $03 $03 $00 $F0
.db $48 $82 $09 $00 $03 $03 $12 $05 $68 $83 $09 $00 $03 $03 $11 $04
.db $88 $84 $09 $00 $03 $03 $11 $06 $A8 $85 $09 $00 $03 $03 $11 $07
.db $C8 $86 $09 $00 $03 $03 $11 $08 $E8 $87 $09 $00 $03 $03 $10 $07
.db $08 $89 $09 $00 $03 $03 $10 $0A $28 $8A $09 $00 $03 $03 $10 $05
.db $48 $8B $09 $00 $03 $03 $10 $03 $68 $8C $09 $00 $03 $03 $10 $04
.db $88 $8D $09 $00 $03 $03 $13 $04 $A8 $8E $09 $00 $03 $03 $13 $04
.db $C8 $8F $09 $00 $03 $03 $13 $04 $E8 $90 $09 $00 $03 $03 $13 $04
.db $08 $92 $09 $00 $03 $03 $12 $06 $28 $93 $09 $00 $03 $03 $12 $05
.db $48 $94 $09 $00 $03 $03 $10 $06 $68 $95 $09 $00 $03 $03 $17 $06
.db $88 $96 $09 $00 $03 $03 $17 $06 $A8 $97 $09 $00 $03 $03 $17 $06
.db $C8 $98 $09 $00 $03 $03 $17 $06 $E8 $99 $09 $00 $03 $03 $17 $06
.db $08 $9B $06 $00 $03 $02 $17 $06 $C8 $9B $06 $00 $03 $02 $17 $06
.db $88 $9C $09 $00 $03 $03 $13 $05 $A8 $9D $09 $00 $03 $03 $11 $07
.db $C8 $9E $09 $00 $03 $03 $11 $06 $E8 $9F $09 $00 $03 $03 $13 $04
.db $08 $A1 $09 $00 $03 $03 $13 $04 $28 $A2 $01 $00 $01 $01 $00 $00
.db $48 $A2 $09 $00 $03 $03 $12 $06 $68 $A3 $09 $00 $03 $03 $12 $06
.db $88 $A4 $09 $00 $03 $03 $12 $06

; Data from 3EB7A to 3ECA1 (296 bytes)
_DATA_3EB7A_:
.db $08 $80 $09 $00 $03 $03 $F8 $00 $28 $81 $09 $00 $03 $03 $F8 $00
.db $48 $82 $09 $00 $03 $03 $F8 $00 $68 $83 $09 $00 $03 $03 $F8 $00
.db $88 $84 $09 $00 $03 $03 $F8 $00 $A8 $85 $09 $00 $03 $03 $F8 $00
.db $C8 $86 $09 $00 $03 $03 $F8 $00 $E8 $87 $09 $00 $03 $03 $F8 $00
.db $08 $89 $09 $00 $03 $03 $F8 $00 $28 $8A $09 $00 $03 $03 $F8 $00
.db $48 $8B $09 $00 $03 $03 $F8 $00 $68 $8C $09 $00 $03 $03 $F8 $00
.db $88 $8D $09 $00 $03 $03 $F8 $00 $A8 $8E $09 $00 $03 $03 $F8 $00
.db $C8 $8F $09 $00 $03 $03 $F8 $00 $E8 $90 $09 $00 $03 $03 $F8 $00
.db $08 $92 $09 $00 $03 $03 $F8 $00 $28 $93 $09 $00 $03 $03 $F8 $00
.db $48 $94 $09 $00 $03 $03 $F8 $00 $68 $95 $09 $00 $03 $03 $F8 $00
.db $88 $96 $09 $00 $03 $03 $F8 $00 $A8 $97 $09 $00 $03 $03 $F8 $00
.db $C8 $98 $09 $00 $03 $03 $F8 $00 $E8 $99 $09 $00 $03 $03 $F8 $00
.db $08 $9B $09 $00 $03 $03 $F8 $00 $28 $9C $09 $00 $03 $03 $F8 $00
.db $48 $9D $09 $00 $03 $03 $F8 $00 $68 $9E $09 $00 $03 $03 $F8 $00
.db $88 $9F $09 $00 $03 $03 $F8 $00 $A8 $A0 $09 $00 $03 $03 $F8 $00
.db $C8 $A1 $09 $00 $03 $03 $F8 $00 $E8 $A2 $09 $00 $03 $03 $F8 $00
.db $08 $A4 $09 $00 $03 $03 $F8 $00 $28 $A5 $09 $00 $03 $03 $F8 $00
.db $48 $A6 $09 $00 $03 $03 $F8 $00 $68 $A7 $06 $00 $02 $03 $10 $00
.db $28 $A8 $06 $00 $02 $03 $10 $00


; Data from 3ECA2 to 3FFFF (4958 bytes)
_DATA_3ECA2_:
.incbin "Ottifants_DATA_3ECA2_.inc"


.enum $C000 export	
_RAM_C000_ dsb $100	
_RAM_C100_ db	
.ende	
	
.enum $C19D export	
_RAM_C19D_ db	
.ende	
	
.enum $C1CF export	
_RAM_C1CF_ db	
_RAM_C1D0_ db	
_RAM_C1D1_ db	
_RAM_C1D2_ db	
_RAM_C1D3_ db	
_RAM_C1D4_ db	
_RAM_C1D5_ db	
_RAM_C1D6_ db	
_RAM_C1D7_ db	
_RAM_C1D8_ db	
.ende	
	
.enum $C1DA export	
_RAM_C1DA_ db	
_RAM_C1DB_ db	
_RAM_C1DC_ db	
_RAM_C1DD_ db	
_RAM_C1DE_ db	
_RAM_C1DF_ dw	
_RAM_C1E1_ dw	
_RAM_C1E3_ db	
_RAM_C1E4_ dw	
_RAM_C1E6_ db	
_RAM_C1E7_ dw	
_RAM_C1E9_ db	
_RAM_C1EA_ db	
_RAM_C1EB_ db	
_RAM_C1EC_ dw	
_RAM_C1EE_ dw	
.ende	
	
.enum $C1F6 export	
_RAM_C1F6_ db	
_RAM_C1F7_ db	
.ende	
	
.enum $C1FA export	
_RAM_C1FA_ db	
_RAM_C1FB_ db	
_RAM_C1FC_ dw	
_RAM_C1FE_ db	
_RAM_C1FF_ db	
_RAM_C200_ dw	
_RAM_C202_ db	
_RAM_C203_ db	
_RAM_C204_ db	
_RAM_C205_ db	
_RAM_C206_ db	
_RAM_C207_ dw	
_RAM_C209_ db	
_RAM_C20A_ db	
_RAM_C20B_ db	
_RAM_C20C_ db	
_RAM_C20D_ db	
_RAM_C20E_ db	
_RAM_C20F_ dw	
_RAM_C211_ dw	
_RAM_C213_ dw	
_RAM_C215_ dw	
.ende	
	
.enum $C218 export	
_RAM_C218_ dw	
_RAM_C21A_ dw	
_RAM_C21C_ dw	
.ende	
	
.enum $C220 export	
_RAM_C220_ db	
_RAM_C221_ db	
_RAM_C222_ db	
_RAM_C223_ db	
.ende	
	
.enum $C225 export	
_RAM_C225_ db	
_RAM_C226_ db	
_RAM_C227_ dw	
_RAM_C229_ db	
_RAM_C22A_ db	
_RAM_C22B_ db	
_RAM_C22C_ dsb $16	
_RAM_C242_ db	
_RAM_C243_ db	
.ende	
	
.enum $C25E export	
_RAM_C25E_ dsb $14	
.ende	
	
.enum $C275 export	
_RAM_C275_ db	
.ende	
	
.enum $C290 export	
_RAM_C290_ db	
.ende	
	
.enum $C2A6 export	
_RAM_C2A6_ db	
_RAM_C2A7_ db	
.ende	
	
.enum $C2C2 export	
_RAM_C2C2_ db	
_RAM_C2C3_ db	
_RAM_C2C4_ dw	
_RAM_C2C6_ dw	
_RAM_C2C8_ dw	
_RAM_C2CA_ db	
_RAM_C2CB_ db	
_RAM_C2CC_ db	
_RAM_C2CD_ db	
_RAM_C2CE_ db	
_RAM_C2CF_ db	
_RAM_C2D0_ db	
_RAM_C2D1_ dw	
.ende	
	
.enum $C2D4 export	
_RAM_C2D4_ db	
_RAM_C2D5_ db	
_RAM_C2D6_ db	
_RAM_C2D7_ db	
_RAM_C2D8_ db	
_RAM_C2D9_ db	
_RAM_C2DA_ db	
_RAM_C2DB_ db	
.ende	
	
.enum $C2DD export	
_RAM_C2DD_ db	
.ende	
	
.enum $C2FD export	
_RAM_C2FD_ dw	
_RAM_C2FF_ db	
_RAM_C300_ db	
_RAM_C301_ db	
.ende	
	
.enum $C303 export	
_RAM_C303_ db	
_RAM_C304_ dsb $10	
_RAM_C314_ dsb $10	
_RAM_C324_ db	
.ende	
	
.enum $C344 export	
_RAM_C344_ db	
_RAM_C345_ db	
_RAM_C346_ db	
.ende	
	
.enum $C34A export	
_RAM_C34A_ dw	
_RAM_C34C_ dw	
_RAM_C34E_ db	
_RAM_C34F_ dsb $20	
_RAM_C36F_ db	
.ende	
	
.enum $C5EF export	
_RAM_C5EF_ db	
.ende	
	
.enum $C5FC export	
_RAM_C5FC_ db	
.ende	
	
.enum $C78C export	
_RAM_C78C_ dw	
_RAM_C78E_ dw	
_RAM_C790_ dw	
_RAM_C792_ db	
.ende	
	
.enum $C892 export	
_RAM_C892_ db	
.ende	
	
.enum $C8B2 export	
_RAM_C8B2_ db	
_RAM_C8B3_ db	
_RAM_C8B4_ db	
_RAM_C8B5_ db	
_RAM_C8B6_ db	
_RAM_C8B7_ db	
_RAM_C8B8_ db	
_RAM_C8B9_ db	
_RAM_C8BA_ db	
_RAM_C8BB_ db	
_RAM_C8BC_ dw	
_RAM_C8BE_ db	
_RAM_C8BF_ db	
.ende	
	
.enum $C8C7 export	
_RAM_C8C7_ db	
_RAM_C8C8_ db	
_RAM_C8C9_ db	
_RAM_C8CA_ db	
_RAM_C8CB_ db	
_RAM_C8CC_ db	
.ende	
	
.enum $C8CF export	
_RAM_C8CF_ db	
.ende	
	
.enum $C8D2 export	
_RAM_C8D2_ dw	
_RAM_C8D4_ dw	
_RAM_C8D6_ dsb $31	
.ende	
	
.enum $C92A export	
_RAM_C92A_ dsb $62	
.ende	
	
.enum $C9D3 export	
_RAM_C9D3_ dw	
_RAM_C9D5_ db	
.ende	
	
.enum $C9D7 export	
_RAM_C9D7_ db	
_RAM_C9D8_ db	
_RAM_C9D9_ dw	
_RAM_C9DB_ db	
_RAM_C9DC_ db	
.ende	
	
.enum $C9DE export	
_RAM_C9DE_ dsb $10	
.ende	
	
.enum $CEB6 export	
_RAM_CEB6_ db	
.ende	
	
.enum $CEB8 export	
_RAM_CEB8_ db	
_RAM_CEB9_ db	
_RAM_CEBA_ db	
_RAM_CEBB_ db	
_RAM_CEBC_ dw	
_RAM_CEBE_ db	
_RAM_CEBF_ db	
.ende	
	
.enum $CEC6 export	
_RAM_CEC6_ dw	
.ende	
	
.enum $CECA export	
_RAM_CECA_ db	
.ende	
	
.enum $CECE export	
_RAM_CECE_ db	
_RAM_CECF_ dw	
.ende	
	
.enum $CED5 export	
_RAM_CED5_ db	
.ende	
	
.enum $CED7 export	
_RAM_CED7_ db	
.ende	
	
.enum $CEDA export	
_RAM_CEDA_ db	
.ende	
	
.enum $CEE6 export	
_RAM_CEE6_ dsb $2c	
.ende	
	
.enum $D0E0 export	
_RAM_D0E0_ db	
_RAM_D0E1_ db	
_RAM_D0E2_ db	
_RAM_D0E3_ db	
.ende	
	
.enum $D10C export	
_RAM_D10C_ db	
.ende	
	
.enum $D10F export	
_RAM_D10F_ db	
_RAM_D110_ db	
_RAM_D111_ db	
_RAM_D112_ db	
_RAM_D113_ db	
_RAM_D114_ db	
_RAM_D115_ db	
_RAM_D116_ db	
_RAM_D117_ db	
_RAM_D118_ db	
_RAM_D119_ db	
_RAM_D11A_ db	
_RAM_D11B_ dw	
_RAM_D11D_ db	
_RAM_D11E_ db	
_RAM_D11F_ dw	
_RAM_D121_ db	
_RAM_D122_ db	
_RAM_D123_ db	
_RAM_D124_ db	
_RAM_D125_ dw	
_RAM_D127_ db	
.ende	
	
.enum $D12A export	
_RAM_D12A_ db	
_RAM_D12B_ dw	
.ende	
	
.enum $D12E export	
_RAM_D12E_ db	
_RAM_D12F_ db	
_RAM_D130_ db	
_RAM_D131_ db	
_RAM_D132_ db	
_RAM_D133_ dw	
_RAM_D135_ dw	
_RAM_D137_ db	
_RAM_D138_ dw	
_RAM_D13A_ dw	
.ende	
	
.enum $D142 export	
_RAM_D142_ dw	
_RAM_D144_ dw	
_RAM_D146_ db	
.ende	
	
.enum $D148 export	
_RAM_D148_ db	
_RAM_D149_ db	
_RAM_D14A_ dw	
_RAM_D14C_ dw	
_RAM_D14E_ dw	
_RAM_D150_ dw	
_RAM_D152_ dw	
_RAM_D154_ dw	
_RAM_D156_ db	
_RAM_D157_ dw	
_RAM_D159_ dw	
_RAM_D15B_ dw	
_RAM_D15D_ dw	
_RAM_D15F_ dw	
_RAM_D161_ dw	
.ende	
	
.enum $D164 export	
_RAM_D164_ db	
_RAM_D165_ dw	
_RAM_D167_ dw	
_RAM_D169_ db	
.ende	
	
.enum $D16B export	
_RAM_D16B_ db	
.ende	
	
.enum $D1F7 export	
_RAM_D1F7_ db	
.ende	
	
.enum $D3F7 export	
_RAM_D3F7_ db	
_RAM_D3F8_ db	
_RAM_D3F9_ db	
_RAM_D3FA_ db	
_RAM_D3FB_ dw	
_RAM_D3FD_ db	
_RAM_D3FE_ db	
_RAM_D3FF_ db	
_RAM_D400_ db	
_RAM_D401_ dw	
_RAM_D403_ dw	
_RAM_D405_ dw	
_RAM_D407_ dw	
_RAM_D409_ db	
_RAM_D40A_ db	
_RAM_D40B_ db	
_RAM_D40C_ db	
_RAM_D40D_ db	
_RAM_D40E_ dw	
_RAM_D410_ db	
_RAM_D411_ dw	
_RAM_D413_ db	
_RAM_D414_ db	
_RAM_D415_ db	
_RAM_D416_ db	
_RAM_D417_ db	
_RAM_D418_ db	
.ende	
	
.enum $D41A export	
_RAM_D41A_ dw	
_RAM_D41C_ db	
_RAM_D41D_ db	
_RAM_D41E_ db	
_RAM_D41F_ db	
_RAM_D420_ db	
_RAM_D421_ dw	
_RAM_D423_ dw	
_RAM_D425_ db	
_RAM_D426_ db	
_RAM_D427_ dw	
_RAM_D429_ dw	
.ende	
	
.enum $D436 export	
_RAM_D436_ db	
_RAM_D437_ db	
_RAM_D438_ db	
_RAM_D439_ dw	
_RAM_D43B_ dw	
_RAM_D43D_ dw	
_RAM_D43F_ dw	
_RAM_D441_ db	
_RAM_D442_ db	
_RAM_D443_ db	
_RAM_D444_ db	
_RAM_D445_ db	
_RAM_D446_ db	
_RAM_D447_ db	
.ende	
	
.enum $D449 export	
_RAM_D449_ db	
_RAM_D44A_ db	
_RAM_D44B_ db	
_RAM_D44C_ db	
_RAM_D44D_ db	
_RAM_D44E_ db	
_RAM_D44F_ db	
_RAM_D450_ db	
_RAM_D451_ db	
_RAM_D452_ db	
_RAM_D453_ db	
_RAM_D454_ db	
_RAM_D455_ db	
_RAM_D456_ db	
.ende	
	
.enum $D45A export	
_RAM_D45A_ db	
.ende	
	
.enum $D45F export	
_RAM_D45F_ db	
.ende	
	
.enum $D7A0 export	
_RAM_D7A0_ db	
_RAM_D7A1_ db	
_RAM_D7A2_ dsb $3	
.ende	
	
.enum $D7BA export	
_RAM_D7BA_ db	
_RAM_D7BB_ db	
.ende	
	
.enum $D7BF export	
_RAM_D7BF_ db	
_RAM_D7C0_ db	
_RAM_D7C1_ db	
_RAM_D7C2_ dw	
_RAM_D7C4_ dw	
_RAM_D7C6_ dw	
.ende	
	
.enum $D7C9 export	
_RAM_D7C9_ db	
_RAM_D7CA_ db	
_RAM_D7CB_ dw	
_RAM_D7CD_ dw	
_RAM_D7CF_ dw	
.ende	
	
.enum $D7D2 export	
_RAM_D7D2_ db	
_RAM_D7D3_ db	
_RAM_D7D4_ db	
_RAM_D7D5_ db	
_RAM_D7D6_ db	
_RAM_D7D7_ db	
_RAM_D7D8_ db	
_RAM_D7D9_ db	
_RAM_D7DA_ db	
_RAM_D7DB_ db	
_RAM_D7DC_ db	
_RAM_D7DD_ db	
.ende	
	
.enum $D7DF export	
_RAM_D7DF_ db	
.ende	
	
.enum $D7F4 export	
_RAM_D7F4_ db	
_RAM_D7F5_ db	
_RAM_D7F6_ db	
.ende	
	
.enum $D7F8 export	
_RAM_D7F8_ db	
.ende	
	
.enum $D7FB export	
_RAM_D7FB_ db	
.ende	
	
.enum $D7FF export	
_RAM_D7FF_ dsb $3	
.ende	
	
.enum $D807 export	
_RAM_D807_ db	
.ende	
	
.enum $D816 export	
_RAM_D816_ db	
.ende	
	
.enum $D81D export	
_RAM_D81D_ db	
.ende	
	
.enum $D820 export	
_RAM_D820_ db	
.ende	
	
.enum $D823 export	
_RAM_D823_ db	
.ende	
	
.enum $D826 export	
_RAM_D826_ db	
.ende	
	
.enum $D82B export	
_RAM_D82B_ db	
.ende	
	
.enum $D82E export	
_RAM_D82E_ db	
_RAM_D82F_ db	
_RAM_D830_ dsb $3	
.ende	
	
.enum $D900 export	
_RAM_D900_ db	
.ende	
	
.enum $D99F export	
_RAM_D99F_ db	
_RAM_D9A0_ db	
_RAM_D9A1_ db	
_RAM_D9A2_ dw	
_RAM_D9A4_ dw	
_RAM_D9A6_ db	
_RAM_D9A7_ db	
_RAM_D9A8_ db	
_RAM_D9A9_ db	
_RAM_D9AA_ db	
_RAM_D9AB_ db	
_RAM_D9AC_ db	
.ende	
	
.enum $D9AF export	
_RAM_D9AF_ db	
_RAM_D9B0_ db	
.ende	
	
.enum $DFEE export	
_RAM_DFEE_ dw	
.ende	
	
.enum $FFF8 export	
_RAM_FFF8_ db	
.ende	
.enum $FFFB export
_RAM_FFFB_ db
_RAM_FFFC_ db
_RAM_FFFD_ db
_RAM_FFFE_ db
_RAM_FFFF_ db
.ende
