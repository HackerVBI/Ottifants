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

.DEF emu_page $10
.DEF sys_page $11
.DEF Tile_page $3f

.DEF decode_pix $e404	
.DEF tileram_adr $e408
.DEF tilemap_adr $e40a	;tilemap ram adress

.DEF update_tilemem $e416
.DEF update_2byte_tilemem $e41f
.DEF view_tilemem $e41c

.DEF cls_tileset $e419
.DEF send_palette $e40e	
.DEF key_scan $e410
.DEF send_sprites $e413
.DEF ay_send $e422
.DEF ay_send_byte $e425
.DEF music_player $e428
.DEF music_init $e42b
.DEF spr_descr	$ea00

.DEF temp0 $e3f0
.DEF temp1 temp0+1
.DEF temp2 temp1+1
.DEF temp3 temp2+1
.DEF posx temp3+1
.DEF posy posx+1	

.include "WLADX/tsconfig.asm"
.background "_spg/Ottifants.sms"	;Ottifants.sms

.BANK 0 SLOT 0

.org $10		; rst $10
	jp view_tilemem


.org $20		; rst $20
	jp ay_send

.org $28		; rst $20
	jp ay_send_byte

.org $400
	nop
	nop
	nop

.org $286b
	.dsb 5,0

; music routines
;.org $0912
;	jp music_init
;	Music change
;	A - number of music



;.org $2868
;	call music_player
;	call $0b27	; 	effects


; end music routines


.org $0bc1
	rst $28
	nop

.org $0c3e
	rst $28
	nop

.org $0c56
	rst $28
	nop

.org $0c62
	rst $28
	nop

.org $0c78
	rst $20
	nop

.org $0c82
	rst $20
	nop


.org $0d50
	rst $28
	nop

.org $0d5b
	rst $28
	nop

.org $0d62
	rst $28
	nop


.org $0e5d
	.dsb 3,0


.org $0e65
	.dsb 19,0

.org $0e78
	.dsb 3,0


.org $110c			; send HUD sprites in pos. 0-$0f 
;	inc hl
;	inc hl
;	jp $113b
	call setup_hud
	jp $113e

.org $192c			; show screens procedure
	set 7,h
	set 6,h
	ld (tilemap_adr),hl
	nop
	
.org $194b			; show screens tileset procedure
	call update_tilemem
	ld a, c
	call update_tilemem_attr

.org $1962
	set 7,h
	set 6,h
	ld (tilemap_adr),hl
	nop


.org $1abd		; STAGE
	xor a
	call stage


.org $2293
	ld b,0
	call send_sprites
	ld hl,$f800
	ld de,$f801
	ld bc,$6ff
	ld (hl),l
	ldir
	xor a
	ld (posx),a
	ld (posy),a
	ld hl,spr_descr+6+1
	ld de,6
	ld b,$0f
	xor a
clrs1	ld (hl),a
	add hl,de
	djnz clrs1
	jp send_sprites
;	ld hl,0
;	jp cls_tileset


.org $248d		; ld #3f00,#d0 - terminate vdp sprite show?
	ret
;	.dsb 9,0


.org $2451
	ex de,hl
	call translate_gfx_adr
	ex de,hl
;	ld (tileram_adr),hl
	srl b
	rr c
	srl b
	rr c
	jp unpack1

.org $249c
	ret

.org $2800
	rst $10
	nop

; interrupt x/y offset
.org $2821
	ld (posx),a
	.dsb 5,0
.org $2842
	ld (posy),a
	.dsb 3,0




/*
.org $243C
	ld hl,0
	jp cls_tileset
*/



; sprite outs
.org $28B5		; sprite out to vdp
	.dsb 10,0

.org $28d4

;	ld hl,	; Y pos adr
	ld de,$C92A	; X pos,I
;	ld b,			; sprite count
	jp send_sprites

.org $28ef
	ld b,0
	jp send_sprites



.org $28f4		; set screen off
	.dsb 10,0
.org $2927		; set screen on
	ret
;	.dsb 9,0



; update tiles while moving. 
.org $2fd1	; 17 bytes
	ld a,h
	or $f8
	ld h,a
;	ld (tilemap_adr),hl
	ld a,(de)
	ld (hl),a
	inc de
	inc l
	ld a,(de)
	call tile_attrs_a
	dec l
	.dsb 4,0

.org $3027
	ld a,h
	or $f8
	ld h,a
;	ld (tilemap_adr),hl
	ld a,(de)
	ld (hl),a
	inc de
	inc l
	ld a,(de)
	call tile_attrs_a
	dec l
	.dsb 4,0


.org $30b8	; 8 bytes

	ld a,h
	or $f8
	ld h,a
	ld (tilemap_adr),hl
	nop
/*
	call set_hl_c0
	.dsb 5,0
*/
.org $30e7
	call tiles1
	inc e
	call update_tilemem_attr
	nop
	nop

.org $3121
	ld a,h
	or $f8
	ld h,a
	ld (tilemap_adr),hl
	nop
/*	call set_hl_c0
	.dsb 5,0
*/
.org $312a
	call tiles1
	inc e
	call update_tilemem_attr
	nop
	nop


.org $316c		; view game blocks tiles like bears, others
-:	
	call set_H
	inc hl
	ldi
	call tile_attrs
	dec a
	jr nz,-
	nop
	nop
	nop

.org $320f
/*	ld a,(hl): out (#bf),a	
	inc hl
	ld a,(hl): ld a,(hl)
	inc hl
	out (bf),a	; #61e0 ??		*/
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	call translate_gfx_adr
	nop
	nop
;	nop

.org $3226		;1. update sprite mem
/*	ld b,#10
	ld a,(hl)
	inc hl
	out (#be),a
	ld e,(hl)
	ld a,(de)
	inc hl
	nop: nop
	out (#be),a
	djnz $3228	*/

	call unpack2
	jp $3235

.org $324b		;2. update sprite mem
/*	ld b,#10
	ld a,(hl)
	inc hl
	out (#be),a
	ld e,(hl)
	ld a,(de)
	inc hl
	nop: nop
	out (#be),a
	djnz $3228	*/

	call unpack3
	jp $325a


.org $3325
	push de
	ld a,h
	or $f8
	ld h,a
	ld e,(hl)
	inc hl
	ld a,(hl)
	and 1
	ld h,a
	call $3376
	pop de
	jp $3352

.org $3356
	; print procedure
	ld a,h
	or $f8
	ld h,a
	ld (hl),c
	inc hl
	ld a,b
	call tile_attrs_a2
;	ld (hl),b
	ld de, ($C1E1)
	inc de
	ld ($C1E1), de
	ld hl, ($C1DF)
	jp $32A3


.org $3439			; decode font
	call $2a0
	xor a
	ld (temp2),a
	ld (temp3),a
-:	ld a, (hl)
	ld (temp0),a
	inc hl
	dec bc
	ld a, (hl)
	ld (temp1),a
	inc hl
	dec bc
	push hl
	ld hl,temp3
	call decode_pix
	pop hl
	ld a,c
	or b
	jr nz,-
	nop
	nop
	nop
	nop


.org $3d09		; palette update
	ld bc,$2000
	jp send_palette


/*
.org $31a2
-:	ld e,(hl)
	inc hl
	ld d,(hl)	; #79
	set 7,d
	inc hl
	ldi
	ldi
	nop
	nop
	dec a
	jr nz,-
;	.dsb 4,0
*/

; -----------------------------
; mem for procedures

.org $2a0
	push hl
	push de
	ld de, ($C2FD)
	call translate_gfx_adr
	pop de
	pop hl
	ret

unpack3
	ld b,8
-:	
	ld e,(hl)
	ld a, (de)
	ld (temp0),a
	inc hl
	ld a, (hl)
;	and #ff
	ld (temp1),a
	inc hl

	ld e,(hl)
	ld a, (de)
	ld (temp2),a
	inc hl
	ld a, (hl)
;	and #ff
	ld (temp3),a
	inc hl
	push hl
	ld hl,temp3
	call decode_pix
	pop hl
	djnz -
	ret

unpack2
	ld b,8
-:	ld	a, (hl)
	ld (temp0),a
	inc	hl
	ld e,(hl)
	ld	a, (de)
	ld (temp1),a
	inc hl

	ld	a, (hl)
	ld (temp2),a
	inc	hl
	ld e,(hl)
	ld	a, (de)
	ld (temp3),a
	inc hl
	push hl
	ld hl,temp3
	call decode_pix
	pop hl
	djnz -
	ret


unpack1
-:	ld	a, (de)
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
	dec bc
	ld a, b
	or c
	jr nz, -
	ret

tiles1
;	ld a,(de)
	call update_tilemem
	inc de
	ld a,(de)
	and $f6
	ret

set_hl_c0
	ld a,h
	or $f8
	ld (tilemap_adr+1),a
	ld a,l
	ld (tilemap_adr),a
	ret

tile_attrs_a
	and $f6
tile_attrs_a2
	ld c,0
	bit 1,a
	jr z,uta11
	set 6,c
uta11:	
	bit 2,a
	jr z,uta21
	set 7,c
uta21:	
	bit 3,a
	jr z,uta22
	set 4,c
uta22:	
	bit 0,a
	jr z,uta23
	set 0,c
uta23:	
	ld (hl),c
	ret



tile_attrs
	push af
	xor a
	bit 1,(hl)
	jr z,ti1
	set 6,a
ti1:	
	bit 2,(hl)
	jr z,ti2
	set 7,a
ti2:	
	bit 0,(hl)
	jr z,ti3
	set 0,a
ti3
	ld (de),a
	inc hl
	inc e
	pop af
	ret

update_tilemem_attr
	push hl
	ld c,0
	bit 1,a
	jr z,uta1
	set 6,c
uta1:	
	bit 2,a
	jr z,uta2
	set 7,c
uta2:	
	bit 0,a
	jr z,uta3
	set 0,c
uta3:
	ld hl,(tilemap_adr)
	ld (hl),c
	inc hl
	ld (tilemap_adr),hl
	pop hl
	ret

translate_gfx_adr
	ld a,d
	and $f8
	push af
	ld a,d
	and $07
	srl a
	rr e
	srl a
	rr e
	srl a
	rr e
	ld d,a
	pop af
	or d
	ld d,a
	ld (tileram_adr),de
	ret


setup_hud
	push ix
	ld ix,spr_descr+6
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld b,(hl)
;	push hl
	ex de,hl
	ld de,6

	push ix
	ld c,b
o11	
	ld a,(hl)	; Y
	inc hl
	ld (ix+0),a
	add ix,de
	djnz o11

	pop ix
	ld b,c
o12	ld a,$20
	ld (ix+1),a
	ld a,(hl)	;X
	ld (ix+2),a
	inc hl
	ld a,(hl)	;I
	ld (ix+4),a
	ld a,$10+1
	ld (ix+5),a
	inc hl
	add ix,de
	djnz o12
;	pop hl
	pop ix
	ret

set_H	ld e,(hl)
	inc hl
	ld d,(hl)	; #79
	push af
	ld a,d
	or $f8
	ld d,a
	pop af
	ret

stage
	ld a,0
	ld ($D9AA), a
	ret