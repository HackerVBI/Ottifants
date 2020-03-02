			device zxspectrum128
Sprite_page		equ #18
Tile_page		equ #3f
Vid_page		equ #40
Tile0_spr_page		equ #30

Main_emu_page		equ #10
muz_page		equ #13
screen_page		equ #14
lines_data_len		equ 320/4
roll_num	equ 32

		org #c000

start:		di
;		jp #e400
		
		ld sp,#ffff
		call sprite_off
		ld bc,PAGE2
		ld a,Tile0_spr_page
		out (c),a
		ld hl,0
		ld (#8000),hl
		ld hl,tclear
		call set_ports

		ld bc,PAGE2
		ld a,Vid_page
		out (c),a
		ld hl,0
		ld (#8000),hl
		ld hl,init_ts
		call set_ports
		call dma_stats
		ld b,high DMALEN
		ld a,$ff
		out (c),a
		ld b,high DMASTATUS
		ld a,DMA_FILL
		out (c),a

		xor a
		ld bc,GXOFFSL
		ld e,4
		call fill_ports
		ld b,high T0XOFFSL
		ld e,8
		call fill_ports

		ld hl,#0000
		call clear_tileset0
		ld hl,pal_db2
		ld de,pal_db
		ld bc,#e0
		ldir
		ld hl,pal_db1+2
		ld de,pal_db+#102
		ld bc,30
		ldir

		ld hl,pal_dma
		call set_ports

		call rnd_screen

		ld hl,#030c
		ld de,#0000
		ld bc,#1f1e;(chunk_x/4)*256+2
		ld a,Tile_page
		call tile_filler_page0


		ld bc,PAGE2
		ld a,muz_page
		out (c),a
		call #8000
		ld hl,roll_db
		ld b,roll_num
1		push hl
		call rnd
		and #f8
		pop hl
		ld (hl),a
		inc hl
		inc hl
		djnz 1b
		ld hl,line_db
		ld b,lines_data_len
1		push hl
		call rnd
		and #f8
		pop hl
		ld (hl),a
		inc hl
		inc hl
		djnz 1b

	ld	hl,sin_scrollY
	ld	bc,SCROLL_AMPL_Y
	call	SineGen
	ld	hl,sin_scrollX
	ld	bc,SCROLL_AMPL_X
	call	SineGen

		xor a
		ld bc,VSINTH
		out (c),a
		ld a,#f0
		ld i,a
		ld hl,int
		ld (#f0ff),hl
		ld hl,line_int
		ld (#f0fd),hl
		im 2
		ld bc,VCONFIG
		ld a,VID_360X288+VID_256C+VID_NOGFX
		out (c),a
		ld b,high TSCONFIG
		ld a,TSU_T0EN+TSU_T0ZEN+TSU_SEN
		out (c),a
		ld bc,HSINT
		xor a
		out (c),a
		ld hl,screen
		call set_ports
	
		ei
/*
		ld bc,GYOFFSL
		ld hl,512-12
		out (c),l
		inc b
		out (c),h
*/
intro		call int_wait
i_wait		ld hl,368-4
		dec hl
		ld a,l
		or h
		jr z,1f
		ld (i_wait+1),hl
	ld bc,#27af
	ld a,DMA_DALGN + DMA_RAM
	out (c),a
		JR 2F
1
		ld bc,VCONFIG
		ld a,VID_360X288+VID_256C
		out (c),a

		call line_screen

		call scroll_gfx_pal
		call roll_screen

		call	INDICATOR.draw
		call	scroll
		ld	hl,dma_scroll
		call	set_ports
2
		LD BC,#F7FE	;1
		IN A,(C)
		BIT 0,A
		jr nz,1f
		ld a,1	; bridge
		jr exit

1		BIT 1,A
		jr nz,1f
		ld a,2
		jr exit

1		BIT 2,A
		jr nz,1f
		ld a,3
		jr exit

1		BIT 3,A
		jr nz,1f
		ld a,4
		jr exit

1		LD BC,#EFFE	;0
		IN A,(C)
		BIT 0,A
		jr nz,1f
		ld a,#ff	; New Music 
		jr exit

1		LD BC,#7FFE	;space
		IN A,(C)
		BIT 0,A
		jr nz,intro
		xor a
exit		ld (#e000),a
		call int_wait
		call #8000
		ld bc,PAGE2
		ld a,Vid_page
		out (c),a
		ld hl,0
		ld (#8000),hl
		ld b,high TSCONFIG
		xor a
		out (c),a
		inc a
		ld b,high INTMASK
		out (c),a

		ld hl,init_ts
		call set_ports
		jp #e400



int2		push af
		push hl
		push bc
		ld bc,INTMASK
		ld a,3
		out (c),a

		xor a
		ld bc,VSINTL
		out (c),a
		inc b
		out (c),a
		ld (li1+1),a
		ld hl,int
		ld (#f0ff),hl
		pop bc
		pop hl
		pop af
		ei 
		ret

int		push af
		push hl
		push bc
		ld bc,VSINTL
		ld a,31
		out (c),a
		inc b
		xor a
		out (c),a
		ld hl,int2
		ld (#f0ff),hl
		call #8005
		ld a,1
		ld (int_wait+1),a
		pop bc
		pop hl
		pop af
		ei
		ret

line_int
		push hl
		push bc
		push af
li1		ld a,0
		inc a
		ld (li1+1),a
		ld h,high lines_scr_data
		ld l,a
		ld bc,GXOFFSL
		ld a,(hl)
		out (c),a
		pop af
		pop bc
		pop hl
		ei
		ret

int_wait	ld a,0
		or a
		jr z,int_wait
 		xor a
		ld (int_wait+1),a
		ret


line_screen
		ld de,lines_scr_data
		ld hl,line_db

		ld b,lines_data_len
ls1		ld a,(hl)
		dec a
		ld (hl),a
		cp 5
		jr nc,ls2
		or a
		jr nz,ls4
		ld a,#80
		ld (hl),a
		jr ls2

ls4		inc hl
		dec (hl)
		jr ls3

ls2		inc hl
ls3		ld a,(hl)
		inc hl
		dup 4
		ld (de),a
		inc de
		edup

		djnz ls1
		ret




roll_screen	ld hl,roll_db
		ld b,roll_num

roll_screen1	push bc
		ld a,(hl)
		dec (hl)
		cp 5
		jr nc,1f
		or a
		jr nz,2f
		ld a,#20
		ld (hl),a
		inc hl
		push hl
		call rnd
		pop hl
		and #fe
		ld (hl),a
		jr 3f

2		inc hl
		ld a,(hl)
		push hl
		call roller
		pop hl
		jr 3f
1		inc hl
3		inc hl
		pop bc
		djnz roll_screen1
		ret

roller		ld l,a
		ld h,0
		add hl,hl
		ld a,l
		ld (roll_scr+1),a
		ld (roll_scr1+1),a
		ld a,h
		ld (roll_scr1+3),a
		add 2
		ld (roll_scr+3),a
		ld hl,roll_scr
		call set_ports
		call dma_stats
		ld b,#28
		ld a,#fe
		out (c),a
		ld hl,roll_scr2
		jp set_ports

		ret
roll_scr
		db #1a,0
	        db #1b,2
		db #1c,Vid_page
roll_scr1       db #1d,0
	        db #1e,0
	        db #1f,Vid_page
	        db #28,#ff
roll_scr2       db #26,1
		db #27,DMA_DALGN +DMA_SALGN + DMA_RAM + DMA_ASZ
		db #ff


roll_db		ds 2*roll_num


scroll_gfx_pal
		ld hl,pal_db+#c0*2
		ld e,(hl)
		inc hl
		ld d,(hl)

		ld hl,pal_roll
		call set_ports
		call dma_stats

		ld hl,pal_db+#ff*2
		ld (hl),e
		inc hl
		ld (hl),d

		ld hl,pal_dma
		jp set_ports



rnd_screen	
		ld hl,#8000
1		exx
		call rnd
		exx
		or #c0
		ld b,3
		call rfill
		inc hl
		bit 1,h
		jr z,1b
		ld a,h
		add 6
		ld h,a
		cp #c0
		jr nz,1b
		ld bc,PAGE2
rs1		ld a,Vid_page
		inc a
		ld (rs1+1),a
		cp Vid_page+#11
		ret z
		out (c),a
		ld h,#80
		jr 1b

rfill		ld d,h
2		ld (hl),a
		inc h
		inc h
		ld (hl),a
		inc h
		inc h
		ld (hl),a
		ld h,d
		inc l
		djnz 2b
		ret


sprite_off	ld bc, FMADDR
		ld a, FM_EN+#8
		out (c), a      ; open FPGA arrays at #0000
		; clean SFILE

FM_SFILE        equ #0200+#8000
		ld hl,FM_SFILE
		xor a
spr_off_l1	ld (hl), a
		inc l
		jr nz,spr_off_l1
		inc h
spr_off_l2	ld (hl), a
		inc l
		jr nz,spr_off_l2
		out (c), a      ; close FPGA arrays at #0000
		ret


		include "sinescroll.a80"
screen
		db #1a,0
	        db #1b,0
		db #1c,screen_page
	        db #1d,0
	        db #1e,0
	        db #1f,Tile0_spr_page
	        db #26,248/4-1
	        db #28,0 ; 240-1
		db #27,DMA_DALGN + DMA_RAM
		db #ff

tclear
		db #1a,0
	        db #1b,0
		db #1c,Tile0_spr_page
	        db #1d,0
	        db #1e,0
	        db #1f,Tile0_spr_page
	        db #26,248/4-1
	        db #28,240-1
		db #27,DMA_FILL+DMA_DALGN
		db #ff


init_ts			db high VCONFIG,VID_360X288+VID_256C+VID_NOGFX
			db high MEMCONFIG,%00001110
			db high SYSCONFIG,6	; 
			db high VPAGE,Vid_page
			db high BORDER,0	; border
			db high TSCONFIG,0;TSU_T0EN+TSU_T0ZEN;+TSU_T1EN +TSU_T1ZEN		; TSConfig
			db high PALSEL,#20
			db high TMPAGE, Tile_page
			db high T0GPAGE,Tile0_spr_page
			DB high SGPAGE, Sprite_page
			db high GYOFFSL,0
			db high GXOFFSL,0

clr_screen
		defb #1a,0	;
		defb #1b,0	;
		defb #1c,Vid_page	;
		defb #1d,0	;
		defb #1e,0	;
		defb #1f,Vid_page	;
		defb #28,#ff	;
		defb #26,#ff	;
		defb #27,DMA_FILL
		db #ff


pal_dma			db #1a,low pal_db
		        db #1b,high pal_db
			db #1c,Main_emu_page
		        db #1d,0
		        db #1e,0
		        db #1f,0
		        db #26,#ff
		        db #28,0
			db #27,#84
			db #ff

pal_roll
			db #1a,low (pal_db+#c0*2+2)
		        db #1b,high (pal_db+#c0*2+2)
			db #1c,Main_emu_page
		        db #1d,low (pal_db+#c0*2)
		        db #1e,high (pal_db+#c0*2)
		        db #1f,Main_emu_page
		        db #26,#40-1
		        db #28,0
			db #27,DMA_RAM
			db #ff

	include "includes.asm"
	include "tsconfig.asm"


	align 2
pal_db	incbin "_spg/logo.tga.pal"

pal_db1	incbin "_spg/Ottifants_intro1.tga.pal4"
pal_db2	INCBIN	"SineScroll/res/font_32x32.tga.pal";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

line_db		ds lines_data_len*2
	align 256
lines_scr_data	ds lines_data_len*4
end

		SAVEBIN "_spg/intro.bin",start, end-start