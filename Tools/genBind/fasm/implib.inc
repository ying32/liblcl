; IMPLIB.INC v1.8
; ---------------
; (c) 2006-2008 Quantum

; A set of FASM macros useful for building MS-COFF import
; libraries. Currently there are 3 macros, which you can
; safely mix together in a single library:
; implib   - a general purpose symbol definition macro.
; pbimplib - a slightly modified version, intended for
;            PureBasic Userlib development.
; vbimplib - another mod, intended for Visual Basic 6
;            advanced developers. For instance, it is
;            useful for calling CDECL DLL functions
;            directly from Visual Basic 6, without
;            wrapper DLLs and/or delegators.
; MACRO implib   dllname*, name*, thunk, pubname
; MACRO pbimplib dllname*, cconv*, argcount*, name*, pb_name, pubname
; MACRO vbimplib module*, dllname*, cconv*, argcount*, name*, vb_name, pubname
; Parameters:
;    dllname  : DLL file name.
;    cconv    : Calling convention. Possible
;               values: STDCALL, CDECL, PBCALL
;    argcount : Number of DWORD arguments.
;               (A QWORD parameter counts as 2
;               DWORDs and so on.)
;    name     : Symbol name or an ordinal value,
;               prefixed with ord. This value should
;               match exactly the required DLL
;               exported symbol.
;    module   : Visual Basic module name.
;    pubname  : Optional public name.
;               This symbolic name allows calling
;               an external function directly
;               (not using a thunk).
;               If not specified, 'name' prefixed
;               with '__imp__' is used as the pubname.
;    thunk    : Optional thunk name.
;               A thunk function consists of a
;               single jmp [pubname] instruction.
;               If not specified, 'name' is assumed
;               to be the thunk name as well.
;    pb_name  : Optional PureBasic function name.
;               If not specified, 'name' is used
;               as the PureBasic name.
;    vb_name  : Optional Visual Basic function name.
;               If not specified, 'name' is used
;               as the Visual Basic name.
; Examples:
;    implib   dsound.dll,   ord.1, _DirectSoundCreate@12
;    implib   kernel32.dll, ExitProcess, _ExitProcess@4, __imp__ExitProcess@4
;    pbimplib openal32.dll, CDECL, 2, ord.1, alGenBuffers
;    vbimplib vbopenal, openal32.dll, CDECL, 2, alGenBuffers

; CONFIG: -----------------------------------------------------------------

; Set to 0 to disable original thunks (lookup table). Doing so
; reduces the IAT size in the final executable by 4 x n bytes,
; n being the number of imported symbols.
; However, you won't be able to bind (using BIND.EXE from PSDK)
; an IAT without original thunks! So, stripping the lookups
; is recommended only when importing from standart API DLLs,
; like KERNEL32, USER32 and so on.
KEEP_ORIGINAL_THUNKS equ 1

; Optional header is never required in object files.
; Very old MS LINK versions possibly use it for some reason.
; The following option is included for compatibility.
USE_OPTIONAL_HEADER equ 0

; An additional @comp.id symbol per object file
; is appended by MS LINK, possibly to watermark
; the import library.
USE_COMPID equ 0

; Disable the 2nd linker member for compatibility with the latest GNU AR
; tool. For some reason it prefers using the deprecated 1st linker member.
; LINKER_MEMBER_2_REMOVE equ 1

; -------------------------------------------------------------------------

STDCALL = 0
CDECL   = 1
PBCALL  = 2
str_tbl_size  = 0
last_dll_name = 0
num_symbols_indexed         = 0
IMAGE_FILE_MACHINE_I386     = 14Ch
IMAGE_FILE_32BIT_MACHINE    = 100h
IMAGE_CNT_CODE              = 20h
IMAGE_CNT_INITIALIZED_DATA  = 40h
IMAGE_LNK_COMDAT            = 1000h
IMAGE_ALIGN_2BYTES          = 200000h
IMAGE_ALIGN_4BYTES          = 300000h
IMAGE_MEM_EXECUTE           = 20000000h
IMAGE_MEM_READ              = 40000000h
IMAGE_MEM_WRITE             = 80000000h
IMAGE_SYM_CLASS_EXTERNAL    = 2
IMAGE_SYM_CLASS_STATIC      = 3
IMAGE_SYM_CLASS_SECTION     = 104
IMAGE_SYM_DTYPE_FUNCTION    = 20h
IMAGE_SYM_ABSOLUTE          = -1
IMAGE_REL_I386_DIR32        = 6
IMAGE_REL_I386_DIR32NB      = 7
IMAGE_COMDAT_SELECT_NODUPLICATES = 1
IMAGE_COMDAT_SELECT_ASSOCIATIVE  = 5

use32
org 0

; Print timestamp
macro print_timestamp {
	timestmp=%t
	t0='0'+timestmp mod 10
	t1='0'+(timestmp/10) mod 10
	t2='0'+(timestmp/100) mod 10
	t3='0'+(timestmp/1000) mod 10
	t4='0'+(timestmp/10000) mod 10
	t5='0'+(timestmp/100000) mod 10
	t6='0'+(timestmp/1000000) mod 10
	t7='0'+(timestmp/10000000) mod 10
	t8='0'+(timestmp/100000000) mod 10
	t9='0'+(timestmp/1000000000) mod 10
	tA='0'+(timestmp/10000000000) mod 10
	tB='0'+(timestmp/100000000000) mod 10
	if tB>'0'
		db tB,tA,t9,t8,t7,t6,t5,t4,t3,t2,t1,t0
	else
		if tA>'0'
			db tA,t9,t8,t7,t6,t5,t4,t3,t2,t1,t0,' '
		else
			if t9>'0'
				db t9,t8,t7,t6,t5,t4,t3,t2,t1,t0,'  '
			else
				if t8>'0'
					db t8,t7,t6,t5,t4,t3,t2,t1,t0,'   '
				else
					db t7,t6,t5,t4,t3,t2,t1,t0,'    '
				end if
			end if
		end if
	end if
}

; Get next object's size in bytes and store it in text format
macro pack_size pk_size* {
	lnkmem0='0'+pk_size mod 10
	lnkmem1='0'+(pk_size/10) mod 10
	lnkmem2='0'+(pk_size/100) mod 10
	lnkmem3='0'+(pk_size/1000) mod 10
	lnkmem4='0'+(pk_size/10000) mod 10
	if lnkmem4>'0'
		db lnkmem4,lnkmem3,lnkmem2,lnkmem1,lnkmem0
	else
		if lnkmem3>'0'
			db lnkmem3,lnkmem2,lnkmem1,lnkmem0,' '
		else
			if lnkmem2>'0'
				db lnkmem2,lnkmem1,lnkmem0,'  '
			else
				if lnkmem1>'0'
					db lnkmem1,lnkmem0,'   '
				else
					db lnkmem0,'    '
				end if
			end if
		end if
	end if
	db '     ',60h,0Ah
}

; Copy n bytes from src to dest
macro do_memcpy src*, dest*, n* {
	l_src=src
	l_dest=dest
	d=1
	if src+n>dest
		l_src=src+n-1
		l_dest=dest+n-1
		d=-1
	end if
	repeat n
		load chr byte from l_src
		store byte chr at l_dest
		l_src=l_src+d
		l_dest=l_dest+d
	end repeat
}

macro add_compid {
if USE_COMPID
	db '@comp.id'
	dd 131F62h
	dw IMAGE_SYM_ABSOLUTE
	dw 0
	db IMAGE_SYM_CLASS_STATIC
	db 0
end if
}

; Update linker directory: make sure string tables are sorted in ascending lexical order
macro add_2_dir new_str*,new_str_size* {
	if new_str_size=1
		display '-ERR: Empty string not allowed',13,10
	end if
	num_symbols_indexed=num_symbols_indexed+1
if ~ defined LINKER_MEMBER_2_REMOVE
	str_found2=sym_arr2
	obj_cursor=obj_tbl2-4
	repeat num_symbols_indexed
		str_found1=str_found2
		chr=1
		while chr
			load chr byte from str_found2
			str_found2=str_found2+1
		end while
		strcmp_r=0
		load chr byte from str_found1
		load chr2 byte from new_str
		while strcmp_r=0 & chr>=chr2
			if chr=chr2 & chr=0
				display '-ERR: [',`new_str,'] Duplicate symbol',13,10
			end if
			if chr>chr2
				strcmp_r=1
			end if
			load chr byte from str_found1+%
			load chr2 byte from new_str+%
		end while
		obj_cursor=obj_cursor+4
		if strcmp_r
			do_memcpy obj_cursor,obj_cursor+4,obj_tbl2+(num_symbols_indexed-1)*4-obj_cursor
			do_memcpy str_found1,str_found1+new_str_size,sym_arr2+str_tbl_size-str_found1
			break
		end if
	end repeat

	; Append to Linker member #2
	store dword obj__addr at obj_cursor
	do_memcpy new_str,str_found1,new_str_size
end if

	; Append to Linker member #1
	obj_cursor=(obj__addr shl 24)or((obj__addr and 0FF00h) shl 8)or((obj__addr and 0FF0000h) shr 8)or(obj__addr shr 24)
	obj_cursor=obj_cursor and 0FFFFFFFFh
	store dword obj_cursor at obj_tbl1
	obj_tbl1=obj_tbl1+4
	do_memcpy new_str,lnk1_str_tbl,new_str_size
	lnk1_str_tbl=lnk1_str_tbl+new_str_size
	str_tbl_size=str_tbl_size+new_str_size
}

; Get next even address
macro get_next_even_addr {
	virtual
		align 2
		skip_bytes=$-$$
		dd $
		load obj__addr dword from $-4
	end virtual
	if skip_bytes
		db 0Ah
	end if
}

	; Archive file signature
	db '!<arch>',0Ah
	; 1st header
	db '/               ' ; name
	print_timestamp       ; date
	db '            '     ; user id, group id (not used in Win32)
	db '0       '         ; mode
	pack_size lnk_member1_size
	; 1st linker member (deprecated, but still in use)
lnk_member1:
	; number of public symbols (in big endian format)
	dd ((num_symbols shl 24) or ((num_symbols and 0FF00h) shl 8) or ((num_symbols and 0FF0000h) shr 8)) and 0FFFFFFFFh
	obj_tbl1=$
	times num_symbols dd 0
	; String table
	lnk1_str_tbl=$
	times symarr_size db 0
	lnk_member1_size=$-lnk_member1

if ~ defined LINKER_MEMBER_2_REMOVE
	; 2nd header
	get_next_even_addr
	db '/               '
	print_timestamp
	db '            0       '
	pack_size lnk_member2_size
	; 2nd linker member
lnk_member2:
	dd num_symbols ; number of object files
	label obj_tbl2 dword
	times num_symbols dd 0
	dd num_symbols ; number of public symbols
	times num_symbols dw %
	; String table
	label sym_arr2 byte
	times symarr_size db 0
	lnk_member2_size=$-lnk_member2
end if

macro coff_header {
	; number of symbols
	if USE_COMPID
		if KEEP_ORIGINAL_THUNKS
			dd 8
		else
			dd 7
		end if
	else
		if KEEP_ORIGINAL_THUNKS
			dd 7
		else
			dd 6
		end if
	end if
	; size of optional header
	if USE_OPTIONAL_HEADER
		dw 0E0h
	else
		dw 0
	end if
	dw IMAGE_FILE_32BIT_MACHINE ; characteristics

	if USE_OPTIONAL_HEADER
		; *** Optional header
		dw 10Bh
		db 0,0,0,0,0,0,0,0,0
		dd 1000h,200h,4
		dq 0,0,0
		dd 3
		dd 100000h,1000h,100000h,1000h,0,1
		dq 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	end if

	; *** Section table
	; Section #1
	db '.idata$2' ; name
	dd 0,0,20     ; virtual size, virtual address, size of raw data
}

macro implib dllname*,name*,thunk,pubname {
	; When a dll name is used for the first time, add import descriptor and null thunks
	if ~ defined dllname#.alreadydefined
		get_next_even_addr

		; *** Import descriptor header
		dllname#.descr_name db `dllname # '/'
		x=$-dllname#.descr_name
		if x<16
			times 16-x db ' '
		end if
		print_timestamp
		db '            0       '
		pack_size (dllname#.descr_strtbl_end-dllname#.descr)
	dllname#.descr:

		; *** COFF header
		dw IMAGE_FILE_MACHINE_I386
		dw 2
		dd %t
		dd dllname#.descr_sym_tbl-dllname#.descr
		coff_header
		dd dllname#.descr_data1-dllname#.descr
		dd dllname#.descr_rel-dllname#.descr
		dd 0 ; pointer to line numbers
		; number of relocations, number of line numbers
		if KEEP_ORIGINAL_THUNKS
			dd 3
		else
			dd 2
		end if
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		; Section #2
		db '.idata$6'
		dd 0,0,dllname#.descr_data1-dllname#.descr_data2+1,dllname#.descr_data2-dllname#.descr
		dd dllname#.descr_rel2-dllname#.descr,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	dllname#.descr_data2 db `dllname
	dllname#.descr_data1 dd 0,0,0,0
		if KEEP_ORIGINAL_THUNKS
		dllname#.descr_rel:
			dd 0
			; symbol table index
			if USE_COMPID
				dd 4
			else
				dd 3
			end if
			dw IMAGE_REL_I386_DIR32NB
		else
			dd 0
		dllname#.descr_rel:
		end if
	dllname#.descr_rel2:
		dd 12
		if USE_COMPID
			dd 3
		else
			dd 2
		end if
		dw IMAGE_REL_I386_DIR32NB
		dd 16
		if USE_COMPID
			if KEEP_ORIGINAL_THUNKS
				dd 5
			else
				dd 4
			end if
		else
			if KEEP_ORIGINAL_THUNKS
				dd 4
			else
				dd 3
			end if
		end if
		dw IMAGE_REL_I386_DIR32NB

		; *** Symbol table
	dllname#.descr_sym_tbl:
		add_compid
		dd 0,29,0,1
		db IMAGE_SYM_CLASS_EXTERNAL
		db 0
		db '.idata$2'
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,1
		db IMAGE_SYM_CLASS_SECTION,0
		db '.idata$6'
		dd 0,2
		db IMAGE_SYM_CLASS_STATIC,0
		if KEEP_ORIGINAL_THUNKS
			db '.idata$4'
			dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,0
			db IMAGE_SYM_CLASS_SECTION,0
		end if
		db '.idata$5'
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,0
		db IMAGE_SYM_CLASS_SECTION,0
		dd 0,4,0,0
		db IMAGE_SYM_CLASS_EXTERNAL,0
		dd 0,dllname#.descr_strtbl_1-dllname#.descr_strtbl,0,0
		db IMAGE_SYM_CLASS_EXTERNAL,0

		; *** String table
	dllname#.descr_strtbl dd dllname#.descr_strtbl_end-$
		db '__NULL_IMPORT_DESCRIPTOR',0
	dllname#.descr_strtbl2 db '__IMPORT_DESCRIPTOR_' # `dllname,0
		add_2_dir dllname#.descr_strtbl2,$-dllname#.descr_strtbl2
		dllname#.descr_strtbl_1 db 07Fh,`dllname,'_NULL_THUNK_DATA',0
	dllname#.descr_strtbl_end:
		get_next_even_addr

		; *** Null thunks header
		dllname#.descr_name2 db `dllname # '/'
		x=$-dllname#.descr_name2
		if x<16
			times 16-x db ' '
		end if
		print_timestamp
		db '            0       '
		pack_size (dllname#.descr_strtbl2_end-dllname#.descr2)
	dllname#.descr2:

		; *** COFF header
		dw IMAGE_FILE_MACHINE_I386
		; number of sections
		if KEEP_ORIGINAL_THUNKS
			dw 2
		else
			dw 1
		end if
		dd %t ; time-date stamp
		dd dllname#.descr_sym_tbl2-dllname#.descr2
		; number of symbols
		if USE_COMPID
			dd 2
		else
			dd 1
		end if
		dw 0,IMAGE_FILE_32BIT_MACHINE
		; Section #1
		db '.idata$5'
		dd 0,0,4,dllname#.descr2_data-dllname#.descr2,0,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		; Section #2
		if KEEP_ORIGINAL_THUNKS
			db '.idata$4'
			dd 0,0,4,dllname#.descr2_data-dllname#.descr2,0,0,0
			dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		end if
		; *** Symbol table
	dllname#.descr_sym_tbl2:
		add_compid
	dllname#.descr2_data:
		dd 0,4,0,1
		db IMAGE_SYM_CLASS_EXTERNAL
		db 0 ; number of aux symbols
		; *** String table
		dd dllname#.descr_strtbl2_end-$
		dllname#.descr_strtbl3 db 07Fh,`dllname,'_NULL_THUNK_DATA',0
	dllname#.descr_strtbl2_end:
		add_2_dir dllname#.descr_strtbl3,$-dllname#.descr_strtbl3

	end if ; import descriptor and null thunks added
	define dllname#.alreadydefined 1
	get_next_even_addr

	; *** Nth object header
	last_dll_name=dllname#name#.obj_name
	dllname#name#.obj_name db `dllname # '/'
	x=$ - dllname#name#.obj_name
	if x>16
		display '-ERR: ' # `dllname # ' is a very long DLL name',13,10
	end if
	if x<16
		times 16-x db ' '
	end if
	print_timestamp
	db '            0       '
	pack_size (dllname#name#.strtbl_end-dllname#name#.lnk_memberN)
dllname#name#.lnk_memberN:

	; Symbolic name or Ordinal value?
	byordinal=0
	ordinal_value=0
	virtual at 0
		db `name
		if $>4
			load prefix dword from 0
			if prefix=2E64726Fh
				byordinal=1
				count=$-4
				repeat count
					load chr byte from %+3
					chr=chr-'0'
					if chr>9
						display '-ERR: Bad ordinal value: ' # `name,13,10
					else
						ordinal_value=ordinal_value*10+chr
					end if
				end repeat
			end if
		end if
	end virtual

	; *** COFF header
	dw IMAGE_FILE_MACHINE_I386
	; number of sections
	if KEEP_ORIGINAL_THUNKS
		if byordinal=0
			dw 4
		else
			dw 3
		end if
	else
		if byordinal=0
			dw 3
		else
			dw 2
		end if
	end if
	dd %t
	dd dllname#name#.symbol_table-dllname#name#.lnk_memberN
	if KEEP_ORIGINAL_THUNKS
		if byordinal=0
			if USE_COMPID
				dd 12
			else
				dd 11
			end if
		else
			if USE_COMPID
				dd 10
			else
				dd 9
			end if
		end if
	else
		if byordinal=0
			if USE_COMPID
				dd 10
			else
				dd 9
			end if
		else
			if USE_COMPID
				dd 8
			else
				dd 7
			end if
		end if
	end if
	dw 0
	dw IMAGE_FILE_32BIT_MACHINE

	; Section #1:
	db '.text',0,0,0
	dd 0,0,6
	dd dllname#name#.thunk-dllname#name#.lnk_memberN
	dd dllname#name#.rel1-dllname#name#.lnk_memberN
	dd 0,1
	dd IMAGE_CNT_CODE or IMAGE_LNK_COMDAT or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_EXECUTE
	; Section #2:
	db '.idata$5'
	dd 0,0,4
	dd dllname#name#.rel2-dllname#name#.lnk_memberN
	if byordinal=0
		dd dllname#name#.rel2-dllname#name#.lnk_memberN,0,1
	else
		dd 0,0,0
	end if
	dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	; Section #3:
	if KEEP_ORIGINAL_THUNKS
		db '.idata$4'
		dd 0,0,4
		dd dllname#name#.rel2-dllname#name#.lnk_memberN
		if byordinal=0
			dd dllname#name#.rel2-dllname#name#.lnk_memberN,0,1
		else
			dd 0,0,0
		end if
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	end if
	if byordinal=0
		; Section #4:
		db '.idata$6'
		dd 0,0,dllname#name#.data3_end-dllname#name#.data3
		dd dllname#name#.data3-dllname#name#.lnk_memberN,0,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		dllname#name#.thunk:
			; jmp [pubname]
			dw 25FFh
		dllname#name#.rel2 dd 0
			if KEEP_ORIGINAL_THUNKS
				if USE_COMPID
					dd 9
				else
					dd 8
				end if
			else
				if USE_COMPID
					dd 7
				else
					dd 6
				end if
			end if
			dw IMAGE_REL_I386_DIR32NB
		dllname#name#.rel1 dd 2
			if USE_COMPID
				dd 6
			else
				dd 5
			end if
			dw IMAGE_REL_I386_DIR32
		dllname#name#.data3 db 0,0,`name,0
			virtual
				align 2
				skip_bytes=$-$$
			end virtual
			if skip_bytes
				db 0
			end if
		dllname#name#.data3_end:
	else
		dllname#name#.thunk:
			; jmp [pubname]
			dw 25FFh
			dd 0
		dllname#name#.rel1 dd 2
			if USE_COMPID
				dd 6
			else
				dd 5
			end if
			dw IMAGE_REL_I386_DIR32
		dllname#name#.rel2 dd 80000000h+ordinal_value
	end if
dllname#name#.symbol_table:
	add_compid
	db '.text',0,0,0
	dd 0,1
	db IMAGE_SYM_CLASS_STATIC,1
	dd 6,1,0
	dw 0
	dd IMAGE_COMDAT_SELECT_NODUPLICATES
	dd 0,dllname#name#.thunkname-dllname#name#.strtbl,0
	dw 1
	dw IMAGE_SYM_DTYPE_FUNCTION,IMAGE_SYM_CLASS_EXTERNAL
	db '.idata$5'
	dd 0,2
	db IMAGE_SYM_CLASS_STATIC,1
	dd 4,1,0
	dw 0
	dd IMAGE_COMDAT_SELECT_NODUPLICATES
	dd 0,4,0,2
	dw IMAGE_SYM_CLASS_EXTERNAL
	if KEEP_ORIGINAL_THUNKS
		db '.idata$4'
		dd 0,3
		db IMAGE_SYM_CLASS_STATIC,1
		dd 4,1,0
		dw 2
		dd IMAGE_COMDAT_SELECT_ASSOCIATIVE
	end if
	if byordinal=0
		db '.idata$6'
		dd 0
		if KEEP_ORIGINAL_THUNKS
			dd 4
		else
			dd 3
		end if
		db IMAGE_SYM_CLASS_STATIC,1
		dd dllname#name#.data3_end-dllname#name#.data3,0,0
		dw 2
		dd IMAGE_COMDAT_SELECT_ASSOCIATIVE
	end if
	dd 0,dllname#name#.strtbl_2-dllname#name#.strtbl,0,0
	dw IMAGE_SYM_CLASS_EXTERNAL

dllname#name#.strtbl dd dllname#name#.strtbl_end-$
dllname#name#.publicname:
if pubname eqtype 0
	db `pubname,0
	dllname#name#.publicname_end:
	dllname#name#.thunkname db `thunk,0
	dllname#name#.thunkname_end:
else
	db '__imp__'
	if thunk eqtype 0
		db `name,0
		dllname#name#.publicname_end:
		dllname#name#.thunkname db `thunk,0
	else
		dllname#name#.thunkname db `name,0
		dllname#name#.publicname_end:
	end if
	dllname#name#.thunkname_end:
end if
dllname#name#.strtbl_2 db '__IMPORT_DESCRIPTOR_' # `dllname,0
dllname#name#.strtbl_end:
	add_2_dir dllname#name#.thunkname,dllname#name#.thunkname_end-dllname#name#.thunkname
	add_2_dir dllname#name#.publicname,dllname#name#.publicname_end-dllname#name#.publicname
}

macro pbimplib dllname*,cconv*,argcount*,name*,pb_name,pubname {
	if argcount>31 | argcount<0
		display '-ERR: argument [argcount] should be in the range [0..31]',13,10
	end if
	if cconv>PBCALL | cconv<STDCALL
		display '-WNG: argument [cconv] is assigned an invalid value. Defaulting to STDCALL.',13,10
	end if
	if ~ defined dllname#.alreadydefined
		get_next_even_addr
		dllname#.descr_name db `dllname # '/'
		x=$-dllname#.descr_name
		if x<16
			times 16-x db ' '
		end if
		print_timestamp
		db '            0       '
		pack_size (dllname#.descr_strtbl_end-dllname#.descr)
	dllname#.descr:
		dw IMAGE_FILE_MACHINE_I386,2
		dd %t
		dd dllname#.descr_sym_tbl-dllname#.descr
		coff_header
		dd dllname#.descr_data1-dllname#.descr
		dd dllname#.descr_rel-dllname#.descr
		dd 0
		if KEEP_ORIGINAL_THUNKS
			dd 3
		else
			dd 2
		end if
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		db '.idata$6'
		dd 0,0,dllname#.descr_data1-dllname#.descr_data2+1,dllname#.descr_data2-dllname#.descr
		dd dllname#.descr_rel2-dllname#.descr,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	dllname#.descr_data2 db `dllname
	dllname#.descr_data1 dd 0,0,0,0
		if KEEP_ORIGINAL_THUNKS
		dllname#.descr_rel:
			dd 0
			if USE_COMPID
				dd 4
			else
				dd 3
			end if
			dw IMAGE_REL_I386_DIR32NB
		else
			dd 0
		dllname#.descr_rel:
		end if
	dllname#.descr_rel2:
		dd 12
		if USE_COMPID
			dd 3
		else
			dd 2
		end if
		dw IMAGE_REL_I386_DIR32NB
		dd 16
		if KEEP_ORIGINAL_THUNKS
			if USE_COMPID
				dd 5
			else
				dd 4
			end if
		else
			if USE_COMPID
				dd 4
			else
				dd 3
			end if
		end if
		dw IMAGE_REL_I386_DIR32NB
	dllname#.descr_sym_tbl:
		add_compid
		dd 0,29,0,1
		db IMAGE_SYM_CLASS_EXTERNAL
		db 0
		db '.idata$2'
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,1
		db IMAGE_SYM_CLASS_SECTION,0
		db '.idata$6'
		dd 0,2
		db IMAGE_SYM_CLASS_STATIC,0
		if KEEP_ORIGINAL_THUNKS
			db '.idata$4'
			dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,0
			db IMAGE_SYM_CLASS_SECTION,0
		end if
		db '.idata$5'
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,0
		db IMAGE_SYM_CLASS_SECTION,0
		dd 0,4,0,0
		db IMAGE_SYM_CLASS_EXTERNAL,0
		dd 0,dllname#.descr_strtbl_1-dllname#.descr_strtbl,0,0
		db IMAGE_SYM_CLASS_EXTERNAL,0
	dllname#.descr_strtbl dd dllname#.descr_strtbl_end-$
		db '__NULL_IMPORT_DESCRIPTOR',0
	dllname#.descr_strtbl2 db '__IMPORT_DESCRIPTOR_' # `dllname,0
		add_2_dir dllname#.descr_strtbl2,$-dllname#.descr_strtbl2
		dllname#.descr_strtbl_1 db 07Fh,`dllname,'_NULL_THUNK_DATA',0
	dllname#.descr_strtbl_end:
		get_next_even_addr
		dllname#.descr_name2 db `dllname # '/'
		x=$-dllname#.descr_name2
		if x<16
			times 16-x db ' '
		end if
		print_timestamp
		db '            0       '
		pack_size (dllname#.descr_strtbl2_end-dllname#.descr2)
	dllname#.descr2:
		dw IMAGE_FILE_MACHINE_I386
		if KEEP_ORIGINAL_THUNKS
			dw 2
		else
			dw 1
		end if
		dd %t
		dd dllname#.descr_sym_tbl2-dllname#.descr2
		if USE_COMPID
			dd 2
		else
			dd 1
		end if
		dw 0,IMAGE_FILE_32BIT_MACHINE
		db '.idata$5'
		dd 0,0,4,dllname#.descr2_data-dllname#.descr2,0,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		if KEEP_ORIGINAL_THUNKS
			db '.idata$4'
			dd 0,0,4,dllname#.descr2_data-dllname#.descr2,0,0,0
			dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		end if
	dllname#.descr_sym_tbl2:
		add_compid
	dllname#.descr2_data:
		dd 0,4,0,1
		db IMAGE_SYM_CLASS_EXTERNAL
		db 0
		dd dllname#.descr_strtbl2_end-$
		dllname#.descr_strtbl3 db 07Fh,`dllname,'_NULL_THUNK_DATA',0
	dllname#.descr_strtbl2_end:
		add_2_dir dllname#.descr_strtbl3,$-dllname#.descr_strtbl3
	end if
	define dllname#.alreadydefined 1
	get_next_even_addr
	last_dll_name=dllname#name#.obj_name
	dllname#name#.obj_name db `dllname # '/'
	x=$ - dllname#name#.obj_name
	if x>16
		display '-ERR: ' # `dllname # ' is a very long DLL name',13,10
	end if
	if x<16
		times 16-x db ' '
	end if
	print_timestamp
	db '            0       '
	pack_size (dllname#name#.strtbl_end-dllname#name#.lnk_memberN)
dllname#name#.lnk_memberN:
	byordinal=0
	ordinal_value=0
	virtual at 0
		db `name
		if $>4
			load prefix dword from 0
			if prefix=2E64726Fh
				byordinal=1
				count=$-4
				repeat count
					load chr byte from %+3
					chr=chr-'0'
					if chr>9
						display '-ERR: Bad ordinal value: ' # `name,13,10
					else
						ordinal_value=ordinal_value*10+chr
					end if
				end repeat
			end if
		end if
	end virtual
	dw IMAGE_FILE_MACHINE_I386
	if KEEP_ORIGINAL_THUNKS
		if byordinal=0
			dw 4
		else
			dw 3
		end if
	else
		if byordinal=0
			dw 3
		else
			dw 2
		end if
	end if
	dd %t
	dd dllname#name#.symbol_table-dllname#name#.lnk_memberN
	if KEEP_ORIGINAL_THUNKS
		if byordinal=0
			if USE_COMPID
				dd 12
			else
				dd 11
			end if
		else
			if USE_COMPID
				dd 10
			else
				dd 9
			end if
		end if
	else
		if byordinal=0
			if USE_COMPID
				dd 10
			else
				dd 9
			end if
		else
			if USE_COMPID
				dd 8
			else
				dd 7
			end if
		end if
	end if
	dw 0,IMAGE_FILE_32BIT_MACHINE
	db '.text',0,0,0
	dd 0,0
	if cconv=CDECL
		if argcount>0
			dd 12+argcount*4
		else
			dd 6
		end if
	else
		if cconv=PBCALL
			if argcount>0
				dd 7
			else
				dd 6
			end if
		else
			dd 6
		end if
	end if
	dd dllname#name#.thunk-dllname#name#.lnk_memberN
	dd dllname#name#.rel1-dllname#name#.lnk_memberN
	dd 0,1
	dd IMAGE_CNT_CODE or IMAGE_LNK_COMDAT or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_EXECUTE
	db '.idata$5'
	dd 0,0,4
	dd dllname#name#.rel2-dllname#name#.lnk_memberN
	if byordinal=0
		dd dllname#name#.rel2-dllname#name#.lnk_memberN,0,1
	else
		dd 0,0,0
	end if
	dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	if KEEP_ORIGINAL_THUNKS
		db '.idata$4'
		dd 0,0,4
		dd dllname#name#.rel2-dllname#name#.lnk_memberN
		if byordinal=0
			dd dllname#name#.rel2-dllname#name#.lnk_memberN,0,1
		else
			dd 0,0,0
		end if
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	end if
	if byordinal=0
		db '.idata$6'
		dd 0,0,dllname#name#.data3_end-dllname#name#.data3
		dd dllname#name#.data3-dllname#name#.lnk_memberN,0,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		dllname#name#.thunk:
			if cconv=CDECL
				if argcount>0
					; push [esp+argcount*4]
					times argcount db 0FFh,74h,24h,argcount*4
					; call [pubname]
					dw 15FFh
					dd 0
					; add esp,argcount*4
					db 83h,0C4h,argcount*4
					; retn argcount*4
					db 0C2h,argcount*4,0
				else
					; jmp [pubname]
					dw 25FFh
				end if
			else
				if cconv=PBCALL
					if argcount>0
						; pop edx
						db 5Ah
						; pop eax
						db 58h
						; push edx
						db 52h
					end if
				end if
				; jmp [pubname]
				dw 25FFh
			end if
		dllname#name#.rel2 dd 0
			if KEEP_ORIGINAL_THUNKS
				if USE_COMPID
					dd 9
				else
					dd 8
				end if
			else
				if USE_COMPID
					dd 7
				else
					dd 6
				end if
			end if
			dw IMAGE_REL_I386_DIR32NB
		dllname#name#.rel1:
			if cconv=CDECL
				dd 2+argcount*4
			else
				if cconv=PBCALL
					if argcount>0
						dd 5
					else
						dd 2
					end if
				else
					dd 2
				end if
			end if
			if USE_COMPID
				dd 6
			else
				dd 5
			end if
			dw IMAGE_REL_I386_DIR32
		dllname#name#.data3 db 0,0,`name,0
			virtual
				align 2
				skip_bytes=$-$$
			end virtual
			if skip_bytes
				db 0
			end if
		dllname#name#.data3_end:
	else
		dllname#name#.thunk:
			if cconv=CDECL
				if argcount>0
					; push [esp+argcount*4]
					times argcount db 0FFh,74h,24h,argcount*4
					; call [pubname]
					dw 15FFh
					dd 0
					; add esp,argcount*4
					db 83h,0C4h,argcount*4
					; retn argcount*4
					db 0C2h,argcount*4,0
				else
					; jmp [pubname]
					dw 25FFh
					dd 0
				end if
			else
				if cconv=PBCALL
					if argcount>0
						; pop edx
						db 5Ah
						; pop eax
						db 58h
						; push edx
						db 52h
					end if
				end if
				; jmp [pubname]
				dw 25FFh
				dd 0
			end if
		dllname#name#.rel1:
			if cconv=CDECL
				dd 2+argcount*4
			else
				if cconv=PBCALL
					if argcount>0
						dd 5
					else
						dd 2
					end if
				else
					dd 2
				end if
			end if
			if USE_COMPID
				dd 6
			else
				dd 5
			end if
			dw IMAGE_REL_I386_DIR32
		dllname#name#.rel2 dd 80000000h+ordinal_value
	end if
dllname#name#.symbol_table:
	add_compid
	db '.text',0,0,0
	dd 0,1
	db IMAGE_SYM_CLASS_STATIC
	db 1
	dd 6,1,0
	dw 0
	dd IMAGE_COMDAT_SELECT_NODUPLICATES
	dd 0,dllname#name#.thunkname-dllname#name#.strtbl,0
	dw 1,IMAGE_SYM_DTYPE_FUNCTION,IMAGE_SYM_CLASS_EXTERNAL
	db '.idata$5'
	dd 0,2
	db IMAGE_SYM_CLASS_STATIC
	db 1
	dd 4,1,0
	dw 0
	dd IMAGE_COMDAT_SELECT_NODUPLICATES
	dd 0,4,0,2
	dw IMAGE_SYM_CLASS_EXTERNAL
	if KEEP_ORIGINAL_THUNKS
		db '.idata$4'
		dd 0,3
		db IMAGE_SYM_CLASS_STATIC,1
		dd 4,1,0
		dw 2
		dd IMAGE_COMDAT_SELECT_ASSOCIATIVE
	end if
	if byordinal=0
		db '.idata$6'
		dd 0
		if KEEP_ORIGINAL_THUNKS
			dd 4
		else
			dd 3
		end if
		db IMAGE_SYM_CLASS_STATIC,1
		dd dllname#name#.data3_end-dllname#name#.data3,0,0
		dw 2
		dd IMAGE_COMDAT_SELECT_ASSOCIATIVE
	end if
	dd 0,dllname#name#.strtbl_2-dllname#name#.strtbl,0,0
	db IMAGE_SYM_CLASS_EXTERNAL,0
dllname#name#.strtbl dd dllname#name#.strtbl_end-$
dllname#name#.publicname:
if pubname eqtype 0
	db `pubname,0
	dllname#name#.thunkname db '_PB_',`pb_name,0
	dllname#name#.thunkname_end:
else
	db '__imp__',`name,0
	dllname#name#.thunkname:
	if pb_name eqtype 0
		db '_PB_',`pb_name,0
	else
		db '_PB_',`name,0
	end if
	dllname#name#.thunkname_end:
end if
dllname#name#.strtbl_2 db '__IMPORT_DESCRIPTOR_' # `dllname,0
dllname#name#.strtbl_end:
	add_2_dir dllname#name#.thunkname,dllname#name#.thunkname_end-dllname#name#.thunkname
	add_2_dir dllname#name#.publicname,dllname#name#.thunkname-dllname#name#.publicname
}

macro vbimplib module*,dllname*,cconv*,argcount*,name*,vb_name,pubname {
	if argcount>31 | argcount<0
		display '-ERR: argument [argcount] should be in the range [0..31]',13,10
	end if
	if cconv>PBCALL | cconv<STDCALL
		display '-WNG: argument [cconv] is assigned an invalid value. Defaulting to STDCALL.',13,10
	end if
	if ~ defined dllname#.alreadydefined
		get_next_even_addr
		dllname#.descr_name db `dllname # '/'
		x=$-dllname#.descr_name
		if x<16
			times 16-x db ' '
		end if
		print_timestamp
		db '            0       '
		pack_size (dllname#.descr_strtbl_end-dllname#.descr)
	dllname#.descr:
		dw IMAGE_FILE_MACHINE_I386
		dw 2
		dd %t
		dd dllname#.descr_sym_tbl-dllname#.descr
		coff_header
		dd dllname#.descr_data1-dllname#.descr
		dd dllname#.descr_rel-dllname#.descr
		dd 0
		if KEEP_ORIGINAL_THUNKS
			dd 3
		else
			dd 2
		end if
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		db '.idata$6'
		dd 0,0,dllname#.descr_data1-dllname#.descr_data2+1,dllname#.descr_data2-dllname#.descr
		dd dllname#.descr_rel2-dllname#.descr,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	dllname#.descr_data2 db `dllname
	dllname#.descr_data1 dd 0,0,0,0
		if KEEP_ORIGINAL_THUNKS
		dllname#.descr_rel:
			dd 0
			if USE_COMPID
				dd 4
			else
				dd 3
			end if
			dw IMAGE_REL_I386_DIR32NB
		else
			dd 0
		dllname#.descr_rel:
		end if
	dllname#.descr_rel2:
		dd 12
		if USE_COMPID
			dd 3
		else
			dd 2
		end if
		dw IMAGE_REL_I386_DIR32NB
		dd 16
		if KEEP_ORIGINAL_THUNKS
			if USE_COMPID
				dd 5
			else
				dd 4
			end if
		else
			if USE_COMPID
				dd 4
			else
				dd 3
			end if
		end if
		dw IMAGE_REL_I386_DIR32NB
	dllname#.descr_sym_tbl:
		add_compid
		dd 0,29,0,1
		db IMAGE_SYM_CLASS_EXTERNAL
		db 0
		db '.idata$2'
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,1
		db IMAGE_SYM_CLASS_SECTION,0
		db '.idata$6'
		dd 0,2
		db IMAGE_SYM_CLASS_STATIC,0
		if KEEP_ORIGINAL_THUNKS
			db '.idata$4'
			dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,0
			db IMAGE_SYM_CLASS_SECTION,0
		end if
		db '.idata$5'
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_MEM_READ or IMAGE_MEM_WRITE,0
		db IMAGE_SYM_CLASS_SECTION,0
		dd 0,4,0,0
		db IMAGE_SYM_CLASS_EXTERNAL,0
		dd 0,dllname#.descr_strtbl_1-dllname#.descr_strtbl,0,0
		db IMAGE_SYM_CLASS_EXTERNAL,0
	dllname#.descr_strtbl dd dllname#.descr_strtbl_end-$
		db '__NULL_IMPORT_DESCRIPTOR',0
	dllname#.descr_strtbl2 db '__IMPORT_DESCRIPTOR_' # `dllname,0
		add_2_dir dllname#.descr_strtbl2,$-dllname#.descr_strtbl2
		dllname#.descr_strtbl_1 db 07Fh,`dllname,'_NULL_THUNK_DATA',0
	dllname#.descr_strtbl_end:
		get_next_even_addr
		dllname#.descr_name2 db `dllname # '/'
		x=$-dllname#.descr_name2
		if x<16
			times 16-x db ' '
		end if
		print_timestamp
		db '            0       '
		pack_size (dllname#.descr_strtbl2_end-dllname#.descr2)
	dllname#.descr2:
		dw IMAGE_FILE_MACHINE_I386
		if KEEP_ORIGINAL_THUNKS
			dw 2
		else
			dw 1
		end if
		dd %t
		dd dllname#.descr_sym_tbl2-dllname#.descr2
		if USE_COMPID
			dd 2
		else
			dd 1
		end if
		dw 0,IMAGE_FILE_32BIT_MACHINE
		db '.idata$5'
		dd 0,0,4,dllname#.descr2_data-dllname#.descr2,0,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		if KEEP_ORIGINAL_THUNKS
			db '.idata$4'
			dd 0,0,4,dllname#.descr2_data-dllname#.descr2,0,0,0
			dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		end if
	dllname#.descr_sym_tbl2:
		add_compid
	dllname#.descr2_data:
		dd 0,4,0,1
		db IMAGE_SYM_CLASS_EXTERNAL
		db 0
		dd dllname#.descr_strtbl2_end-$
		dllname#.descr_strtbl3 db 07Fh,`dllname,'_NULL_THUNK_DATA',0
	dllname#.descr_strtbl2_end:
		add_2_dir dllname#.descr_strtbl3,$-dllname#.descr_strtbl3
	end if
	define dllname#.alreadydefined 1
	get_next_even_addr
	last_dll_name=dllname#name#.obj_name
	dllname#name#.obj_name db `dllname # '/'
	x=$ - dllname#name#.obj_name
	if x>16
		display '-ERR: ' # `dllname # ' is a very long DLL name',13,10
	end if
	if x<16
		times 16-x db ' '
	end if
	print_timestamp
	db '            0       '
	pack_size (dllname#name#.strtbl_end-dllname#name#.lnk_memberN)
dllname#name#.lnk_memberN:
	byordinal=0
	ordinal_value=0
	virtual at 0
		db `name
		if $>4
			load prefix dword from 0
			if prefix=2E64726Fh
				byordinal=1
				count=$-4
				repeat count
					load chr byte from %+3
					chr=chr-'0'
					if chr>9
						display '-ERR: Bad ordinal value: ' # `name,13,10
					else
						ordinal_value=ordinal_value*10+chr
					end if
				end repeat
			end if
		end if
	end virtual
	dw IMAGE_FILE_MACHINE_I386
	if KEEP_ORIGINAL_THUNKS
		if byordinal=0
			dw 4
		else
			dw 3
		end if
	else
		if byordinal=0
			dw 3
		else
			dw 2
		end if
	end if
	dd %t
	dd dllname#name#.symbol_table-dllname#name#.lnk_memberN
	if KEEP_ORIGINAL_THUNKS
		if byordinal=0
			if USE_COMPID
				dd 12
			else
				dd 11
			end if
		else
			if USE_COMPID
				dd 10
			else
				dd 9
			end if
		end if
	else
		if byordinal=0
			if USE_COMPID
				dd 10
			else
				dd 9
			end if
		else
			if USE_COMPID
				dd 8
			else
				dd 7
			end if
		end if
	end if
	dw 0
	dw IMAGE_FILE_32BIT_MACHINE
	db '.text',0,0,0
	dd 0,0
	if cconv=CDECL
		if argcount>0
			dd 12+argcount*4
		else
			dd 6
		end if
	else
		if cconv=PBCALL
			if argcount>0
				dd 7
			else
				dd 6
			end if
		else
			dd 6
		end if
	end if
	dd dllname#name#.thunk-dllname#name#.lnk_memberN
	dd dllname#name#.rel1-dllname#name#.lnk_memberN
	dd 0,1
	dd IMAGE_CNT_CODE or IMAGE_LNK_COMDAT or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_EXECUTE
	db '.idata$5'
	dd 0,0,4
	dd dllname#name#.rel2-dllname#name#.lnk_memberN
	if byordinal=0
		dd dllname#name#.rel2-dllname#name#.lnk_memberN,0,1
	else
		dd 0,0,0
	end if
	dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	if KEEP_ORIGINAL_THUNKS
		db '.idata$4'
		dd 0,0,4
		dd dllname#name#.rel2-dllname#name#.lnk_memberN
		if byordinal=0
			dd dllname#name#.rel2-dllname#name#.lnk_memberN,0,1
		else
			dd 0,0,0
		end if
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
	end if
	if byordinal=0
		db '.idata$6'
		dd 0,0,dllname#name#.data3_end-dllname#name#.data3
		dd dllname#name#.data3-dllname#name#.lnk_memberN,0,0,0
		dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_LNK_COMDAT or IMAGE_ALIGN_2BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
		dllname#name#.thunk:
			if cconv=CDECL
				if argcount>0
					; push [esp+argcount*4]
					times argcount db 0FFh,74h,24h,argcount*4
					; call [pubname]
					dw 15FFh
					dd 0
					; add esp,argcount*4
					db 83h,0C4h,argcount*4
					; retn argcount*4
					db 0C2h,argcount*4,0
				else
					; jmp [pubname]
					dw 25FFh
				end if
			else
				if cconv=PBCALL
					if argcount>0
						; pop edx
						db 5Ah
						; pop eax
						db 58h
						; push edx
						db 52h
					end if
				end if
				; jmp [pubname]
				dw 25FFh
			end if
		dllname#name#.rel2 dd 0
			if KEEP_ORIGINAL_THUNKS
				if USE_COMPID
					dd 9
				else
					dd 8
				end if
			else
				if USE_COMPID
					dd 7
				else
					dd 6
				end if
			end if
			dw IMAGE_REL_I386_DIR32NB
		dllname#name#.rel1:
			if cconv=CDECL
				dd 2+argcount*4
			else
				if cconv=PBCALL
					if argcount>0
						dd 5
					else
						dd 2
					end if
				else
					dd 2
				end if
			end if
			if USE_COMPID
				dd 6
			else
				dd 5
			end if
			dw IMAGE_REL_I386_DIR32
		dllname#name#.data3 db 0,0,`name,0
			virtual
				align 2
				skip_bytes=$-$$
			end virtual
			if skip_bytes
				db 0
			end if
		dllname#name#.data3_end:
	else
		dllname#name#.thunk:
			if cconv=CDECL
				if argcount>0
					; push [esp+argcount*4]
					times argcount db 0FFh,74h,24h,argcount*4
					; call [pubname]
					dw 15FFh
					dd 0
					; add esp,argcount*4
					db 83h,0C4h,argcount*4
					; retn argcount*4
					db 0C2h,argcount*4,0
				else
					; jmp [pubname]
					dw 25FFh
					dd 0
				end if
			else
				if cconv=PBCALL
					if argcount>0
						; pop edx
						db 5Ah
						; pop eax
						db 58h
						; push edx
						db 52h
					end if
				end if
				; jmp [pubname]
				dw 25FFh
				dd 0
			end if
		dllname#name#.rel1:
			if cconv=CDECL
				dd 2+argcount*4
			else
				if cconv=PBCALL
					if argcount>0
						dd 5
					else
						dd 2
					end if
				else
					dd 2
				end if
			end if
			if USE_COMPID
				dd 6
			else
				dd 5
			end if
			dw IMAGE_REL_I386_DIR32
		dllname#name#.rel2 dd 80000000h+ordinal_value
	end if
dllname#name#.symbol_table:
	add_compid
	db '.text',0,0,0
	dd 0,1
	db IMAGE_SYM_CLASS_STATIC
	db 1
	dd 6,1,0
	dw 0
	dd IMAGE_COMDAT_SELECT_NODUPLICATES
	dd 0,dllname#name#.thunkname-dllname#name#.strtbl,0
	dw 1
	dw IMAGE_SYM_DTYPE_FUNCTION,IMAGE_SYM_CLASS_EXTERNAL
	db '.idata$5'
	dd 0,2
	db IMAGE_SYM_CLASS_STATIC
	db 1
	dd 4,1,0
	dw 0
	dd IMAGE_COMDAT_SELECT_NODUPLICATES
	dd 0,4,0,2
	dw IMAGE_SYM_CLASS_EXTERNAL
	if KEEP_ORIGINAL_THUNKS
		db '.idata$4'
		dd 0,3
		db IMAGE_SYM_CLASS_STATIC,1
		dd 4,1,0
		dw 2
		dd IMAGE_COMDAT_SELECT_ASSOCIATIVE
	end if
	if byordinal=0
		db '.idata$6'
		dd 0
		if KEEP_ORIGINAL_THUNKS
			dd 4
		else
			dd 3
		end if
		db IMAGE_SYM_CLASS_STATIC,1
		dd dllname#name#.data3_end-dllname#name#.data3,0,0
		dw 2
		dd IMAGE_COMDAT_SELECT_ASSOCIATIVE
	end if
	dd 0,dllname#name#.strtbl_2-dllname#name#.strtbl,0,0
	db IMAGE_SYM_CLASS_EXTERNAL,0
dllname#name#.strtbl dd dllname#name#.strtbl_end-$
dllname#name#.publicname:
if pubname eqtype 0
	db `pubname,0
else
	db '__imp__',`name,0
end if
dllname#name#.thunkname:
if vb_name eqtype 0
	db '?',`vb_name
else
	db '?',`name
end if
	db '@',`module,'@@AAGXXZ',0
dllname#name#.thunkname_end:
dllname#name#.strtbl_2 db '__IMPORT_DESCRIPTOR_' # `dllname,0
dllname#name#.strtbl_end:
	add_2_dir dllname#name#.thunkname,dllname#name#.thunkname_end-dllname#name#.thunkname
	add_2_dir dllname#name#.publicname,dllname#name#.thunkname-dllname#name#.publicname
}

macro endlib {
	get_next_even_addr
	null_dll_name db 'dllname.dll/    '
	if last_dll_name
		do_memcpy last_dll_name, null_dll_name, 16
	end if
	print_timestamp
	db '            0       '
	if USE_COMPID
		db '145'
	else
		db '127'
	end if
	db '       ',60h,0Ah
null_import_descr:
	dw IMAGE_FILE_MACHINE_I386
	dw 1
	dd %t,null_import_descr_sym_tbl-null_import_descr
	if USE_COMPID
		dd 2
	else
		dd 1
	end if
	dw 0
	dw IMAGE_FILE_32BIT_MACHINE
	db '.idata$3'
	dd 0,0,20,null_import_descr_data-null_import_descr,0,0,0
	dd IMAGE_CNT_INITIALIZED_DATA or IMAGE_ALIGN_4BYTES or IMAGE_MEM_READ or IMAGE_MEM_WRITE
null_import_descr_data dd 0,0,0,0,0
null_import_descr_sym_tbl:
	add_compid
	dd 0,4,0,1
	db IMAGE_SYM_CLASS_EXTERNAL,0
	dd 1Dh
null_import_descr_sym:
	db '__NULL_IMPORT_DESCRIPTOR',0
	add_2_dir null_import_descr_sym, $-null_import_descr_sym
	num_symbols=num_symbols_indexed
	symarr_size=str_tbl_size
}
