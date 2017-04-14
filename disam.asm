;Kiekviena karta nuskaicius baita arba worda patikrinama ar ivyko klaida. Jei klaida ivyko panaudojamas sitas macro
;MAIN_LOOP cikle po kiekvieno nuskaitymo yra patikrinama ar [feof] = 1. Jei taip iseinama is ciklo

%define end_program mov byte[feof], 1

;Pastaba: %n, kur n - macro argumento skaicius

;Paaiskinimas: Nuskaitomas string failo pavadinimu iveimui. Skaitoma po viena baita tol kol randamas baitas 0x0d(newline)
;%1 = string bufferis i kuri irasyti stringa
%macro read_str_stdin 1   
	
	mov cx, 0
	mov bx, %1
	%%read_next_char:
	mov ah, 0x01
	int 0x21
	cmp al, 0x0d
	je %%finished_read
	mov byte [bx], al
	inc bx
	jmp %%read_next_char
	%%finished_read:
	mov byte[bx], 0
	
%endmacro

;Pastaba: [_byte] yra is anksto irasytas baitas, nuskaitytas is failo kuri reikia dissasemblinti
;Paaiskinimas: Jei [_byte] = %1 - sokti i label %2
;%1 = baitas kuri patikrinti
;%2 = label pavadinimas

%macro if_equal_jump 2  

	cmp byte [_byte], %1
	je %2
	
%endmacro

;Paaiskinimas: Atidaromas failas skaitymui/rasymui. Jei %1 = 0 - skaitymui, jei %1 = 1 - rasymui
;%1 = 0 arba 1
;%2 string bufferis kuriame yra failo pavadinimas

%macro open_file 2 
	mov ah, 0x3d
	mov al, %1
	mov dx, %2
	int 0x21
	jnc %%no_error
	mov byte[feof], 1
	append_unique_string str_buff, "Unable to open file", 0
	print_string str_buff
	mov ax, 0x4c00
	int 0x21
	%%no_error:
	
	
%endmacro

;Pastaba: sitame macro yra naudojami [pos], [count] ir [bits_gotten] kurie yra proceduros get_bits argumentai. Kodel sitaip nusprendziau implementuoti nepamenu
;Paaiskinimas: i [pos] irasomas pirmas argumentas, [count] - antras. iskvieciama procedura get_bits, kuri is [_byte] baito pradedant nuo [pos] pozicijos isrenka [count] kieki bitu ir iraso i trecia argumenta, kuris butinai turi buti registeris.
;Pavizdys: jei nuskaitome baita [_byte] = 00011100b ir nuo penktos pozicijos (skaiciuojant nuo kaires pradedant vienetu) norime isgauti tris bitus - get_bits_macro 5, 3, al. Tada al = 00000111b
;%1 = pozicija nuo kurios pradeti 
;%2 = kiekis bitu kuriuos istraukti
;%3 = registeris, kuriame irasomas rezultatas

%macro get_bits_macro 3 ;

	
	mov byte[pos], %1
	mov byte[count], %2
	call get_bits
	mov %3, byte [bits_gotten]
	
	
%endmacro

;Paaiskinimas: String bufferis irasomas i faila. [file_handle_w] yra anksciau irasytas failo handle, naudojant macro open_file.
;%1 = string bufferis kuri irasyti i faila
%macro write_line_to_file 1 

	append_string %1, newline
	mov bx, %1
	call str_len ;str_len funkcija priima argumenta is registerio BX ir string ilgi palieka registeryje CX
	mov ah, 0x40
	mov bx, [file_handle_w]
	mov dx, %1
	int 0x21
	

%endmacro

;Pastaba: str_buff yra pagrindinis string bufferis, kuris MAIN_LOOP ciklo gale yra irasomas i faila. instrukcijos dydzio bitas yra is anksto nuskaitytas [sz], pries naudojant macro.
;Paaiskinimas: skaiciuojant E(paaiskinima ziureti .bss segmente) baita nustatoma [_ptr] reiksme. Jei E baitas yra MEMORY tada [_ptr] = 1, priesingu atveju [_ptr] = 0
;Tam tikrose instrukcijose kuriu operndai susideda is MEMORY ir IMMEDIATE, reikia parasyti instrukcijos dydi. sis macro prie str_buff, kuriame anksciau jau irasytas instrukcijos pavadinimas(pavyzdziui MOV)
;pridedama "BYTE PTR"/"WORD PTR priklausomai nuo instrukcijos dydzio bito [sz].
%macro append_ptr 0 
	
	cmp byte[_ptr], 1
	je %%append
	jmp %%no_append
	%%append:
	cmp byte[sz], 0
	je %%byte_ptr
	jmp %%word_ptr
	%%byte_ptr:
	append_unique_string str_buff, "BYTE PTR ", 0 ;str_buff - pagrindinis string bufferis
	jmp %%no_append
	%%word_ptr:
	append_unique_string str_buff, "WORD PTR ", 0
	%%no_append:

%endmacro

;Paaiskinimas: Nuskaitomas baitas is failo ir irasomas i [_byte]
%macro read_byte 0 

	and word[_byte], 0
	mov ah, 0x3f
	mov bx, [file_handle_r]
	mov cx, 1
	mov dx, _byte
	int 0x21
	cmp ax, 0
	je %%end
	jmp %%no_end
	%%end:
	end_program
	%%no_end:
	add word [bytes_read], 1
	
%endmacro 

;Paaiskinimas: Nuskaitomas wordas is failo ir irasomas i [_word]
%macro read_word 0  ; Tas pats kaip read_byte, tačiau nuskaitomas wordas
	
	mov ah, 0x3f
	mov bx, [file_handle_r]
	mov cx, 2
	mov dx, _word
	int 0x21
	cmp ax, 0
	je %%end
	jmp %%no_end
	%%end:
	end_program
	%%no_end:
	mov ax, [_word]
	mov word [_word], ax
	add word[bytes_read], 2
	
%endmacro 

;Paaiskinimas: isspausdinamas(ne i faila) baitas
%macro print_byte 1  
	
	mov ah, 02
	mov dl, %1
	int 0x21
	
%endmacro
;Paaiskinimas: isspausdinamas(ne i faila) wordas
%macro print_word 1  
	mov bx, %1
	print_byte bh
	print_byte bl
	
%endmacro

;Paaiskinimas: is pirmo argumento padalinams antras argumentas. Liekaba irasoma i registeri AL
;%1 = skaicius kuri padalinti
;%2 = daliklis
%macro modulus 2

	mov ax, %1
	mov bl, %2
	div bl ;ah remainder
	xchg ah, al
	and ah, 0 ;al remainder
	
%endmacro

;Paaiskinimas: prie pirmo argumento prijungiamas antras argumentas. ekvivalentiska funkcija C kalboje butu strcat(unsigned *char, unsigned *char)
;%1, %2 = string bufferiai
%macro append_string 2
	
	mov ax, %1
	push ax
	mov ax, %2
	push ax
	call append_str_f
	pop ax
	pop ax
	
	
%endmacro

;Paaiskinimas: inicializuojamas string bufferis. C ekvivalentas butu some_string[0] = 0;
%macro init_str 1  

	mov byte[%1], 0
	
%endmacro

;Pastaba: sita macro radau internete ir nezinau kaip jis veikia
;Paaiskinimas: is zemesnio nibble esancio registeryje AL, paverciama i ASCII representacija
;Pavizdys: Jei AL = 0x0f, po sito macro AL = 0x46. Jei AL irasysime i faila, tai irasytas baitas atrodys sitaip - "F"
%macro nibble_to_hex 0 

	cmp     al, 10           
    sbb     al, 0x69    
    das
	
%endmacro

;Paaiskinimas: Naudojant nibble_to_hex, argumentas %1 paverciamas i ASCII reprezentacija. Paversti du baitai yra irasomi i string bufferi byte_hex_str
;%1 = baitas kuri paversti
%macro byte_to_hex 1 ;

	mov bl, %1
	mov al, bl
	mov cl, 4
	shl al, cl
	shr al, cl
	nibble_to_hex
	mov ah, al
	mov al, bl
	shr al, cl
	nibble_to_hex
	mov bx, byte_hex_str
	mov word [bx], ax
	add bx, 2
	mov word [bx], 0
	
%endmacro

;Paaiskinimas: Naudojant byte_to_hex, argumentas %1 paverciamas i ASCII reprezentacija. Paversti keturi baitai yra irasomi i string bufferi word_hex_str
;%1 = wordas krui paversti
%macro word_to_hex 1 
	mov cx, %1
	push cx
	byte_to_hex ch
	append_string word_hex_str, byte_hex_str
	pop cx
	byte_to_hex cl
	append_string word_hex_str, byte_hex_str
	
%endmacro


;Paaiskinimas: Sudetis su zenklu. Jei antro argumento auksciausias bitas yra 1, tada signed_add(%1, %2) = %1 - %2. Priesingu atveju signed_add(%1, %2) = %1 + %2.
;Rezultatas paliekamas AX registeryje. Naudojama suoliu(jmp,je,...) offsetam skaiciuoti
;Pavizdys: signed_add 0x05, 0x03 = 0x08, signed_add 0x05, 0xff = 0x04
;%1 = pirmas skaicius
;%2 = antras skaicius
%macro signed_add 2 
	mov ax, ax
	mov ax, %1
	mov bx, %2
	cmp byte[sz], 0
	je %%size0
	jmp %%size1
	%%size0:
	mov cl, 7
	shr bx, cl
	cmp bl, 0
	je %%positive
	jmp %%negative_0
	%%size1:
	mov cl, 15
	shr bx, cl
	cmp bl, 0
	je %%positive
	jmp %%negative_1
	%%positive:
	add ax, %2
	jmp %%end
	%%negative_1:
	mov bx, %2
	not bx
	inc bx
	sub ax, bx
	jmp %%end
	%%negative_0:
	mov bl, %2
	not bl
	inc bl
	sub ax, bx
	%%end:
	
%endmacro

;Pastaba: Jei kyla klausimas kodel kai kur vietoj proceduros naudojamos macros arba atvirksciai, tai todel, nes rasymo momentu atrode kad greiciau iseis parasyti.
;Programos iskaitomumas del to smarkiai nukencia, taciau techniskai viskas funkcionuoja.
;Paaiskinimas: Prie pirmo argumento prijungiamas registerio stringas("AL",..."BH","AX",..."DI") atsizvelgiant i dydzio bita [sz], naudojant macro fetch_op
;%1 = string bufferis
;%2 = n-tasis masyvo elementas pradedant nuo 0 (masyvo implementacija skaityti prie fetch_op macro)
%macro calculate_G 2 
	mov ax, %2
	cmp byte[sz], 1
	je %%16bit
	jmp %%8bit
	%%16bit: 
	add ax, 8
	%%8bit:
	fetch_op registers, ax
	append_string %1, bx 
	
%endmacro

;Paaiskinimas: Jei krypties bitas [direction] = 1 - prie antro argumento prijungiamas kablelis ir antras argumentas. Priesingu atveju prie pirmo argumento prijungiamas antras.
;Is esmes sitas macro nusprendzia instrukcijos krypti. pavyzdziui ADD AX, BX vietoj ADD BX, AX
;%1,%2 = string bufferiai
%macro decide_direction 2 
	cmp byte [direction], 1
	je %%change
	jmp %%no_change
	%%change:
	append_string str_buff, %2
	append_string str_buff, comma
	append_string str_buff, %1
	jmp %%end
	%%no_change:
	append_string str_buff, %1
	append_string str_buff, comma
	append_string str_buff, %2
	%%end:
	
%endmacro

;Pastaba: Sitokio tipo macro randamas tik NASM assembleryje (man atrodo). 2+ reiskia kad argumentu skaicius gali buti 2 arba daugiau
;Paaiskinimas: Prie pirmo argumento prijungiami visi argumentai einantis po jo.
;Pavizdys: append_unique_string str_buff, "foo", "bar", "whatever", 0. Tada str_buff = str_buff + "foobarwhatever", 0
;%1 = string bufferis prie kurio prijungti
;%2+ = 1 arba daugiau string reiksmes
%macro  append_unique_string 2+ 

        jmp     %%endstr
  %%str:        db      %2
  %%endstr:
        append_string %1, %%str

%endmacro

;Paaiskinimas: Jei [_byte] = %1, i [sz] irasyti %3 ir sokti i %2. Yra tam tikru instrukciju, kuriu dydzio bitu negalima pasikliauti. Pavyzdziui 0xE8
;%1 = skaicius su kuriuo palyginti
;%2 = label i kuri sokti
;%3 = 0 arba 1
%macro  if_equal_adjust_sz 3  

	cmp byte [_byte], %1
	je %%do_jmp
	jmp %%no_jump
	%%do_jmp:
	mov byte[sz], %3
	jmp %2
	%%no_jump:
	
%endmacro

;Paaiskinimas: isspausdinamas string(ne i faila)
;%1 = string kuri isspausdinti
%macro print_string 1 

	mov bx, %1
	call print_str_f
	
%endmacro

;Paaiskinimas: Jei [_byte] yra tarp %1 ir %2, sokti i %3. Priesingu atveju sokti i %4. Sakant tarp, turima omenyje %1 <= [_byte] <= %2
;%1,%2 = skaiciai
;%3,%4 = labels
%macro if_between_jump_else 4 
	cmp byte [_byte], %1
	jbe %%maybe
	jmp %%no
	%%maybe:
	cmp byte [_byte], %2
	jae %3
	%%no:
	jmp %4
	

%endmacro

;Paaiskinimas: Jei [_byte] yra tarp %1 ir %2, sokti i %3.
;%1,%2 = skaiciai
;%3 = label
%macro if_between_jump 3

	cmp byte [_byte], %1
	jbe %%maybe
	jmp %%no
	%%maybe:
	cmp byte [_byte], %2
	jae %3
	%%no:
	
%endmacro

;Paaiskinimas: Jei [_byte] = %1, sokti i %2. Priesingu atvieju sokti i %3.
;%1 = skaicius
;%2,%3 = labels
%macro if_equal_jump_else 3 

	cmp byte [_byte], %1
	je %2
	jmp %3
	
%endmacro

;Pastaba: tam tikrose MOV instrukcijose registeris isrenkamas ne is paprastu registeriu (AX,BX,...), o is segmento registeriu ES, CS, SS, DS 
;Paaiskinimas: Prie string bufferio S prijungiamas segmento registeris, atsizvelgiant i [reg] bitus
%macro calculate_S 0  
	cmp byte[reg], 0
	je %%_ES
	cmp byte[reg], 1
	je %%_CS
	cmp byte[reg], 2
	je %%_SS
	cmp byte[reg], 3
	je %%_DS
	%%_ES: append_string S, S_ES
	jmp %%end
	%%_CS: append_string S,  S_CS
	jmp %%end
	%%_SS: append_string S, S_SS
	jmp %%end
	%%_DS: append_string S, S_DS	
	%%end:
%endmacro

;Pastaba: Daznai pasitaiko, kad reikia gauti AL ir AX registerius
;Paaiskinimas: Prie G string bufferio prijungiamas "AL" arba "AX", atsizvelgiant i [sz] bita
%macro get_al_ax 0 

	cmp byte [sz], 0
	je %%al
	jmp %%ax
	%%al:
	append_string	G, reg_AL
	jmp %%end
	%%ax:
	append_string G, reg_AX
	%%end:
	
%endmacro

;Pastaba: Masyvai yra implementuoti sitaip - pirmas_elementas, 0, antras_elementas, 0,... t.y. 0 - terminuojantis charakteris. Pavyzdziui some_array db "string1", 0, "string2", 0
;Paaiskinimas: I BX registei yra irasoma masyvo pradzia atmintyje, I CX - kiek nuliu reikia praleisti. Rezultatas paliekamas BX registeryje
;Pavizdys: Jei turime array group1_array db "ADD", 0, "OR", 0, "ADC", 0, "SBB", 0, "AND", 0,"SUB", 0, "XOR", 0 , "CMP", 0 ir norime isgauti trecia("SBB", pradedant nuo nulio) masyvo nari
;fetch_op group1_array, 3.Tada BX = group1_array + offsetas kur prasideda "SBB"
;%1 = masyvas
;%2 = masyvo elementas pradedant nuo nulio
%macro fetch_op 2 

	mov cx, %2	
	mov bx, %1	
	mov dx, 0	
	cmp cx, 0
	je %%fetch_end
	jmp %%compare
	%%not_null_charachter:
	inc bx 
	inc dx 
	%%compare:
	cmp byte [bx], 0
	jne %%not_null_charachter
	cmp cx, 0
	jne %%decrement
	jmp %%fetch_end
	%%decrement:
	dec cx
	mov dx, -1
	jmp %%not_null_charachter
	%%fetch_end:
	sub bx, dx

	
	
%endmacro

;MAIN_LOOP ciklo pradzioje inicializuojami kintamieji
%macro init 0 

	init_str str_buff 	
	init_str I
	init_str E
	init_str S
	init_str byte_hex_str
	init_str word_hex_str
	init_str G
	init_str segment_offset_to_use
	init_str A
	init_str J
	and word [_word], 0
	and word [_byte], 0
	and word [direction], 0
	and word [mode], 0
	and word [reg], 0
	and word [r_rm], 0
	and byte [segment_use_flag], 0
	and word [O], 0
	and word[sz], 0
	and word[pos], 0
	and byte [bits_gotten], 0
	mov byte[_ptr], 1

%endmacro

;NASM assemblerio direktyvai
cpu 8086
org 100h 

segment .data


	D_BX db "BX", 0
	D_SI db "SI", 0
	D_DI db "DI", 0
	D_DP db "SI", 0
	registers db "AL",0,"CL",0,"DL",0,"BL",0,"AH",0,"CH",0,"DH",0,"BH",0,"AX",0,"CX",0,"DX",0,"BX",0,"SP",0,"BP",0,"SI",0,"DI", 0
	segment_displace db "BX + SI",0,"BX + DI",0,"BP + SI",0,"BP + DI",0,"SI",0,"DI",0,"BP",0,"BX", 0
	group1_array db "ADD", 0, "OR", 0, "ADC", 0, "SBB", 0, "AND", 0,"SUB", 0, "XOR", 0 , "CMP", 0 
	group2_array db "ROL", 0, "ROR", 0, "RCL", 0, "RCR", 0, "SHL", 0, "SHR", 0, 0, "SAR", 0
	group3_array db "TEST", 0, 0, "NOT", 0, "NEG",0, "MUL", 0, "IMUL", 0, "DIV", 0, "IDIV", 0
	group4_array db "INC", 0, "DEC", 0,0,0,0,0,0,0
	group5_array db "INC", 0, "DEC", 0, "CALL", 0, "CALL", 0, "JMP", 0, "JMP", 0, "PUSH", 0
	jump_array db "JO",0,"JNO",0,"JB",0,"JNB",0,"JZ",0,"JNZ",0,"JBE",0,"JA",0,"JA",0,"JS",0,"JNS",0,"JPE",0,"JPO",0,"JL",0,"JGE",0,"JLE",0,"JG",0
	input_ db "Enter input filename: ", 0
	output_ db "Enter output filename: ", 0 
	S_ES db "ES", 0
	S_CS db "CS", 0
	S_SS db "SS", 0
	S_DS db "DS", 0
	colon db ":",0
	bracket_left db "[", 0
	bracket_right db "]", 0
	newline db 13, 10, 0
	plus db " + ", 0
	comma db ", ", 0
	space db " ", 0
	reg_AX db "AX", 0
	reg_AL db "AL", 0

;Sis segmentas skiriasi nuo .data tuom, kad duomenys sitam segmente nera inicializuoti
segment .bss 
	file_name_buff resb  50
	str_buff resb 50
	mode resb 2
	reg resb 2
	r_rm resb 2
	file_handle_r resb 4
	file_handle_w resb 4
	sz resb 2
	direction resb 2
	count resb 2
	pos resb 2
	_byte resb 2
	_word resb 2
	O resb 2
	bits_gotten resb 2
	segment_offset_to_use resb 4
	segment_use_flag resb 1
	
	;String buferiai. Teoriskai butu galima apsieiti su dviejais, taciau patogiau naudoti kelis
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	E resb 50 ;A ModR/M byte follows the opcode and specifies the operand. 
	;The operand is either a general-purpose register or a memory address. 
	;If it is a memory address, the address is computed from a segment register and any of the following values: a base register, an index register, a displacement.
	G resb 50 ;The reg field of the ModR/M byte selects a general register.
	I resb 50 ;Immediate data. The operand value is encoded in subsequent bytes of the instruction.
	J resb 50 ;The instruction contains a relative offset to be added to the address of the subsequent instruction. Applicable, e.g., to short JMP (opcode EB), or LOOP.
	A resb 50 ;Direct address. The instruction has no ModR/M byte; the address of the operand is encoded in the instruction. Applicable, e.g., to far JMP (opcode EA).
	S resb 50 ;The reg field of the ModR/M byte selects a segment register.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	byte_hex_str resb 20
	word_hex_str resb 20
	bytes_read resb 2
	feof resb 1
	_ptr resb 1
	
segment .text

_main:
	
	push bp
	mov bp, sp 
	print_string input_
	read_str_stdin file_name_buff
	open_file 0, file_name_buff
	mov [file_handle_r], ax
	print_string output_
	read_str_stdin file_name_buff
	open_file 1, file_name_buff
	mov [file_handle_w], ax
	init
	mov byte [feof], 0
	mov word [bytes_read], 0x0100
	
	;Pagrindinis ciklas
	MAIN_LOOP:
	 init
	 read_byte
	 cmp byte[feof], 1
	 je end__
	 mov al, 0
	 call check_prefix
	 cmp al, 1
	 je read_new_byte
	 jmp no_new_byte
	 read_new_byte:
	 read_byte
	 cmp byte[feof], 1
	 je end__
	 no_new_byte:
	 call set_direction_size
	 call check_single
	 cmp al, 1
	 je new_opcode
	 call check_regular
	 cmp al, 1
	 je new_opcode
	 call check_jump
	 cmp al, 1
	 je new_opcode
	 call check_others
	 cmp al, 1
	 je new_opcode
	 call check_group
	 cmp al, 1
	 je new_opcode
	 append_unique_string str_buff, "DB ",0 ; Jei nerandama instrukcija, darom prielaida kad tai paprastas baitas 
	 byte_to_hex [_byte]
	 append_string str_buff, byte_hex_str
	 new_opcode:
	 cmp byte[feof], 1
	 je end__
	 write_line_to_file str_buff
	jmp MAIN_LOOP
	end__:
	pop bp
	ret


;Pastaba: instrukcijas bandziau kategorizuoti pagal ju panasuma pavyzdziui check_jump patirkina visas instrukcijas su suoliais (jmp,je,jnz...)
;Kiekviena tikrinanti funkcija, atradusi instrukcija registeryje AL iraso 1, priesingu atveju 0. MAIN_LOOP cikle po kiekvienos iskviestos funkcijos yra patikrinama ar instrukcija atrasta.
;Jei instrukcija atrasta, ji irasoma i faila ir ciklas pradedamas is naujo
	
;Patikrinamos instrukcijos kurios neturi papildomu baitu. Pavyzdziui "RET"
check_single: 
	push bp
	mov bp, sp
	if_equal_jump 0x06, push_es
	if_equal_jump 0x07, pop_es
	if_equal_jump 0x0e, push_cs
	if_equal_jump 0x16, push_ss
	if_equal_jump 0x17, pop_ss
	if_equal_jump 0x1e, push_ds
	if_equal_jump 0x1f, pop_ds
	if_equal_jump 0xd7, _xlat
	if_equal_jump 0x27, _daa
	if_equal_jump 0x2f, _das
	if_equal_jump 0x37, _aaa
	if_equal_jump 0x3f, _aas
	if_equal_jump 0x90, _nop
	if_equal_jump 0x98, _cbw
	if_equal_jump 0x20, _int3
	if_equal_jump 0x99, _cwd
	if_equal_jump 0xa4, _movsb
	if_equal_jump 0xa5, _movsw
	if_equal_jump 0xa6, _cmpsb
	if_equal_jump 0xa7, _cmpsw
	if_equal_jump 0xaa, _stosb
	if_equal_jump 0xab, _stosw
	if_equal_jump 0xac, _lodsb
	if_equal_jump 0xad, _lodsw
	if_equal_jump 0xae, _scasb
	if_equal_jump 0xaf, _scasw
	if_equal_jump 0xc3, _ret
	if_equal_jump 0xcb, _retf
	if_equal_jump 0xce, _into
	if_equal_jump 0xcf, _iret
	if_equal_jump 0xd4, _aam
	if_equal_jump 0xd5, _aad
	if_equal_jump 0xf4, _hlt
	if_equal_jump 0xf5, _cmc
	if_equal_jump 0xf9, _clc
	if_equal_jump 0xfa, _cli
	if_equal_jump 0xfb, _sti
	if_equal_jump 0xfc, _cld
	if_equal_jump 0xfd, _std	
	if_equal_jump 0x9c, _pushf
	if_equal_jump 0x9b, _wait
	if_equal_jump 0x9d, _popf
	if_equal_jump 0x9e, _sahf
	if_equal_jump 0x9f, _lahf
	
	jmp maybe_other_single
	push_es: append_unique_string str_buff, "PUSH ES", 0
	jmp found_single
	pop_es:  append_unique_string str_buff, "POP ES", 0
	jmp found_single
	push_cs: append_unique_string str_buff, "PUSH CS", 0
	jmp found_single
	push_ss: append_unique_string str_buff, "PUSH SS", 0
	jmp found_single
	pop_ss: append_unique_string str_buff, "POP SS", 0
	jmp found_single
	push_ds: append_unique_string str_buff, "PUSH DS", 0
	jmp found_single
	pop_ds: append_unique_string str_buff, "POP DS", 0
	jmp found_single
	_xlat: append_unique_string str_buff, "XLAT", 0
	jmp found_single
	_daa: append_unique_string str_buff, "DAA", 0
	jmp found_single
	_das: append_unique_string str_buff, "DAS", 0
	jmp found_single
	_aaa: append_unique_string str_buff, "AAA", 0
	jmp found_single
	_aas: append_unique_string str_buff, "ASS", 0
	jmp found_single
	_nop: append_unique_string str_buff, "NOP", 0
	jmp found_single
	_cbw: append_unique_string str_buff, "CBW", 0
	jmp found_single
	_int3: append_unique_string str_buff, "INT 3", 0
	jmp found_single
	_cwd: append_unique_string str_buff, "CWD", 0
	jmp found_single
	_movsb: append_unique_string str_buff, "MOVSB", 0
	jmp found_single
	_movsw: append_unique_string str_buff, "MOVSW", 0
	jmp found_single
	_cmpsb: append_unique_string str_buff, "CMPSB", 0
	jmp found_single
	_cmpsw: append_unique_string str_buff, "CMPSW", 0
	jmp found_single
	_stosb: append_unique_string str_buff, "STOSB", 0
	jmp found_single
	_stosw: append_unique_string str_buff, "STOSW", 0
	jmp found_single
	_lodsb: append_unique_string str_buff, "LODSB", 0
	jmp found_single
	_lodsw: append_unique_string str_buff, "LODSW", 0
	jmp found_single
	_scasb: append_unique_string str_buff, "SCASB", 0
	jmp found_single
	_scasw: append_unique_string str_buff, "SCASW", 0
	jmp found_single
	_ret: append_unique_string str_buff, "RET", 0
	jmp found_single
	_retf: append_unique_string str_buff, "RETF", 0
	jmp found_single
	_iret: append_unique_string str_buff, "IRET", 0
	jmp found_single
	_into: append_unique_string str_buff, "INTO", 0
	jmp found_single
	_aam: append_unique_string str_buff, "AAM", 0
	jmp found_single
	_aad: append_unique_string str_buff, "AAD", 0
	jmp found_single
	_hlt: append_unique_string str_buff, "HLT", 0
	jmp found_single
	_cmc: append_unique_string str_buff, "CMC", 0
	jmp found_single
	_clc: append_unique_string str_buff, "CLC", 0
	jmp found_single
	_stc: append_unique_string str_buff, "STC", 0
	jmp found_single
	_cli: append_unique_string str_buff, "CLI", 0
	jmp found_single
	_sti: append_unique_string str_buff, "STI", 0
	jmp found_single
	_cld: append_unique_string str_buff, "CLD", 0
	jmp found_single
	_std: append_unique_string str_buff, "STD", 0
	jmp found_single
	_pushf: append_unique_string str_buff, "PUSHF", 0
	jmp found_single
	_wait: append_unique_string str_buff, "WAIT", 0
	jmp found_single
	_popf: append_unique_string str_buff, "POPF", 0
	jmp found_single
	_sahf: append_unique_string str_buff, "SAHF", 0
	jmp found_single
	_lahf: append_unique_string str_buff, "LAHF", 0
	jmp found_single
	
	maybe_other_single:
	
	cmp byte[_byte], 0x40
	jb no_single
	jmp _inc_dec_reg?
	
	_inc_dec_reg?:
	cmp byte[_byte], 0x4f
	jbe _inc_dec_reg
	jmp maybe_other
	
	_inc_dec_reg:
	mov al, [_byte]
	sub al, 0x40
	cmp al, 7
	jle INC_str
	jmp DEC_str
	
	INC_str:
	append_unique_string str_buff, "INC ", 0
	jmp next_thing
	
	DEC_str:
	append_unique_string str_buff, "DEC ", 0
	
	next_thing:

	modulus [_byte], 8
	mov byte[sz], 1
	calculate_G G, ax
	append_string str_buff, G
	jmp found_single
	
	maybe_other:
	cmp byte[_byte], 0x5f
	jbe push_pop_reg
	jmp maybe_something_else
	
	
	push_pop_reg:
	mov al, [_byte]
	sub al, 0x50
	cmp al, 7
	jle PUSH_str
	jmp POP_str
	
	PUSH_str:
	append_unique_string str_buff, "PUSH ", 0
	jmp even_nexter_thing
	
	POP_str:
	append_unique_string str_buff, "POP ", 0
	
	even_nexter_thing:
	modulus [_byte], 8
	mov byte[sz], 1
	calculate_G G, ax
	append_string str_buff, G
	jmp found_single
	
	maybe_something_else:
	if_between_jump_else 0x97, 0x91, _excg, no_single
	
	_excg:
	append_unique_string str_buff, "XCHG ", 0
	modulus [_byte], 8
	mov byte[sz], 1
	calculate_G G, ax
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, reg_AX
	jmp found_single
	
	
	
	found_single:
	mov al, 1
	jmp end_single
	no_single:
	mov al, 0
	end_single:
	pop bp
	ret
	

;Patikrinamos instrukcijos kurias sunku arba neimanoma kategorizuoti
check_others: 
	
	push bp
	mov bp, sp
	if_between_jump 0x85, 0x84, _test
	if_between_jump_else 0x87, 0x86, _xchg, _mov88_8b?
	_test:
	append_unique_string str_buff, "TEST ", 0
	jmp _test_xchg
	_xchg:
	append_unique_string str_buff, "XCHG ", 0
	_test_xchg:
	call calculate_E
	calculate_G G, [reg]
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, E
	jmp found_other
	
	_mov88_8b?:
	if_between_jump_else 0x8b, 0x88, _mov88_8b, _mov_8c?
	_mov88_8b:
	append_unique_string str_buff, "MOV ", 0
	call calculate_E
	calculate_G G, [reg]
	decide_direction E, G
	jmp found_other
	
	_mov_8c?:
	if_equal_jump 0x8c, _mov_8c_8e
	if_equal_jump_else 0x8e, _mov_8c_8e, _lea_8d?
	_mov_8c_8e:
	mov byte[sz], 1
	append_unique_string str_buff, "MOV ", 0
	call calculate_E
	calculate_S
	decide_direction E, S
	jmp found_other
	_lea_8d?:
	if_equal_jump_else 0x8d, _lea_8d, _pop_8f?
	_lea_8d:
	append_unique_string str_buff, "LEA ", 0
	call calculate_E
	calculate_G G, [reg]
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, E
	jmp found_other
	_pop_8f?:
	if_equal_jump_else 0x8f, _pop_8f, _mov_a3_a0?
	_pop_8f:
	append_unique_string str_buff, "POP ", 0
	call calculate_E
	append_string str_buff, E
	jmp found_other
	_mov_a3_a0?:
	if_between_jump_else 0xa3, 0xa0, _mov_a3_a0, _test_a9_a8?
	_mov_a3_a0:
	append_unique_string str_buff, "MOV ", 0
	get_al_ax
	mov byte [O], 1
	call calculate_I
	decide_direction G, I
	jmp found_other
	_test_a9_a8?:
	if_between_jump_else 0xa9, 0xa8, _test_a9_a8, _mov_bf_b0?
	_test_a9_a8:
	get_al_ax
	call calculate_I
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, I
	jmp found_other
	_mov_bf_b0?:
	if_between_jump_else 0xbf, 0xb0, _mov_bf_b0, _ret_c2?
	_mov_bf_b0:
	append_unique_string str_buff, "MOV ", 0
	cmp byte [_byte], 0xb7
	jbe _size0
	jmp _size1
	_size0:
	mov byte[sz], 0
	jmp _next
	_size1:
	mov byte[sz], 1
	_next:
	modulus [_byte], 8
	calculate_G str_buff, ax
	call calculate_I
	append_string str_buff, comma
	append_string str_buff, I
	jmp found_other
	_ret_c2?:
	if_equal_jump_else 0xc2, _ret_c2, _les_c4?
	_ret_c2:
	append_unique_string str_buff, "RET ", 0
	mov byte[sz], 1
	call calculate_I
	append_string str_buff, I
	jmp found_other
	_les_c4?:
	if_equal_jump_else 0xc4, _les_c4, _lds_c5?
	_les_c4:
	append_unique_string str_buff, "LES ", 0
	mov byte[sz], 1
	call calculate_E
	calculate_G G, [reg]
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, E
	jmp found_other
	_lds_c5?:
	if_equal_jump_else 0xc5, _lds_c5, _mov_c7_c6?
	_lds_c5:
	append_unique_string str_buff, "LDS ", 0
	calculate_G G, [reg]
	call calculate_E
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, E
	jmp found_other
	_mov_c7_c6?:
	
	if_between_jump_else 0xc7, 0xc6, _mov_c7_c6, _retf_ca?
	
	_mov_c7_c6:
	append_unique_string str_buff, "MOV ",0
	call calculate_E
	call calculate_I
	append_ptr
	append_string str_buff, E
	append_string str_buff, comma
	append_string str_buff, I
	jmp found_other
	
	
	
	_retf_ca?:
	if_equal_jump_else 0xca, _retf_ca, _int_cd?
	_retf_ca:
	append_unique_string str_buff, "RETF ", 0
	mov byte[sz], 1
	call calculate_I
	append_string str_buff, I
	jmp found_other
	_int_cd?:
	if_equal_jump_else 0xcd, _int_cd, _loop_e3_e0?
	_int_cd:
	append_unique_string str_buff, "INT ", 0
	mov byte[sz], 0
	call calculate_I
	append_string str_buff, I
	jmp found_other
	_loop_e3_e0?:
	if_between_jump_else 0xe3, 0xe0, _loop_e3_e0, _out_in_e7_e4?
	_loop_e3_e0:
	mov byte [sz], 0
	mov ax, [_byte]
	push ax
	call calculate_J
	pop ax
	mov byte [_byte], al
	if_equal_jump 0xe0, _loopnz
	if_equal_jump 0xe1, _loopz
	if_equal_jump 0xe2, _loop
	if_equal_jump 0xe3, _jcxz
	_loopnz:
	append_unique_string str_buff, "LOOPNZ ", 0
	append_string str_buff, J
	jmp found_other
	_loopz:
	append_unique_string str_buff, "LOOPZ ", 0
	append_string str_buff, J
	jmp found_other
	_loop:
	append_unique_string str_buff, "LOOP ", 0
	append_string str_buff, J
	jmp found_other
	_jcxz:
	append_unique_string str_buff, "JCXZ ", 0
	append_string str_buff, J
	jmp found_other
	_out_in_e7_e4?:
	if_between_jump_else 0xe7, 0xe4, _out_in_e7_e4, _out_in_ef_ec?
	_out_in_e7_e4:
	mov ax, [_byte]
	sub ax, 0xe4
	cmp ax, 2
	jb _in
	jmp _out
	_in:
	append_unique_string str_buff, "IN ", 0
	jmp skip_out
	_out:
	append_unique_string str_buff, "OUT ", 0
	skip_out:
	get_al_ax
	mov byte [sz], 0
	call calculate_I
	decide_direction G, I
	jmp found_other
	_out_in_ef_ec?:
	if_between_jump_else 0xef, 0xec, _out_in_ef_ec, no_other
	_out_in_ef_ec:
	mov ax, [_byte]
	sub ax, 0xec
	cmp ax, 2
	jb __in
	jmp __out
	__in:
	append_unique_string str_buff, "IN ", 0
	jmp _skip_out
	__out:
	append_unique_string str_buff, "OUT ", 0
	_skip_out:
	append_unique_string E, "DX", 0
	get_al_ax
	decide_direction G, E
	jmp found_other
	found_other:
	mov al, 1
	jmp end_other
	no_other:
	mov al, 0
	end_other:
	pop bp
	ret

;Patikrinamos grupinės instrukcijos kurių operacija yra nusakoma [reg] bitais
;"The one remaining complexity involves "group" opcodes, such as 80. 
;These opcodes perform different operations depending upon the value of the reg bits in the ModR/M byte following the opcode byte. 
;For example, opcode 80 followed by a ModR/M byte with a reg of 4 is an AND Eb, Ib instruction, while that same opcode followed by a ModR/M byte with a reg of 7 is a CMP Eb, Ib instruction."

check_group:
	push bp
	mov bp, sp
	
	if_between_jump_else 0x83, 0x80, _group1, _group2?
	_group1: 
	push word [_byte]
	call calculate_E
	fetch_op group1_array, [reg]
	append_string str_buff, bx
	append_unique_string str_buff, " ", 0
	append_ptr
	append_string str_buff, E
	pop word [_byte]
	
	if_between_jump_else 0x82, 0x80, group_10, group_11
	group_10:
	call calculate_I
	append_string str_buff, comma
	append_string str_buff, I
	jmp found_group
	group_11:
	mov byte[sz], 0
	call calculate_I
	append_string str_buff, comma
	append_string str_buff, I
	jmp found_group
	
	 _group2?:
	 if_between_jump_else 0xd3, 0xd0, _group2, _group3?
	 _group2:
	 push word[_byte]
	 call calculate_E
	 fetch_op group2_array, [reg]
	 append_string str_buff, bx
	 append_unique_string str_buff, " ", 0
	 append_string str_buff, E
	 append_string str_buff, comma
	 pop ax
	 sub ax, 0xd0
	 cmp ax, 2
	 jle _add_one
	 jmp _append_cl
	 _add_one: 
	 append_unique_string str_buff, "1", 0
	 jmp found_group
	 _append_cl:
	 append_unique_string str_buff, "CL", 0
	 jmp found_group
	 
	 _group3?:
	 if_between_jump_else 0xf7, 0xf6, _group3, _group4?
	 _group3:
	 call calculate_E
	 fetch_op group3_array, [reg]
	 append_string str_buff, bx
	 append_unique_string str_buff, " ", 0
	 append_ptr
	 append_string str_buff, E
	 cmp byte[reg], 0
	 je __test
	 jmp __no_test
	 __test:
	 call calculate_I
	 append_string str_buff, comma
	 append_string str_buff, I
	 __no_test:
	 jmp found_group
	 
	 _group4?:
	 if_equal_jump_else 0xfe, _group4, _group5?
	 _group4:
	 call calculate_E
	 fetch_op group4_array, [reg]
	 append_string str_buff, bx
	 append_unique_string str_buff, " ", 0
	 append_ptr
	 append_string str_buff, E
	 jmp found_group
	 
	 _group5?:
	 if_equal_jump_else 0xff, _group5, no_group
	 _group5:
	 call calculate_E
	 fetch_op group5_array, [reg]
	 append_string str_buff, bx
	 append_unique_string str_buff, " ", 0
	 cmp byte[reg], 3
	 je _segment_offset
	 cmp byte[reg], 5
	 je _segment_offset
	 append_ptr
	 append_string str_buff, E
	 jmp found_group
	 _segment_offset:
	 call calculate_A
	 append_unique_string str_buff, "FAR ", 0
	 append_string str_buff, E
	 jmp found_group
	
	
	 found_group:
	 mov al, 1
	 jmp group_end
	 no_group:
	 mov al, 0
	 group_end:
	 pop bp
	 ret
	
	
;Patikrinamos instrukcijos kurios atitinka sias salygas: [_byte] <= 0x3e ir [_byte] % 8 < 6
;Instrukcijos pavadinimas yra skaiciuojamas: x = [byte] / 8; instrukcija = group1_array[x]


check_regular:
	push bp
	mov bp, sp
	cmp byte[_byte], 0x3e 
	jb maybe_regular
	jmp not_regular
	maybe_regular:
	mov ax, [_byte]
	mov bl, 8
	div bl
	cmp ah, 6
	jb regular
	jmp not_regular
	regular:
	push ax
	and ah, 0
	fetch_op group1_array, ax
	append_string str_buff, bx
	append_unique_string str_buff, " ", 0
	pop ax
	cmp ah, 4
	jb not_ax_al
	jmp ax_al
	not_ax_al:
	call calculate_E
	calculate_G G, [reg]
	decide_direction E, G
	jmp found_regular
	ax_al:
	get_al_ax
	call calculate_I
	append_string str_buff, G
	append_string str_buff, comma
	append_string str_buff, I
	jmp found_regular
	found_regular:
	mov al, 1
	jmp exit
	not_regular:
	mov al, 0
	exit:
	pop bp
	ret

calculate_A: 
	push bp
	mov bp, sp
	read_word
	word_to_hex [_word]
	append_string A, bracket_left
	append_string A, word_hex_str
	append_string A, bracket_right
	append_string A, colon
	and word [_word], 0
	and word [word_hex_str], 0
	read_word
	word_to_hex [_word]
	append_string A, bracket_left
	append_string A, word_hex_str
	append_string A, bracket_right
	pop bp
	ret
	
;Patikrinami suoliai. suoliu offsetas skaiciuojamas su macro signed_add
check_jump:
	push bp
	mov bp, sp
	if_between_jump_else 0x7f, 0x70, regular_jumps, maybe_other_jumps
	regular_jumps:
	mov byte[sz], 0
	mov ax, [_byte]
	sub ax, 0x70
	fetch_op jump_array, ax
	append_string str_buff, bx
	call calculate_J
	append_unique_string str_buff, " ", 0
	append_string str_buff, J
	jmp found_jump
	maybe_other_jumps:
	if_equal_adjust_sz 0xE8, call_word, 1
	if_equal_adjust_sz 0xE9, jump_word, 1
	if_equal_jump 0xEA, jump_pointer
	if_equal_adjust_sz 0xEB, jump_byte, 0
	if_equal_jump 0x9a, call_pointer
	jmp no_jumps
	call_word:
	append_unique_string str_buff, "CALL ", 0
	mov byte [sz], 1
	call calculate_J
	append_string str_buff, J
	jmp found_jump
	jump_word:
	
	append_unique_string str_buff, "JMP ", 0
	call calculate_J
	append_string str_buff, J
	jmp found_jump
	
	jump_pointer:
	append_unique_string str_buff, "JMP ", 0
	call calculate_A
	append_string str_buff, A
	jmp found_jump
	
	jump_byte:
	append_unique_string str_buff, "JMP ", 0
	call calculate_J
	append_string str_buff, J
	jmp found_jump
	
	call_pointer:
	append_unique_string str_buff, "CALL0 ", 0
	call calculate_A
	append_string str_buff, A
	found_jump:
	mov al, 1
	jmp jump_end
	no_jumps:
	mov al, 0
	jump_end:
	pop bp
	ret
	

	

calculate_E:
	push bp
	mov bp, sp
	read_byte
	get_bits_macro 8, 2, al ; Gaunami mode bitai nusakantis displacement kiekį (0, 8, 16 bitų), kai (mode < 3) arba kad r_rm bitai nusakys antrą registeri, kai (mode = 3) 
	mov [mode], al
	byte_to_hex [mode]
	get_bits_macro 6, 3, al ;Gaunami registerio bitai
	mov [reg], al
	byte_to_hex [reg]
	get_bits_macro 3, 3, al ;gaunami r_rm bitai nusakantys displacement rūšį arba antra registeri
	mov [r_rm], al
	byte_to_hex [r_rm]
	cmp byte [mode], 0 ; mode = 0?
	je exception?
	jmp no_exception
	exception?:
	cmp byte [r_rm], 6 ; r_rm = 6?
	je exception
	jmp no_exception
	exception: ; Išskirtinis atvėjis kai [mode] = 0 ir [r_rm] = 6
	cmp byte [segment_use_flag], 1 ; Jei yra nustatyas [segment_use_flag] po funkcijos check_prefix iššaukimo, pridėti segmento prefixa prie atminties adreso
	je append_prefix_segment
	jmp no_prefix_segment
	append_prefix_segment:
	append_string E, segment_offset_to_use
	append_string E, colon
	no_prefix_segment:
	append_string E, bracket_left
	xor ax, ax
	mov al, byte[sz]
	push ax
	mov byte[sz], 1
	call calculate_I
	append_string E, I
	append_string E, bracket_right
	init_str I
	pop ax
	mov byte[sz], al
	byte_to_hex [sz]
	jmp end_E
	no_exception:
	cmp byte [mode], 3
	je register
	jmp displacement
	register:
	mov byte[_ptr], 0
	calculate_G E, [r_rm]
	jmp end_E
	displacement:
	cmp byte [segment_use_flag], 1
	je attach_segment
	jmp no_segmet
	attach_segment:
	append_string E, segment_offset_to_use
	append_string E, colon
	no_segmet:
	append_string E, bracket_left 
	fetch_op segment_displace, [r_rm]
	append_string E, bx
	disp_mode:
	cmp byte [mode], 0
	je zero_bit_disp
	cmp byte [mode], 1
	je eight_bit_disp
	jmp sixteen_bit_disp
	zero_bit_disp:
	append_string E, bracket_right
	jmp end_E
	eight_bit_disp:
	read_byte
	append_string E, plus
	byte_to_hex [_byte]
	append_string E, byte_hex_str
	append_string E, bracket_right
	jmp end_E
	sixteen_bit_disp:
	read_word
	word_to_hex [_word]
	append_string E, plus
	append_string E, word_hex_str
	append_string E, bracket_right
	end_E:
	pop bp
	ret
	
calculate_I: 
	push bp
	mov bp, sp
	cmp byte[O], 1
	je add_brackets_l
	jmp no_brackets_l
	add_brackets_l:
	append_string I, bracket_left
	no_brackets_l:
	cmp byte [sz], 0
	je size_00
	jmp size_01
	size_00:
	read_byte
	byte_to_hex [_byte]
	append_string I, byte_hex_str
	jmp end_I
	size_01:
	read_word
	word_to_hex [_word]
	append_string I, word_hex_str
	init_str word_hex_str
	end_I:
	cmp byte[O], 1
	je add_brackets_r
	jmp no_brackets_r
	add_brackets_r:
	append_string I, bracket_right
	no_brackets_r:
	pop bp
	ret

	
calculate_J: 
	push bp
	mov bp, sp
	cmp byte[sz], 0
	je sz_0
	jmp sz_1
	sz_0:
	and word [_byte], 0
	read_byte
	signed_add [bytes_read], [_byte]
	word_to_hex ax
	append_string J, word_hex_str
	jmp end_J
	sz_1:
	and word [_word], 0
	read_word
	signed_add [bytes_read], [_word]
	word_to_hex ax
	append_string J, word_hex_str
	end_J:
	pop bp
	ret
	

	
print_str_f: 
	push bp
	mov bp, sp
	not_finished:
	cmp byte [bx], 0
	je finished
	mov cx, [bx]
	print_byte cl
	inc bx
	jmp not_finished
	finished:	
	pop bp
	ret 
	
; Apskaiciuojamas string ilgis. Visi stringai pasibaigia 0.
; BX - string bufferis, CX - paliekamas rezultatas
str_len: 
	push bp
	mov bp, sp
	mov cx, 0
	str_len_start:
	cmp byte [bx], 0
	je str_len_end
	inc bx
	inc cx
	jmp str_len_start
	str_len_end:
	pop bp
	ret

; prie pirmo stringo esančio [bp + 6], pridjungiamas antras stringas esantis [bp + 4].
; antras stringas islieka nepakeistas.
append_str_f: 
	push bp
	mov bp, sp
	mov bx, [bp + 6] ;+4 str2, +6 str1
	call str_len
	mov ax, cx ; ax = str1 len
	mov bx, [bp + 4]
	call str_len ; cx = str2 len
	mov di, [bp + 6] ; di = address of str1
	mov si, [bp + 4] ; si = address of str2
	add di, ax 
	inc cx
	rep movsb
	pop bp
	ret

;Patikrinami prefixai
check_prefix:
	push bp
	mov bp, sp
	if_equal_jump 0x26, seg_es
	if_equal_jump 0x2e, seg_cs
	if_equal_jump 0x36, seg_ss
	if_equal_jump 0x3e, seg_ds
	jmp no_seg
	seg_es: 
	append_string segment_offset_to_use, S_ES
	mov byte [segment_use_flag], 1
	jmp is_prefix
	seg_cs: 
	append_string segment_offset_to_use, S_CS
	mov byte [segment_use_flag], 1
	jmp is_prefix
	seg_ss:
	append_string segment_offset_to_use, S_SS
	mov byte [segment_use_flag], 1
	jmp is_prefix
	seg_ds:
	append_string segment_offset_to_use, S_DS
	mov byte [segment_use_flag], 1
	jmp is_prefix
	no_seg:
	if_equal_jump 0xF0, pref_lock
	if_equal_jump 0xF3, pref_rep
	if_equal_jump 0xF2, pref_repne
	jmp no_pref
	pref_lock:
	append_unique_string str_buff, "LOCK ", 0
	jmp is_prefix
	pref_rep:
	append_unique_string str_buff, "REP ", 0
	jmp is_prefix
	pref_repne:
	append_unique_string str_buff, "REPNE ", 0
	is_prefix:
	mov al, 1
	pop bp
	ret
	no_pref:
	mov al, 0
	pop bp
	ret
	
;Nustatomas krypties ir dydžio bitas. Jie atitinkamai irasomi i [durection] ir [sz]
set_direction_size: 
	push bp
	mov bp, sp
	get_bits_macro 2, 1, al
	mov byte [direction], al
	get_bits_macro 1, 1, al
	mov byte [sz], al
	pop bp
	ret
	
; Iš dabar nuskaityto baito [_byte] gaunami [count] bitai pradedant nuo [pos]. Rezultatas paliekamas [bits_gotten]
get_bits: 
	push bp
	mov bp, sp
	mov cl, byte[pos]
	mov al, byte[_byte]
	mov byte[bits_gotten], al
	shr al, cl
	shl al, cl
	xor byte[bits_gotten], al
	mov cl, [pos]
	sub cl, [count]
	shr byte[bits_gotten], cl
	pop bp
	ret

	
	


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



