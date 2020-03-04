	  ;; game state memory location
  .equ T_X, 0x1000                  ; falling tetrominoe position on x
  .equ T_Y, 0x1004                  ; falling tetrominoe position on y
  .equ T_type, 0x1008               ; falling tetrominoe type
  .equ T_orientation, 0x100C        ; falling tetrominoe orientation
  .equ SCORE,  0x1010               ; score
  .equ GSA, 0x1014                  ; Game State Array starting address
  .equ SEVEN_SEGS, 0x1198           ; 7-segment display addresses
  .equ LEDS, 0x2000                 ; LED address
  .equ RANDOM_NUM, 0x2010           ; Random number generator address
  .equ BUTTONS, 0x2030              ; Buttons addresses

  ;; type enumeration
  .equ C, 0x00
  .equ B, 0x01
  .equ T, 0x02
  .equ S, 0x03
  .equ L, 0x04

  ;; GSA type
  .equ NOTHING, 0x0
  .equ PLACED, 0x1
  .equ FALLING, 0x2

  ;; orientation enumeration
  .equ N, 0
  .equ E, 1
  .equ So, 2
  .equ W, 3
  .equ ORIENTATION_END, 4

  ;; collision boundaries
  .equ COL_X, 4
  .equ COL_Y, 3

  ;; Rotation enumeration
  .equ CLOCKWISE, 0
  .equ COUNTERCLOCKWISE, 1

  ;; Button enumeration
  .equ moveL, 0x01
  .equ rotL, 0x02
  .equ reset, 0x04
  .equ rotR, 0x08
  .equ moveR, 0x10
  .equ moveD, 0x20

  ;; Collision return ENUM
  .equ W_COL, 0
  .equ E_COL, 1
  .equ So_COL, 2
  .equ OVERLAP, 3
  .equ NONE, 4

  ;; start location
  .equ START_X, 6
  .equ START_Y, 1

  ;; game rate of tetrominoe falling down (in terms of game loop iteration)
  .equ RATE, 5

  ;; standard limits
  .equ X_LIMIT, 12
  .equ Y_LIMIT, 8


; BEGIN:main
main:
	addi sp, zero, 0x2000
	call reset_game

	loop_main:
		main_repeat_big:
			main_repeat_small:
			addi sp, sp, -8
			stw s0, 0(sp)
			stw s1, 4(sp)
			addi s1, zero, 1

			addi s0, zero, 0
			main_whileInfRate:
				addi t0, zero, RATE
				beq s0, t0, main_whileInfRateEnd
				call draw_gsa
				call display_score
				call wait
				addi a0, zero, NOTHING
				call draw_tetromino
				call draw_gsa
				call get_input
				beq v0, zero, main_whileInfRate_NoButtonPressed
				add a0,zero,v0
				call act
				addi a0, zero, FALLING
				call draw_tetromino
				call draw_gsa
				main_whileInfRate_NoButtonPressed:
				addi a0, zero, FALLING 
				call draw_tetromino
				call draw_gsa
				addi s0, s0, 1
				jmpi main_whileInfRate
			main_whileInfRateEnd:
			addi a0, zero, NOTHING
			call draw_tetromino
			addi a0, zero, moveD
			call act
			addi s1, v0, 0 ;save act return (succeed or failed)
			addi a0, zero, 2;falling
			call draw_tetromino
			call draw_gsa
			beq s1, zero, main_repeat_small
		addi a0, zero, PLACED ;placed
		call draw_tetromino
		call draw_gsa
		main_whileFullLine:
			call detect_full_line
			addi t0, zero, 8 ; no full lines value of return detect
			beq v0, t0, full_line_NotFound
			addi a0, v0, 0
			call remove_full_line
			call increment_score
			jmpi main_whileFullLine
			full_line_NotFound:
				
			call generate_tetromino
			addi a0, zero, OVERLAP
			call detect_collision	
	
			addi t0, zero, NONE
			bne v0, t0, main_ifEnd
			addi a0, zero, 2 ;fallin	
			call draw_tetromino
			call draw_gsa
			jmpi main_repeat_big

			main_ifEnd:
			jmpi main
			



;  	addi sp, sp, 0x1500
;	call reset_game

;----detect collision
;	addi a0, zero, So_COL
;	call detect_collision
;	addi t0, t0, 0

;--- score
;	call display_score
;	call increment_score
;	call display_score


;----- reset
;	call reset_game

;----- genTetro, drawTetro
;	call generate_tetromino
;	addi a0, zero, 2
;	call draw_tetromino
;	call draw_gsa
	
	
	
		;	call clear_leds
  		;	;call wait
		 ;	addi a0, a0, 11
			;addi a1, a1, 7
		    ;call set_pixels
		    ;call wait
		   ; br loop_main

break
; END:main


; BEGIN:clear_leds
clear_leds:
  stw zero, LEDS(zero)
  stw zero, LEDS+4(zero)
  stw zero, LEDS+8(zero)
ret
; END:clear_leds


; BEGIN:set_pixels
set_pixel:
	andi t0, a0, 3
	slli t0, t0, 3
	add t0, t0, a1
	srli t1, a0, 2
	addi t2, zero, 1
	sll t2, t2, t0
	slli t1, t1, 2
 
	ldw t3, LEDS(t1)
	or t3, t3, t2
	stw t3, LEDS(t1)
	ret

; END:set_pixels
; BEGIN:wait
wait:
	addi t0, t0, 20
  	slli t0, t0, 17
  	loop_wait:
    	addi t0, t0, -1
    	bge t0, zero, loop_wait
	ret
; END:wait


; BEGIN:in_gsa
in_gsa:
	cmplti t0, a0, 0 		;1 if out of bounds
	cmplti t1, a1, 0		;1 if out of bounds
	cmpgei t2, a0, X_LIMIT	;1 if out of bounds 
	cmpgei t3, a1, Y_LIMIT
	or t0, t0, t1
	or t0, t0, t2
	or v0, t0, t3
ret
; END:in_gsa

; BEGIN:get_gsa
get_gsa:
	slli t0, a0, 3 				; to get to a line
	add t0, t0, a1 				; we add the column
	slli t0, t0, 2 				; words are 4 bytes
	ldw v0, GSA(t0)				; we load pixel value
ret	
; END:get_gsa

; BEGIN:set_gsa
set_gsa:
	slli t0, a0, 3
	add t0, a1, t0
	slli t0, t0, 2 
	stw a2, GSA(t0) 			; we store pixel value
ret
; END:set_gsa

; BEGIN:draw_gsa
draw_gsa:
	addi sp, sp, -12
	stw s0, 0(sp)
	stw s1, 4(sp)
	stw ra, 8(sp)
	
	call clear_leds
	addi s0, zero, -1 ;x
	draw_gsa_loopX:
		addi s0, s0, 1
		addi t0, zero, 12
		beq s0, t0, draw_gsa_endLoop
		addi s1, zero, -1 ;y
		draw_gsa_loopY:
			addi s1, s1, 1
			addi t0, zero, 8
			beq s1, t0, draw_gsa_loopX
			addi a0, s0, 0
			addi a1, s1, 0
			call get_gsa
			beq v0, zero, draw_gsa_loopY
			addi a0, s0, 0
			addi a1, s1, 0
			call set_pixel
			jmpi draw_gsa_loopY
	draw_gsa_endLoop:
	ldw s0, 0(sp)
	ldw s1, 4(sp)
	ldw ra, 8(sp)
	addi sp, sp, 12
	ret
; END:draw_gsa


; BEGIN:draw_tetromino
draw_tetromino:
  	addi sp, sp, -20
 	stw ra, 0(sp)
	stw s0, 4(sp)  	;counter to be
	stw s1, 8(sp)	;draw_ax
	stw s2, 12(sp)	;draw_ay
	stw s3, 16(sp)	;value to put in gsa (fallin, nothing, 

	addi s3, a0, 0	; save value to put in gsa forever

	;first we draw the anchor:
	ldw t0, T_X(zero)
	ldw t1, T_Y(zero)
	add a0, zero, t0
	add a1, zero, t1
	add a2, zero, s3
	call set_gsa

	;now we compute an draw the 3 other pixels
	
	;calculations to get coordinates for DrawAxAy
 	ldw t0, T_type(zero)
  	slli t0, t0, 4
  	ldw t1, T_orientation(zero)
	slli t1, t1, 2
  	add t0, t0, t1
	;done
  	ldw s1, DRAW_Ax(t0)
  	ldw s2, DRAW_Ay(t0)

	addi s0, zero, 2 ; notre compteur --> il faut draw les 3 tetrominos d l'exterieur.
  	draw_tetromino_loop:
		add t7, zero, s0 
		slli t7, t7, 2 ;le offset relatif au [0;2] tetro


		add t2, t7, s1
		ldw t0, 0(t2) ;x coordinate of tetro piece
		
		add t2, t7, s2 
		ldw t1, 0(t2) ;y coorrdinate of tetro piece

		ldw t2, T_X(zero)
		add t0, t0, t2

		ldw t3, T_Y(zero)
		add t1, t1, t3
		
		add a0, zero, t0
		add a1, zero, t1
		add a2, zero, s3
		call set_gsa
		;made it --> now check if 3 tetros or less
    	addi t0, zero, -1
		addi s0, s0, -1
		bne s0, t0,  draw_tetromino_loop
	;all done
  	ldw ra, 0(sp)
	ldw s0, 4(sp)
	ldw s1, 8(sp)
	ldw s2, 12(sp)
	ldw s3, 16(sp)
  	addi sp, sp, 20
	ret
; END:draw_tetromino


; BEGIN:generate_tetromino
generate_tetromino:

  	addi t0, zero, 6
	stw t0, T_X(zero)
  	addi t0, zero, 1
  	stw t0, T_Y(zero)
  	stw zero, T_orientation(zero) ; generated tetromino are always oriented N 
 	

  	rd_tetro_type:
  	  	ldw t3, RANDOM_NUM(zero)
  	 	andi t3, t3, 7
		addi t2, zero, 5
    	bge t3, t2, rd_tetro_type
  	
  	stw t3, T_type(zero)
	ret
; END:generate_tetromino

;--------------
; BEGIN:detect_collision
detect_collision:
  	addi sp, sp, -28 ;save in stack the save and ra registers.
  	stw ra, 0(sp)
	stw s0, 4(sp)	;collision type
	stw s1, 8(sp)	;compteur de 1 a 4
	stw s2, 12(sp) 	; x of tetro
	stw s3, 16(sp) 	; y of tetro
	stw s4, 20(sp) 	;DrawAx
	stw s5, 24(sp) 	;DrawAy

  	add s0, zero, a0 ; COLLISION TYPE TO BE CHECKED
	
 	ldw s2, T_X(zero)
	ldw s3, T_Y(zero)

	addi a0, s2, 0
	addi a1, s3, 0
	call in_gsa
	addi t1, zero, FALLING
	addi t0, v0, 0
	addi v0, zero, 0
  	;bne t0, t1, end ; IF tetro is not falling gotoend

  	; same way than draw_gsa
	;calculations to get coordinates for DrawAxAy
 	ldw t0, T_type(zero)
  	slli t0, t0, 4
  	ldw t1, T_orientation(zero)
	slli t1, t1, 2
  	add t0, t0, t1
	;done
  	ldw s4, DRAW_Ax(t0)
  	ldw s5, DRAW_Ay(t0)

 	add a2, zero, a0
 	ldw a0, T_X(zero)
  	ldw a1, T_Y(zero)

  	addi s1, zero, 0
	addi a0, s2, 0
	addi a1, s3, 0
  	loop_through_tetro:
    	call check_col ;check call saving 

		;;;calc next tetro
		add t7, zero, s1
		slli t7, t7, 2
		
		add t2, t7, s4
		ldw t0, 0(t2) 	;x coordinate offset of tetro piece
		add t2, t7, s5
		ldw t1, 0(t2)  	;y coorrdinate offset of tetro piece
		;done
		ldw t5, T_X(zero)
		add t0, t0, t5
  		ldw t6, T_Y(zero)
		add t1, t1, t6
		
    	add a0, zero, t0 	; on ajoute drawAx
    	add a1, zero, t1 	; on ajoute drawAy
		
    	addi s1, s1, 1
		addi t3, zero, 4
    	bne s1, t3, loop_through_tetro
	
  	jmpi end_detect_wo_collisionA

  	check_col:
		addi sp, sp, -16
		stw ra, 0(sp)
		stw s0, 4(sp)	;x coor
		stw s1, 8(sp)	;y coor
		stw s5, 12(sp)	;col to check
		

		addi s5, s0, 0
		addi s0, a0, 0
		addi s1, a1, 0
		
		addi t5, zero, E_COL
   		beq s5, t5, e_col_handler
		addi t5, zero, W_COL
    	beq s5, t5, w_col_handler
		addi t5, zero, So_COL
    	beq s5, t5, so_col_handler

    	; or handle overlap vlow
   		call in_gsa
		addi t0, zero, 1
   		beq v0, t0, end_detect_with_collision
		addi a0, s0, 0
		addi a1, s1, 0
   		call get_gsa
		addi t0, zero, 1
    	beq v0, t0, end_detect_with_collision

		ldw ra, 0(sp)
		ldw s0, 4(sp)	;x coor
		ldw s1, 8(sp)	;y coor
		ldw s5, 12(sp)
		addi sp, sp, 16
    	ret


    e_col_handler:
      	;x+1
		
      	addi a0, s0, 1
		addi a1, s1, 0
      	call in_gsa
		addi t1, zero, PLACED
      	beq v0, t1, end_detect_with_collision
		addi a0, s0, 1
		addi a1, s1, 0
      	call get_gsa
		addi t1, zero, PLACED
      	beq v0, t1, end_detect_with_collision
    	jmpi end_col_handler

 	w_col_handler:
      ;x-1
     	addi a0, s0, -1
		addi a1, s1, 0
   		call in_gsa
		addi t1, zero, PLACED
      	beq v0, t1, end_detect_with_collision
		addi a0, s0, -1
		addi a1, s1, 0
   		call get_gsa
		addi t1, zero, PLACED
      	beq v0, t1, end_detect_with_collision
		jmpi end_col_handler

    so_col_handler:
      ;y+1
		addi a0, s0, 0
      	addi a1, s1, 1
      	call in_gsa
		addi t1, zero, PLACED
      	beq v0, t1, end_detect_with_collision
		addi a0, s0, 0
		addi a1, s1, 1
      	call get_gsa
		addi t1, zero, PLACED
      	beq v0, t1, end_detect_with_collision
    	jmpi end_col_handler

	end_col_handler:
		ldw ra, 0(sp)
		ldw s0, 4(sp)	;x coor
		ldw s1, 8(sp)	;y coor
		ldw s5, 12(sp)
		addi sp, sp, 16
		ret


  	end_detect_wo_collisionA:
    	addi v0, zero, NONE ; no col so v0 = 0
    	jmpi end


  	end_detect_with_collision:
    	add v0, zero, s0 ; assign v0 to the saved col type
		ldw ra, 0(sp)
		ldw s0, 4(sp)	;x coor
		ldw s1, 8(sp)	;y coor
		ldw s5, 12(sp)
		addi sp, sp, 16
		jmpi end

  	end: 
  		ldw ra, 0(sp)
		ldw s0, 4(sp)	;collision type
		ldw s1, 8(sp)	;compteur de 1 a 4
		ldw s2, 12(sp) 	; x of tetro
		ldw s3, 16(sp) 	; y of tetro
		ldw s4, 20(sp) 	;DrawAx
		ldw s5, 24(sp) 	;DrawAy
		addi sp, sp, 28 ;save in stack the save and ra registers.
  		ret

; END:detect_collision


; BEGIN:rotate_tetromino
rotate_tetromino:

	addi t0, zero, COUNTERCLOCKWISE ;rotL
	beq a0, t0, rotateL
	addi t0, zero, CLOCKWISE ;rotR
	beq a0, t0, rotateR

	rotateL:
  		ldw t0, T_orientation(zero)
  		addi t0, t0, -1
		bge t0, zero, moduloLeftNotNeeded
		addi t0, t0, 4
		moduloLeftNotNeeded:
  		andi t0, t0, 3
  		stw t0, T_orientation(zero)
  		jmpi end_rotate

	rotateR:
 		ldw t0, T_orientation(zero)
  		addi t0, t0, 1
		addi t1, zero, 4
		blt t0, t1, moduloRightNotNeeded
		addi t0, t0, -4
		moduloRightNotNeeded:
  		andi t0, t0, 3
 		stw t0, T_orientation(zero)
  		jmpi end_rotate

	end_rotate:
	ret
; END:rotate_tetromino


; BEGIN:act
act:
   ; Save the value of T_X, T_Y and T_orientation in stack
    ldw t0, T_X(zero)
    ldw t1, T_Y(zero)
    ldw t2, T_orientation(zero)
 
    addi sp, sp, -28
	stw ra, 0(sp)
	stw a0, 4(sp)
    stw s0, 8(sp)
    stw s1, 12(sp)
 
    stw t0, 16(sp) ; to reput them back in
    stw t1, 20(sp)
    stw t2, 24(sp)
 
   
    addi t0, zero, moveL
    beq a0, t0, moveL_handler
    addi t0, zero, rotL
    beq a0, t0, rotL_handler
    addi t0, zero, reset
    beq a0, t0, reset_handler
    addi t0, zero, rotR
    beq a0, t0, rotR_handler
    addi t0, zero, moveR
    beq a0, t0, moveR_handler
    addi t0, zero, moveD
    beq a0, t0, moveD_handler
 
 
    moveL_handler:
        addi a0, zero, W_COL
        call detect_collision ; test if there is a W_COL
        cmpeqi t2, v0, NONE ; = 0 if W_COL (if colw alors1)
        ldw t3, T_X(zero)
        sub t3, t3, t2 ; Modify x accordingly
        stw t3, T_X(zero)
 		addi t0, zero, NONE
        bne v0, t0, end_act_fail
       	jmpi end_act_suceed

 	moveR_handler:
        addi a0, zero, E_COL
        call detect_collision
        cmpeqi t2, v0, NONE
        ldw t3, T_X(zero)
        add t3, t3, t2 ; go to the left with one px
        stw t3, T_X(zero)
		addi t0, zero, NONE
        bne v0, t0, end_act_fail
        jmpi end_act_suceed

    moveD_handler:
        addi a0, zero, So_COL
        call detect_collision
        cmpeqi t2, v0, NONE
        ldw t3, T_Y(zero)
        add t3, t3, t2 ; go to the top with one px
        stw t3, T_Y(zero)
 		addi t0, zero, NONE
        bne v0, t0, end_act_fail
        jmpi end_act_suceed
 
    reset_handler:
		;ldw ra, 20(sp)
		;addi sp, sp, 28
        call reset_game ;TODO save stack TOOOOOOOooooooooo
		jmpi end_act_suceed
 
    rotL_handler:
        addi a0, zero, COUNTERCLOCKWISE
        call rotate_tetromino
        jmpi adjut_rot
 
    rotR_handler:
        addi a0, zero, CLOCKWISE
        call rotate_tetromino
        jmpi adjut_rot
 
    adjut_rot:
        ldw t0, T_X(zero)
        cmplti t1, t0, 6
        addi t2, zero, 1
 
        addi a0, zero, OVERLAP
        call detect_collision
		addi t0, t0, NONE
        beq v0, t0, end_act_suceed
 		ldw t0, T_X(zero) ;;;;;;;;;;;;;;;
        cmplti t1, t0, 6
        addi t2, zero, 1;;;;;;;;;;;;;

        beq t1, t2, increase_x
		jmpi decrease_x 
 
    increase_x:
        ldw t4, T_X(zero)
        addi t4, t4, 1
        stw t4, T_X(zero)
		;;done increase1
	
		call detect_collision	
		addi t0, zero, NONE
        beq v0, t0, end_act_suceed
		
		ldw t4, T_X(zero)
        addi t4, t4, 1
        stw t4, T_X(zero)

		call detect_collision
		addi t0, zero, NONE
        beq v0, t0, end_act_suceed
		jmpi end_act_fail
 
    decrease_x:
        ldw t4, T_X(zero)
        addi t4, t4, -1
        stw t4, T_X(zero)
		;;done increase1
	
		call detect_collision	
		addi t0, zero, NONE
        beq v0, t0, end_act_suceed
		
		ldw t4, T_X(zero)
        addi t4, t4, -1
        stw t4, T_X(zero)

		call detect_collision
		addi t0, zero, NONE
        beq v0, t0, end_act_suceed
		jmpi end_act_fail
  
   
    end_act_fail:
        addi v0, zero, 1
        ; REST VALUE OF T_X, T_Y and T_orientation
        ldw t0, 16(sp)
        ldw t1, 20(sp)
        ldw t2, 24(sp)

        stw t0, T_X(zero)
        stw t1, T_Y(zero)
        stw t2, T_orientation(zero)
        jmpi end_act
 
    end_act_suceed:
        addi v0, zero, 0
        jmpi end_act
 
 
    end_act:
		ldw ra, 0(sp)
        ldw a0, 4(sp)
        ldw s0, 8(sp)
        ldw s1, 12(sp)
        addi sp, sp, 28
 
    ret
; END:act


; BEGIN:get_input
get_input:
	
	ldw t0, BUTTONS+4(zero)
	stw zero, BUTTONS+4(zero) ;set it back to zero for next check
	addi t1, zero, 1
	
	
	andi t2, t0, 1 ;first bit/button check
	addi v0, zero, moveL
	beq t2, t1, get_input_foundIt
	srli t0, t0, 1
	andi t2, t0, 1 ; second bit/button check
	addi v0, zero, rotL
	beq t2, t1, get_input_foundIt
	srli t0, t0, 1
	andi t2, t0, 1 ; third bit/button check
	addi v0, zero, reset
	beq t2, t1, get_input_foundIt
	srli t0, t0, 1
	andi t2, t0, 1 ; fourth bit/button check
	addi v0, zero, rotR
	beq t2, t1, get_input_foundIt
	srli t0, t0, 1
	andi t2, t0, 1 ; fifth bit/button check
	addi v0, zero, moveR
	beq t2, t1, get_input_foundIt
	addi v0, zero, 0
	get_input_foundIt: ;that's all folks
		ret
	
; END:get_input

; BEGIN:detect_full_line
detect_full_line:
	addi sp, sp, -12
	stw ra, 0(sp)
	stw s0, 4(sp)
	stw s1, 8(sp)
	
	addi s0, zero, -1 ;counter of y
	loop_detect_full_line_y:
		addi s0, s0, 1

		addi v0, zero, 8
		beq s0, v0, end_detect_full_line ;if checked all lines

		addi s1, zero, -1 ; counter of x
		loop_detect_full_line_x:

			addi v0, s0, 0 ; v0 <= y
			addi t0, zero, 11
			beq s1, t0, end_detect_full_line ;si on est arrivé à la dernière colonne, on a trouvé une ligne a enlever :)

			addi s1, s1, 1
			addi a0, s1, 0 ; a0 --> x
			addi a1, s0, 0 ; a1 --> y
			call get_gsa ;y'a quoi à (x,y)??
			addi t1, zero, PLACED 
			bne v0, t1, loop_detect_full_line_y ; si rien de placé, jump to check next line
			beq v0, t1, loop_detect_full_line_x; si placé, on regarde l'x suivant
	
			jmpi loop_detect_full_line_y
	
	end_detect_full_line:
		ldw ra, 0(sp)
		ldw s0, 4(sp)
		ldw s1, 8(sp)
		addi sp, sp, 4
		ret

; END:detect_full_line

; BEGIN:remove_full_line
remove_full_line:
    addi sp, sp, -16
    stw ra, 0(sp)
    stw s0, 4(sp)
    stw s1, 8(sp)
    stw s2, 12(sp)

    addi s2, a0, 0 ;save the line to erase jic
    addi s0, zero, 0;blinking counter
   
    remove_full_line_blink:
       
    addi t3, zero, 0
    call remove_full_line_blink_loop
    call wait
    addi t3, zero, 1
    call remove_full_line_blink_loop
    call wait
    addi t3, zero, 0
    call remove_full_line_blink_loop
    call wait
    addi t3, zero, 1
    call remove_full_line_blink_loop
    call wait
    addi t3, zero, 0
    call remove_full_line_blink_loop
    call wait
    jmpi end_blink 
 
   
        remove_full_line_blink_loop:
            addi sp, sp, -12
            stw ra, 0(sp)
            stw t3, 4(sp)
            stw s0, 8(sp)
 
            addi s0, zero, 12
            remove_full_line_blink_loopX:
            	beq s0,zero, end_set_line
            	addi s0,s0, -1
            	add a0, zero, s0
            	add a1, zero, s2
            	add a2, zero, t3
            	call set_gsa
            	jmpi remove_full_line_blink_loopX
           
            end_set_line:
                call draw_gsa
                ldw ra, 0(sp)
                ldw t3, 4(sp)
                ldw s0, 8(sp)
                addi sp, sp, 12
                ret
 
    end_blink:
   
   
        ;TO_DO
 
    ;erase line and move upper down:
    addi s1,  s2, 0  ;y counter
    remove_full_line_loopY: ;just my fancy loop :D
        addi s1, s1, -1
        addi t0, zero, -1
        beq s1, t0, remove_full_line_EndLoop
 
        addi s0, zero, -1
        remove_full_line_loopX:
            addi s0, s0, 1
            addi t0, zero, 12
            beq s0, t0, remove_full_line_loopY
            addi a0, s0, 0
            addi a1, s1, 0
            call get_gsa
            addi a0, s0, 0 ;; CHANGED
            addi a1, s1, 1 ;; CHANGED
            addi a2, v0, 0
            call set_gsa
            jmpi remove_full_line_loopX
    remove_full_line_EndLoop:
    addi s0, zero, -1
    remove_full_line_lastXLoop:
        addi s0, s0, 1
        addi t0, zero, 12
        beq s0, t0, remove_full_line_End
        addi a0, s0, 0
        addi a1, zero, 0
        addi a2, zero, 0 ;; CHANGED
        call set_gsa
        jmpi remove_full_line_lastXLoop
 
    remove_full_line_End:
    ldw ra, 0(sp)
    ldw s0, 4(sp)
    ldw s1, 8(sp)
    ldw s2, 12(sp)
    addi sp, sp, 16
    ret
 
; END:remove_full_line
; BEGIN:increment_score
increment_score: ; just plus 1 if score<9999.
	ldw t0, SCORE(zero)
	addi t1, zero, 9999
	beq t0, t1, end_increment_score
	addi t0, t0, 1
	stw t0, SCORE(zero)
	end_increment_score:
		ret
; END:increment_score

; BEGIN:display_score
display_score:
	ldw t7, SCORE(zero)
	addi t0, zero, 0 ;value of seg0 (milliers)
	addi t1, zero, 0 ;value of seg1 (centaines)
	addi t2, zero, 0 ;value of seg2 (dizaines)
	addi t3, zero, 0 ;value of seg3 (unitées)
	
	addi t4, t7, 0 
	display_score_checkMilliers:
		addi t4, t4, -1000 							;si inferieur a 0 --> fini :)
		blt t4, zero, display_score_checkCentaines	
		addi t0, t0, 1     							;seg0++
		jmpi display_score_checkMilliers			;one more millier


	display_score_checkCentaines:		;et passe au centaines de la même façon
		addi t4, t4, 1000				;rajoute le millier qu'il manque
		addi t4, t4, -100
		blt t4, zero, display_score_checkDizaines
		addi t1, t1, 1
		jmpi display_score_checkCentaines

	display_score_checkDizaines:
		addi t4, t4, 100
		addi t4, t4, -10
		blt t4, zero, display_score_checkUnitees
		addi t2, t2, 1
		jmpi display_score_checkDizaines

	display_score_checkUnitees:
		addi t4, t4, 10
		addi t3, t4, 0
	
	;now we put value in each seg
	slli t0, t0, 2
	ldw t5, font_data(t0)
	stw t5, SEVEN_SEGS    (zero)
	slli t1, t1, 2
	ldw t5, font_data(t1)
	stw t5, SEVEN_SEGS +4 (zero)
	slli t2, t2, 2
	ldw t5, font_data(t2)
	stw t5, SEVEN_SEGS +8 (zero)
	slli t3, t3, 2
	ldw t5, font_data(t3)
	stw t5, SEVEN_SEGS +12(zero)
	ret
; END:display_score


; BEGIN:reset_game
reset_game:
	addi sp, sp, -12
	stw ra, 0(sp)
	stw s0, 4(sp)
	stw s1, 8(sp)
	
	stw zero, SCORE(zero)		;score at 0
	call display_score
	call generate_tetromino ; tetronimo at (6,1)

	;gsa set to 0:
	addi s0, zero, -1
	reset_game_loopX: ;just my fancy loop :D
		addi s0, s0, 1
		addi t0, zero, 12
		beq s0, t0, reset_game_loopEnd
		addi s1, zero, -1
		reset_game_loopY:
			addi s1, s1, 1
			addi t0, zero, 8
			beq s1, t0, reset_game_loopX
			addi a0, s0, 0
			addi a1, s1, 0
			addi a2, zero, 0
			call set_gsa
			jmpi reset_game_loopY
	reset_game_loopEnd:
	
	addi a0, zero, FALLING
	call draw_tetromino ; draw the falling tetro
	call draw_gsa		; fire the leeeeds :)
	
	ldw ra, 0(sp)
	ldw s0, 4(sp)
	ldw s1, 8(sp)
	addi sp, sp, 12
	ret
; END:reset_game


  ;; TODO Insert your code here
font_data:
    .word 0xFC  ; 0
    .word 0x60  ; 1
    .word 0xDA  ; 2
    .word 0xF2  ; 3
    .word 0x66  ; 4
    .word 0xB6  ; 5
    .word 0xBE  ; 6
    .word 0xE0  ; 7
    .word 0xFE  ; 8
    .word 0xF6  ; 9

C_N_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_N_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_E_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_E_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_So_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

C_W_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_W_Y:
  .word 0x00
  .word 0x01
  .word 0x01

B_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_N_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_So_X:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

B_So_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_Y:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

T_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_E_X:
  .word 0x00
  .word 0x01
  .word 0x00

T_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_Y:
  .word 0x00
  .word 0x01
  .word 0x00

T_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_W_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_E_X:
  .word 0x00
  .word 0x01
  .word 0x01

S_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_So_X:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

S_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

S_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_W_Y:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

L_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_N_Y:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_E_X:
  .word 0x00
  .word 0x00
  .word 0x01

L_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_So_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0xFFFFFFFF

L_So_Y:
  .word 0x00
  .word 0x00
  .word 0x01

L_W_X:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_W_Y:
  .word 0x01
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

DRAW_Ax:                        ; address of shape arrays, x axis
    .word C_N_X
    .word C_E_X
    .word C_So_X
    .word C_W_X
    .word B_N_X
    .word B_E_X
    .word B_So_X
    .word B_W_X
    .word T_N_X
    .word T_E_X
    .word T_So_X
    .word T_W_X
    .word S_N_X
    .word S_E_X
    .word S_So_X
    .word S_W_X
    .word L_N_X
    .word L_E_X
    .word L_So_X
    .word L_W_X

DRAW_Ay:                        ; address of shape arrays, y_axis
    .word C_N_Y
    .word C_E_Y
    .word C_So_Y
    .word C_W_Y
    .word B_N_Y
    .word B_E_Y
    .word B_So_Y
    .word B_W_Y
    .word T_N_Y
    .word T_E_Y
    .word T_So_Y
    .word T_W_Y
    .word S_N_Y
    .word S_E_Y
    .word S_So_Y
    .word S_W_Y
    .word L_N_Y
    .word L_E_Y
    .word L_So_Y
    .word L_W_Y




