;; xmkit - a toolbox for parsing eXtended Modules

;; (c) 2019 Michael Neidel
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; This file contains definitions for file offsets, magic bytes, and other
;; specifications used by the eXtended Module (XM) file format.

;; offsets, magic bytes, etc.
(define xm:magicbytes "Extended Module: ")
(define xm:legal-version #x0104)
(define xm:offset-module-name 17)
(define xm:offset-tracker-name 38)
(define xm:offset-version-number 58)
(define xm:offset-header-size 60)
(define xm:offset-song-length 64)
(define xm:offset-restart-position 66)
(define xm:offset-number-of-tracks 68)
(define xm:offset-number-of-patterns 70)
(define xm:offset-number-of-instruments 72)
(define xm:offset-frequency-table-flags 74)
(define xm:offset-default-tempo 76)
(define xm:offset-default-bpm 78)
(define xm:offset-sequence 80)
(define xm:pattern-offset-rows 5)
(define xm:pattern-offset-packed-size 7)
(define xm:instr-offset-header-size 0)
(define xm:instr-offset-name 4)
(define xm:instr-offset-number-of-samples 27)
(define xm:instr-offset-sample-headers-size 29)
(define xm:instr-offset-sample-map 33)
(define xm:instr-offset-volume-env 129)
(define xm:instr-offset-panning-env 177)
(define xm:instr-offset-volume-env-length 225)
(define xm:instr-offset-panning-env-length 226)
(define xm:instr-offset-volume-sustain-point 227)
(define xm:instr-offset-volume-loop-start-point 228)
(define xm:instr-offset-volume-loop-end-point 229)
(define xm:instr-offset-panning-sustain-point 230)
(define xm:instr-offset-panning-loop-start-point 231)
(define xm:instr-offset-panning-loop-end-point 232)
(define xm:instr-offset-volume-type 233)
(define xm:instr-offset-panning-type 234)
(define xm:instr-offset-vibrato-type 235)
(define xm:instr-offset-vibrato-sweep 236)
(define xm:instr-offset-vibrato-depth 237)
(define xm:instr-offset-vibrato-rate 238)
(define xm:instr-offset-volume-fadeout 239)
(define xm:sample-offset-length 0)
(define xm:sample-offset-loop-start 4)
(define xm:sample-offset-loop-length 8)
(define xm:sample-offset-volume 12)
(define xm:sample-offset-finetune 13)
(define xm:sample-offset-loop-type 14)
(define xm:sample-offset-panning 15)
(define xm:sample-offset-relative-note 16)
(define xm:sample-offset-name 18)
(define xm:flag-note 1)
(define xm:flag-instrument 2)
(define xm:flag-volume 4)
(define xm:flag-fx-cmd 8)
(define xm:flag-fx-param 16)
(define xm:flag-packed-row #x80)
(define xm:flags-pattern-data
  (list xm:flag-note xm:flag-instrument xm:flag-volume xm:flag-fx-cmd
	xm:flag-fx-param))
(define xm:slot-note 0)
(define xm:slot-instrument 1)
(define xm:slot-volume 2)
(define xm:slot-fx-cmd 3)
(define xm:slot-fx-param 4)
(define xm:slots-pattern-data
  (list xm:slot-note xm:slot-instrument xm:slot-volume xm:slot-fx-cmd
	xm:slot-fx-param))
(define xm:unpacked-row-size 5)
(define xm:sample-header-size 40)
(define xm:module-name-length 20)
(define xm:tracker-name-length 20)
(define xm:instr-name-length 22)
(define xm:instr-sample-map-length 96)
(define xm:instr-vibrato-sine 0)
(define xm:instr-vibrato-square 1)
(define xm:instr-vibrato-saw 2)
(define xm:instr-vibrato-inverse-saw 3)
(define xm:sample-name-length 22)
(define xm:flag-env-on 1)
(define xm:flag-env-sustain 2)
(define xm:flag-env-loop 4)
(define xm:flag-sample-loop-forward 1)
(define xm:flag-sample-loop-ping-pong 2)
(define xm:flag-16bit-sample #x10)
(define xm:fx '((0xx 0) (1xx 1) (2xx 2) (3xx 3) (4xx 4) (5xx 5) (6xx 6)
		(7xx 7) (8xx 8) (9xx 9) (Axx 10) (Bxx 11) (Cxx 12)
		(Dxx 13) (Exx 14) (Fxx 15) (Gxx 16) (Hxx 17) (Kxx 20)
		(Lxx 21) (Pxx 25) (Rxx 27) (Txx 29) (Xxx 33)))
(define xm:extended-fx (zip '(E0x E1x E2x E3x E4x E5x E6x E7x E8x E9x EAx EBx
				  ECx EDx EEx EFx)
			    (iota 16 0 16)))
(define xm:fine-port-fx '((X1x #x10) (X2x #x20)))
(define xm:volume-fx '((+x #x70) (-x #x60) (Dx #x80) (Lx #xd0) (Mx #xf0)
		       (Px #xc0) (Rx #xe0) (Sx #xa0) (Ux #x90) (Vx #xb0)))
