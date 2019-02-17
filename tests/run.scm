;; xmkit unit tests

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


(use xmkit test simple-md5)

(define my-xm (xm:file->module "test.xm"))
(define my-ptn0 (xm:pattern-ref my-xm 0))
(define my-ptn1 (xm:pattern-ref my-xm 1))
(define my-ptn2 (xm:pattern-ref my-xm 2))
(define my-instr (xm:instrument-ref my-xm 1))
(define my-instr2 (xm:instrument-ref my-xm 2))
(define my-instr3 (xm:instrument-ref my-xm 3))
(define my-instr4 (xm:instrument-ref my-xm 4))
(define my-smp (xm:instrument-sample-ref my-xm 1 0))
(define my-smp3 (xm:instrument-sample-ref my-xm 3 0))
(define my-smp4 (xm:instrument-sample-ref my-xm 4 3))

(test-group
 "Module Parser"
 (test-assert "xm:is-valid-xm-file?" (xm:is-valid-xm-file? "test.xm"))
 (test "xm:module-name" "xmkit test case" (xm:module-name my-xm))
 (test "xm:tracker-name" "MilkyTracker 1.00.00" (xm:tracker-name my-xm))
 (test "xm:song-length" 6 (xm:song-length my-xm))
 (test "xm:restart-position" 1 (xm:restart-position my-xm))
 (test "xm:number-of-tracks" 8 (xm:number-of-tracks my-xm))
 (test "xm:number-of-patterns" 3 (xm:number-of-patterns my-xm))
 (test "xm:number-of-instruments" 4 (xm:number-of-instruments my-xm))
 (test-assert "xm:use-linear-frequency-table?"
   (xm:use-linear-frequency-table? my-xm))
 (test "xm:default-tempo" 6 (xm:default-tempo my-xm))
 (test "xm:default-bpm" 125 (xm:default-bpm my-xm))
 (test "xm:sequence" '(0 1 2 2 2 1) (xm:sequence my-xm))
 (test "xm:sequence-ref" 1 (xm:sequence-ref my-xm 5))
 (test-assert "xm:pattern-used?" (xm:pattern-used? my-xm 2)))

(test-group
 "Pattern Parser"
 (test "xm:pattern-tracks" '(57 4 #f 3 16)
       (list-ref (list-ref (xm:pattern-tracks my-ptn2) 1) 4))
 (test "xm:pattern-row-ref" '(57 4 #f 3 16)
       (list-ref (xm:pattern-row-ref my-ptn2 4) 1))
 (test "xm:pattern-notes" '(#f #f 75 #f 62 40 64 #f)
       (list-ref (xm:pattern-notes my-ptn0) #x1a))
 (test "xm:pattern-instruments" '(#f #f 3 4 3 #f #f #f)
       (list-ref (xm:pattern-instruments my-ptn2) #x14))
 (test "xm:pattern-volumes-normalized" '(#x3d #x30 #x20 #x10 #f #f #f #f)
       (list-ref (xm:pattern-volumes-normalized my-ptn2) #x1e))
 (test "xm:pattern-volume-fx" '(#f 113 #f 98 #f #f #f #f)
       (list-ref (xm:pattern-volume-fx my-ptn2 '+x '-x) #xd))
 (test "xm:pattern-fx-params" '(1 2 3 #f #f #f #f 7)
       (list-ref (xm:pattern-fx-params my-ptn2 '4xx 'Axx) #x10))
 (test "xm:pattern-track-ref" '(68 3 #f 14 1)
       (list-ref (xm:pattern-track-ref my-ptn1 2) 8))
 (test "xm:pattern-track-notes" 546
       (apply + (filter number? (xm:pattern-track-notes my-ptn1 2))))
 (test "xm:pattern-track-instruments" 56
       (apply + (filter number? (xm:pattern-track-instruments my-ptn1 1))))
 (test "xm:pattern-track-volumes" 1521
       (apply + (filter number? (xm:pattern-track-volumes my-ptn2 1))))
 (test "xm:pattern-track-volumes-normalized" #x60
       (apply + (filter number?
			(xm:pattern-track-volumes-normalized my-ptn0 8))))
 (test "xm:pattern-track-volume-fx" 342
       (apply + (filter number?
			(xm:pattern-track-volume-fx my-ptn0 8 '+x '-x 'Dx))))
 (test "xm:pattern-track-fx-cmds" 120
       (apply + (filter number? (xm:pattern-track-fx-cmds my-ptn2 4))))
 (test "xm:pattern-track-fx-params" 336
       (apply + (filter number? (xm:pattern-track-fx-params my-ptn2 2))))
 (test "xm:pattern-track-fx" 509
       (let ((rows (xm:pattern-track-fx my-ptn2 4)))
	 (apply + (filter number? (append (map car rows) (map cadr rows)))))))

(test-group
 "Instrument Parser"
 (test "xm:instrument-name" "1x8bit smp+env+v3D+p81"
       (xm:instrument-name my-instr))
 (test "xm:instrument-number-of-samples" 1
       (xm:instrument-number-of-samples my-instr))
 (test-assert "xm:instrument-has-samples?"
   (xm:instrument-has-samples? my-instr))
 (test-assert "xm:instrument-has-samples?: empty instrument"
   (not (xm:instrument-has-samples? my-instr2)))
 (test "xm:instrument-sample-map: empty" 0
       (apply + (xm:instrument-sample-map my-instr)))
 (test "xm:instrument-sample-map: mapped" 20
       (apply + (xm:instrument-sample-map my-instr4)))
 (test "xm:instrument-volume-env-length" 5
       (xm:instrument-volume-env-length my-instr))
 (test "xm:instrument-volume-envelope"
       '((0 37) (32 64) (44 41) (52 64) (169 27))
       (xm:instrument-volume-envelope my-instr))
 (test "xm:instrument-volume-sustain-point" 0
       (xm:instrument-volume-sustain-point my-instr))
 (test "xm:instrument-volume-loop-start" 1
       (xm:instrument-volume-loop-start my-instr))
 (test "xm:instrument-volume-loop-end" 4
	(xm:instrument-volume-loop-end my-instr))
 (test "xm:instrument-volume-type" 5
       (xm:instrument-volume-type my-instr))
 (test-assert "xm:instrument-volume-env-on?"
   (xm:instrument-volume-env-on? my-instr))
 (test-assert "xm:instrument-volume-env-sustain?"
   (not (xm:instrument-volume-env-sustain? my-instr)))
 (test-assert "xm:instrument-volume-env-loop?"
   (xm:instrument-volume-env-loop? my-instr))
 (test "xm:instrument-volume-fadeout: cut" #x7fff
       (xm:instrument-volume-fadeout my-instr))
 (test "xm:instrument-volume-fadeout: #xffe" #xffe
       (xm:instrument-volume-fadeout my-instr4))
 (test "xm:instrument-panning-env-length" 6
       (xm:instrument-panning-env-length my-instr))
 (test "xm:instrument-panning-envelope"
       '((0 32) (32 32) (37 52) (52 32) (95 8) (179 32))
       (xm:instrument-panning-envelope my-instr))
 (test "xm:instrument-panning-sustain-point" 5
       (xm:instrument-panning-sustain-point my-instr))
 (test "xm:instrument-panning-loop-start" 0
       (xm:instrument-panning-loop-start my-instr))
 (test "xm:instrument-panning-loop-end" 1
	(xm:instrument-panning-loop-end my-instr))
 (test "xm:instrument-panning-type" 3
       (xm:instrument-panning-type my-instr))
 (test-assert "xm:instrument-panning-env-on?"
   (xm:instrument-panning-env-on? my-instr))
 (test-assert "xm:instrument-panning-env-sustain?"
   (xm:instrument-panning-env-sustain? my-instr))
 (test-assert "xm:instrument-panning-env-loop?"
   (not (xm:instrument-panning-env-loop? my-instr)))
 (test "xm:instrument-vibrato-waveform: sine" 0
       (xm:instrument-vibrato-waveform my-instr))
 (test "xm:instrument-vibrato-waveform: square" 1
       (xm:instrument-vibrato-waveform my-instr3))
 (test-assert "xm:instrument-vibrato-sine?"
   (xm:instrument-vibrato-sine? my-instr))
 (test-assert "xm:instrument-vibrato-square?"
   (not (xm:instrument-vibrato-square? my-instr)))
 (test-assert "xm:instrument-vibrato-saw?"
   (not (xm:instrument-vibrato-saw? my-instr)))
 (test-assert "xm:instrument-vibrato-inverse-saw?"
   (not (xm:instrument-vibrato-inverse-saw? my-instr)))
 (test "xm:instrument-vibrato-depth" 0
       (xm:instrument-vibrato-depth my-instr))
 (test "xm:instrument-vibrato-rate" 0
       (xm:instrument-vibrato-rate my-instr))
 (test "xm:instrument-vibrato-sweep" 0
       (xm:instrument-vibrato-sweep my-instr)))

(test-group
 "Sample Parser"
 (test "xm:sample-data-length" #x80 (xm:sample-length my-smp))
 (test "xm:sample-name" "tri, fwd loop" (xm:sample-name my-smp))
 (test "xm:sample-loop-start" #x28 (xm:sample-loop-start my-smp))
 (test "xm:sample-loop-length" #x41 (xm:sample-loop-length my-smp))
 (test "xm:sample-loop-type" 1 (xm:sample-loop-type my-smp))
 (test-assert "xm:sample-loop-on" (xm:sample-loop-enabled? my-smp))
 (test-assert "xm:sample-loop-forward" (xm:sample-loop-forward? my-smp))
 (test-assert "xm:sample-loop-ping-pong"
   (not (xm:sample-loop-ping-pong? my-smp)))
 (test-assert "xm:sample-16bit-data?" (not (xm:sample-16bit-data? my-smp)))
 (test "xm:sample-volume" #x3d (xm:sample-volume my-smp))
 (test "xm:sample-finetune" -4 (xm:sample-finetune (xm:sample-ref my-instr4 0)))
 (test "xm:sample-panning" #x81 (xm:sample-panning my-smp))
 (test "xm:sample-relative-note" -2 (xm:sample-relative-note my-smp))
 (test "xm:sample->dpcm" -5 (apply + (xm:sample->dpcm my-smp)))
 (test "xm:sample->pcm: 8-bit data" -18 (apply + (xm:sample->pcm my-smp)))
 (test "xm:sample->pcm: 16-bit data" -46848 (apply + (xm:sample->pcm my-smp3)))
 (test "xm:sample->pcm: mapped" -36 (apply + (xm:sample->pcm my-smp4)))
 (test "xm:export-sample:" "a090c56015b803b3a8b2bd2e3afee9c2"
       (begin (xm:export-sample my-smp4 "sample.raw")
	      (file-md5sum "sample.raw"))))

(test-exit)
