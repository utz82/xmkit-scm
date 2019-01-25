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

;;; [[tags: egg]]
;;;
;;; == xmkit
;;;
;;; [[toc:]]
;;;
;;; === Description
;;;
;;; The xmkit egg provides a toolbox for extracting information from
;;; [[https://en.wikipedia.org/wiki/XM_(file_format)|eXtended Module (XM)]]
;;; files.
;;;
;;; === Requirements
;;;
;;; Those wishing to build the documentation files locally will need
;;; [[https://github.com/utz82/scm2wiki|scm2wiki]], and optionally
;;; [[manual-labor]].
;;;
;;; === Documentation
;;;
;;; ==== General Notes
;;;
;;; xmkit preserves some quirks of XM terminology when it comes to indices.
;;; Specifically, indices for pattern tracks and instruments start at 1, whereas
;;; everything else uses 0-based indexing.
;;;
;;; The order list, pattern list, or song structure is called {{sequence}} in
;;; xmkit.
;;;

(module xmkit
    (xm:u8vector->module
     xm:file->module
     xm:is-valid-xm?
     xm:module-name
     xm:tracker-name
     xm:song-length
     xm:restart-position
     xm:number-of-tracks
     xm:number-of-patterns
     xm:number-of-instruments
     xm:use-linear-frequency-table?
     xm:default-tempo
     xm:default-bpm
     xm:sequence
     xm:sequence-ref
     xm:pattern-used?
     xm:patterns
     xm:pattern-data-ref
     xm:pattern-row-ref
     xm:pattern-track-ref
     xm:pattern-track-notes
     xm:pattern-track-instruments
     xm:pattern-track-volumes
     xm:pattern-track-volumes-normalized
     xm:pattern-track-volume-fx
     xm:pattern-track-fx
     xm:pattern-track-fx-cmds
     xm:pattern-track-fx-params
     xm:instrument-name
     xm:instrument-number-of-samples
     xm:instruments
     xm:instrument-ref
     xm:instrument-sample-ref
     xm:instrument-name
     xm:instrument-number-of-samples
     xm:instrument-has-samples?
     xm:instrument-sample-map
     xm:instrument-volume-type
     xm:instrument-volume-envelope
     xm:instrument-volume-env-length
     xm:instrument-volume-env-on?
     xm:instrument-volume-env-sustain?
     xm:instrument-volume-env-loop?
     xm:instrument-volume-sustain-point
     xm:instrument-volume-loop-start
     xm:instrument-volume-loop-end
     xm:instrument-volume-fadeout
     xm:instrument-panning-type
     xm:instrument-panning-envelope
     xm:instrument-panning-env-length
     xm:instrument-panning-env-on?
     xm:instrument-panning-env-sustain?
     xm:instrument-panning-env-loop?
     xm:instrument-panning-sustain-point
     xm:instrument-panning-loop-start
     xm:instrument-panning-loop-end
     xm:instrument-vibrato-waveform
     xm:instrument-vibrato-sine?
     xm:instrument-vibrato-square?
     xm:instrument-vibrato-saw?
     xm:instrument-vibrato-inverse-saw?
     xm:instrument-vibrato-depth
     xm:instrument-vibrato-rate
     xm:instrument-vibrato-sweep
     xm:samples
     xm:sample-ref
     xm:sample-length
     xm:sample-name
     xm:sample-loop-start
     xm:sample-loop-length
     xm:sample-loop-type
     xm:sample-loop-enabled?
     xm:sample-loop-forward?
     xm:sample-loop-ping-pong?
     xm:sample-16bit-data?
     xm:sample-volume
     xm:sample-finetune
     xm:sample-panning
     xm:sample-relative-note
     xm:sample->dpcm
     xm:sample->pcm
     xm:export-sample)

  (import chicken scheme)
  (use srfi-1 srfi-4 srfi-13 extras data-structures)
  (include "definitions.scm")

  ;; All xm objects are internally treated as a single record type that is just
  ;; a thin wrapper around the binary content of a module/pattern/instrument
  ;; and a flag to distinguish the data type.
  ;; The constructor is inaccessible outside the xm Scheme module, to enforce
  ;; additional checks on construction.
  (define-record-type xm:data
    (xm:make-data type bytes)
    xm:data?
    (type xm:data-type)
    (bytes xm:data-bytes))

  (define-record-printer (xm:data data out)
    (fprintf out "#<~s>" (xm:data-type data)))

  ;; could short circuit this with an AND, but that would mean relying on
  ;; evaluation order which is not specified by Scheme standard
  (define (xm:module? xmdata)
    (if (xm:data? xmdata)
	(eq? (xm:data-type xmdata) 'xm:module)
	#f))

  (define (xm:pattern? xmdata)
    (if (xm:data? xmdata)
	(eq? (xm:data-type xmdata) 'xm:pattern)
	#f))

  (define (xm:instrument? xmdata)
    (if (xm:data? xmdata)
	(eq? (xm:data-type xmdata) 'xm:instrument)
	#f))

  (define (xm:sample? xmdata)
    (if (xm:data? xmdata)
	(eq? (xm:data-type xmdata) 'xm:sample)
	#f))

  ;; aliases for constructing xm:data records of various types from a u8vector
  (define (xm:make-module u8v)
    (xm:make-data 'xm:module u8v))

  (define (xm:make-pattern u8v)
    (xm:make-data 'xm:pattern u8v))

  (define (xm:make-instrument u8v)
    (xm:make-data 'xm:instrument u8v))

  (define (xm:make-sample u8v)
    (xm:make-data 'xm:sample u8v))

  (define (xm:data-size xmdata)
    (u8vector-length (xm:data-bytes xmdata)))

  ;; check if all flags given in mask are set
  (define (xm:flags-set? mask val)
    (= mask (bitwise-and mask val)))

  ;; convert a flag to a 1 if set, else to a 0
  (define (xm:flag->1 mask val)
    (if (xm:flags-set? mask val)
	1 0))

  ;; convert unsigned int represented in an arbitrary number of bits to its
  ;; signed counterpart, where posint-max is the largest positive number that
  ;; can be represented with the given bit width, eg. #x7f for u8, #x7fff for
  ;; u16
  (define (xm:signed val posint-max)
    (if (> val posint-max)
	(- (+ 1 (bitwise-and posint-max (bitwise-not val))))
	val))

  ;; read a signed byte from an xm:data object
  (define (xm:read-s8 xmdata offset)
    (xm:signed (xm:read-u8 xmdata offset) #x7f))

  ;; read a signed word from an xm:data object
  (define (xm:read-s16 xmdata offset)
    (xm:signed (xm:read-u16 xmdata offset) #x7fff))

  ;; read an unsigned byte from an xm:data object
  (define (xm:read-u8 xmdata offset)
    (u8vector-ref (xm:data-bytes xmdata) offset))

  ;; read an unsigned word from a u8vector
  (define (xm:read-u16-raw u8v offset)
    (+ (u8vector-ref u8v offset)
       (* 256 (u8vector-ref u8v (+ 1 offset)))))

  ;; read an unsigned word from an xm:data object
  (define (xm:read-u16 xmdata offset)
    (xm:read-u16-raw (xm:data-bytes xmdata) offset))

  ;; read an unsigned dword from a u8vector
  (define (xm:read-u32-raw u8v offset)
    (+ (xm:read-u16-raw u8v offset)
       (* #x10000 (xm:read-u16-raw u8v (+ 2 offset)))))

  ;; read an unsigned dword from an xm:data object
  (define (xm:read-u32 xmdata offset)
    (xm:read-u32-raw (xm:data-bytes xmdata) offset))

  ;; drop the leading n-1 bytes from the given u8vector
  (define (xm:drop-bytes u8v n)
    (subu8vector u8v n (u8vector-length u8v)))

  ;; merge two u8vectors
  (define (xm:merge-u8vec v1 v2)
    (list->u8vector (append (u8vector->list v1)
			    (u8vector->list v2))))

  (define (xm:read-string xmdata offset len)
    (blob->string
     (u8vector->blob
      (list->u8vector
       (filter (lambda (x)
		 (not (equal? x #x0)))
	       (u8vector->list (subu8vector (xm:data-bytes xmdata)
					    offset
					    (+ offset len))))))))

  ;;;
  ;;; ==== Module Related Procedures

  ;;; Checks if the given {{xm}} record contains a well-formed eXtended Module.
  (define (xm:is-valid-xm? xm)
    (and (= (xm:read-u8 xm 37) #x1a)
	 (= (xm:read-u16 xm xm:offset-version-number) xm:legal-version)
	 (string=? (xm:read-string xm 0 17) xm:magicbytes)))

  ;;; Constructs an xm:module record from a u8vector.
  (define (xm:u8vector->module u8v)
    (let ((mod (xm:make-module u8v)))
      (if (xm:is-valid-xm? mod)
	  mod
	  (error "Not a valid eXtended Module"))))

  ;; return the header size, including the first 60 bytes
  (define (xm:header-size xm)
    (+ (xm:read-u32 xm xm:offset-header-size)
       xm:offset-header-size))

  ;;; Construct an xm:module record from an .xm file
  (define (xm:file->module filename)
    (xm:u8vector->module (with-input-from-file filename read-u8vector)))

  ;;; Returns the module name.
  (define (xm:module-name xm)
    (xm:read-string xm xm:offset-module-name xm:module-name-length))

  ;;; Returns the tracker identifier string.
  (define (xm:tracker-name xm)
    (xm:read-string xm xm:offset-tracker-name xm:tracker-name-length))

  ;;; Returns the song (sequence) length.
  (define (xm:song-length xm)
    (xm:read-u8 xm xm:offset-song-length))

  ;;; Returns the restart position.
  (define (xm:restart-position xm)
    (xm:read-u8 xm xm:offset-restart-position))

  ;;; Returns the number of tracks (channels).
  (define (xm:number-of-tracks xm)
    (xm:read-u8 xm xm:offset-number-of-tracks))

  ;;; Returns the number of patterns.
  (define (xm:number-of-patterns xm)
    (xm:read-u8 xm xm:offset-number-of-patterns))

  ;;; Returns the number of instruments.
  (define (xm:number-of-instruments xm)
    (xm:read-u8 xm xm:offset-number-of-instruments))

  ;;; Check whether the module uses linear or Amiga frequencies. Returns true
  ;;; if using a linear table.
  (define (xm:use-linear-frequency-table? xm)
    (equal? 1 (xm:read-u8 xm xm:offset-frequency-table-flags)))

  ;;; Returns the default tempo.
  (define (xm:default-tempo xm)
    (xm:read-u8 xm xm:offset-default-tempo))

  ;;; Returns the default BPM.
  (define (xm:default-bpm xm)
    (xm:read-u8 xm xm:offset-default-bpm))

  ;;; Returns the sequence (order list) as a list.
  (define (xm:sequence xm)
    (u8vector->list
     (subu8vector (xm:data-bytes xm)
		  xm:offset-sequence
		  (+ xm:offset-sequence (xm:song-length xm)))))

  ;;; Returns the position {{pos}} in the sequence of {{xm}}.
  (define (xm:sequence-ref xm pos)
    (if (< pos (xm:song-length xm))
	(list-ref (xm:sequence xm) pos)
	(error "Invalid sequence position")))

  ;;; Check if the given {{pattern}} is used in {{xm}}. A pattern is considered
  ;;; used if it is linked in the sequence at least once.
  (define (xm:pattern-used? xm pattern)
    (member pattern (xm:sequence xm)))

  ;;;
  ;;; ==== Pattern Related Procedures
  ;;;
  ;;; Note the deliberate omission of a pattern-ref equivalent to the other
  ;;; *-ref procedures. This is because compressed XM pattern data is
  ;;; meaningless without knowledge of the containing module.

  ;;; Extract the module's patterns. Returns a list of xm:pattern records.
  (define (xm:patterns xm)
    (letrec* ((pattern-size
	       (lambda (init-offset)
		 (+ (xm:read-u32 xm init-offset)
		    (xm:read-u16 xm (+ init-offset
				       xm:pattern-offset-packed-size)))))
	      (extract-patterns
  	       (lambda (init-offset remaining)
  		 (if (= 0 remaining)
  		     '()
		     (let ((pattern-end (+ init-offset
					   (pattern-size init-offset))))
  		       (cons (xm:make-pattern
			      (subu8vector (xm:data-bytes xm)
					   init-offset pattern-end))
  			     (extract-patterns pattern-end
					       (- remaining 1))))))))
      (extract-patterns (xm:header-size xm)
  			(xm:number-of-patterns xm))))

  (define (xm:raw-pattern-ref xm i)
    (if (< i (xm:number-of-patterns xm))
	(list-ref (xm:patterns xm) i)
	(error: "Pattern does not exist")))

  ;; Returns the number of rows in the given pattern.
  ;; Accepts either a raw pattern (xm:data) or unpacked pattern as input.
  (define (xm:pattern-length pattern)
    (if (xm:pattern? pattern)
	(xm:read-u16 pattern xm:pattern-offset-rows)
	(length pattern)))

  ;; return the size of the first packed track segment of the first packed
  ;; pattern row in the given pattern data block
  (define (xm:packed-track-row-size data)
    (let ((flag->1 (lambda (mask)
		     (xm:flag->1 mask (u8vector-ref data 0)))))
      (if (= 0 (flag->1 xm:flag-packed-row))
	  xm:unpacked-row-size
	  (+ 1 (flag->1 xm:flag-note) (flag->1 xm:flag-instrument)
	     (flag->1 xm:flag-volume) (flag->1 xm:flag-fx-cmd)
	     (flag->1 xm:flag-fx-param)))))

  ;; return the size of the first packed pattern row in the given pattern data
  ;; block
  (define (xm:packed-row-size data tracks)
    (if (= 0 tracks)
	0
	(let ((next-ch-size (xm:packed-track-row-size data)))
	  (+ next-ch-size
	     (xm:packed-row-size (xm:drop-bytes data next-ch-size)
				 (- tracks 1))))))

  ;; determine offset of the given slot (note, instrument, volume, etc)
  ;; counting from the flag byte
  (define (xm:packed-offset flag-byte slot)
    (apply + (map (lambda (pos)
		    (xm:flag->1 pos flag-byte))
		  (take xm:flags-pattern-data (+ 1 slot)))))

  ;; auxiliary function for unpacking compressed pattern data
  ;; unpacks one pattern row from the given block of data that starts with a
  ;; note or flag byte
  ;; unset values are returned as #f, except for unset fx parameters after set
  ;; fx commands, which are normalized to 0.
  (define (xm:unpack-track-row data)
    (let ((flag-byte (u8vector-ref data 0)))
      (if (xm:flags-set? xm:flag-packed-row flag-byte)
	  (let ((unpacked-row
		 (map (lambda (flag slot)
			(if (xm:flags-set? flag flag-byte)
			    (u8vector-ref data (xm:packed-offset flag-byte slot))
			    #f))
		      xm:flags-pattern-data xm:slots-pattern-data)))
	    (if (and (cadddr unpacked-row)
		     (not (list-ref unpacked-row 4)))
		(append (take unpacked-row 4) '(0))
		unpacked-row))
	  ;; if row is unpacked, just copy bytes to resulting list
	  (u8vector->list (subu8vector data 0 xm:unpacked-row-size)))))

  ;; unpack a row of raw pattern data
  (define (xm:unpack-row data tracks)
    (if (= 0 tracks)
	'()
	(cons (xm:unpack-track-row data)
	      (xm:unpack-row
	       (xm:drop-bytes data (xm:packed-track-row-size data))
	       (- tracks 1)))))

  ;; unpack raw pattern data into a list of rows, where each row is a list
  ;; containing values for note, instrument, volume, fx command, and fx param
  (define (xm:unpack-pattern pattern tracks)
    (letrec
	((unpack-rows
	  (lambda (init-offset)
	    (if (>= init-offset (sub1 (xm:data-size pattern)))
		'()
		(let ((data-block (xm:drop-bytes (xm:data-bytes pattern)
						 init-offset)))
		  (cons (xm:unpack-row data-block tracks)
			(unpack-rows (+ init-offset
					(xm:packed-row-size data-block
							    tracks)))))))))
      ;; dword at offset 0 specifies header size
      (unpack-rows (xm:read-u32 pattern 0))))

  ;;; Get the given pattern as a nested list of rows, which are in turn given
  ;;; as a nested list of track values.
  ;;; Any track values that are not set are returned as {{#f}}.
  (define (xm:pattern-data-ref xm pattern)
    (if (>= pattern (xm:number-of-patterns xm))
	(error "Pattern does not exist")
	(xm:unpack-pattern (xm:raw-pattern-ref xm pattern)
			   (xm:number-of-tracks xm))))

  ;;; Get the {{row}} of {{pattern}} as a nested list of track values.
  (define (xm:pattern-row-ref xm pattern row)
    (if (>= pattern (xm:number-of-patterns xm))
	(error "Pattern does not exist")
	(let ((raw-pattern (xm:raw-pattern-ref xm pattern)))
	  (if (>= row (xm:pattern-length raw-pattern))
	      (error "Row does not exist")
	      (list-ref (xm:unpack-pattern raw-pattern (xm:number-of-tracks xm))
			row)))))

  ;;; Get the given {{track}} of the {{pattern}} as a nested list of values.
  ;;; Note that track indices are 1-based, in line with XM terminology.
  (define (xm:pattern-track-ref xm pattern track)
    (cond ((>= pattern (xm:number-of-patterns xm))
	   (error "Pattern does not exist"))
	  ((or (> track (xm:number-of-tracks xm))
	       (= track 0))
	   (error "Track does not exist"))
	  (else (let ((raw-pattern (xm:raw-pattern-ref xm pattern)))
		  (map (lambda (row)
			 (list-ref row (- track 1)))
		       (xm:unpack-pattern raw-pattern
					  (xm:number-of-tracks xm)))))))

  ;;; Extract the note column of the given {{track}} in the given {{pattern}}.
  (define (xm:pattern-track-notes xm pattern track)
    (map car (xm:pattern-track-ref xm pattern track)))

  ;;; Extract the instrument column of the given {{track}} in the given
  ;;; {{pattern}}.
  (define (xm:pattern-track-instruments xm pattern track)
    (map cadr (xm:pattern-track-ref xm pattern track)))

  ;;; Extract the volume column of the given {{track}} in the given {{pattern}}.
  (define (xm:pattern-track-volumes xm pattern track)
    (map caddr (xm:pattern-track-ref xm pattern track)))

  ;;; Extract the volume column of the given {{track}} in the given {{pattern}},
  ;;; and normalize volumes to the 0..#x40 range, omitting volume column fx.
  (define (xm:pattern-track-volumes-normalized xm pattern track)
    (map (lambda (v)
	   (if v
	       (if (and (>= v #x10)
			(<= v #x50))
		   (- v #x10)
		   #f)
	       #f))
	 (xm:pattern-track-volumes xm pattern track)))

  ;;; Extract the volume effects of the given {{track}} in the given
  ;;; {{pattern}}. The output can optionally be filtered to return only the
  ;;; given {{effects}}. {{effects}} can be any combination of '+x, '-x',
  ;;; Dx', Lx', 'Mx, 'Px, 'Rx, 'Sx, 'Ux, and 'Vx.
  (define (xm:pattern-track-volume-fx xm pattern track . effects)
    (let ((volumes (xm:pattern-track-volumes xm pattern track))
	  (filter-pred (if (null? effects)
			   (lambda (v)
			     (if v
				 (if (>= v #x60) v #f)
				 #f))
			   (let ((filter-list
				  (map (lambda (fx)
					 (car (alist-ref fx xm:volume-fx)))
				       effects)))
			     (lambda (v)
			       (if v
				   (if (member (bitwise-and v #xf0)
					       filter-list)
				       v #f)
				   #f))))))
      (map filter-pred volumes)))

  ;;; Extract the effect command/parameter columns of the given {{track}} in
  ;;; the given {{pattern}}. The output can optionally be filtered to return
  ;;; only the given {{effects}}. For example,
  ;;; <enscript highlight="scheme">
  ;;; (xm:pattern-track-fx my-xm 0 1 '1xx '2xx '3xx)</enscript>
  ;;; will only return portamento effects. All common effects (0xx, 1xx, ..
  ;;; Fxx) are supported, as well as the extended effects (E0x, E1x, ... EFx),
  ;;; and the fine portamento effects (X1x, X2x).
  (define (xm:pattern-track-fx xm pattern track . effects)
    (let ((pattern-fx (map cdddr (xm:pattern-track-ref xm pattern track))))
      (if (null? effects)
	  pattern-fx
	  (let* ((make-filter-list
		  (lambda (fx-alist)
		    (map (lambda (f) (car (alist-ref f fx-alist)))
			 (filter (lambda (f) (member f (map car fx-alist)))
				 effects))))
		 (regular-fx (make-filter-list xm:fx))
		 (ext-fx (make-filter-list xm:extended-fx))
		 (port-fx (make-filter-list xm:fine-port-fx)))
	    (map (lambda (cmd/param)
		   (if (or (member (car cmd/param) regular-fx)
			   (and (equal? #x0e (car cmd/param))
				(member (bitwise-and #xf0 (cadr cmd/param))
					ext-fx))
			   (and (equal? (car (alist-ref 'Xxx xm:fx))
					(car cmd/param))
				(member (bitwise-and #x30 (cadr cmd/param))
					port-fx)))
		       cmd/param
		       '(#f #f)))
		 pattern-fx)))))

  ;;; Extract the effect command column of the given {{track}} in the given
  ;;; {{pattern}}. The output can optionally be filtered to return on the
  ;;; {{effects}}. See xm:pattern-track-fx for details.
  (define (xm:pattern-track-fx-cmds xm pattern track . effects)
    (map car (apply xm:pattern-track-fx (append (list xm pattern track)
						effects))))

  ;;; Extract the effect paramter column of the given {{track}} in the given
  ;;; {{pattern}}. The output can optionally be filtered to return only the
  ;;; parameters of the given {{effects}}. See xm:pattern-track-fx for details.
  (define (xm:pattern-track-fx-params xm pattern track . effects)
    (map cadr (apply xm:pattern-track-fx (append (list xm pattern track)
						 effects))))

  ;;;
  ;;; ==== Instrument Related Procedures
  ;;;
  ;;; These procedures will generally return 0 or null if the given instrument
  ;;; has no samples.
  ;;;

  (define (xm:instrument-header-size instr)
    (xm:read-u32 instr xm:instr-offset-header-size))

  ;;; Returns the instrument name.
  (define (xm:instrument-name instr)
    (xm:read-string instr xm:instr-offset-name xm:instr-name-length))

  ;;; Returns the number of samples in the given instrument.
  (define (xm:instrument-number-of-samples instr)
    (xm:read-u16 instr xm:instr-offset-number-of-samples))

  ;;; Returns {{#t}} if the given instrument contains samples, else {{#f}}.
  (define (xm:instrument-has-samples? instr)
    (> (xm:instrument-number-of-samples instr) 0))

  (define (xm:instrument-sample-header-size instr)
    (if (xm:instrument-has-samples? instr)
	(* (xm:instrument-number-of-samples instr)
	   (xm:read-u32 instr xm:instr-offset-sample-headers-size))
	0))

  ;;; Sample to note mapping for all notes. Returns a list.
  (define (xm:instrument-sample-map instr)
    (if (xm:instrument-has-samples? instr)
	(u8vector->list
	 (subu8vector (xm:data-bytes instr) xm:instr-offset-sample-map
		      (+ xm:instr-offset-sample-map
			 xm:instr-sample-map-length)))
	'()))

  ;;; Returns the length of the volume envelope.
  (define (xm:instrument-volume-env-length instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-volume-env-length)
	0))

  ;; convenience proc to convert volume/panning envelopes to offset/value pairs
  (define (xm:raw-env->pt/val raw-env)
    (if (= 0 (u8vector-length raw-env))
	'()
	(cons (list (xm:read-u16-raw raw-env 0)
		    (xm:read-u16-raw raw-env 2))
	      (xm:raw-env->pt/val (xm:drop-bytes raw-env 4)))))

  ;; call to xm:instrument-has-samples? may seem redundant, but is necessary
  ;; to prevent a potential out-of-range error from subu8vector
  ;;; Returns the volume envelope as a list of offset/value pairs.
  (define (xm:instrument-volume-envelope instr)
    (if (xm:instrument-has-samples? instr)
	(xm:raw-env->pt/val
	 (subu8vector (xm:data-bytes instr) xm:instr-offset-volume-env
		      (+ xm:instr-offset-volume-env
			 (* 4 (xm:instrument-volume-env-length instr)))))
	'()))

  ;;; Returns the volume envelope sustain point.
  (define (xm:instrument-volume-sustain-point instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-volume-sustain-point)
	0))

  ;;; Returns the volume envelope loop start point.
  (define (xm:instrument-volume-loop-start instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-volume-loop-start-point)
	0))

  ;;; Returns the volume envelope loop end point.
  (define (xm:instrument-volume-loop-end instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-volume-loop-end-point)
	0))

  ;;; Returns the volume envelope configuration byte.
  (define (xm:instrument-volume-type instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-volume-type)
	0))

  ;;; Returns {{#t}} if the volume envelope is enabled.
  (define (xm:instrument-volume-env-on? instr)
    (xm:flags-set? xm:flag-env-on (xm:instrument-volume-type instr)))

  ;;; Returns {{#t}} if volume envelope sustain is enabled.
  (define (xm:instrument-volume-env-sustain? instr)
    (xm:flags-set? xm:flag-env-sustain (xm:instrument-volume-type instr)))

  ;;; Returns {{#t}} if volume envelope looping is enabled.
  (define (xm:instrument-volume-env-loop? instr)
    (xm:flags-set? xm:flag-env-loop (xm:instrument-volume-type instr)))

  ;;; Returns the instrument volume fadeout setting.
  (define (xm:instrument-volume-fadeout instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u16 instr xm:instr-offset-volume-fadeout)
	0))

  ;;; Returns the length of the panning envelope.
  (define (xm:instrument-panning-env-length instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-panning-env-length)
	0))

  ;;; Returns the panning envelope as a list of offset/value pairs.
  (define (xm:instrument-panning-envelope instr)
    (if (xm:instrument-has-samples? instr)
	(xm:raw-env->pt/val
	 (subu8vector (xm:data-bytes instr) xm:instr-offset-panning-env
		      (+ xm:instr-offset-panning-env
			 (* 4 (xm:instrument-panning-env-length instr)))))
	'()))

  ;;; Returns the panning envelope sustain point.
  (define (xm:instrument-panning-sustain-point instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-panning-sustain-point)
	0))

  ;;; Returns the panning envelope loop start point.
  (define (xm:instrument-panning-loop-start instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-panning-loop-start-point)
	0))

  ;;; Returns the panning envelope loop end point.
  (define (xm:instrument-panning-loop-end instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-panning-loop-end-point)
	0))

  ;;; Returns the panning envelope configuration byte.
  (define (xm:instrument-panning-type instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-panning-type)
	0))

  ;;; Returns {{#t}} if the panning envelope is enabled.
  (define (xm:instrument-panning-env-on? instr)
    (xm:flags-set? xm:flag-env-on (xm:instrument-panning-type instr)))

  ;;; Returns {{#t}} if panning envelope sustain is enabled.
  (define (xm:instrument-panning-env-sustain? instr)
    (xm:flags-set? xm:flag-env-sustain (xm:instrument-panning-type instr)))

  ;;; Returns {{#t}} if panning envelope looping is enabled.
  (define (xm:instrument-panning-env-loop? instr)
    (xm:flags-set? xm:flag-env-loop (xm:instrument-panning-type instr)))

  ;; Returns the instrument vibrato waveform byte
  (define (xm:instrument-vibrato-waveform instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-vibrato-type)
	0))

  ;;; Returns {{#t}} if using sine waveform vibrato
  (define (xm:instrument-vibrato-sine? instr)
    (= xm:instr-vibrato-sine (xm:instrument-vibrato-waveform instr)))

  ;;; Returns {{#t}} if using square waveform vibrato
  (define (xm:instrument-vibrato-square? instr)
    (= xm:instr-vibrato-square (xm:instrument-vibrato-waveform instr)))

  ;;; Returns {{#t}} if using saw waveform vibrato
  (define (xm:instrument-vibrato-saw? instr)
    (= xm:instr-vibrato-saw (xm:instrument-vibrato-waveform instr)))

  ;;; Returns {{#t}} if using inverse saw waveform vibrato
  (define (xm:instrument-vibrato-inverse-saw? instr)
    (= xm:instr-vibrato-inverse-saw (xm:instrument-vibrato-waveform instr)))

  ;;; Returns the instrument vibrato depth.
  (define (xm:instrument-vibrato-depth instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-vibrato-depth)
	0))

  ;;; Returns the instrument vibrato rate.
  (define (xm:instrument-vibrato-rate instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-vibrato-rate)
	0))

  ;;; Returns the instrument vibrato sweep setting.
  (define (xm:instrument-vibrato-sweep instr)
    (if (xm:instrument-has-samples? instr)
	(xm:read-u8 instr xm:instr-offset-vibrato-sweep)
	0))

  ;; returns all of the instrument's sample headers as a list of u8vectors
  ;; will fail if instrument does not contain any sample headers
  (define (xm:instrument-sample-headers instr)
    (letrec* ((extract-sample-headers
	      (lambda (init-offset remaining)
		(if (= 0 remaining)
		    '()
		    (cons (subu8vector (xm:data-bytes instr) init-offset
				       (+ init-offset xm:sample-header-size))
			  (extract-sample-headers
			   (+ init-offset xm:sample-header-size)
			   (- remaining 1)))))))
      (extract-sample-headers (xm:instrument-header-size instr)
			      (xm:instrument-number-of-samples instr))))

  ;; not a lazy check of xm:data-size, so it works for proto-instruments
  (define (xm:raw-instrument-size instr)
    (if (xm:instrument-has-samples? instr)
	(+ (xm:instrument-header-size instr)
	   (xm:instrument-sample-header-size instr)
	   (apply + (map (lambda (header)
			   (xm:sample-data-size header))
			 (xm:instrument-sample-headers instr))))
	(xm:instrument-header-size instr)))


  ;; determine the start of the instrument data
  (define (xm:instrument-block-offset xm)
    (+ (xm:header-size xm)
       (apply + (map xm:data-size (xm:patterns xm)))))

  ;;; Returns a list of raw instrument data blocks
  (define (xm:instruments xm)
    (letrec
	((extract-instruments
	  (lambda (init-offset remaining)
	    (if (= 0 remaining)
		'()
		(let* ((proto-instr
			(xm:make-instrument (xm:drop-bytes (xm:data-bytes xm)
							   init-offset)))
		       (instr
			(xm:make-instrument
			 (subu8vector (xm:data-bytes proto-instr)
				      0 (xm:raw-instrument-size proto-instr)))))
		  (cons instr
			(extract-instruments
			 (+ init-offset (xm:data-size instr))
			 (- remaining 1))))))))
      (extract-instruments (xm:instrument-block-offset xm)
			   (xm:number-of-instruments xm))))

  ;;; Returns the instrument at the given index {{i}}. This uses 1-based
  ;;; indexing, in line with the way indexing is done in XM.
  (define (xm:instrument-ref xm i)
    (let ((instruments (xm:instruments xm)))
      (if (> i (length instruments))
	  #f
	  (list-ref instruments (- i 1)))))

  ;;;
  ;;; ==== Sample Related Procedures

  ;;; Returns the sample at index {{smp}} of the instrument at index {{instr}}.
  ;;; For {{instr}}, 1-based indexing, in line with the way indexing is done in
  ;;; XM.
  (define (xm:instrument-sample-ref xm instr smp)
    (let ((instrument (xm:instrument-ref xm instr)))
      (if instrument
	  (xm:sample-ref instrument smp)
	  #f)))

  ;;; Returns the number of sample points, not the number of bytes.
  (define (xm:sample-length sample)
    (if (xm:sample-16bit-data? sample)
	(/ (xm:read-u32 sample xm:sample-offset-length) 2)
	(xm:read-u32 sample xm:sample-offset-length)))

  ;;; Returns the sample name.
  (define (xm:sample-name sample)
    (xm:read-string sample xm:sample-offset-name xm:sample-name-length))

  ;;; Returns the sample loop start position.
  (define (xm:sample-loop-start sample)
    (xm:read-u32 sample xm:sample-offset-loop-start))

  ;;; Returns the sample loop length.
  (define (xm:sample-loop-length sample)
    (xm:read-u32 sample xm:sample-offset-loop-length))

  ;;; Returns the sample loop type byte.
  (define (xm:sample-loop-type sample)
    (xm:read-u8 sample xm:sample-offset-loop-type))

  ;;; Returns {{#t}} if sample looping is enabled.
  (define (xm:sample-loop-enabled? sample)
    (let ((loop-type (xm:sample-loop-type sample)))
      (or (xm:flags-set? xm:flag-sample-loop-forward loop-type)
	  (xm:flags-set? xm:flag-sample-loop-ping-pong loop-type))))

  ;;; Returns {{#t}} if using forward type looping. Correctly handles "invalid"
  ;;; flag settings produced by MPT 1.09.
  (define (xm:sample-loop-forward? sample)
    (let ((loop-type (xm:sample-loop-type sample)))
      (if (xm:flags-set? (bitwise-ior xm:flag-sample-loop-forward
				      xm:flag-sample-loop-ping-pong)
			 loop-type)
	  #f
	  (xm:flags-set? xm:flag-sample-loop-forward loop-type))))

  ;;; Returns {{#t}} if using ping-pong type looping.
  (define (xm:sample-loop-ping-pong? sample)
    (xm:flags-set? xm:flag-sample-loop-ping-pong (xm:sample-loop-type sample)))

  ;;; Returns {{#t}} if the sample data uses 16-bit values.
  (define (xm:sample-16bit-data? sample)
    (xm:flags-set? xm:flag-16bit-sample (xm:sample-loop-type sample)))

  ;;; Returns the volume setting.
  (define (xm:sample-volume sample)
    (xm:read-u8 sample xm:sample-offset-volume))

  ;;; Returns the finetune setting.
  (define (xm:sample-finetune sample)
    (xm:read-s8 sample xm:sample-offset-finetune))

  ;;; Returns the panning position.
  (define (xm:sample-panning sample)
    (xm:read-u8 sample xm:sample-offset-panning))

  ;;; Returns the relative note setting.
  (define (xm:sample-relative-note sample)
    (xm:read-s8 sample xm:sample-offset-relative-note))

  ;; Determine the size of the raw sample data. This function expects a
  ;; sample header (plain u8vector) as input.
  (define (xm:sample-data-size header)
    (xm:read-u32-raw header xm:sample-offset-length))

  ;;; Returns a list of the given instrument's samples, preserving the original
  ;;; order.
  (define (xm:samples instr)
    (letrec
	((extract-samples
	  (lambda (init-offset headers)
	    (if (null? headers)
		'()
		(let ((next-offset
		       (+ init-offset (xm:sample-data-size (car headers)))))
		  (cons (xm:make-sample
			 (xm:merge-u8vec (car headers)
					 (subu8vector (xm:data-bytes instr)
						      init-offset next-offset)))
			(extract-samples next-offset (cdr headers))))))))
      (extract-samples (+ (xm:instrument-header-size instr)
			  (xm:instrument-sample-header-size instr))
		       (xm:instrument-sample-headers instr))))

  ;;; Returns the sample at index {{i}}. This uses 1-based indexing, in line
  ;;; with the way indexing is done in XM.
  (define (xm:sample-ref instr i)
    (let ((samples (xm:samples instr)))
      (if (>= i (length samples))
	  #f
	  (list-ref samples i))))

  ;;; Retrieves the raw XM sample data in internal DPCM format.
  (define (xm:sample->dpcm sample)
    (letrec* ((is-16bit? (xm:sample-16bit-data? sample))
	      (extract-sample-data
	       (lambda (init-offset)
		 (if (>= init-offset (xm:data-size sample))
		     '()
		     (if is-16bit?
			 (cons (xm:read-s16 sample init-offset)
			       (extract-sample-data (+ 2 init-offset)))
			 (cons (xm:read-s8 sample init-offset)
			       (extract-sample-data (+ 1 init-offset))))))))
      (extract-sample-data xm:sample-header-size)))

  ;;; Retrieves the internal sample data and convert it to standard RAW PCM.
  (define (xm:sample->pcm sample)
    (letrec* ((dpcm->pcm
	       (lambda (dpcm prev-val)
		 (if (null? dpcm)
		     '()
		     (let ((next-val
			    (xm:signed (+ prev-val (car dpcm))
				       (if (xm:sample-16bit-data? sample)
					   #x7fff #x7f))))
		       (cons next-val (dpcm->pcm (cdr dpcm) next-val)))))))
      (dpcm->pcm (xm:sample->dpcm sample) 0)))

  ;;; Extract the sample data of given {{sample}} and export as a
  ;;; little-endian, signed, mono PCM RAW file with 8-bit or 16-bit data
  ;;; depending on input sample data type
  (define (xm:export-sample sample filename)
    (let* ((pcm (xm:sample->pcm sample))
	   (bytes (if (xm:sample-16bit-data? sample)
		      (concatenate
		       (map (lambda (word)
			      (list (bitwise-and #xff word)
				    (/ (bitwise-and #xff00 word) #x100)))
			    pcm))
		      pcm)))
      (call-with-output-file filename
	(lambda (port)
	  (for-each (lambda (byte)
		      (write-byte byte port))
		    bytes)))))

  ) ;; end module

;;;
;;; ==== Author
;;;
;;; (c) 2019 Michael Neidel
;;;
;;; ==== License
;;;
;;; MIT
;;;
;;; ==== Version History
;;;
;;; * 0.1.0 Initial Release
;;;
