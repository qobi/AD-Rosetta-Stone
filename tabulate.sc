(define *n* 3)

(define (best l) (reduce min l infinity))

(define (skip string strings)
 (if (substring? string (first strings))
     strings
     (skip string (rest strings))))

(define (skipn string i strings)
 (skip (string-append  string " " (number->string i)) strings))

(define (u+s string lines)
 (best
  (map-n
   (lambda (i)
    (+ (string->number
	(list->string
	 (but-last
	  (string->list
	   (field-ref (first (skip "pf+" (skipn string i lines))) 0)))))
       (string->number
	(list->string
	 (but-last
	  (string->list
	   (field-ref (first (skip "pf+" (skipn string i lines))) 1)))))))
   *n*)))

(define (ikarus-u+s string lines)
 (best
  (map-n
   (lambda (i)
    (/ (string->number
	(field-ref (first (skip "elapsed cpu time" (skipn string i lines))) 0))
       1000))
   *n*)))

(define (larceny-u+s string lines)
 (best
  (map-n
   (lambda (i)
    (/ (+ (string->number
	   (field-ref (first (skip "Elapsed time" (skipn string i lines))) 5))
	  (string->number
	   (field-ref (first (skip "Elapsed time" (skipn string i lines))) 8)))
       1000))
   *n*)))

(define (scmutils-u+s string lines)
 (best
  (map-n
   (lambda (i)
    (+ (string->number
	(field-ref
	 (first
	  (skip "Timings:" (rest (skip "Timings:" (skipn string i lines)))))
	 1))
       (string->number
	(field-ref
	 (first
	  (skip "Timings:" (rest (skip "Timings:" (skipn string i lines)))))
	 2))))
   *n*)))

(define (entry string implementation factor lines)
 (list
  string
  implementation
  (/ ((cond ((string=? implementation "Ikarus") ikarus-u+s)
	    ((string=? implementation "Larceny") larceny-u+s)
	    ((string=? implementation "MITScheme") scmutils-u+s)
	    ((string=? implementation "SCMUTILS") scmutils-u+s)
	    (else u+s))
      (string-append string " " implementation)
      lines)
     factor)))

(define (lookup? string implementation entries)
 (find-if (lambda (entry)
	   (and (string=? string (first entry))
		(string=? implementation (second entry))))
	  entries))

(define (lookup string implementation entries)
 (third (find-if (lambda (entry)
		  (and (string=? string (first entry))
		       (string=? implementation (second entry))))
		 entries)))

(define (existing-tool? implementation)
 (or (string=? implementation "ADIFOR")
     (string=? implementation "Tapenade")
     (string=? implementation "ADIC")
     (string=? implementation "ADOLC")
     (string=? implementation "CppAD")
     (string=? implementation "FADBAD++")
     (string=? implementation "SCMUTILS")))

(define (not-implemented-but-could-implement? example implementation)
 (or
  ;; needs work: RF for Tapenade.
  (and (or (string=? example "saddle-RF")
	   (string=? example "particle-RF"))
       (string=? implementation "Tapenade"))
  ;; needs work: FR, RF, and RR for FADBAD++.
  (and (or (string=? example "saddle-FR")
	   (string=? example "saddle-RF")
	   (string=? example "saddle-RR")
	   (string=? example "particle-FR")
	   (string=? example "particle-RF")
	   (string=? example "particle-RR"))
       (string=? implementation "FADBAD++"))
  ;; Not going to implement in Fortran (ADIFOR, Tapenade), C (ADIC), or C++
  ;; (ADOLC, CppAD, FADBAD++).
  (or (and (or (string=? example "probabilistic-lambda-calculus-F")
	       (string=? example "probabilistic-lambda-calculus-R")
	       (string=? example "probabilistic-prolog-F")
	       (string=? example "probabilistic-prolog-R"))
	   (or (string=? implementation "Tapenade")
	       (string=? implementation "ADOLC")
	       (string=? implementation "CppAD")
	       (string=? implementation "FADBAD++")))
      (and (or (string=? example "probabilistic-lambda-calculus-F")
	       (string=? example "probabilistic-prolog-F"))
	   (or (string=? implementation "ADIFOR")
	       (string=? implementation "ADIC"))))
  ;; needs work: To implement forward vector mode for Stalingrad, MLton, OCaml,
  ;;             SML/NJ, Haskell-AD, Bigloo, Chicken, Gambit, Ikarus, Larceny,
  ;;             MIT Scheme, MzC, MzScheme, Scheme->C, and Stalin.
  (and (string=? example "mlp-Fv")
       (or (string=? implementation "Stalingrad")
	   (string=? implementation "MLton")
	   (string=? implementation "OCaml")
	   (string=? implementation "SML/NJ")
	   (string=? implementation "Haskell-AD")
	   (string=? implementation "Bigloo")
	   (string=? implementation "Chicken")
	   (string=? implementation "Gambit")
	   (string=? implementation "Ikarus")
	   (string=? implementation "Larceny")
	   (string=? implementation "MITScheme")
	   (string=? implementation "MzC")
	   (string=? implementation "MzScheme")
	   (string=? implementation "Scheme->C")
	   (string=? implementation "Stalin")))))

(define (not-implemented-in-existing-tool? example implementation)
 (or
  ;; ADIFOR, ADIC, and SCMUTILS don't do reverse mode.
  (or (and (or (string=? example "saddle-FR")
	       (string=? example "saddle-RF")
	       (string=? example "saddle-RR")
	       (string=? example "particle-FR")
	       (string=? example "particle-RF")
	       (string=? example "particle-RR")
	       (string=? example "probabilistic-lambda-calculus-R")
	       (string=? example "probabilistic-prolog-R")
	       (string=? example "mlp-R"))
	   (or (string=? implementation "ADIFOR")
	       (string=? implementation "SCMUTILS")))
      (and (or (string=? example "probabilistic-lambda-calculus-R")
	       (string=? example "probabilistic-prolog-R")
	       (string=? example "mlp-R"))
	   (string=? implementation "ADIC")))
  ;; CppAD and SCMUTILS don't do forward vector mode.
  (and (string=? example "mlp-Fv")
       (or (string=? implementation "CppAD")
	   (string=? implementation "SCMUTILS")))))

(define (cannot-implement? example implementation)
 (or
  ;; ADIC, ADOLC, and CppAD can't nest.
  (and (or (string=? example "saddle-FF")
	   (string=? example "saddle-FR")
	   (string=? example "saddle-RF")
	   (string=? example "saddle-RR")
	   (string=? example "particle-FF")
	   (string=? example "particle-FR")
	   (string=? example "particle-RF")
	   (string=? example "particle-RR"))
       (or (string=? implementation "ADIC")
	   (string=? implementation "ADOLC")
	   (string=? implementation "CppAD")))
  ;; Tapenade can't transform reverse-transformed code.
  (and (or (string=? example "saddle-FR")
	   (string=? example "saddle-RR")
	   (string=? example "particle-FR")
	   (string=? example "particle-RR"))
       (string=? implementation "Tapenade"))))

(define (language implementation)
 (second (assoc implementation
		'(("Stalingrad" "\\VLAD")
		  ("ADIFOR" "\\Fortran")
		  ("Tapenade" "\\Fortran")
		  ("ADIC" "\\Clang")
		  ("ADOLC" "\\Cplusplus")
		  ("CppAD" "\\Cplusplus")
		  ("FADBAD++" "\\Cplusplus")
		  ("MLton" "\\ML")
		  ("OCaml" "\\ML")
		  ("SML/NJ" "\\ML")
		  ("Haskell-AD" "\\Haskell")
		  ("Bigloo" "\\Scheme")
		  ("Chicken" "\\Scheme")
		  ("Gambit" "\\Scheme")
		  ("Ikarus" "\\Scheme")
		  ("Larceny" "\\Scheme")
		  ("MITScheme" "\\Scheme")
		  ("MzC" "\\Scheme")
		  ("MzScheme" "\\Scheme")
		  ("Scheme->C" "\\Scheme")
		  ("SCMUTILS" "\\Scheme")
		  ("Stalin" "\\Scheme")))))

(define (pretty implementation)
 (second (assoc implementation
		'(("Stalingrad" "\\Stalingrad")
		  ("ADIFOR" "\\ADIFOR")
		  ("Tapenade" "\\Tapenade")
		  ("ADIC" "\\ADIC")
		  ("ADOLC" "\\ADOLC")
		  ("CppAD" "\\CppAD")
		  ("FADBAD++" "\\FADBADplusplus")
		  ("MLton" "\\MLton")
		  ("OCaml" "\\OCaml")
		  ("SML/NJ" "\\SMLNJ")
		  ("Haskell-AD" "\\HaskellAD")
		  ("Bigloo" "\\Bigloo")
		  ("Chicken" "\\Chicken")
		  ("Gambit" "\\Gambit")
		  ("Ikarus" "\\Ikarus")
		  ("Larceny" "\\Larceny")
		  ("MITScheme" "\\MITScheme")
		  ("MzC" "\\MzC")
		  ("MzScheme" "\\MzScheme")
		  ("Scheme->C" "\\SchemeToC")
		  ("SCMUTILS" "\\SCMUTILS")
		  ("Stalin" "\\Stalin")))))

(let* ((lines (read-file "run.text"))
       (entries
	(append
	 (map-reduce
	  append
	  '()
	  (lambda (example)
	   (map-reduce append
		       '()
		       (lambda (d)
			(map (lambda (implementation factor)
			      (entry (string-append example "-" d)
				     implementation
				     factor
				     lines))
			     '("Bigloo"
			       "Chicken"
			       "Gambit"
			       "Haskell-AD"
			       "Ikarus"
			       "Larceny"
			       "MITScheme"
			       "MLton"
			       "MzC"
			       "MzScheme"
			       "OCaml"
			       "Scheme->C"
			       "SML/NJ"
			       "Stalin"
			       "Stalingrad")
			     '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1000)))
		       '("FF" "FR" "RF" "RR")))
	  '("saddle" "particle"))
	 (map-reduce
	  append
	  '()
	  (lambda (example)
	   (map (lambda (implementation factor)
		 (entry (string-append example "-FF")
			implementation
			factor
			lines))
		'("ADIFOR" "FADBAD++" "SCMUTILS" "Tapenade")
		'(1000 1 1 1000)))
	  '("saddle" "particle"))
	 (map-reduce
	  append
	  '()
	  (lambda (example)
	   (map-reduce
	    append
	    '()
	    (lambda (d)
	     (map (lambda (implementation factor)
		   (entry (string-append example "-" d)
			  implementation
			  factor
			  lines))
		  '("Bigloo"
		    "Chicken"
		    "Gambit"
		    "Haskell-AD"
		    "Ikarus"
		    "Larceny"
		    "MITScheme"
		    "MLton"
		    "MzC"
		    "MzScheme"
		    "OCaml"
		    "Scheme->C"
		    "SML/NJ"
		    "Stalin"
		    "Stalingrad")
		  '(10 10 10 10 10 10 10 1000 10 10 1000 10 1000 10 100000)))
	    '("F" "R")))
	  '("probabilistic-lambda-calculus" "probabilistic-prolog"))
	 (map (lambda (example)
	       (entry (string-append example "-F") "SCMUTILS" 10 lines))
	      '("probabilistic-lambda-calculus" "probabilistic-prolog"))
	 (list (entry "mlp-Fs" "ADIC" 1 lines)
	       (entry "mlp-Fv" "ADIC" 1 lines)
	       (entry "mlp-Fs" "ADIFOR" 1 lines)
	       (entry "mlp-Fv" "ADIFOR" 1 lines)
	       (entry "mlp-Fs" "ADOLC" 1 lines)
	       (entry "mlp-Fv" "ADOLC" 1 lines)
	       (entry "mlp-R" "ADOLC" 1 lines)
	       (entry "mlp-Fs" "FADBAD++" 1 lines)
	       (entry "mlp-Fv" "FADBAD++" 1 lines)
	       (entry "mlp-R" "FADBAD++" 1 lines)
	       (entry "mlp-Fs" "SCMUTILS" 1 lines)
	       (entry "mlp-Fs" "Tapenade" 1 lines)
	       (entry "mlp-Fv" "Tapenade" 1 lines)
	       (entry "mlp-R" "Tapenade" 1 lines))
	 (map-reduce append
		     '()
		     (lambda (d)
		      (map (lambda (implementation factor)
			    (entry (string-append "mlp-" d)
				   implementation
				   factor
				   lines))
			   '("CppAD"
			     "Bigloo"
			     "Chicken"
			     "Gambit"
			     "Haskell-AD"
			     "Ikarus"
			     "Larceny"
			     "MITScheme"
			     "MLton"
			     "MzC"
			     "MzScheme"
			     "OCaml"
			     "Scheme->C"
			     "SML/NJ"
			     "Stalin"
			     "Stalingrad")
			   '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
		     '("Fs" "R")))))
 (call-with-output-file "run1.tex"
  (lambda (run)
   (format run "\\begin{tabular}{ll|rrrr|rrrr|rr|rr|rrr}~%")
   (format run "&&\\multicolumn{4}{|c}{}&\\multicolumn{4}{|c}{}&\\multicolumn{2}{|l}{\\texttt{probabilistic-}}&\\multicolumn{2}{|l}{\\texttt{probabilistic-}}&\\multicolumn{3}{|c}{}\\\\~%")
   (format run "&&\\multicolumn{4}{|c}{\\texttt{particle}}&\\multicolumn{4}{|c}{\\texttt{saddle}}&\\multicolumn{2}{|l}{\\texttt{lambda-calculus}}&\\multicolumn{2}{|l}{\\texttt{prolog}}&\\multicolumn{3}{|c}{\\texttt{mlp}}\\\\~%")
   (format run "&&\\multicolumn{1}{|c}{\\texttt{FF}}&\\multicolumn{1}{c}{\\texttt{FR}}&\\multicolumn{1}{c}{\\texttt{RF}}&\\multicolumn{1}{c}{\\texttt{RR}}&\\multicolumn{1}{|c}{\\texttt{FF}}&\\multicolumn{1}{c}{\\texttt{FR}}&\\multicolumn{1}{c}{\\texttt{RF}}&\\multicolumn{1}{c}{\\texttt{RR}}&\\multicolumn{1}{|c}{\\texttt{F}}&\\multicolumn{1}{c}{\\texttt{R}}&\\multicolumn{1}{|c}{\\texttt{F}}&\\multicolumn{1}{c}{\\texttt{R}}&\\multicolumn{1}{|c}{\\texttt{Fs}}&\\multicolumn{1}{c}{\\texttt{Fv}}&\\multicolumn{1}{c}{\\texttt{R}}\\\\~%")
   (format run "\\hline~%")
   (for-each
    (lambda (implementation)
     (if (or (string=? implementation "Stalingrad")
	     (string=? implementation "ADIFOR")
	     (string=? implementation "ADIC")
	     (string=? implementation "ADOLC")
	     (string=? implementation "MLton")
	     (string=? implementation "Haskell-AD")
	     (string=? implementation "Bigloo"))
	 (format run "~a~%" (language implementation))
	 (format run "~%"))
     (if (existing-tool? implementation)
	 (format run "&{\\blue ~a}~%" (pretty implementation))
	 (format run "&~a~%" (pretty implementation)))
     (for-each
      (lambda (example)
       (when (or
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-but-could-implement? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-in-existing-tool? example implementation)
	       (cannot-implement? example implementation)))
	(fuck-up))
       (cond
	((lookup? example implementation entries)
	 (format run "&~a~%"
		 (number->string-of-length-and-precision
		  (/ (lookup example implementation entries)
		     (lookup (if (string=? example "mlp-Fv")
				 "mlp-Fs"
				 example)
			     "Stalingrad"
			     entries))
		  9
		  2)))
	((not-implemented-but-could-implement? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\green\\rule{1ex}{1ex}}}~%"
		 (if (or (string=? example "particle-RR")
			 (string=? example "saddle-RR")
			 (string=? example "probabilistic-lambda-calculus-R")
			 (string=? example "probabilistic-prolog-R"))
		     "|"
		     "")))
	((not-implemented-in-existing-tool? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\blue\\rule{1ex}{1ex}}}~%"
		 (if (or (string=? example "particle-RR")
			 (string=? example "saddle-RR")
			 (string=? example "probabilistic-lambda-calculus-R")
			 (string=? example "probabilistic-prolog-R"))
		     "|"
		     "")))
	((cannot-implement? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\red\\rule{1ex}{1ex}}}~%"
		 (if (or (string=? example "particle-RR")
			 (string=? example "saddle-RR")
			 (string=? example "probabilistic-lambda-calculus-R")
			 (string=? example "probabilistic-prolog-R"))
		     "|"
		     "")))
	(else (fuck-up))))
      '("particle-FF"
	"particle-FR"
	"particle-RF"
	"particle-RR"
	"saddle-FF"
	"saddle-FR"
	"saddle-RF"
	"saddle-RR"
	"probabilistic-lambda-calculus-F"
	"probabilistic-lambda-calculus-R"
	"probabilistic-prolog-F"
	"probabilistic-prolog-R"
	"mlp-Fs"
	"mlp-Fv"
	"mlp-R"))
     (format run "\\\\~%")
     (when (member implementation
		   '("Stalingrad"
		     "Tapenade"
		     "ADIC"
		     "FADBAD++"
		     "SML/NJ"
		     "Haskell-AD"))
      (format run "\\hline~%")))
    '("Stalingrad"
      ;; Fortran
      "ADIFOR"
      "Tapenade"
      ;; C
      "ADIC"
      ;; C++
      "ADOLC"
      "CppAD"
      "FADBAD++"
      ;; ML
      "MLton"
      "OCaml"
      "SML/NJ"
      ;; Haskell
      "Haskell-AD"
      ;; Scheme
      "Bigloo"
      "Chicken"
      "Gambit"
      "Ikarus"
      "Larceny"
      "MITScheme"
      "MzC"
      "MzScheme"
      "Scheme->C"
      "SCMUTILS"
      "Stalin"))
   (format run "\\end{tabular}~%")))
 (call-with-output-file "run2.tex"
  (lambda (run)
   (format run "\\begin{tabular}{ll|rrrr|rrrr}~%")
   (format run "&&\\multicolumn{4}{|c}{\\texttt{particle}}&\\multicolumn{4}{|c}{\\texttt{saddle}}\\\\~%")
   (format run "&&\\multicolumn{1}{|c}{\\texttt{FF}}&\\multicolumn{1}{c}{\\texttt{FR}}&\\multicolumn{1}{c}{\\texttt{RF}}&\\multicolumn{1}{c}{\\texttt{RR}}&\\multicolumn{1}{|c}{\\texttt{FF}}&\\multicolumn{1}{c}{\\texttt{FR}}&\\multicolumn{1}{c}{\\texttt{RF}}&\\multicolumn{1}{c}{\\texttt{RR}}\\\\~%")
   (format run "\\hline~%")
   (for-each
    (lambda (implementation)
     (if (or (string=? implementation "Stalingrad")
	     (string=? implementation "ADIFOR")
	     (string=? implementation "FADBAD++")
	     (string=? implementation "MLton")
	     (string=? implementation "Haskell-AD")
	     (string=? implementation "Bigloo"))
	 (format run "~a~%" (language implementation))
	 (format run "~%"))
     (if (existing-tool? implementation)
	 (format run "&{\\blue ~a}~%" (pretty implementation))
	 (format run "&~a~%" (pretty implementation)))
     (for-each
      (lambda (example)
       (when (or
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-but-could-implement? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-in-existing-tool? example implementation)
	       (cannot-implement? example implementation)))
	(fuck-up))
       (cond
	((lookup? example implementation entries)
	 (format run "&~a~%"
		 (number->string-of-length-and-precision
		  (/ (lookup example implementation entries)
		     (lookup example "Stalingrad" entries))
		  9
		  2)))
	((not-implemented-but-could-implement? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\green\\rule{1ex}{1ex}}}~%"
		 (if (string=? example "particle-RR") "|" "")))
	((not-implemented-in-existing-tool? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\blue\\rule{1ex}{1ex}}}~%"
		 (if (string=? example "particle-RR") "|" "")))
	((cannot-implement? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\red\\rule{1ex}{1ex}}}~%"
		 (if (string=? example "particle-RR") "|" "")))
	(else (fuck-up))))
      '("particle-FF"
	"particle-FR"
	"particle-RF"
	"particle-RR"
	"saddle-FF"
	"saddle-FR"
	"saddle-RF"
	"saddle-RR"))
     (format run "\\\\~%")
     (when (member implementation
		   '("Stalingrad"
		     "Tapenade"
		     "ADIC"
		     "FADBAD++"
		     "SML/NJ"
		     "Haskell-AD"))
      (format run "\\hline~%")))
    '("Stalingrad"
      ;; Fortran
      "ADIFOR"
      "Tapenade"
      ;; C++
      "FADBAD++"
      ;; ML
      "MLton"
      "OCaml"
      "SML/NJ"
      ;; Haskell
      "Haskell-AD"
      ;; Scheme
      "Bigloo"
      "Chicken"
      "Gambit"
      "Ikarus"
      "Larceny"
      "MITScheme"
      "MzC"
      "MzScheme"
      "Scheme->C"
      "SCMUTILS"
      "Stalin"))
   (format run "\\end{tabular}~%")))
 (call-with-output-file "run3.tex"
  (lambda (run)
   (format run "\\begin{tabular}{ll|rr|rr}~%")
   (format run "&&\\multicolumn{2}{|l}{\\texttt{probabilistic-}}&\\multicolumn{2}{|l}{\\texttt{probabilistic-}}\\\\~%")
   (format run "&&\\multicolumn{2}{|l}{\\texttt{lambda-calculus}}&\\multicolumn{2}{|l}{\\texttt{prolog}}\\\\~%")
   (format run "&&\\multicolumn{1}{|c}{\\texttt{F}}&\\multicolumn{1}{c}{\\texttt{R}}&\\multicolumn{1}{|c}{\\texttt{F}}&\\multicolumn{1}{c}{\\texttt{R}}\\\\~%")
   (format run "\\hline~%")
   (for-each
    (lambda (implementation)
     (if (or (string=? implementation "Stalingrad")
	     (string=? implementation "MLton")
	     (string=? implementation "Haskell-AD")
	     (string=? implementation "Bigloo"))
	 (format run "~a~%" (language implementation))
	 (format run "~%"))
     (if (existing-tool? implementation)
	 (format run "&{\\blue ~a}~%" (pretty implementation))
	 (format run "&~a~%" (pretty implementation)))
     (for-each
      (lambda (example)
       (when (or
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-but-could-implement? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-in-existing-tool? example implementation)
	       (cannot-implement? example implementation)))
	(fuck-up))
       (cond
	((lookup? example implementation entries)
	 (format run "&~a~%"
		 (number->string-of-length-and-precision
		  (/ (lookup example implementation entries)
		     (lookup example "Stalingrad" entries))
		  9
		  2)))
	((not-implemented-but-could-implement? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\green\\rule{1ex}{1ex}}}~%"
		 (if (string=? example "probabilistic-lambda-calculus-R")
		     "|"
		     "")))
	((not-implemented-in-existing-tool? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\blue\\rule{1ex}{1ex}}}~%"
		 (if (string=? example "probabilistic-lambda-calculus-R")
		     "|"
		     "")))
	((cannot-implement? example implementation)
	 (format run "&\\multicolumn{1}{c~a}{{\\red\\rule{1ex}{1ex}}}~%"
		 (if (string=? example "probabilistic-lambda-calculus-R")
		     "|"
		     "")))
	(else (fuck-up))))
      '("probabilistic-lambda-calculus-F"
	"probabilistic-lambda-calculus-R"
	"probabilistic-prolog-F"
	"probabilistic-prolog-R"))
     (format run "\\\\~%")
     (when (member implementation
		   '("Stalingrad"
		     "Tapenade"
		     "ADIC"
		     "FADBAD++"
		     "SML/NJ"
		     "Haskell-AD"))
      (format run "\\hline~%")))
    '("Stalingrad"
      ;; ML
      "MLton"
      "OCaml"
      "SML/NJ"
      ;; Haskell
      "Haskell-AD"
      ;; Scheme
      "Bigloo"
      "Chicken"
      "Gambit"
      "Ikarus"
      "Larceny"
      "MITScheme"
      "MzC"
      "MzScheme"
      "Scheme->C"
      "SCMUTILS"
      "Stalin"))
   (format run "\\end{tabular}~%")))
 (call-with-output-file "run4.tex"
  (lambda (run)
   (format run "\\begin{tabular}{ll|rrr}~%")
   (format run "&&\\multicolumn{3}{|c}{\\texttt{mlp}}\\\\~%")
   (format run "&&\\multicolumn{1}{|c}{\\texttt{Fs}}&\\multicolumn{1}{c}{\\texttt{Fv}}&\\multicolumn{1}{c}{\\texttt{R}}\\\\~%")
   (format run "\\hline~%")
   (for-each
    (lambda (implementation)
     (if (or (string=? implementation "Stalingrad")
	     (string=? implementation "ADIFOR")
	     (string=? implementation "ADIC")
	     (string=? implementation "ADOLC")
	     (string=? implementation "MLton")
	     (string=? implementation "Haskell-AD")
	     (string=? implementation "Bigloo"))
	 (format run "~a~%" (language implementation))
	 (format run "~%"))
     (if (existing-tool? implementation)
	 (format run "&{\\blue ~a}~%" (pretty implementation))
	 (format run "&~a~%" (pretty implementation)))
     (for-each
      (lambda (example)
       (when (or
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-but-could-implement? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (lookup? example implementation entries)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (not-implemented-in-existing-tool? example implementation))
	      (and
	       (not-implemented-but-could-implement? example implementation)
	       (cannot-implement? example implementation))
	      (and
	       (not-implemented-in-existing-tool? example implementation)
	       (cannot-implement? example implementation)))
	(fuck-up))
       (cond ((lookup? example implementation entries)
	      (format run "&~a~%"
		      (number->string-of-length-and-precision
		       (/ (lookup example implementation entries)
			  (lookup (if (string=? example "mlp-Fv")
				      "mlp-Fs"
				      example)
				  "Stalingrad"
				  entries))
		       9
		       2)))
	     ((not-implemented-but-could-implement? example implementation)
	      (format run "&\\multicolumn{1}{c}{{\\green\\rule{1ex}{1ex}}}~%"))
	     ((not-implemented-in-existing-tool? example implementation)
	      (format run "&\\multicolumn{1}{c}{{\\blue\\rule{1ex}{1ex}}}~%"))
	     ((cannot-implement? example implementation)
	      (format run "&\\multicolumn{1}{c}{{\\red\\rule{1ex}{1ex}}}~%"))
	     (else (fuck-up))))
      '("mlp-Fs"
	"mlp-Fv"
	"mlp-R"))
     (format run "\\\\~%")
     (when (member implementation
		   '("Stalingrad"
		     "Tapenade"
		     "ADIC"
		     "FADBAD++"
		     "SML/NJ"
		     "Haskell-AD"))
      (format run "\\hline~%")))
    '("Stalingrad"
      ;; Fortran
      "ADIFOR"
      "Tapenade"
      ;; C
      "ADIC"
      ;; C++
      "ADOLC"
      "CppAD"
      "FADBAD++"
      ;; ML
      "MLton"
      "OCaml"
      "SML/NJ"
      ;; Haskell
      "Haskell-AD"
      ;; Scheme
      "Bigloo"
      "Chicken"
      "Gambit"
      "Ikarus"
      "Larceny"
      "MITScheme"
      "MzC"
      "MzScheme"
      "Scheme->C"
      "SCMUTILS"
      "Stalin"))
   (format run "\\end{tabular}~%"))))
