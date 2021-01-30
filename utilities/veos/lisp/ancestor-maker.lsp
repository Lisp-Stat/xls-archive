;;-----------------------------------------------------------
;; file: fern.lsp
;;
;; FERN is the Fractal Entity Relativity Node.
;; This file builds the 'ancestor files' used by FERN
;; when creating new a new entity.
;; see functions fgod-make-node and fgod-be-node for details
;;
;; creation: February 28, 1992
;;
;; by Geoffrey P. Coco at the HITLab, Seattle
;;-----------------------------------------------------------

;;-----------------------------------------------------------
;; Copyright (C) 1992  Geoffrey P. Coco,
;; Human Interface Technology Lab, Seattle
;;-----------------------------------------------------------

(setq hosts
      '("bandersnatch"
	"callay"
	"frabjous"
	"vorpal"
	"slithy"
	"jabberwock"
	"brillig"
	"hawaii"
	"iris2"
	"hal"
	"passion"
	"envy"
	"enos"
	))

(do ((hosts hosts) fp filename host-name)
    ((null hosts))
    (setq host-name (car hosts))
    
    (do ((port 5500))
	((> port 5510))
	
	(setq file-name (sprintf host-name "_" port ".lsp"))
	
	(cond ((setq fp (open file-name :direction :output))

	       ;; actual code generation
	       (print `(setq fern-ancestor ,(vector host-name port)) fp)
	       (print `(setq host-xdisplay ,(sprintf host-name ":0.0")) fp)
	       (close fp)))

	(setq port (1+ port))
	)
    (setq hosts (cdr hosts))
    )


