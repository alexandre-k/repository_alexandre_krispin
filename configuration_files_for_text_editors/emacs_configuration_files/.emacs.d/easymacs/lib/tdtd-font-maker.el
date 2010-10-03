;;;; tdtd-font-maker.el --- Specify and generate font-lock keywords
;; $Id: tdtd-font-maker.el,v 0.18 1999/03/25 23:17:24 tkg Exp $
;; $Name: tdtd071 $

;; Copyright (C) 1997, 1998, 1999 Tony Graham

;; Author: Tony Graham <tgraham@mulberrytech.com>

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; `dtd-make-tdtd-font' in this file writes the tdtd-font.el file that
;; contains the font-lock keywords for DTD mode.  The convoluted logic
;; for the regular expressions for the font lock mode keywords for use
;; when editing DTDs are contained in this file.  Changes to the DTD
;; mode font-lock keywords should be made in this file and a new
;; tdtd-font.el regenerated instead of changing tdtd-font.el directly
;; (and if you've looked in tdtd-font.el, you'll know why).

;; Use `dtd-submit-bug-report' to report bugs.

;; The font lock keywords are complicated firstly by the need to match
;; declaration formats and reserved names in XML DTDs, SGML DTDs, SGML
;; Declarations, and SGML System Declarations, and secondly because we
;; match on both complete declarations and reserved names such that
;; the reserved names change colour as they are entered, then some or
;; all of the declaration changes colour again when the closing
;; delimiter is entered.  This makes the keywords more complicated,
;; but it provides visual feedback that a declaration is complete.

;; Reserved words in SGML, XML, and SGML Declarations
;;
;; Meaning of the columns:
;; XML -- Allowed in XML DTDs
;; SGML -- Allowed in SGML DTDs and LPDs
;; Decl -- Allowed in SGML Declarations
;; System -- Allowed in System Declarations
;;
;; Note that the font lock keywords for an SGML Declaration includes
;; both the keywords identified as allowed in SGML Declarations and
;; the keywords identified as allowed in SGML DTDs and LPDs since the
;; SGML Declaration may be followed by a DTD or DTDs and LPD or LPDs.
;;
;; In several cases, keywords beginning with "#" (RNI) are shown as
;; allowed in XML and SGML DTDs, and the word without the RNI is shown
;; as allowed in SGML Declarations.  SGML's reserved words that are
;; not used in the SGML Declaration may be redefined in the
;; Declaration, hence the non-RNI form is a keyword in the Declaration
;; (even though this font lock stuff doesn't match on the redefined
;; reserved word).
;;
;;
;;  Word		XML	SGML    Decl 	System
;;  -//			x	x	x	x
;;  +//			x	x	x	x
;;  +//IDN		x	x	x	x
;;  //			x	x	x	x
;;  #ALL			x
;;  #CONREF			x
;;  #CURRENT			x
;;  #DEFAULT			x
;;  #EMPTY			x	
;;  #FIXED		x       x
;;  #IMPLICIT			x
;;  #IMPLIED		x       x
;;  #INITIAL			x 	
;;  #NOTATION			x
;;  #PCDATA		x       x
;;  #POSTLINK			x
;;  #REQUIRED		x       x
;;  #RESTORE			x
;;  #SIMPLE			x
;;  #USELINK			x
;;  ALL					x	x
;;  AND					x	x
;;  ANY			x	x	x	x
;;  APPINFO				x
;;  ASN1					x
;;  ATTCAP				x	x
;;  ATTCHCAP				x	x
;;  ATTCNT				x	x
;;  ATTLIST		x	x	x	x
;;  ATTRIB				x	x
;;  ATTSPLEN				x	x
;;  AVGRPCAP				x	x
;;  BASESET				x	x
;;  BB					x	x
;;  BSEQLEN				x	x
;;  CAPACITY				x	x
;;  CDATA		x	x	x	x
;;  CHANGES				x	x
;;  CHARSET				x	x
;;  COM					x	x
;;  CONCUR			 	x
;;  CONREF			x	x	x
;;  CONTROLS				x	x
;;  CRO					x	x
;;  CURRENT			x	x	x
;;  DATA			x	x	x
;;  DATATAG				x	x
;;  DEFAULT			x	x	x
;;  DELIMLEN					x
;;  DELIM				x	x
;;  DESCSET				x	x
;;  DOCTYPE		x	x	x	x
;;  DOCUMENT				x	x
;;  DSC					x	x
;;  DSO					x	x
;;  DTAGLEN				x	x
;;  DTD				x       x
;;  DTEMPLEN				x	x
;;  DTGC				x	x
;;  DTGO				x	x
;;  ELEMCAP				x
;;  ELEMENT		x	x	x	x
;;  EMPTY		x	x	x	x
;;  EMPTYNRM				x	x
;;  ENDTAG			x	x	x
;;  ENTITIES		x	x	x	x
;;  ENTITY		x	x	x	x
;;  ENTLVL				x	x
;;  ENTCAP				x	x
;;  ENTCHCAP				x	x
;;  ERO					x	x
;;  ETAGO				x	x
;;  EXCLUDE					x
;;  EXGRPCAP				x	x
;;  EXNMCAP				x	x
;;  EXPLICIT				x
;;  FEATURES				x	x
;;  FIXED				x	x
;;  FORMAL				x	x
;;  FUNCHAR				x	x
;;  FUNCTION				x	x
;;  GENERAL				x	x
;;  GRPC				x	x
;;  GRPCNT				x	x
;;  GRPGTCNT				x	x
;;  GRPLVL				x	x
;;  GRPO				x	x
;;  HCRO				x	x
;;  ID			x	x       x	x
;;  IDCAP				x	x
;;  IDLINK			x	x	x
;;  IDREF		x	x	x	x
;;  IDREFCAP				x	x
;;  IDREFS		x	x	x	x
;;  IGNORE			x	x	x
;;  IMPLIED				x	x
;;  IMPLICIT				x	x
;;  IMPLYDEF				x	x
;;  INCLUDE			x	x	x
;;  INITIAL				x	x
;;  INSTANCE				x	x
;;  INTEGRAL				x	x
;;  INTERNAL				x	x
;;  ISO 8879-1986			x	x
;;  ISO 8879:1986			x	x
;;  KEEPRSRE				x	x
;;  LCNMCHAR				x	x
;;  LCNMSTRT				x	x
;;  LINK				x	x
;;  LINKTYPE				x	x
;;  LIT					x	x
;;  LITA				x	x
;;  LITLEN				x	x
;;  LKNMCAP				x	x
;;  LKSETCAP				x	x
;;  MAPCAP				x	x
;;  MD					x	x
;;  MDC					x	x
;;  MDO					x	x
;;  MINIMIZE				x
;;  MINUS				x	x
;;  MODEL					x
;;  MSICHAR				x
;;  MSOCHAR				x
;;  MSSCHAR				x
;;  MS					x	x
;;  MSC					x	x
;;  NAME	  	  	x	x	x
;;  NAMECASE				x	x
;;  NAMECHAR				x	x
;;  NAMELEN				x	x
;;  NAMES			x	x	x
;;  NAMESTRT				x	x
;;  NAMING				x	x
;;  NDATA		x	x	x	x
;;  NET					x	x
;;  NESTC				x	x
;;  NETENABL				x	x
;;  NMTOKEN		x	x	x	x
;;  NMTOKENS		x	x	x	x
;;  NOASSERT				x	x
;;  NONE				x	x
;;  NONSGML			x	x	x
;;  NORMSEP				x	x
;;  NOTATION		x	x	x	x
;;  NO					x	x
;;  NOTCAP				x	x
;;  NOTCHCAP				x	x
;;  NUMBER			x       x	x
;;  NUMBERS			x	x	x
;;  NUTOKEN			x	x	x
;;  NUTOKENS			x	x	x
;;  O					x	x
;;  OMITNAME				x	x
;;  OMITTAG				x
;;  OPT					x	x
;;  OR					x	x
;;  OTHER				x
;;  PACK					x
;;  PCDATA		x	x	x	x
;;  PERO				x	x
;;  PI				x	x	x
;;  PIC					x	x
;;  PILEN				x	x
;;  PIO					x	x
;;  PLUS				x	x
;;  POSTLINK				x	x
;;  PUBLIC		x	x	x	x
;;  QUANTITY				x	x
;;  RANK				x	x
;;  RCDATA			x	x	x
;;  RE				x	x	x
;;  REF					x	x
;;  REFC				x	x
;;  REP					x	x
;;  REQUIRED				x	x
;;  RESTORE				x	x
;;  RNI					x	x
;;  RS				x	x	x
;;  SCOPE				x
;;  SDATA			x	x	x
;;  SDIF					x
;;  SEEALSO				x	x
;;  SEPCHAR				x
;;  SEQ					x	x
;;  SEQUENCE				x
;;  SGML				x	x
;;  SGMLREF				x       x
;;  SHORTREF			x	x	x
;;  SHORTTAG				x	x
;;  SHUNCHAR				x	x
;;  SIMPLE				x	x
;;  SPACE				x	x
;;  SRCNT				x	x
;;  SRLEN				x	x
;;  STAGO				x	x
;;  STARTTAG			x	x	x
;;  SUBDOC			x	x	x
;;  SWITCHES				x	x
;;  SYNTAX				x	x
;;  SYSTEM		x	x	x	x
;;  TEMP			x	x	x
;;  TAGC				x	x
;;  TAGLEN				x	x
;;  TAGLVL				x	x
;;  TAB					x	x
;;  TOTALCAP				x	x
;;  TYPE				x	x
;;  UCNMCHAR				x       x
;;  UCNMSTRT				x	x
;;  UNCLOSED				x	x
;;  UNPACK					x
;;  UNUSED				x	x
;;  URN					x	x
;;  USELINK			x	x	x
;;  USEMAP			x	x	x
;;  VALIDATE					x
;;  VALIDITY				x	x
;;  VALUE				x	x
;;  VI					x	x
;;  XML			x
;;  xml			x
;;  xml:lang		x
;;  xml:link		x
;;  xml:space		x
;;  YES					x	x

;;;; Code:
(eval-and-compile
  (autoload 'make-regexp "make-regexp"))
(eval-and-compile
  (autoload 'make-regexps "make-regexp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists of reserved names
;;
;; These lists of reserved names are used later in the declarations
;; for parts of font lock keyword variables or in the declarations for
;; complete font lock keywords.
;;
;; The XML and SGML keyword lists are kept separate, despite their
;; overlap, for two reasons: (1) making extra variables to hold the
;; lists of common keywords would complicate things beyond even the
;; current level; and (2) XML may yet change its reserved names or add
;; new ones.
;;
;; The lists of reserved names are further divided into those that
;; begin with an Emacs-Lisp non-word character, those that contain a
;; non-word character, and those that don't contain any non-word
;; characters.  This is necessary since the keyword regular
;; expressions are bracketed by "\\b", which is necessary so the
;; regular expressions produced by `make-regexps' match on the longest
;; possible string.  I have previously tried producing font lock
;; keywords regular expressions for these reserved names by hand, but
;; they're not particularly readable or maintainable when done by
;; hand.
;;
;; There are several lists of reserved names for portions of SGML
;; Declarations and System Declarations.  This reflects the
;; productions and tables in the SGML standard, and it makes it easier
;; to keep track of why everything is included.

;; Common reserved name lists

(defconst dtd-public-text-class-keywords
  (list
   "CAPACITY" "CHARSET" "DOCUMENT" "DTD" "ELEMENTS" "ENTITIES" "LPD"
   "NONSGML" "NOTATION" "SHORTREF" "SUBDOC" "SYNTAX" "TEXT")
  "Reserved names for Public Text Class in a Formal Public Identifier.")

;; XML DTD reserved name lists

(defconst dtd-xml-keyword-list-1
  (list
   "-//" "\+//" "//" "\+//IDN"
   "#DEFAULT" "#FIXED" "#IMPLIED" "#NOTATION"
   "#PCDATA" "#REQUIRED"
   )
  "XML reserved names strings that don't start with word character.")

(defconst dtd-xml-keyword-list-2
  (list
   "xml:lang" "xml:link" "xml:space"
   )
  "XML reserved names containing non-word characters.")

(defconst dtd-xml-keyword-list-3
  (append
   dtd-public-text-class-keywords
   (list
    "ANY" "ATTLIST" "CDATA" "DOCTYPE" "DTD"
    "ELEMENT" "EMPTY" "ENTITIES" "ENTITY" "ID" "IDREF" "IDREFS"
    "NDATA" "NMTOKEN" "NMTOKENS" "NOTATION" "NUTOKEN" "NUTOKENS"
    "PUBLIC" "SYSTEM" "XML" "xml"
    )
   )
  "Reserved names significant in XML.")

;; SGML DTD reserved name lists

(defconst dtd-sgml-keyword-list-1
  (list
   "-//" "\+//" "//" "\+//IDN"
   "#ALL" "#CONREF" "#CURRENT" "#DEFAULT"
   "#FIXED" "#IMPLICIT" "#IMPLIED" "#INITIAL" "#NOTATION" "#PCDATA"
   "#POSTLINK" "#REQUIRED" "#RESTORE" "#SIMPLE" "#USELINK"
   )
  "SGML reserved names and strings beginning with a non-word character.")


(defconst dtd-sgml-keyword-list-2
  (list
   "ANY" "ATTLIST" "CDATA" "DATA" "DOCTYPE" "DTD" "ELEMENT" "EMPTY"
   "ENTITIES" "ENTITY" "EXPLICIT" "ID" "IDLINK" "IDREF" "IDREFS"
   "IMPLICIT" "LINKTYPE" "LINK" "NAME" "NAMECHAR" "NAMES" "NDATA"
   "NMTOKEN" "NMTOKENS" "NOTATION" "NUMBER" "NUMBERS" "NUTOKEN"
   "NUTOKENS" "POSTLINK" "PUBLIC" "RCDATA" "SDATA" "SHORTREF" "SIMPLE"
   "SUBDOC" "SYSTEM" "TEMP" "USELINK" "USEMAP")
  "SGML reserved names that do not contain any non-word characters.")

;; SGML Declaration reserved name lists

(defconst dtd-sgml-declaration-minimum-literal-keywords
  (list
   "ISO 8879:1986" "ISO 8879-1986" "ISO 8879:1986(ENR)"
   "ISO 8879:1986(WWW)")
  "Minimum literals at the start of an SGML Declaration.")

(defconst dtd-delimiter-set-keywords
  (append
   ;; Non-shortref delimiters
   (list
    "AND" "COM" "CRO" "DSC" "DSO" "DTGC" "DTGO" "ERO" "ETAGO" "GRPC"
    "GRPO" "LIT" "LITA" "MDC" "MDO" "MINUS" "MSC" "NET" "OPT" "OR"
    "PERO" "PIC" "PIO" "PLUS" "REFC" "REP" "RNI" "SEQ"
    "STAGO" "TAGC" "VI")
   ;; Alphabetic short reference delimiters
   (list "BB")
   ;; Delimiters added with SGML TC2
   (list "HCRO" "NESTC"))
  "Reserved names for specifying delimiter set, including shortrefs."
  )

(defconst dtd-redefinable-reserved-name-keywords
  (list
   "ALL" "ANY" "ATTLIST" "CDATA" "CONREF" "CURRENT" "DEFAULT"
   "DOCTYPE" "ELEMENT" "EMPTY" "ENDTAG" "ENTITIES" "ENTITY" "FIXED"
   "ID" "IDLINK" "IDREF" "IDREFS" "IGNORE" "IMPLICIT" "IMPLIED"
   "INCLUDE" "INITIAL" "LINK" "LINKTYPE" "MD" "MS" "NAME" "NAMES"
   "NDATA" "NMTOKEN" "NMTOKENS" "NOTATION" "NUMBER" "NUMBERS"
   "NUTOKEN" "NUTOKENS" "O" "PCDATA" "PI" "POSTLINK" "PUBLIC" "RCDATA"
   "RE" "REQUIRED" "RESTORE" "RS" "SDATA" "SHORTREF" "SIMPLE" "SPACE"
   "STARTTAG" "SUBDOC" "SYSTEM" "TEMP" "USELINK" "USEMAP")
  "Reserved names that can be modified in NAMES portion of SGML Declaration.")

(defconst dtd-quantity-set-keywords
  (list
   "ATTCNT" "ATTSPLEN" "BSEQLEN" "DTAGLEN" "DTEMPLEN" "ENTLVL"
   "GRPCNT" "GRPGTCNT" "GRPLVL" "LITLEN" "NAMELEN" "NORMSEP"
   "PILEN" "TAGLEN" "TAGLVL")
  "Reserved names for specifying quantity set.")

(defconst dtd-common-sgml-declaration-keywords
  (list
   "SGMLREF" "PUBLIC" "NONE" "NO" "YES")
  "Reserved names common to multiple parts of an SGML Declaration.")

(defconst dtd-charset-keywords
  (list
   "CHARSET" "BASESET" "DESCSET" "UNUSED")
  "Reserved names used in describing the document character set.")

(defconst dtd-capacity-set-keywords
  (list
   "CAPACITY" "TOTALCAP" "ENTCAP" "ENTCHCAP" "ELEMCAP" "GRPCAP"
   "EXGRPCAP" "EXNMCAP" "ATTCAP" "ATTCHCAP" "AVGRPCAP" "NOTCAP"
   "NOTCHCAP" "IDCAP" "IDREFCAP" "MAPCAP" "LKSETCAP" "LKNMCAP")
  "Reserved names for specifying capacities.")

(defconst dtd-concrete-syntax-scope-keywords
  (list
   "SCOPE" "DOCUMENT" "INSTANCE")
  "Reserved names for specifying concrete syntax scope.")

(defconst dtd-concrete-syntax-keywords
  (append
   (list
    "SYNTAX" "SWITCHES")
   (list
    "SHUNCHAR" "CONTROLS")
   (list
    "FUNCTION" "RE" "RS" "SPACE" "FUNCHAR" "MSICHAR" "MSOCHAR"
    "MSSCHAR" "SEPCHAR" "TAB")
   (list
    "NAMING" "LCNMSTRT" "UCNMSTRT" "NAMESTRT" "LCNMCHAR" "UCNMCHAR"
    "NAMECHAR" "NAMECASE" "GENERAL" "ENTITY")
   (append
    (list
     "DELIM" "GENERAL" "SHORTREF")
    dtd-delimiter-set-keywords)
   (append
    (list
     "NAMES")
    dtd-redefinable-reserved-name-keywords)
   (append
    (list
     "QUANTITY")
    dtd-quantity-set-keywords))
  "Reserved names in specifying a concrete syntax.")

(defconst dtd-features-keywords
  (append
   (list
    "FEATURES" "MINIMIZE" "DATATAG" "OMITTAG" "RANK" "SHORTTAG"
    "LINK" "SIMPLE" "IMPLICIT" "EXPLICIT"
    "OTHER" "CONCUR" "SUBDOC" "FORMAL")
   ;; Reserved names added with SGML TC2
   (list
    "NETENABL" "IMMEDNET" "UNCLOSED" "ATTRIB" "OMITNAME" "VALUE"
    "EMPTYNRM" "IMPLYDEF" "URN" "KEEPRSRE" "VALIDITY" "NOASSERT" "TYPE"
    "REF" "INTERNAL" "INTEGRAL" "SEEALSO"))
  "Reserved names in specifying SGML feature use.")

(defconst dtd-appinfo-keywords
  (list
   "APPINFO")
  "Reserved names in specifying application-specific information.")

;; System Declaration reserved name lists

(defconst dtd-system-declaration-keyword-list-1
  (list
   "-//" "\+//" "//" "\+//IDN")
  "System Declaration reserved names beginning with a non-word character.")

(defconst dtd-concrete-syntax-changes-keywords
  (list
   "CHANGES" "SWITCES" "DELIMLEN" "SEQUENCE" "SRCNT" "SRLEN")
  "Reserved names for concrete syntax changes in System Declaration.")

(defconst dtd-validation-services-keywords
  (list
   "VALIDATE" "GENERAL" "MODEL" "EXCLUDE" "CAPACITY" "NONSGML" "SGML"
   "FORMAL")
  "Reserved names for validation services in System Declaration.")

(defconst dtd-sdif-support-keywords
  (list
   "ASN1" "PACK" "SDIF" "UNPACK")
  "Reserved names for SDIF support in System Declaration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reusable portions of keyword variable declarations

;; These are lists of patterns plus face assignments that can be
;; plugged into the definition of the font lock keywords

(defvar dtd-comment-declaration-keywords
  (list
   ;;
   ;; XML comment and one style of SGML comment
   ;;
   ;; SGML-style comments within declarations are covered by
   ;; a separate pattern that is not included in the XML font lock
   ;; keywords.
   ;;
   ;; Put comment patterns first so they mask any declarations
   ;; that might be inside the comment.
   ;;
   '("<!--[^-]*\\(-[^-]+\\)*-->" . font-lock-comment-face)
   )
  "XML comment and one style of SGML comment keywords.")

(defvar dtd-comment-in-declaration-keywords
  (list
   ;;
   ;; SGML-only comment within a declaration
   ;;
   '("--[^-]*\\(-[^-]+\\)*--" . font-lock-comment-face)
   )
  "SGML-only comment within a declaration keywords.")

(defvar dtd-entity-reference-keywords
  (list
   ;;
   ;; Entity references
   ;; These come early so entity references as the names in element, etc.
   ;; declarations retain their colour and don't get turned into
   ;; font-lock-variable-name-face.  E.g:
   ;; <!ENTITY % %entity; "..." >
   ;;
   '("[%&][^; \t]+;" . font-lock-reference-face)
   )
  "Font lock keywords pattern matching an entity reference.")

(defvar dtd-common-font-lock-keywords-1
  (append
   dtd-comment-declaration-keywords
   dtd-entity-reference-keywords
   (list
    ;;
    ;; Lump together attribute, entity, and notation declarations
    ;;
    '("\\(<!\\(ATTLIST\\|ENTITY\\(\\s-+%\\|\\)\\|NOTATION\\)\\)\\s-+\\(\\S-+\\)[^>]+\\(>\\)"
      (1 font-lock-keyword-face)
      (4 font-lock-variable-name-face)
      (5 font-lock-keyword-face))
    ;;
    ;; Doctype declaration
    ;;
    '("\\(<!DOCTYPE\\)\\s-+\\([^[]+\\)\\s-+\\(\\[\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)
      (3 font-lock-keyword-face))
    ;;
    ;; XML element declaration and SGML element declaration without
    ;; minimisation parameters
    ;;
    '("\\(<!ELEMENT\\)\\s-+\\([^ \t()|]+\\)\\s-+[^>]+\\(>\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)
      (3 font-lock-keyword-face))
    ;;
    ;; Marked section start
    ;;
    '("\\(<!\\[\\)[^[]*\\(\\[\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-keyword-face))
    )
   )
  "Font lock keyword patterns common to XML and SGML DTDs to apply first.")

(defvar dtd-common-font-lock-keywords-2
  (list
   ;;
   ;; Declaration subset close and markup delimiter close
   ;;
   '("\\(\\]?\\]\\)\\s-*\\(>\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face))
   ;;
   ;; Mark the start and end of literals, but don't do anything to their
   ;; contents
   ;;
   '("\\('\\)[^']*\\('\\)"
     (1 font-lock-string-face)
     (2 font-lock-string-face))
   '("\\(\"\\)[^\"]*\\(\"\\)"
     (1 font-lock-string-face)
     (2 font-lock-string-face))
   ;;
   ;; Connectors
   ;;
   '("[,()|&]" . font-lock-function-name-face)
   ;;
   ;; Occurrence indicators
   ;;
   '("[+*?]" . font-lock-string-face)
   ;;
   )
  "Common DTD font lock keyword patterns to apply last.")

(defvar dtd-sgml-dtd-keywords
  (list
   ;;
   ;; Lump together shortref and usemap declarations
   ;;
   '("\\(<!\\(SHORTREF\\|USEMAP\\)\\)\\s-+\\(\\S-+\\)[^>]+\\(>\\)"
	 (1 font-lock-keyword-face)
	 (3 font-lock-variable-name-face)
	 (4 font-lock-keyword-face))
   ;;
   ;; SGML element declaration
   ;;
   ;; Only match on same case of "O", i.e. "o o" or "O O", if both
   ;; start-tag and end-tag are omissible.
   ;;
   '("\\(<!ELEMENT\\)\\s-+\\([^ \t()|]+\\)\\s-+\\(o\\s-+o\\|O\\s-+O\\|-\\s-+[oO]\\|[oO]\\s-+-\\)\\s-+[^>]+\\(>\\)"
	 (1 font-lock-keyword-face)
	 (2 font-lock-variable-name-face)
	 (3 font-lock-keyword-face)
	 (4 font-lock-keyword-face))
   ;;
   ;; Put exclusions in a face that will stand out
   ;;
   '("\\(\\(-\\|+\\)(\\)[^)]*\\()\\)"
     (1 font-lock-variable-name-face)
     (3 font-lock-variable-name-face))
   ;;
   ;; Implicit Link Specification
   ;;
   '("\\(<!LINKTYPE\\)\\s-+\\([^ \t]+\\)\\s-+\\([^ \t]+\\)\\s-+\\(\\#IMPLIED\\)[ \t\n]*\\(\\[\\)"
	 (1 font-lock-keyword-face)
	 (2 font-lock-variable-name-face)
	 (3 font-lock-variable-name-face)
	 (4 font-lock-keyword-face)
	 (5 font-lock-keyword-face))
   ;;
   ;; Explicit Link Specification
   ;;
   '("\\(<!LINKTYPE\\)\\s-+\\([^ \t]+\\)\\s-+\\([^ \t]+\\)\\s-+\\([^ \t]+\\)\\s-*\\(\\[\\)"
	 (1 font-lock-keyword-face)
	 (2 font-lock-variable-name-face)
	 (3 font-lock-variable-name-face)
	 (4 font-lock-type-face)
	 (5 font-lock-keyword-face))
   ;;
   ;; IDLINK link, will appear within an implicit or explicit link
   ;; declaration
   ;;
   '("\\(<!IDLINK\\)\\b[^>]+\\(>\\)"
	 (1 font-lock-keyword-face)
	 (2 font-lock-keyword-face))
   ;;
   ;; SGML Processing instruction
   ;;
   '("\\(<\\?\\)\\([^ \t>]+\\)\\s-*[^>]*\\(>\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-keyword-face))
   )
  "Font lock keyword patterns for declarations specific to SGML DTDs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete font lock keywords variable declarations

;; XML DTDs
(defvar dtd-xml-font-lock-keywords
  (append
   dtd-common-font-lock-keywords-1
   (list
    ;;
    ;; Reserved XML Processing instruction
    ;;
    ;; An 'XML Declaration' requires the version information, but a
    ;; 'Text Declaration' does not, therefore don't require the version
    ;; information.
    '(
      "\\(<\\?\\)\\(xml\\)\\(\\s-+version\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+encoding\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+standalone\\s-*=\\s-*\\('\\(yes\\|no\\)'\\|\"\\(yes\\|no\\)\"\\)\\)?\\s-*\\(\\?>\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face nil)
      (3 font-lock-type-face nil t)
      (5 font-lock-type-face nil t)
      (7 font-lock-type-face nil t)
      (11 font-lock-keyword-face))
    ;;
    ;; Non-reserved XML Processing instruction
    ;; Any XML PI that doesn't start with "<?xml"
    ;;
    '(
      "\\(<\\?\\)\\([^ \t?>]+\\)\\s-*\\([^?>]\\|\\?[^>]\\|>[^\n\r]\\)*\\(\\?>\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)
      (4 font-lock-keyword-face))
    ;;
    ;; Keywords starting with a non-word character
    ;;
    (make-regexps
     (list dtd-xml-keyword-list-1
	   font-lock-type-face)
     "\\b")
    ;; Stupidity because I haven't found a better separator than "\\b"
    ;; and the ":" in "xml:link", etc., counts as a "\\b" so "xml:link"
    ;; can't be in the same keyword list as "xml".
    ;;
    ;; The alternative is constructing the regular expression by hand,
    ;; which I've done before, but using `make-regexps' should make more
    ;; readable code.
    (make-regexps "\\b"
		  (list dtd-xml-keyword-list-2
			font-lock-type-face)
		  "\\b")
    (make-regexps "\\b"
		  (list dtd-xml-keyword-list-3
			font-lock-type-face)
		  "\\b")
    )
   dtd-common-font-lock-keywords-2
   )
  "Font lock keywords for use in XML DTDs.")

;; SGML DTDs
(defvar dtd-sgml-font-lock-keywords
  (append
   dtd-comment-declaration-keywords
   dtd-comment-in-declaration-keywords
   dtd-common-font-lock-keywords-1
   dtd-sgml-dtd-keywords
   (list
    ;;
    ;; Keywords
    (make-regexps
     (list dtd-sgml-keyword-list-1
	   font-lock-type-face)
     "\\b")
    ;;
    (make-regexps "\\b"
		  (list dtd-sgml-keyword-list-2
			font-lock-type-face)
		  "\\b")
    )
   dtd-common-font-lock-keywords-2
   )
  "Font lock keywords for SGML DTDs.")

;; SGML Declarations and SGML DTDs (since one can follow the other)
(defvar dtd-decl-font-lock-keywords
  (append
   dtd-comment-in-declaration-keywords
   dtd-common-font-lock-keywords-1
   (list
    ;;
    ;; SGML Declaration start and end
    ;;
    '("\\(<!SGML\\)\\s-+\\([^>]\\|>[^$]\\)+\\(>$\\)"
      (1 font-lock-keyword-face)
      (3 font-lock-keyword-face))
    ;;
    ;; Keywords
    (make-regexps "\\b"
		  (list
		   (append
		    dtd-common-sgml-declaration-keywords
		    dtd-charset-keywords
		    dtd-capacity-set-keywords
		    dtd-concrete-syntax-scope-keywords
		    dtd-concrete-syntax-keywords
		    dtd-features-keywords
		    dtd-appinfo-keywords
		    dtd-public-text-class-keywords)
		   font-lock-type-face)
		  "\\b")
    ;;
    ;; Minimum literal keywords
    ;;
    (make-regexps (list
		   dtd-sgml-declaration-minimum-literal-keywords
		   font-lock-type-face))
    ;;
    ;; Reserved names beginning with a non-word characters
    ;;
    (make-regexps
     (list dtd-xml-keyword-list-1
	   font-lock-type-face)
     "\\b")
    )
   dtd-common-font-lock-keywords-2
   )
  "Font lock keywords for SGML Declarations.")

;; SGML System Declarations
(defvar dtd-sys-decl-font-lock-keywords
  (append
   dtd-comment-in-declaration-keywords
   dtd-entity-reference-keywords
   (list
    ;;
    ;; System Declaration start and end
    ;;
    '("\\(<!SYSTEM\\)\\([^>]\\|>[^$]\\)+\\(>$\\)"
      (1 font-lock-keyword-face)
      (3 font-lock-keyword-face))
    ;;
    ;; Keywords
    ;;
    ;; Keywords starting with a non-word character
    ;;
    (make-regexps
     (list dtd-system-declaration-keyword-list-1
	   font-lock-type-face)
     "\\b")
    ;;
    ;; Other keywords
    ;;
    (make-regexps "\\b"
		  (list
		   (append
		    dtd-common-sgml-declaration-keywords
		    dtd-charset-keywords
		    dtd-capacity-set-keywords
		    dtd-concrete-syntax-scope-keywords
		    dtd-concrete-syntax-changes-keywords
		    dtd-concrete-syntax-keywords
		    dtd-features-keywords
		    dtd-validation-services-keywords
		    dtd-sdif-support-keywords
		    dtd-public-text-class-keywords)
		   font-lock-type-face)
		  "\\b")
    ;;
    ;; Minimum literal keywords
    ;;
    (make-regexps (list
		   dtd-sgml-declaration-minimum-literal-keywords
		   font-lock-type-face))
    ;;
    )
   dtd-common-font-lock-keywords-2
   )
  "Font lock keywords for System Declarations.")

(defun dtd-make-tdtd-font ()
  "Generate the file \"tdtd-font.el\"."
  (interactive)
  (find-file "tdtd-font.el")
  (kill-region (point-min) (point-max))
  (insert ";;;; tdtd-font.el --- Font-lock keywords for Tony's DTD mode\n")
  (insert ";; $\Id$\n")
  (insert ";; $\Name$\n")
  (insert ";;\n")
  (insert ";; Copyright (C) 1999, Tony Graham\n")
  (insert ";;\n")
  (insert ";; Author: Tony Graham <tgraham@mulberrytech.com>\n")
  (insert "\n")
  (insert ";;; This file is not part of GNU Emacs.\n")
  (insert "\n")
  (insert ";; This program is free software; you can redistribute it and/or\n")
  (insert ";; modify it under the terms of the GNU General Public License\n")
  (insert ";; as published by the Free Software Foundation; either version 2\n")
  (insert ";; of the License, or (at your option) any later version.\n")
  (insert ";;\n")
  (insert ";; This program is distributed in the hope that it will be useful,\n")
  (insert ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n")
  (insert ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n")
  (insert ";; GNU General Public License for more details.\n")
  (insert ";;\n")
  (insert ";; You should have received a copy of the GNU General Public License\n")
  (insert ";; along with this program; if not, write to the Free Software\n")
  (insert ";; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n")
  (insert "\n")
  (insert ";; Generated by tdtd-font-maker.el\n")
  (insert ";; Do not modify this file.  Make changes in tdtd-font-maker.el\n")
  (insert ";; and regenerate.\n")
  (insert "\n")
  (insert ";; Send bugs to tdtd-bug@menteith.com\n")
  (insert "\n\n")
  (insert (format "(defvar dtd-sgml-font-lock-keywords\n  '%s)\n\n"
		  (dtd-make-safe-var-def dtd-sgml-font-lock-keywords)))
  (insert (format "(defvar dtd-xml-font-lock-keywords\n  '%s)\n\n"
		  (dtd-make-safe-var-def dtd-xml-font-lock-keywords)))
  (insert (format "(defvar dtd-decl-font-lock-keywords\n  '%s)\n\n"
		  (dtd-make-safe-var-def dtd-decl-font-lock-keywords)))
  (insert (format "(defvar dtd-sys-decl-font-lock-keywords\n  '%s)\n\n"
		  (dtd-make-safe-var-def dtd-sys-decl-font-lock-keywords)))
  (insert "(provide 'tdtd-font)\n")
  ;; Don't bother saving a backup
  (setq backup-inhibited t)
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun dtd-make-safe-var-def (var)
  "Tidy up a variable's definition so it's safe to output in a defun."
  (let ((var-string (format "%S" var)))
    (while (string-match "\t" var-string)
      (setq var-string (replace-match "\\t" nil t var-string)))
    (while (string-match "\n" var-string)
      (setq var-string (replace-match "\\n" nil t var-string)))
    (while (string-match "\r" var-string)
      (setq var-string (replace-match "\\r" nil t var-string)))
    (format "%s" var-string)))

(provide 'tdtd-font-maker)
