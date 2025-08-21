;; -*- lexical-binding: t -*-

(defgroup sanskrit nil
  "Tools for editing Sanskrit"
  :version "0.1"
  :prefix "sanskrit-"
  :group 'editing)

(defvar sanskrit-mode-map (make-sparse-keymap)
  "Keymap for the Sanskrit minor mode")

(define-minor-mode sanskrit-mode
  "Toggle Sanskrit mode"
  :init-value nil
  :lighter " Sanskrit")

(define-derived-mode sanskrit-dictionary-mode special-mode "Dictionary"
  "Major mode for viewing Sanskrit dictionary entries"
  (sanskrit-mode +1))

(defface sanskrit-headword
  '((t :height 1.1 :inherit font-lock-keyword-face))
  "Faced use for the headword in a dictionary entry")

(defface sanskrit-reference
  '((t :inherit shadow))
  "Face used for references and abbreviations in a dictionary entry")

(defface sanskrit-numeral
  '((t :inherit font-lock-type-face))
  "Face used for the item number in a dictionary entry")

(defvar sanskrit-input-method "sanskrit-postfix"
  "Name of the QUAIL-based input method for writing IAST")

(quail-define-package
 sanskrit-input-method "UTF-8" "InR<" t
 "Input method for Sanskrit transliteration with postfix modifiers"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; long vowels
 ("aa" "ā") ("Aa" "Ā") ("AA" "Ā")
 ("ii" "ī") ("Ii" "Ī") ("II" "Ī")
 ("uu" "ū") ("Uu" "Ū") ("UU" "Ū")
 ("rr." "ṝ") ("Rr." "Ṝ") ("RR." "Ṝ")
 ;; retroflex
 ("r." "ṛ") ("R." "Ṛ")
 ("l." "ḷ") ("L." "Ḷ")
 ("m." "ṃ") ("M." "Ṃ")
 ("h." "ḥ") ("H." "Ḥ")
 ("t." "ṭ") ("T." "Ṭ")
 ("d." "ḍ") ("D." "Ḍ")
 ("n." "ṇ") ("N." "Ṇ")
 ("s." "ṣ") ("S." "Ṣ")
 ;; diacritic above
 ("n'" "ṅ") ("N'" "Ṅ")
 ("s'" "ś") ("S'" "Ś")
 ("n~" "ñ") ("N~" "Ñ")
 ;; dandas
 ("|" "।") ("||" "॥"))

(defun sanskrit-toggle-input-method ()
  (interactive)
  (if (equal current-input-method sanskrit-input-method)
      (progn
        (set-input-method nil)
        (message "Sanskrit input disabled"))
    (set-input-method sanskrit-input-method)
    (message "Sanskrit input enabled")))

(defvar sanskrit--consonants
  ;; unvoiced    aspirated     voiced      voiced/asp   nasal
  '(("k" . "क") ("kh" . "ख") ("g" . "ग") ("gh" . "घ") ("ṅ" . "ङ") ; velar
    ("c" . "च") ("ch" . "छ") ("j" . "ज") ("jh" . "झ") ("ñ" . "ञ") ; palatal
    ("ṭ" . "ट") ("ṭh" . "ठ") ("ḍ" . "ड") ("ḍh" . "ढ") ("ṇ" . "ण") ; retroflex
    ("t" . "त") ("th" . "थ") ("d" . "द") ("dh" . "ध") ("n" . "न") ; dental
    ("p" . "प") ("ph" . "फ") ("b" . "ब") ("bh" . "भ") ("m" . "म") ; labial
    ("y" . "य") ("r" . "र") ("l" . "ल") ("v" . "व")	; semi-vowels
    ("ś" . "श") ("ṣ" . "ष") ("s" . "स") ("h" . "ह")))	; sibilants and h

(defvar sanskrit--consonant-chars
  "kgṅcjñṭḍṇtdnpbmyrlvśṣsh")

(defvar sanskrit--vowels
  '(("a" . "अ") ("ā" . "आ") ("i" . "इ") ("ī" . "ई") ("u" . "उ") ("ū" . "ऊ")
    ("ṛ" . "ऋ") ("ṝ" . "ॠ") ("ḷ" . "ऌ")
    ("e" . "ए") ("ai" . "ऐ") ("o" . "ओ") ("au" . "औ")))

(defvar sanskrit--vowel-chars
  "aāiīuūṛṝḷeo")

(defvar sanskrit--signs
  ;; anusvāra   visarga
  '((?ṃ . "ं") (?ḥ . "ः")))

(defvar sanskrit--vowel-signs
  '(("ā" . "ा") ("i" . "ि") ("ī" . "ी") ("u" . "ु") ("ū" . "ू")
    ("ṛ" . "ृ") ("ṝ" . "ॄ") ("ḷ" . "ॢ")
    ("e" . "े") ("ai" . "ै") ("o" . "ो") ("au" . "ौ")))

(defvar sanskrit--virama "्")

(defvar sanskrit--digits
  '((?0 . "०") (?1 . "१") (?2 . "२") (?3 . "३") (?4 . "४")
    (?5 . "५") (?6 . "६") (?7 . "७") (?8 . "८") (?9 . "९")))

(defun sanskrit--vowel-p (char)
  (seq-contains-p sanskrit--vowel-chars char))

(defun sanskrit--consonant-p (char)
  (seq-contains-p sanskrit--consonant-chars char))

(defun sanskrit--sign-p (char)
  (assq char sanskrit--signs))

(defun sanskrit--delimiter-p (char)
  (memq char '(?\s ?- ?। ?॥ ?\n)))

(defun sanskrit--span (string i matcher)
  (seq-take-while matcher (substring string i)))

(defun sanskrit--take-1 (string i)
  (string (aref string i)))

(defun sanskrit--take-2 (string i)
  (if (> (length string) (1+ i))
      (concat (sanskrit--take-1 string i)
              (sanskrit--take-1 string (1+ i)))
    (sanskrit--take-1 string i)))

(defun sanskrit--next-consonant (string i)
  (or (assoc (sanskrit--take-2 string i) sanskrit--consonants)
      (assoc (sanskrit--take-1 string i) sanskrit--consonants)))

(defun sanskrit--consonant (string i)
  (let* ((string (sanskrit--span string i #'sanskrit--consonant-p))
         (list nil)
         (len (length string))
         (i 0))
    (while (< i len)
      (let ((consonant (sanskrit--next-consonant string i)))
        (unless consonant
          (error "Unexpected character: %c" (elt string i)))
        (incf i (length (car consonant)))
        (push (cdr consonant) list)))
    (cons string (string-join (nreverse list) sanskrit--virama))))

(defun sanskrit--vowel (string i &optional sign)
  (let ((string (sanskrit--span string i #'sanskrit--vowel-p))
        (alist (if sign sanskrit--vowel-signs sanskrit--vowels)))
    (or (assoc string alist)
        (error "Unrecognized vowel: %s" string))))

(defun sanskrit-render (string)
  "Render ‘string’ in IAST format to Devanāgarī script"
  (let ((string (downcase string))
        (list nil)
        (len (length string))
        (i 0)
        (consnt nil))
    (while (< i len)
      (let* ((c (aref string i))
             (d (alist-get c sanskrit--digits))
             (n 1))
        (cond (d (push d list))
              ((sanskrit--delimiter-p c)
               (when consnt
                 (push sanskrit--virama list))
               (unless (eq c ?-)
                 (push (string c) list)))
              ((and consnt (eq c ?a)))
              ((sanskrit--sign-p c)
               (push (alist-get c sanskrit--signs) list))
              ((sanskrit--vowel-p c)
               (let ((vowel (sanskrit--vowel string i consnt)))
                 (push (cdr vowel) list)
                 (setq n (length (car vowel)))))
              ((sanskrit--consonant-p c)
               (let ((consonant (sanskrit--consonant string i)))
                 (push (cdr consonant) list)
                 (setq n (length (car consonant)))))
              (t (error "Unexpected character: %c" c)))
        (setq consnt (sanskrit--consonant-p c))
        (incf i n)))
    (when consnt
      (push sanskrit--virama list))
    (string-join (nreverse list))))

(defun sanskrit-render-current-word ()
  "Copy the current word in IAST format to the kill-ring as Devanāgarī"
  (interactive)
  (let* ((word (current-word))
         (string (sanskrit-render word)))
    (kill-new string)
    (message "Copied: %s" string)))

(defun sanskrit-render-region (point mark)
  "Copy the current region in IAST format to the kill-ring as Devanāgarī"
  (interactive "r")
  (let* ((region (buffer-substring point mark))
         (string (sanskrit-render (string-trim region))))
    (kill-new string)
    (message "Copied: %s" string)))

(defcustom sanskrit-dictionary-file
  (let ((file (or load-file-name (buffer-file-name))))
    (concat (file-name-directory file) "ap.txt"))
  "Path to the dictionary file"
  :type 'string)

(defun sanskrit--dictionary-index-file ()
  (concat sanskrit-dictionary-file ".index"))

(defvar sanskrit--dictionary-index nil)

(defun sanskrit--index-dictionary ()
  (with-temp-file (sanskrit--dictionary-index-file)
    (let ((output (current-buffer))
          (input sanskrit-dictionary-file)
          (n 0))
      (with-temp-buffer
        (insert-file-contents input)
        (while (re-search-forward "<L>" nil t)
          (re-search-forward "<k1>\\(.*\\)<k2>")
          (let ((word (match-string 1)))
            (forward-line)
            (let ((beg (1- (position-bytes (point)))))
              (re-search-forward "<LEND>")
              (let* ((bytes (1- (position-bytes (point))))
                     (end (- bytes (length "<LEND>"))))
                (print (list word beg end) output)
                (incf n))))))
      (message "Indexed %d entries" n))))

(defun sanskrit--dictionary-index-read-entry ()
  (condition-case nil
      (read (current-buffer))
    (end-of-file nil)))

(defun sanskrit--dictionary-read-index ()
  (let ((file (sanskrit--dictionary-index-file)))
    (unless (file-exists-p file)
      (sanskrit--index-dictionary))
    (let ((index (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert-file-contents file)
        (while-let ((entry (sanskrit--dictionary-index-read-entry)))
          (puthash (car entry) (cdr entry) index)))
      (setq sanskrit--dictionary-index index)
      (message "Loaded %s entries" (hash-table-count index)))))

(defun sanskrit--visarga-p (word)
  (equal (substring word -1) "H"))

(defun sanskrit--with-visarga (word)
  (unless (sanskrit--visarga-p word)
    (concat word "H")))

(defun sanskrit--without-visarga (word)
  (when (sanskrit--visarga-p word)
    (substring word 0 (1- (length word)))))

(defun sanskrit--dictionary-index-lookup (word)
  (let ((get (lambda (w)
	       (when-let* ((entry (gethash w sanskrit--dictionary-index)))
		 (cons w entry)))))
    (or (funcall get word)
	(funcall get (sanskrit--with-visarga word))
	(funcall get (sanskrit--without-visarga word)))))

(defun sanskrit--make-face (string face)
  (put-text-property 0 (length string) 'face face string)
  string)

(defun sanskrit--dictionary-entry-header (word)
  (let* ((word (sanskrit-slp1-to-iast word))
	 (deva (sanskrit-render word))
	 (text (concat word " " deva)))
    (save-excursion
      (sanskrit--make-face text 'sanskrit-headword)
      (insert text)
      (insert ?\n ?\n))))

(defun sanskrit--replace-match (regex new)
  (save-excursion
    (while (re-search-forward regex nil t)
      (let ((string (if (functionp new)
			(funcall new (match-string 1))
		      new)))
	(replace-match string)))))

(defun sanskrit--dictionary-process-entry ()
  (save-excursion
    (when (re-search-forward "^[^¦]+¦[ ]*\n?" nil t)
      (replace-match "")))
  (sanskrit--replace-match "{#\\([^#]+\\)#}" #'sanskrit-slp1-to-iast)
  (sanskrit--replace-match
   "{%\\([^%]+\\)%}"
   (lambda (string)
     (sanskrit--make-face string 'italic)))
  (sanskrit--replace-match
   "{@\\([^@]+\\)@}"
   (lambda (string)
     (sanskrit--make-face string 'bold)))
  (sanskrit--replace-match
   "<ls[^>]*>\\([^<]+\\)</ls>"
   (lambda (string)
     (sanskrit--make-face string 'sanskrit-reference)))
  (sanskrit--replace-match
   "<ab[^>]*>\\([^<]+\\)</ab>"
   (lambda (string)
     (sanskrit--make-face string 'sanskrit-reference)))
  (sanskrit--replace-match "\\[Page.*\n" "")
  (sanskrit--replace-match "^\\." "")
  (sanskrit--replace-match
   "^[²³]\\([[:digit:]]+\\) "
   (lambda (string)
     (let ((string (concat string ". ")))
       (sanskrit--make-face string 'sanskrit-numeral)))))

(defun sanskrit--ensure-dictionary-index ()
  (unless (hash-table-p sanskrit--dictionary-index)
    (sanskrit--dictionary-read-index)))

(defun sanskrit--dictionary-show-entry (word)
  (sanskrit--ensure-dictionary-index)
  (if-let* ((location (sanskrit--dictionary-index-lookup word)))
      (pcase-let ((`(,word ,beg ,end) location))
        (let ((file sanskrit-dictionary-file)
              (buffer (get-buffer-create "*Dictionary entry*")))
          (with-current-buffer buffer
	    (fundamental-mode)
	    (setq buffer-read-only nil)
            (insert-file-contents file nil beg end t)
	    (goto-char (point-min))
	    (sanskrit--dictionary-process-entry)
	    (sanskrit--dictionary-entry-header word)
	    (sanskrit-dictionary-mode)
	    (pop-to-buffer buffer))))
    (message "No entry found for ‘%s’" word)))

(defun sanskrit-dictionary-lookup (word)
  "Look up ‘word’ in SLP1 format in the dictionary"
  (interactive
   (progn
     (sanskrit--ensure-dictionary-index)
     (list (completing-read
	    "Dictionary lookup (SLP1): "
	    sanskrit--dictionary-index))))
  (sanskrit--dictionary-show-entry word))

(defun sanskrit-dictionary-lookup-current-word ()
  "Look up the current word in IAST format in the dictionary"
  (interactive)
  (let* ((word (current-word))
	 (first (car (split-string word "-")))
	 (slp1 (sanskrit-iast-to-slp1 first)))
    (sanskrit--dictionary-show-entry slp1)))

(defvar sanskrit--slp1
  '(("a" . "a") ("ā" . "A")  ("i" . "i") ("ī" . "I") ("u" . "u") ("ū" . "U")
    ("ṛ" . "f") ("ṝ" . "F")  ("ḷ" . "x")
    ("e" . "e") ("ai" . "E") ("o" . "o") ("au" . "O")
    ("ṃ" . "M") ("ḥ" . "H")
    ("k" . "k") ("kh" . "K") ("g" . "g") ("gh" . "G") ("ṅ" . "N")
    ("c" . "c") ("ch" . "C") ("j" . "j") ("jh" . "J") ("ñ" . "Y")
    ("ṭ" . "w") ("ṭh" . "W") ("ḍ" . "q") ("ḍh" . "Q") ("ṇ" . "R")
    ("t" . "t") ("th" . "T") ("d" . "d") ("dh" . "D") ("n" . "n")
    ("p" . "p") ("ph" . "P") ("b" . "b") ("bh" . "B") ("m" . "m")
    ("y" . "y") ("r" . "r")  ("l" . "l") ("v" . "v")
    ("ś" . "S") ("ṣ" . "z")  ("s" . "s") ("h" . "h")))

(defun sanskrit-iast-to-slp1 (string)
  "Convert ‘string’ in IAST transliteration format to SLP1"
  (let ((list nil)
	(string (downcase string))
	(i 0)
	(len (length string)))
    (while (< i len)
      (let* ((k2 (sanskrit--take-2 string i))
	     (k1 (sanskrit--take-1 string i))
	     (pair (or (assoc k2 sanskrit--slp1)
		       (assoc k1 sanskrit--slp1))))
	(cond ((null pair)
	       (push k1 list)
	       (incf i))
	      (t (push (cdr pair) list)
		 (incf i (length (car pair)))))))
    (string-join (nreverse list))))

(defun sanskrit-slp1-to-iast (string)
  "Convert ‘string’ in SLP1 transliteration format to IAST"
  (let ((list nil))
    (dotimes (i (length string))
      (let* ((key (sanskrit--take-1 string i))
	     (pair (rassoc key sanskrit--slp1)))
        (push (or (car pair) key) list)))
    (string-join (nreverse list))))

(when (featurep 'ert)
  (ert-deftest sanskrit-render ()
    (should (equal (sanskrit-render "a") "अ"))
    (should (equal (sanskrit-render "ai") "ऐ"))
    (should (equal (sanskrit-render "ī") "ई"))
    (should (equal (sanskrit-render "ka") "क"))
    (should (equal (sanskrit-render "k") "क्"))
    (should (equal (sanskrit-render "ki") "कि"))
    (should (equal (sanskrit-render "14") "१४"))
    (should (equal (sanskrit-render "Śiva") "शिव"))
    (should (equal (sanskrit-render "Śaktī") "शक्ती"))
    (should (equal (sanskrit-render "icchā") "इच्छा"))
    (should (equal (sanskrit-render "jñāna") "ज्ञान"))
    (should (equal (sanskrit-render "kriyā") "क्रिया"))
    (should (equal (sanskrit-render "cit ॥ 1") "चित् ॥ १"))
    (should (equal (sanskrit-render "Ānanda-śakti") "आनन्दशक्ति"))
    (should (equal (sanskrit-render "vāk\n0") "वाक्\n०"))
    (should (equal (sanskrit-render "citiḥ") "चितिः"))
    (should (equal (sanskrit-render "siddhi") "सिद्धि"))
    (should (equal (sanskrit-render "kḷp") "कॢप्")))

  (ert-deftest sanskrit-slp1-to-iast ()
    (should (equal (sanskrit-slp1-to-iast "a") "a"))
    (should (equal (sanskrit-slp1-to-iast "B") "bh"))
    (should (equal (sanskrit-slp1-to-iast "q") "ḍ"))
    (should (equal (sanskrit-slp1-to-iast "aMhUraRa") "aṃhūraṇa"))
    (should (equal (sanskrit-slp1-to-iast "pramAtf") "pramātṛ"))
    (should (equal (sanskrit-slp1-to-iast "mAyA-Sakti") "māyā-śakti")))

  (ert-deftest sanskrit-iast-to-slp1 ()
    (should (equal (sanskrit-iast-to-slp1 "a") "a"))
    (should (equal (sanskrit-iast-to-slp1 "ā") "A"))
    (should (equal (sanskrit-iast-to-slp1 "ṝ") "F"))
    (should (equal (sanskrit-iast-to-slp1 "ai") "E"))
    (should (equal (sanskrit-iast-to-slp1 "māyā") "mAyA"))
    (should (equal (sanskrit-iast-to-slp1 "vimarśaḥ") "vimarSaH"))
    (should (equal (sanskrit-iast-to-slp1 "prakṛtiḥ") "prakftiH"))
    (should (equal (sanskrit-iast-to-slp1 "Ānanda-śakti") "Ananda-Sakti"))
    (should (equal (sanskrit-iast-to-slp1 "ahaṃbhāva") "ahaMBAva")))

  (defun sanskrit-run-tests ()
    (interactive)
    (ert "sanskrit-.*")))

(provide 'sanskrit)
