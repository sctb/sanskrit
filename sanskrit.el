;; -*- lexical-binding: t -*-

(defgroup sanskrit nil
  "Tools for editing Sanskrit"
  :version "0.1"
  :prefix "sanskrit-"
  :group 'editing)

(defvar sanskrit-input-method "sanskrit-postfix"
  "Name of the QUAIL-based input method for writing IAST")

(define-minor-mode sanskrit-mode
  "Toggle Sanskrit mode"
  :init-value nil
  :keymap (make-sparse-keymap)
  :lighter " Sanskrit"
  (set-input-method (and sanskrit-mode sanskrit-input-method)))

(define-derived-mode sanskrit-display-mode special-mode "Sanskrit"
  "Major mode for displaying rendered Devanāgarī script")

(define-derived-mode sanskrit-dictionary-mode special-mode "Dictionary"
  "Major mode for viewing Sanskrit dictionary entries"
  (sanskrit-mode +1)
  (visual-line-mode +1))

(defface sanskrit-script
  '((t :height 1.1))
  "Face used for rendered Devanāgarī script")

(defface sanskrit-headword
  '((t :height 1.1 :inherit font-lock-keyword-face))
  "Face used for the headword in a dictionary entry")

(defface sanskrit-italic
  '((t :inherit italic))
  "Face used for italicized text in a dictionary entry")

(defface sanskrit-bold
  '((t :inherit bold))
  "Face used for bolded text in a dictionary entry")

(defface sanskrit-reference
  '((t :inherit shadow))
  "Face used for references and abbreviations in a dictionary entry")

(defface sanskrit-homonym
  '((t :inherit font-lock-builtin-face))
  "Face used to indicate homonyms in a dictionary entry")

(defface sanskrit-numeral
  '((t :inherit font-lock-type-face))
  "Face used for the item number in a dictionary entry")

(quail-define-package
 sanskrit-input-method "UTF-8" "InR<" t
 "Input method for Sanskrit IAST transliteration with postfix modifiers"
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
 ("m'" "ṁ") ("M'" "Ṁ")
 ("s'" "ś") ("S'" "Ś")
 ("n~" "ñ") ("N~" "Ñ")
 ;; approximant
 ("l_" "ḻ") ("L_" "Ḻ")
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
    ("y" . "य") ("r" . "र") ("l" . "ल") ("v" . "व") ; semi-vowels
    ("ś" . "श") ("ṣ" . "ष") ("s" . "स") ("h" . "ह") ; sibilants and h
    ("ḻ" . "ळ"))) ; approximant

(defvar sanskrit--vowels
  '(("a" . "अ") ("ā" . "आ") ("i" . "इ") ("ī" . "ई") ("u" . "उ") ("ū" . "ऊ")
    ("ṛ" . "ऋ") ("ṝ" . "ॠ") ("ḷ" . "ऌ")
    ("e" . "ए") ("ai" . "ऐ") ("o" . "ओ") ("au" . "औ")))

(defvar sanskrit--vowel-signs
  '(("a" . "") ("ā" . "ा") ("i" . "ि") ("ī" . "ी") ("u" . "ु") ("ū" . "ू")
    ("ṛ" . "ृ") ("ṝ" . "ॄ") ("ḷ" . "ॢ")
    ("e" . "े") ("ai" . "ै") ("o" . "ो") ("au" . "ौ")))

(defvar sanskrit--signs
  '((?ṃ . "ं") (?ṁ . "ं")	; anusvāra
    (?ḥ . "ः")			; visarga
    (?\' . "ऽ") (?’ . "ऽ")))	; avagraha

(defvar sanskrit--virama "्")

(defvar sanskrit--digits
  '((?0 . "०") (?1 . "१") (?2 . "२") (?3 . "३") (?4 . "४")
    (?5 . "५") (?6 . "६") (?7 . "७") (?8 . "८") (?9 . "९")))

(defvar sanskrit--delimiters
  '(?\s ?\n ?\t ?। ?॥))

(defun sanskrit--char-set (list)
  (let ((chars nil))
    (dolist (string list)
      (dotimes (i (length string))
	(let ((c (aref string i)))
	  (unless (memq c chars)
	    (push c chars)))))
    chars))

(defvar sanskrit--consonant-chars
  (sanskrit--char-set (mapcar #'car sanskrit--consonants)))

(defvar sanskrit--vowel-chars
  (sanskrit--char-set (mapcar #'car sanskrit--vowels)))

(defun sanskrit--consonant-p (char)
  (memq char sanskrit--consonant-chars))

(defun sanskrit--vowel-p (char)
  (memq char sanskrit--vowel-chars))

(defun sanskrit--sign-p (char)
  (assq char sanskrit--signs))

(defun sanskrit--delimiter-p (char)
  (memq char sanskrit--delimiters))

(defun sanskrit--span (string i matcher)
  (let ((j i)
	(len (length string)))
    (while (and (< j len) (funcall matcher (aref string j)))
      (incf j))
    (substring string i j)))

(defun sanskrit--take-1 (string i)
  (string (aref string i)))

(defun sanskrit--take-2 (string i)
  (if (> (length string) (1+ i))
      (string (aref string i) (aref string (1+ i)))
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

(defun sanskrit--prepare-iast (string)
  (downcase (string-replace "-" "" string)))

(defun sanskrit-render (string)
  "Render ‘string’ in IAST format to Devanāgarī script"
  (let* ((string (sanskrit--prepare-iast string))
         (len (length string))
         (list nil)
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
               (push (string c) list))
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

(defun sanskrit--display-script (string)
  (with-current-buffer (get-buffer-create "*Sanskrit script*")
    (sanskrit-display-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (sanskrit--make-face string 'sanskrit-script))
    (setq buffer-read-only t)
    (pop-to-buffer
     (current-buffer)
     '(nil (window-height . fit-window-to-buffer)))))

(defun sanskrit--current-word (&optional really-word)
  (or (current-word t really-word) ""))

(defun sanskrit-render-current-word ()
  "Display the current IAST word as Devanāgarī and copy to the kill-ring"
  (interactive)
  (let* ((word (sanskrit--current-word))
         (string (sanskrit-render word)))
    (kill-new string)
    (sanskrit--display-script string)))

(defun sanskrit-render-region (point mark)
  "Display the current IAST region as Devanāgarī and copy to the kill-ring"
  (interactive "r")
  (let* ((region (buffer-substring point mark))
         (string (sanskrit-render (string-trim region))))
    (kill-new string)
    (sanskrit--display-script string)))

(defun sanskrit--relative-file (path)
  (let ((file (or load-file-name (buffer-file-name))))
    (concat (file-name-directory file) path)))

(defcustom sanskrit-dictionary-file
  (sanskrit--relative-file "ap.txt")
  "Path to the Sanskrit-English dictionary file"
  :type 'string)

(defun sanskrit--dictionary-index-file ()
  (concat sanskrit-dictionary-file ".index"))

(defvar sanskrit--dictionary-index nil
  "Hash table of dictionary offsets keyed by word in SLP1 format")

(defun sanskrit--index-dictionary ()
  (with-temp-file (sanskrit--dictionary-index-file)
    (let ((output (current-buffer))
          (n 0))
      (with-temp-buffer
        (insert-file-contents sanskrit-dictionary-file)
        (while (re-search-forward "<L>" nil t)
          (re-search-forward "<k1>\\(.*\\)<k2>")
          (let ((word (sanskrit-slp1-to-iast (match-string 1))))
            (forward-line)
            (let ((beg (1- (position-bytes (point)))))
              (re-search-forward "<LEND>")
              (let* ((bytes (1- (position-bytes (point))))
                     (end (- bytes (length "<LEND>"))))
                (print (list word beg end) output)
                (incf n))))))
      (message "Indexed %d entries" n))))

(defun sanskrit--dictionary-index-read-form ()
  (condition-case nil
      (read (current-buffer))
    (end-of-file nil)))

(defun sanskrit--dictionary-read-index ()
  (or (hash-table-p sanskrit--dictionary-index)
      (let ((file (sanskrit--dictionary-index-file))
	    (index (make-hash-table :test 'equal)))
	(unless (file-exists-p file)
	  (sanskrit--index-dictionary))
	(with-temp-buffer
	  (insert-file-contents file)
	  (while-let ((form (sanskrit--dictionary-index-read-form)))
	    (pcase-let ((`(,word . ,location) form))
	      (push location (gethash word index))))
	  (setq sanskrit--dictionary-index index)))))

(defun sanskrit--dictionary-entry-header (word)
  (let* ((word (sanskrit-slp1-to-iast word))
	 (deva (sanskrit-render word))
	 (string (concat word " " deva)))
    (save-excursion
      (sanskrit--make-face string 'sanskrit-headword)
      (insert string)
      (insert ?\n ?\n))))

(defun sanskrit--make-face (string face)
  (put-text-property 0 (length string) 'face face string)
  string)

(defun sanskrit--replace-match (regex &optional new)
  (save-excursion
    (while (re-search-forward regex nil t)
      (let ((match (match-string 1)))
	(cond ((null new) (replace-match ""))
	      ((stringp new) (replace-match new))
	      ((functionp new) (replace-match (funcall new match)))
	      ((symbolp new) (replace-match (sanskrit--make-face match new)))
	      (t (error "Unrecognized replacement: %s" new)))))))

(defun sanskrit--replace-tag (tag &optional new)
  (let ((regex (format "<%s[^>]*>\\([^<]*\\)</%1$s>" tag)))
    (sanskrit--replace-match regex new)))

(defun sanskrit--dictionary-process-entry ()
  (sanskrit--replace-match "\\^\\([[:digit:]]+\\)¦" 'sanskrit-homonym)
  (sanskrit--replace-match "¦" ":")
  (sanskrit--replace-match "\\[Page.*\n")
  (sanskrit--replace-match "^\\.³?")
  (sanskrit--replace-match "{#\\([^#]+\\)#}" #'sanskrit-slp1-to-iast)
  (sanskrit--replace-match "{%\\([^%]+\\)%}" 'sanskrit-italic)
  (sanskrit--replace-match "{@\\([^@]+\\)@}" 'sanskrit-bold)
  (sanskrit--replace-match "€\\([^ ]+ \\)" 'sanskrit-reference)
  (sanskrit--replace-match "^²\\([[:digit:]]+\\)" 'sanskrit-numeral)
  (sanskrit--replace-tag "ls" 'sanskrit-reference)
  (sanskrit--replace-tag "ab" 'sanskrit-reference))

(defvar sanskrit-dictionary-history nil
  "History for input to ‘sanskrit-dictionary-lookup’")

(defun sanskrit--dictionary-buffer ()
  (with-current-buffer (get-buffer-create "*Dictionary entry*")
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (current-buffer)))

(defun sanskrit--insert-range (beg end)
  (let* ((file sanskrit-dictionary-file)
	 (n (cadr (insert-file-contents file nil beg end))))
    (forward-char n)))

(defun sanskrit--dictionary-show-entry (word)
  (when-let* ((entries (gethash word sanskrit--dictionary-index)))
    (with-current-buffer (sanskrit--dictionary-buffer)
      (dolist (entry (reverse entries))
	(pcase-let ((`(,beg ,end) entry))
	  (sanskrit--insert-range beg end)))
      (goto-char (point-min))
      (sanskrit--dictionary-entry-header word)
      (sanskrit--dictionary-process-entry)
      (sanskrit-dictionary-mode)
      (pop-to-buffer (current-buffer)))))

(defun sanskrit-dictionary-lookup (word)
  "Look up ‘word’ in SLP1 format in the dictionary"
  (interactive
   (let ((word (sanskrit--current-word t)))
     (list (and (sanskrit-dictionary-available-p)
		(completing-read
		 "Dictionary lookup (SLP1): "
		 sanskrit--dictionary-index
		 nil t word 'sanskrit-dictionary-history)))))
  (cond ((not (sanskrit-dictionary-available-p))
	 (message "Missing dictionary file: %s" sanskrit-dictionary-file))
	((sanskrit--dictionary-show-entry word))
	(t (message "No entry found for ‘%s’" word))))

(defun sanskrit-dictionary-available-p ()
  "Returns T if the dictionary is present and ready to use, NIL otherwise"
  (and (file-exists-p sanskrit-dictionary-file)
       (not (null (sanskrit--dictionary-read-index)))))

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
    ("ś" . "S") ("ṣ" . "z")  ("s" . "s") ("h" . "h") ("ḻ" . "L")
    ("’" . "'")))

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
    (should (equal (sanskrit-render "ahaṃ") "अहं"))
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
    (should (equal (sanskrit-render "kḷp") "कॢप्"))
    (should (equal (sanskrit-render "svabhittau") "स्वभित्तौ"))
    (should (equal (sanskrit-render "aṃga") "अंग"))
    (should (equal (sanskrit-render "aṁga") "अंग"))
    (should (equal (sanskrit-render "aṅga") "अङ्ग"))
    (should (equal (sanskrit-render "agnimīḻe") "अग्निमीळे"))
    (should (equal (sanskrit-render "Śivo'ham") "शिवोऽहम्"))
    (should (equal (sanskrit-render "śivo’ham") "शिवोऽहम्"))
    (should (equal (sanskrit-render "cetano’pi") "चेतनोऽपि"))
    (should (equal (sanskrit-render "viśvam-unmīlayati") "विश्वमुन्मीलयति")))

  (ert-deftest sanskrit-slp1-to-iast ()
    (should (equal (sanskrit-slp1-to-iast "a") "a"))
    (should (equal (sanskrit-slp1-to-iast "B") "bh"))
    (should (equal (sanskrit-slp1-to-iast "q") "ḍ"))
    (should (equal (sanskrit-slp1-to-iast "aMhUraRa") "aṃhūraṇa"))
    (should (equal (sanskrit-slp1-to-iast "pramAtf") "pramātṛ"))
    (should (equal (sanskrit-slp1-to-iast "mAyA-Sakti") "māyā-śakti"))
    (should (equal (sanskrit-slp1-to-iast "Sivo'ham") "śivo’ham")))

  (ert-deftest sanskrit-iast-to-slp1 ()
    (should (equal (sanskrit-iast-to-slp1 "a") "a"))
    (should (equal (sanskrit-iast-to-slp1 "ā") "A"))
    (should (equal (sanskrit-iast-to-slp1 "ṝ") "F"))
    (should (equal (sanskrit-iast-to-slp1 "ai") "E"))
    (should (equal (sanskrit-iast-to-slp1 "māyā") "mAyA"))
    (should (equal (sanskrit-iast-to-slp1 "vimarśaḥ") "vimarSaH"))
    (should (equal (sanskrit-iast-to-slp1 "prakṛtiḥ") "prakftiH"))
    (should (equal (sanskrit-iast-to-slp1 "Ānanda-śakti") "Ananda-Sakti"))
    (should (equal (sanskrit-iast-to-slp1 "ahaṃbhāva") "ahaMBAva"))
    (should (equal (sanskrit-iast-to-slp1 "agnimīḻe") "agnimILe"))
    (should (equal (sanskrit-iast-to-slp1 "Śivo’ham") "Sivo'ham")))

  (defun sanskrit-run-tests ()
    (interactive)
    (ert "sanskrit-.*")))

(provide 'sanskrit)
