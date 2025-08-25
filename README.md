# Sanskrit editing mode for Emacs

<img width="772" height="744" alt="screenshot" src="https://github.com/user-attachments/assets/adc7b23f-d79e-451b-a8fc-759bba327965" />

This repository provides tools for editing Sanskrit, including:
- An input mode for entering IAST transliteration
- Conversion utilities for IAST to SLP1 and vice-versa
- A Sanskrit-English dictionary mode
- Rendering of transliterations to Devanāgarī script

As it is currently under early and active development, it has not yet
been packaged for distribution and requires manual installation and
configuration.

## Installation instructions

1. Clone this repository to your local machine
2. Add the repository to your `load-path` and `require` it:
```
(add-to-list 'load-path "~/path/to/sanskrit")
(require 'ert) ;; for running tests
(require 'sanskrit)
```
3. Configure your desired keybindings:
```
(let ((map sanskrit-mode-map))
  (keymap-set map "C-c i" #'sanskrit-toggle-input-method)
  (keymap-set map "C-c s" #'sanskrit-render-current-word)
  (keymap-set map "C-c r" #'sanskrit-render-region)
  (keymap-set map "C-c l" #'sanskrit-dictionary-lookup))
```

## Downloading and installing the dictionary
1. Download the file `ap90txt.zip` from [Cologne University](https://www.sanskrit-lexicon.uni-koeln.de/scans/AP90Scan/2020/web/webtc/download.html)
2. Extract its contents into this repository directory
3. Check that `M-: (sanskrit-dictionary-available-p)` is `t`

## Using the IAST postfix input method
1. Enable the input method with `M-x sanskrit-toggle-input-method`
2. To modify a letter, enter its modification character afterwards:

|   | Enter | Result |
|---|---|---|
| Long vowel | `aa` | ā |
| Retroflex | `d.` | ḍ |
| Long retroflex | `rr.` | ṝ |
| Diacritic above | `m'`<br>`s'` | ṁ<br>ś |
| Virgulilla | `n~` | ñ |
| Danda | `\|`<br>`\|\|` | ।<br>॥ |

