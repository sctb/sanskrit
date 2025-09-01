# Sanskrit editing mode for Emacs

<img width="772" height="744" alt="screenshot" src="https://github.com/user-attachments/assets/adc7b23f-d79e-451b-a8fc-759bba327965" />

This repository provides tools for editing Sanskrit, including:
- An input mode for entering IAST transliteration
- A local Sanskrit-English dictionary
- Rendering of transliteration to Devanāgarī script
- Conversion utilities for IAST to SLP1 and vice-versa

As it is currently under early and active development, it has not yet
been packaged for distribution and requires manual installation.

## Installation instructions

1. Clone this repository to your local machine
2. Add the repository to your `load-path` and `require` it:
```emacs-lisp
(add-to-list 'load-path "~/path/to/sanskrit")
(require 'sanskrit)
```
3. `M-x sanskrit-mode` to enable the minor mode

### Downloading and installing the dictionary
1. Download and unzip the file `aptxt.zip` from [Cologne University](https://www.sanskrit-lexicon.uni-koeln.de/scans/APScan/2020/web/webtc/download.html)
2. Copy the dictionary file `txt/ap.txt` into this repository
3. Check that `M-: (sanskrit-dictionary-available-p)` is `t`

## Using the IAST postfix input method

|                 | Enter          | Result |
|-----------------|----------------|--------|
| Long vowel      | `aa`           | ā      |
| Retroflex       | `d.`           | ḍ      |
| Long retroflex  | `rr.`          | ṝ      |
| Diacritic above | `n'`<br>`s'`   | ṅ<br>ś |
| Virgulilla      | `n~`           | ñ      |
| Approximant     | `l_`           | ḻ      |
| Danda           | `\|`<br>`\|\|` | ।<br>॥ |

\* Input method was inspired by this [post](https://satish.com.in/20160319/) from Satish B. Setty
