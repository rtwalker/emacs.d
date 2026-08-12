;;; oklch-mono-bg.el --- Monochromatic backgrounds via OKLCH -*- lexical-binding: t; -*-

;;; Commentary:
;; Derive a subtle, same-hue background color from a foreground color by
;; shifting lightness in OKLCH and collapsing chroma toward neutral.

;;; Code:

(require 'cl-lib)
(require 'color)

(defcustom oklch-mono-bg-dark-l 0.24
  "Target OKLab lightness for backgrounds under a dark theme.")

(defcustom oklch-mono-bg-light-l 0.94
  "Target OKLab lightness for backgrounds under a light theme.")

(defcustom oklch-mono-bg-chroma-scale 0.25
  "Fraction of the source chroma to retain.")

(defcustom oklch-mono-bg-chroma-max 0.04
  "Absolute chroma ceiling.  Keeps the result reading as near neutral.")

(defun oklch--cbrt (x)
  (if (< x 0) (- (expt (- x) (/ 1.0 3.0))) (expt x (/ 1.0 3.0))))

(defun oklch--decode (c)
  "sRGB component to linear light."
  (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4)))


(defun oklch--encode (c)
  "Linear light to sRGB component, clamped."
  (let ((c (max 0.0 (min 1.0 c))))
    (if (<= c 0.0031308) (* 12.92 c) (- (* 1.055 (expt c (/ 1.0 2.4))) 0.055))))


(defun oklch--parse (color)
  "COLOR as \"#rgb\" \"#rrggbb\"."
  (let* ((s (replace-regexp-in-string "\\`#" "" (string-trim color)))
         (n (length s)))
    (if (and (memq n '(3 6 12)) (string-match-p "\\`[0-9a-fA-F]'" s))
        (let* ((k (/ n 3))
               (m (float (1- (expt 16 k)))))
          (cl-loop for i below 3
                   collect (/
                            (string-to-number (substring s (* i k) (* (1+ i) k)) 16)
                            m)))
      (or (color-name-to-rgb color)
          (error "Cannot parse color: %s" color)))))

(defun oklch--hex (rgb)
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (c) (round (* 255 (max 0.0 (min 1.0 c))))) rgb)))

(defun oklch--rgb-to-oklab (rgb)
  (cl-destructuring-bind (r g b) (mapcar #'oklch--decode rgb)
    (let ((l (oklch--cbrt (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b))))
          (m (oklch--cbrt (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b))))
          (s (oklch--cbrt (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))))
      (list (+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s))
            (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s))
            (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s))))))

(defun oklch--oklab-to-linear (lab)
  "Return linear RGB, possibly outside [0,1] if LAB is out of gamut."
  (cl-destructuring-bind (bigl a b) lab
    (let ((l (expt (+ bigl (* 0.3963377774 a) (* 0.2158037573 b)) 3))
          (m (expt (+ bigl (* -0.1055613458 a) (* -0.0638541728 b)) 3))
          (s (expt (+ bigl (* -0.0894841775 a) (* -1.2914855480 b)) 3)))
      (list (+ (* 4.0767416621 l) (* -3.3077115913 m) (* 0.2309699292 s))
            (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s))
            (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s))))))

(defun oklch--in-gamut-p (linear)
  (cl-every (lambda (c) (and (>= c -0.0001) (<= c 1.0001))) linear))

(defun oklch--to-hex (l c h)
  "Convert L C H to a hex string, reducing chroma until sRGB can hold it."
  (let ((lo 0.0) (hi c) (best 0.0))
    (dotimes (_ 24)
      (let ((mid (/ (+ lo hi) 2)))
        (if (oklch--in-gamut-p (oklch--oklab-to-linear (list l (* mid (cos h)) (* mid (sin h)))))
            (setq best mid lo mid)
          (setq hi mid))))
    (oklch--hex (mapcar #'oklch--encode (oklch--oklab-to-linear (list l (* best (cos h)) (* best (sin h))))))))

;;;; Public

(cl-defun oklch-mono-bg (color &key (chroma-scale oklch-mono-bg-chroma-scale) (chroma-max oklch-mono-bg-chroma-max))
  (cl-destructuring-bind (l a b) (oklch--rgb-to-oklab (oklch--parse color))
    (let ((c (min (* chroma-scale (sqrt (+ (* a a) (* b b)))) chroma-max))
          (h (atan b a))
          (l* (if (eq (frame-parameter nil 'background-mode) 'light)
                  oklch-mono-bg-light-l
                oklch-mono-bg-dark-l)))
      (ignore l)
      (oklch--to-hex l* c h))))

(provide 'oklch)
;;; oklch.el ends here
