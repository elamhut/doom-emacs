;;; doom-kanagawa-wave.el --- inspired by Kanagawa.Nvim -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 23, 2016 (28620647f838)
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Maintainer: Henrik Lissner <https://github.com/hlissner>
;; Source: https://github.com/atom/one-dark-ui
;;
;;; Commentary:
;;
;; This themepack's flagship theme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-kanagawa-wave-theme nil
  "Options for the `doom-kanagawa-wave' theme."
  :group 'doom-themes)

(defcustom doom-kanagawa-wave-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanagawa-wave-theme
  :type 'boolean)

(defcustom doom-kanagawa-wave-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanagawa-wave-theme
  :type 'boolean)

(defcustom doom-kanagawa-wave-comment-bg doom-kanagawa-wave-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their
legibility."
  :group 'doom-kanagawa-wave-theme
  :type 'boolean)

(defcustom doom-kanagawa-wave-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kanagawa-wave-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-kanagawa-wave
  "A dark theme inspired by Atom One Dark."
  :family 'doom-kanagawa
  :background-mode 'dark

  ;; name        default   256           16
  ((bg         '("#1F1F28" "080808"       "black"))
   (fg         '("#DCD7BA" "#bfbfbf"     "brightwhite"))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#21242b" "black"       "black"))
   (fg-alt     '("#DCD7BA" "#bfbfbf"     "white"))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#16161D" "black"       "black"))
   (base1      '("#181820" "#1e1e1e"     "brightblack"))
   (base2      '("#1a1a22" "#2e2e2e"     "brightblack"))
   (base3      '("#1F1F28" "#262626"     "brightblack"))
   (base4      '("#2A2A37" "#3f3f3f"     "brightblack"))
   (base5      '("#363646" "#525252"     "brightblack"))
   (base6      '("#54546D" "#6b6b6b"     "brightblack"))
   (base7      '("#656582" "#979797"     "brightblack"))
   (base8      '("#727294" "#dfdfdf"     "white"))

   (grey       base4)
   (fuji-gray      '("#727169" "#727169" "fuji-gray"))
   ;; (fuji-white     '("#DCD7BA" "#ffffff" "fuji-white"))
   (sakura-pink    '("#D27E99" "#717C7C" "sakura-pink"))
   (comet          '("#54536D" "#4e4e4e" "comet"))
   (wave-red       '("#E46876" "#717C7C" "wavered"))
   (wave-acqua1    '("#6A9589" "#6a9589" "wave-acqua1"))
   (wave-acqua2    '("#7AA89F" "#717C7C" "wave-acqua2"))
   (spring-violet1 '("#938AA9" "#717C7C" "sprint-violet1"))
   (spring-green   '("#98BB6C" "#717C7C" "spring-green"))
   (spring-blue    '("#7FB4CA" "#717C7C" "spring-blue"))
   (oni-violet     '("#957FB8" "#717C7C" "oni-violet"))
   (carp-yellow    '("#E6C384" "#717C7C" "carp-yellow"))
   (ronin-yellow   '("#FF9E3B" "#585858" "ronin-yellow"))
   (surimi-orange  '("#FFA066" "#717C7C" "surimi-orange"))
   (crystal-blue   '("#7E9CD8" "#717C7C" "crystal-blue"))
   (dragon-red     '("#E46876" "#717C7C" "dragon-red"))

   (red            '("#ff6c6b" "#ff6655" "red"))
   (orange         '("#da8548" "#dd8844" "brightred"))
   (green          '("#98be65" "#99bb66" "green"))
   (teal           '("#4db5bd" "#44b9b1" "brightgreen"))
   (yellow         '("#ECBE7B" "#ECBE7B" "yellow"))
   (blue           '("#51afef" "#51afef" "brightblue"))
   (dark-blue      '("#2257A0" "#2257A0" "blue"))
   (magenta        '("#c678dd" "#c678dd" "brightmagenta"))
   (violet         '("#a9a1e1" "#a9a1e1" "magenta"))
   (cyan           '("#46D9FF" "#46D9FF" "brightcyan"))
   (dark-cyan      '("#5699AF" "#5699AF" "cyan"))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      spring-violet1)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        spring-blue)
   (comments       (if doom-kanagawa-wave-brighter-comments base5 fuji-gray))
   (doc-comments   (doom-lighten (if doom-kanagawa-wave-brighter-comments base5 fuji-gray) 0.25))
   (constants      surimi-orange)
   (functions      crystal-blue)
   (keywords       oni-violet)
   (operators      oni-violet)
   (methods        cyan)
   (type           wave-acqua2)
   (strings        green)
   (variables      carp-yellow)
   (numbers        sakura-pink)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        ronin-yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-kanagawa-wave-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-kanagawa-wave-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-kanagawa-wave-padded-modeline
      (if (integerp doom-kanagawa-wave-padded-modeline) doom-kanagawa-wave-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-kanagawa-wave-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanagawa-wave-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-kanagawa-wave-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-kanagawa-wave-theme.el ends here
