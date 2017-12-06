\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{masterthesis}

\LoadClass[a4paper,11pt]{article}

%%%%% packages
\usepackage{geometry}
\geometry{a4paper,left=30mm,right=30mm, top=3cm, bottom=3cm}

\usepackage[utf8]{inputenc}
\usepackage[english]{babel}
% \usepackage[ngerman]{babel}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsfonts}
\usepackage{amsthm}
\usepackage{IEEEtrantools}
\usepackage{todonotes}
\usepackage{xcolor}
\usepackage{graphicx}
\usepackage{float}
\usepackage{enumerate}
%\usepackage{newalg}
\usepackage{algorithm}
\usepackage[noend]{algpseudocode}
\usepackage{cite}

\let\oldReturn\Return
\renewcommand{\Return}{\State\oldReturn}

\usepackage[pdfpagelabels=true,linktocpage]{hyperref}

\usepackage{tikz}
\usetikzlibrary{arrows,decorations.pathmorphing,positioning,fit,trees,shapes,automata}
\usepackage{pgf-umlsd}
\usepgflibrary{arrows} % for pgf-umlsd

\usepackage{listings}
\lstset{language=C++}

\usepackage{todonotes}

\theoremstyle{definition}

%%%%% theorem environments
\newtheorem{definition}{Definition}
\newtheorem{theorem}{Theorem}
\newtheorem{Algorithm}{Algorithm}

%%%%% Float styling
\floatstyle{ruled}
\newfloat{Algorithm}{thp}{lop}
\floatname{Algorithm}{Algorithm}
\restylefloat{table}
