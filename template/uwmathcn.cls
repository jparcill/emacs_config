% Written by Michael A. La Croix (malacroi@alumni.uwaterloo.ca)
% for use in the Math 136 and related course notes.  With funding
% provided by the Math Endowment Fund.
%
% This class provides a mechanism for creating theorem environments
% (using the syntax of amsthm), that are typeset of coloured
% backgrounds.  All theorem environments produced in this manner
% support an optional argument to include a heading.
%
% An aesthetically matched definition environment is used to
% place keywords in the left margin.
%
% The option
%   nocolor can be passed to the class to switch all margin
%   labels to black, and to produce greyscale backgrounds
%   that are better suited to mass printing.
%
% \theoremcolors{rgb}{0.8,.4,0}{rgb}{1,.85,.5}
%    % Theorems will use orange headings and a light orange background
%
% \newtheorem{thm}{Theorem}[section]
%    % The thm environment is labelled Theorem, and numbered 
%    % within sections
%
% \newtheorem{lem}[thm]{Lemma}
%    % A corresponding environment for lemmas will share the
%    % same counter
%
% \theoremcolors{gray}{0}{gray}{.85}
%    % Subsequent theorems will use black labels and
%    % light gray backgrounds
%
% \newtheorem*{rem}{Remark}
%    % An environment for remarks will be unnumbered, and
%    % place labels within the shaded block
%
% % This theorem will appear on an orange background
% \begin{thm}[The Fundamental Theorem of Everything]
%   What a great theorem.
% \end{thm}
%
% % Note: the marginal definition notes cannot be placed
% % using marginpar, since it is fundamentally incompatible
% % with framed.
% \begin{defn}{term 1\\term 2\\term 3}
% \end{defn}
%
%
% TODO:
%   Integrate the exercises package
%   Hook for controlling the typesetting of theorem numbers
%   Hooks for the defn environment
%
% Notes:
%   The shaded backgrounds are obtained via the framed
%   package.  The mechanism is fundamentally incompatible 
%   with marginpar, footnotes, and column balancing.
%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{uwmathcn}[2010/12/09 A course notes class for UW Math]

% This should probably take the font size as an option
\LoadClass[12pt,openany]{book}

% Make our life a lot easier
\RequirePackage{color}

% make margins behave nicely
\RequirePackage[asymmetric]{geometry} 

% Stop the user from loading amsthm on their own and clobbering
% our newtheorem command.
\RequirePackage{amsmath,amsthm,amssymb}

% This package is a very mature was of dealing with headers
\RequirePackage{fancyhdr}

% Framed is listed as experimental, but it does exactly what
% we need to allow shaded backgrounds to split across page
% boundaries.
\RequirePackage{framed}

\usepackage[dvips]{graphicx}
\usepackage[dvips]{psfrag}
\usepackage{paralist}
\usepackage{array}
\usepackage{tabularx}
\usepackage{polynom}

%% Set up the fonts
\RequirePackage{txfonts}
\renewcommand{\normalsize}{\fontsize{12}{14}\selectfont}
\renewcommand{\large}{\fontsize{13}{15}\selectfont}
\newcommand{\ExampleHeadingFont}{\fontsize{14}{14}\selectfont}
\newcommand{\TheoremHeadingFont}{\fontsize{14}{14}\selectfont}
\newcommand{\ProblemHeadingFont}{\fontsize{14}{14}\bfseries\selectfont}
\newcommand{\TheoremContFont}{\fontsize{12}{14}\selectfont}
\renewcommand{\Large}{\fontsize{14}{16.5}\selectfont}
\newcommand{\DefHeadFont}{\fontsize{14}{14}\bfseries\selectfont}%
\newcommand{\keywordstyle}{\fontsize{8.5}{10}\bfseries\Highlight\selectfont}


% Course Notes will be available in two forms:
%   color - for online viewing
%   nocolor - for printing (make sure most text is black)
\newif\if@nocolor
\DeclareOption{nocolor}{\@nocolortrue}
\ProcessOptions

% A box and a length for measuring the marginal names of theorems
\newsavebox{\thmbox}\newlength{\thmsize}
\renewcommand{\newtheorem}{\@ifstar{\newtheoremStar}{\newtheoremNoStarx}}
% \theoremcolors set the value for margin text and background colours
% associated with all newtheorem environments declared before the
% next use of theoremcolors
\newcommand{\theoremcolors}[4]{%space value space values
  \def\newtheoremNoStarx##1{%
    \@ifnextchar[{\newtheoremNoStary{##1}}{\newtheoremNoStary{##1}[##1]}%
  }
  \def\newtheoremNoStary##1[##2]##3{%
    \@ifnextchar[{\newtheoremNoStar{##1}[##2]{##3}}%
    {\newtheoremNoStar{##1}[##2]{##3}[]}%
  }
  \def\newtheoremNoStar##1[##2]##3[##4]{%
    \@ifundefined{c@##2}{\ifx\relax##4\relax\newcounter{##2}%
      \else\newcounter{##2}[##4]\fi}{}%
    \newenvironment{##1}[1][]{%
      \refstepcounter{##2}%
      \sbox{\thmbox}{\bf\color[#1]{#2}\large\MakeUppercase{##3} %
        \arabic{chapter}.\arabic{section}.\arabic{##2}\hspace*{\marginlabelgap}}%Numbering of environment
      \settowidth{\thmsize}{{\usebox\thmbox}}%
      \definecolor{shadecolor}{#3}{#4}\begin{shaded}%
      \hspace*{-\thmsize}\raisebox{0pt}[0pt][0pt]{\usebox{\thmbox}}\ignorespaces%
      \ifx\relax####1\relax\else%
        \textcolor[#1]{#2}{\large\bf(####1)}\par\nobreak%
      \fi%
      }%
      {\end{shaded}\ignorespacesafterend}%
  }%
  \def\newtheoremStar##1##2{%
    \newenvironment{##1}[1][]{%
      \goodbreak%
      \definecolor{shadecolor}{#3}{#4}\begin{shaded}%
        \textcolor[#1]{#2}{{\large\bf\MakeUppercase{##2}%
          \ifx\relax####1\relax\else\ (####1)\fi}}%
        \par\nobreak}%
      {\end{shaded}}%
  }
}
% The default theorem environment uses black text and a
% light gray background.  If the nocolor option has been
% passed, redefined \theoremcolors to stop the user from
% changing this behaviour.
\theoremcolors{gray}{0}{gray}{.95}
\if@nocolor\renewcommand{\theoremcolors}[4]{}\fi

% This is copied from amsthm, but with \textit replaced
% by \textbf
\renewenvironment{proof}[1][\proofname]{\par
  \pushQED{\qed}%
  \normalfont \topsep6\p@\@plus6\p@\relax
  \trivlist
  \item[\hskip\labelsep%
        \textbf{#1\@addpunct{:}}]\ignorespaces
}{%
  \popQED\endtrivlist\@endpefalse
}
\providecommand{\proofname}{Proof}

% override the spacing of a new line between key words
% also practise working with tail recursion
\def\setkeywords#1\\{%
  \ifx\relax#1\relax\let\nextstep\relax%
  \else#1\\[\keywordskip]\let\nextstep\setkeywords%
  \fi\nextstep%
}
%
\newenvironment{defn}[1]{%
  \ignorespaces%encourage a page break before the definition
  \if@nocolor\definecolor{shadecolor}{gray}{.95}%
  \else\definecolor{shadecolor}{rgb}{1,1,.65}\fi\begin{shaded}%
    \sbox{\thmbox}{\if@nocolor\else\color[rgb]{.5,.3,0}\fi%
      \bf\large\MakeUppercase{Definition}\hspace*{\marginlabelgap}}%
    \settowidth{\thmsize}{\usebox{\thmbox}}%
    \makebox[0pt]{\hspace*{-\thmsize}\raisebox{0pt}[0pt][0pt]{\usebox{\thmbox}}}%
    \ifx\relax#1\relax\else%
    \makebox[0pt]{\hspace*{-\thmsize}\raisebox{0pt}[0pt][0pt]{%
        \addtolength{\thmsize}{-\marginlabelgap}%
        \begin{minipage}[t]{\thmsize}\noindent%
          \renewcommand\baselinestretch{.8}%
          \footnotesize\bf\begin{center}\setkeywords#1\\\\\end{center}%
      \end{minipage}}\hspace*{\marginlabelgap}}%
    \fi\ignorespaces%
}{\ignorespacesafterend\end{shaded}}

% This might not be appropriate for other courses with
% less blocky display math
\allowdisplaybreaks[3]

% The table of contents should have small interline spacing
\let\@oldtableofcontents\tableofcontents
\def\tableofcontents{{\setlength\parskip{0pt}\@oldtableofcontents}}

\let\@origdoublepage\cleardoublepage
\renewcommand{\cleardoublepage}{\clearpage{\pagestyle{empty}\@origdoublepage}}

% Set page measurements
\setlength{\parindent}{0pt}
\setlength{\parskip}{1.5ex plus 1ex minus 0.5ex}
\setlength{\oddsidemargin}{1in}
\setlength{\evensidemargin}{1in}
\setlength{\topmargin}{-0.75in}
\setlength{\headheight}{15pt}
\setlength{\paperheight}{11in}
\setlength{\paperwidth}{8.5in}
\setlength{\textheight}{9.5in}
\setlength{\textwidth}{5.6in}
\setlength{\abovecaptionskip}{1ex}
\newlength{\marginlabelgap}\setlength{\marginlabelgap}{.25in}
\newlength{\keywordskip}\setlength{\keywordskip}{1ex}

\let\@oldmainmatter\mainmatter
\let\@oldfrontmatter\frontmatter
\renewcommand{\mainmatter}{\@oldmainmatter%
  \setlength{\oddsidemargin}{1.13in}%
  \setlength{\evensidemargin}{1.13in}%
  \pagestyle{fancy}%
}
\renewcommand{\frontmatter}{\@oldfrontmatter%
  \setlength{\oddsidemargin}{.47in}%
  \setlength{\evensidemargin}{.47in}%  
  \pagestyle{plain}%
}

% Figures
\setlength{\textfloatsep}{.25in}
\setlength{\intextsep}{15pt}
\let\@beginfigure\figure
\let\@endfigure\endfigure
\renewcommand{\figure}{\@beginfigure\begin{center}}
\renewcommand{\endfigure}{\end{center}\@endfigure}
\newcommand{\captionsize}{\footnotesize}
\newcommand{\captionfonts}{\captionsize}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{{\captionfonts \textbf{#1: }#2}}%
  \ifdim \wd\@tempboxa >\hsize
    {\captionfonts #1: #2\par}
  \else
    \hbox to\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\renewcommand{\fps@figure}{!htb}
\renewcommand{\fps@table}{!htb}

% This is supposed to help TeX understand that stretching white
% space is OK
\addtolength{\topskip}{0pt plus 10pt}
\reversemarginpar

% Set up the headers and footers
\pagestyle{fancy}
\renewcommand{\chaptermark}[1]{\markboth{#1}{}}
\renewcommand{\sectionmark}[1]{\markright{#1}}
\fancyhfoffset[L]{1.5in}
\renewcommand{\headrulewidth}{0.0pt}
\lhead[{\thepage} ]{Section \thesection \quad \rightmark}
\rhead[\chaptername \thickspace \thechapter \quad \leftmark]{{\thepage}}
\cfoot{}

\widowpenalty=100
\clubpenalty=100
\raggedbottom
