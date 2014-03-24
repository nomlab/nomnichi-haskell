%%
%% 演習予稿用クラスファイル
%% $Id: yokou.cls,v 1.3 1999/11/30 17:02:56 kurisu Exp $
%%
%% Update: Thu Oct 05 16:39:15 JST 2000 by noguchi@swlab.csce.kyushu-u.ac.jp

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 識別パート
\NeedsTeXFormat{pLaTeX2e}
\ProvidesClass{yokou}[1999/12/01 kurisu]

% オプションの宣言
\DeclareOption{12pt}{\def\@XXpt{12pt}}
\DeclareOption{11pt}{\def\@XXpt{11pt}}
\DeclareOption{10pt}{\def\@XXpt{10pt}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{jarticle}}

% オプションの実行
\ExecuteOptions{12pt}
\ProcessOptions\relax
\PassOptionsToClass{\@XXpt}{jarticle}

% パッケージ, クラスの読み込み
\LoadClass{jarticle}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% クラスファイル本体

\thispagestyle{empty}
\setlength\topmargin{1cm}
\setlength\textheight{22cm}
\setlength\textwidth{14cm}
\setlength\oddsidemargin{1cm}

\def\title{\def\@title}
\def\author{\def\@author}
\def\supervisor{\def\@supervisor}
\def\date{\def\@date}
\def\place{\def\@place}
\def\keyword{\def\@keyword}
\def\type#1{\def\@thehead{#1\hfill}}

% default value
\title{たいとる}
\author{九大 太郎}
\keyword{キーワード1, キーワード2, キーワード3}
\type{情報工学演習予稿かもしれない}
\supervisor{○○ △△ 教授}
\place{何処かしら}
\date{\today}

\def\maketitle{}		% 何もしない

\AtBeginDocument{%
  \newpage
  \begin{center}
    {\Large\bf\@title}
  \end{center}
}

\AtEndDocument{%
  \vskip .5\baselineskip plus 0pt minus .5\baselineskip
  \noindent
  \vfill
  \noindent
  \textbf{キーワード: \hfill\parbox[t]{.8\linewidth}{\@keyword}}
  \vspace*{.5zh}\\
    \centerline{
    \vbox{\halign{#: & #\cr
      発\hfil 表\hfil 者 & \@author \hfill \cr
      指導教官           & \@supervisor \hfil\cr
      日\hfil 時         & \@date \hfil\cr
      場\hfil 所         & \@place \hfil\cr
    }}}
    \vskip 1zw plus 5zw minus 5zw
}
