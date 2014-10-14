% borrowed some formatting from 
% http://svn.r-project.org/R/trunk/doc/NEWS.Rd
% -*- coding: utf-8 -*-
%%\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
%%\newcommand{\Rlogo}{\if{html}{\figure{../../html/logo.jpg}}\if{latex}{\figure{logo.jpg}{options: width=0.5in}}}
\newcommand{\GITpkg}{\href{https://github.com/abarbour/#1}{\pkg{#1}}}
\newcommand{\pborepo}{\GITpkg{pborepo}}

\name{pborepo}
\title{News for package 'pborepo'}
\encoding{UTF-8}

% template vim:5Y
%
%\section{CHANGES IN \pborepo{} VERSION X.Y-Z (XXXX-YY-ZZ)}{
%  \itemize{ \item \code{} }
%}
%

\section{CHANGES IN \pborepo{} VERSION 0.2013.247-5 (2014-10-14)}{
  \itemize{ 
    \item New function to load from https: \code{read.https}, with wrapper
    to access the data sets as they are on github
  }
}

