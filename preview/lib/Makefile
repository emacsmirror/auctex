listsort.c listsort.h: listsort.tex
	awk '/\\begin{lstlisting}{'`echo "$@"|sed 's/\./\\./'`'}/{inlst=1;next};/\\end{lstlisting}/{inlst=0;next};inlst{print}' $< >$@

