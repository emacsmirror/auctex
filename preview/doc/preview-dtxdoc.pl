#!/usr/bin/perl
# Simpleminded autoconverter from preview.dtx to preview-dtxdoc.texi
# run as 'perl preview-dtxdoc.pl < ../preview.dtx > preview-dtxdoc.texi'
# Author Jan-Åke Larsson <jalar@mai.liu.se>

# Eat header 
MUNGE: while (<STDIN>) {
    last MUNGE if /^%.section/;
}

# Noindent is used sometimes after \end{quote} (see below) 
$noindent="";
# Quote environments is translated into @example _without_
# @code{..} inside (see below) 
$quote="";
MAIN: while (<STDIN>) {
    s/^%//;

    # Text-substitution macros
    s/AUC~\\TeX[\\ ]?/\@AUCTeX{}/g;
    s/\\LaTeX[\\ ]?/\@LaTeX{}/g;
    s/\\TeX[\\ ]?/\@TeX{}/g;
    s/EPS/\@acronym{EPS}/g;
    s/DVI/\@acronym{DVI}/g;

    # Environments
    if (s/\\begin\{quote\}/\n\@example/) { 
	$quote="yes" }
    if (/^\w/) { 
	print $noindent } 
    $noindent = "";
    if (s/\\end\{quote\}/\@end example\n/) { 
	$quote=""; 
	$noindent="\@noindent\n"  }
    s/\\begin\{description\}/\n\@table \@w/;
    # Convoluted pattern: handle 
    # \item[|...|], \item[\meta{..}] and \item[{|[]|}]
    s/\\item\[\{?(.+?[\|\}])\}?\] ?/\@item $1\n/;
    s/\\end\{description\}/\@end table\n/;
    s/\\begin\{enumerate\}/\n\@enumerate/;
    s/\\item /\@item /;
    s/\\end\{enumerate\}/\@end enumerate\n/;

    # Formatting (\cmd is special within {quote})
    s/\\texttt/\@option/g;
    s/\\marg/ \@var/g;
    s/\\meta/\@var/g;
    s/\\emph/\@emph/g;
    s/\\cmd(\\[\(\)\w]+)/|$1|/g;
    s/\\cmd\{(.*?)\}/|$1|/g;
    s/\\oarg\{(\w+?)\}/\[\@var{$1}\]/g;
    s/\\char.//g;
    s/\\raggedright\n//g;
    s/\\DescribeEnv\{(.*?)\} /\@item \\begin\@{$1\@}\@dots{}\\end\@{$1\@}\n/;
    if (s/\\DescribeMacro\{(.*?)\}[ \n]/\@item $1\n/) {
	# Index entries for two important macros
	if (/(\\Preview(Macro|Environment))[ \n]/) {
	    $_ .= "\@findex $1\n";
	}
    }

    # ||||||| Hell... I hate bars
    # Braces WITHIN bars should be escaped like so: @{ @}
    # and |..| translates to @code{..} or @file{..} depending on content
    # and to .. if in {quote}
    split /\|/;
    $odd=0;
    COMMAND: foreach (@_) {
	if ($odd==0) {
	    $odd=1;
	} else {
	    s/\{/\@\{/g;
	    s/\}/\@\}/g;
	    if (! $quote) {
		if (/[.\/]/) {
		    $_="\@file\{".$_."\}";
		} else {
		    $_="\@code\{".$_."\}";
		}
	    }
	    $odd=0;
	}
    }
    $_=join("",@_);
    # Argh! mixed types occurs in @code{...}@var{..}@file{..}
    # Should be @file{...@var{..}..}
    s/\@code(\S*?)\}(\S*)\@file\{/\@file$1$2/g;

    # Texinfo @node-ification
    if (s/\\section\{(.*)\}/\@subsection $1/) {
	if (s/[Oo]ptions/options/) {
	    $_="\@menu
* Package options::             
* Provided commands::           
\@end menu

\@node Package options, Provided commands, The LaTeX style file, The LaTeX style file\n" . $_;
	} elsif (s/[Cc]ommands/commands/) {
        # \Describe... needs @table
	    $_= "\@node Provided commands, ,Package options, The LaTeX style file\n" . 
		$_ . "\n\@table \@code\n";
	}
    }
	
    # Stop here
    # \Describe.... needs @end table
    if (/^.StopEventually/) {
	print "\@end table\n";
	last MAIN;
    }
    print $_;
}
