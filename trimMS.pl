use strict;
use 5.10.0;

while(<>){
	s|\{.*[^{/]*/(.*[.]pdf)|{$1|;
	print;
}
## < \includegraphics[%% height=\pixht]{WillExample1644_page86_1.eps}
## > \includegraphics[height=\pixht]{images/WillExample1644_page86_1.pdf}
