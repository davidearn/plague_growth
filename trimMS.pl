use strict;
use 5.10.0;

while(<>){
	s|\{.*[^{/]*/(.*[.]pdf)|{$1|;
	## s|ms|autosub/Earn_etal_ms| if /externaldocument/;
	print;
}
