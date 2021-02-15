package Parse;

our @EXPORT_OK = qw{cpa};

sub parse {
    my ($ver,$logfn) = @_;
    return cpa($logfn) if $ver eq 'cpa';
    return ult($logfn) if $ver eq 'ult';
    die "parse";
}
sub cpa {
    my ($fn) = @_;
    open(F,$fn) or die "Parse - file $fn - $!";
    my %out = ( time => 8888888, result => NOPARSE );
    while(<F>) {
        if (/EJK TIMEOUT (\d+)$/) { $out{time} = 999999; $out{result} = 'UNKNOWN'; }
    	if (/Total time for CPAchecker:        ([^s]*)s$/) { $out{time} = $1; }
	    if (/Verification result: TRUE./) { $out{result} = 'TRUE'; }
	    if (/Verification result: FALSE./) { $out{result} = 'FALSE'; }
	    if (/Verification result: UNKNOWN/) { $out{result} = 'UNKNOWN'; $out{warning} = 1; }
    }
    close F;
    return \%out;
}

sub ult {
    my ($fn) = @_;
    open(F,$fn) or die "Parse - file $fn - $!";
    my %out = ( time => 8888888, result => NOPARSE, ult => "hi" );
    while(<F>) {
        if (/EJK TIMEOUT (\d+)$/) { $out{time} = 999999; $out{result} = 'UNKNOWN'; }
        if (/RESULT: Ultimate could not prove your program: unable to determine feasibility of some traces/) 
             { $out{result} = 'UNKNOWN'; $out{warning} = 1; }
        if (/RESULT: Ultimate proved your program to be correct/) {  $out{result} = 'TRUE'; }
        if (/RESULT: Ultimate proved your program to be incorrect/) { $out{result} = 'FALSE'; }
	#if (/TraceAbstraction took (\d+\.\d+) m?s/) { $time = $1/1000; }
        if (/OverallTime: (\d+\.\d+)s,/) { $out{time} = $1; }
	if (/, (\d+\.\d+)s OverallTime,/) { $out{time} = $1; }
        if (/out of memory/) { $out{time} = 777777777; $out{result} = 'UNKNOWN';  }
        if (/Cannot allocate memory/) { $out{time} = 777777777; $out{result} = 'UNKNOWN';  }
    }
    close F;
    use Data::Dumper;
    print "strange:".Dumper(\%out);
    return \%out;
}

sub cpaDep {
    my ($logfn) = @_;
    my ($time,$result) = ('\rUNKNOWN','UNKNOWN');
    open(F,$logfn) or warn "Parse - file $logfn - $!";
    while(<F>) {
        if (/EJK TIMEOUT (\d+)$/) { return ("\\rTIMEOUT{$1}",'\rUNKNOWN'); }
    	if (/Total time for CPAchecker:        ([^s]*)s$/) { $time = $1; push @stage_ts, $1; $total_t += $1; }
	    if (/Verification result: TRUE./) { $result = 'TRUE'; }
	    if (/Verification result: FALSE./) { $result = 'FALSE'; }
	    if (/Verification result: UNKNOWN/) { $result = 'UNKNOWN'; $ok = "\\rFAIL"; }
    }
    close F;
    return ($time,$result);
}

sub ultDep {
    my ($logfn) = @_;
    open(F,$logfn) or warn "file $logfn - $!";
    my ($time,$result) = ('\rUNKNOWN','UNKNOWN');
    while(<F>) {
        if (/EJK TIMEOUT (\d+)$/) { return ("\\rTIMEOUT{$1}",'\rUNKNOWN'); }
        if (/RESULT: Ultimate could not prove your program: unable to determine feasibility of some traces/) { $result = 'UNKNOWN'; }
        if (/RESULT: Ultimate proved your program to be correct/) { $result = 'TRUE'; }
        if (/RESULT: Ultimate proved your program to be incorrect/) { $result = 'FALSE'; }
	#if (/TraceAbstraction took (\d+\.\d+) m?s/) { $time = $1/1000; }
        if (/OverallTime: (\d+\.\d+)s,/) { $time = $1; }
	if (/, (\d+\.\d+)s OverallTime,/) { $time = $1; }
        if (/out of memory/) { $result = 'MEMOUT'; }
        if (/Cannot allocate memory/) { $result = 'MEMOUT'; }
    }
    close F;
    #print "$stage - $time - $result\n";
    return ($time,$result);
}

1;
