package CityProver;
use Data::Dumper;
our @EXPORT_OK = qw{binfo bname};

sub binfo {
    my ($bkey) = @_;
    my %bn2source = ( 
        m => 'mem', ac => 'accumulator', c => 'counter', s => 'ss', ssnf => 'ssnf', 
        ss3 => 'simpleset', 'as' => 'stack', q => 'queue', q2 => 'queue2', h => 'hashtable' );
    return $bn2source{$bkey};
}

sub get_logfn {
    my ($ANALYSIS_DIR,$ver,$b,$stage) = @_;
    return "$ANALYSIS_DIR/log-$b-$stage" if $ver eq 'ult';
    return "$ANALYSIS_DIR/cpa-$b-$stage/Statistics.txt" if $ver eq 'cpa';
    die "get_logfn: $ver\n";
}

sub bInfo {
    my ($ANALYSIS_DIR,$benchid,$stage,$verifier) = @_;
    print Dumper(\@_);
    die "expected a-b-c-d got $benchid" unless $benchid =~ m/-/;
    my $adtkey; $adtkey = $1 if $benchid =~ /^([^-]+)-/;
    # calculate expected
    my $expected = 'TRUE';
    if ($stage eq 'oneshot') { $expected = 'FALSE' unless $benchid =~ /-s$/; }
    elsif ($stage eq 'rv') { $expected = 'FALSE' if $benchid =~ /-frv$/; }
    elsif ($stage eq 'oeA') { $expected = 'FALSE' if $benchid =~ /-foeA$/; }
    elsif ($stage eq 'oeB') { $expected = 'FALSE' if $benchid =~ /-foeB$/; }
    else { die; }
    return {
        benchid  => $benchid,
        stage    => $stage,
        adtkey   => $adtkey,
        verifier => $verifier,
        sourcefn => binfo($adtkey),
        nicename => bname($adtkey),
        logfn    => get_logfn($ANALYSIS_DIR,$verifier,$benchid,$stage),
        cpalog   => get_logfn($ANALYSIS_DIR,'cpa',$benchid,$stage),
        ultlog   => get_logfn($ANALYSIS_DIR,'ult',$benchid,$stage),
        expected => $expected
    };
}

sub bname {
    my ($bkey) = @_;
    $bkey = $1 if $bkey =~ /^([^-]+)-/;
    my %bn2name = ( m => 'Memory', c => 'Counter', ac => 'Accum.', 'as' => 'ArrayStack', 
                    ssnf => 'SimpleSet-nf', h => 'Hashtable', 'q2' => 'Queue' );
    die "bname $bkey\n" unless defined $bn2name{$bkey};
    return $bn2name{$bkey};
}

sub all_benches {
    my $bs = qx{./cvv --benches};
    my $benches;
    foreach my $b (split ',', $bs) {
        my ($kind) = split('-', $b);
        push @{$benches->{$kind}}, $b;
    }
    return $benches;
}
sub benches_of_type {
    my @kinds = split(',',$_[0]); my @out;
    my $allb = all_benches();
    push @out, @{$allb->{$_}} for @kinds;
    print Dumper(\@out);
    return @out;
}
sub all_benches_str {
    my $all = all_benches();
    return "All Available Benchmarks:\n\n   ".join("\n   ", 
    ( # list
             map(("ADT $_ : ".(join(',',@{$all->{$_}}))), keys %{$all})
    ));
}



1;
