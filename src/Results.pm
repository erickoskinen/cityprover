package Results;

use Data::Dumper;
use strict;
use CityProver qw{bname};

####################################################################################
# Fetch results


####################################################################################
sub to_tex {
    #return map to_html_row @_;
    die "tex";
}

sub to_tex_concise {
    die "tex concise";
}

sub texProp {
    my ($p) = @_;
    $p =~ s/s([12])->([a-zA-Z]+)/\\sigma[$2]/g;
    $p =~ s/rho_n_1/v_m/g;
    $p =~ s/rho_n_2/v_n/g;
    $p =~ s/rho_x_1/x_1/g;
    $p =~ s/rho_x_2/x_2/g;
    $p =~ s/rho_y_1/y_1/g;
    $p =~ s/rho_//g;
    $p =~ s/\&\&/\\wedge/g;
    $p =~ s/\!=/\\neq/g;
    $p =~ s/1==1/\\textsf{true}/g;
    $p =~ s/==/=/g;
    return '$'.$p.'$';
}

sub bench_to_tex {
    my ($b,$expected) = @_;
    my $mets = qx{./cvv --methods $b}; chomp($mets);
    my $prop = qx{./cvv --varphi  $b}; chomp($prop); $prop = texProp($prop);
    return sprintf("%-8s & %-22s & %-25s & %-8s ", CityProver::bname($b), $mets, $prop,  $expected);
}
########################################################################################
# HTML

sub htmlProp {
    my ($p) = @_;
    $p =~ s/s([12])->([a-zA-Z]+)/\\sigma[$2]/g;
    $p =~ s/\\sigma/&sigma;/g;
    $p =~ s/rho_n_1/v_m/g;
    $p =~ s/rho_n_2/v_n/g;
    $p =~ s/rho_x_1/x_1/g;
    $p =~ s/rho_x_2/x_2/g;
    $p =~ s/rho_y_1/y_1/g;
    $p =~ s/>=/&geq;/g;
    $p =~ s/<=/&leq;/g;
    $p =~ s/rho_//g;
    $p =~ s/\&\&/&xwedge;/g;
    $p =~ s/\!=/&not;/g;
    $p =~ s/1 ?== ?1/true/g;
    $p =~ s/==/=/g;
    return $p;
}
sub to_html_row {
    return Dumper($_[0]);
}

sub to_html {
    return map(to_html_row,@_);
}


        #   ['Mike',  {v: 10000, f: '$10,000'}, true],
        #   ['Jim',   {v:8000,   f: '$8,000'},  false],
        #   ['Alice', {v: 12500, f: '$12,500'}, true],
        #   ['Bob',   {v: 7000,  f: '$7,000'},  true]
sub strings_to_cells {
    return join("",map(sprintf("<td>%-15s</td>",$_), @_));
}
# https://developers.google.com/chart/interactive/docs/gallery/table
sub bench_to_html {
    my ($b) = @_;
    my $mets = qx{./cvv --methods $b}; chomp($mets); $mets =~ s/\\MM\{(.*)\}\{(.*)\}/$1,$2/; $mets =~ s/\$//g;
    my $prop = qx{./cvv --varphi  $b}; chomp($prop); $prop = substr($prop,0,15); $prop = htmlProp($prop);
    my $expected = ($b =~ /-s$/ ? 'true' : 'false');
    #my $bname = 'UNKNOWN'; $bname = CityProver::bname($1) if $b =~ /^([^-]+)-/;
    my $adt= CityProver::bname($b);
    return qq{'$adt', '$mets','$prop', $expected};
    #strings_to_cells(CityProver::bname($b), $mets, $prop,  $expected);
}

sub to_html {
    my ($bInfo,$bRes) = @_;
    print Dumper(\@_);
    my $result = $bRes->{result};
    my $time = $bRes->{time};
    my $color = 'black'; 
    $color = 'red' if $result ne $bInfo->{expected};
    if ($result eq 'UNKNOWN') { $color = 'orange'; $result = 'UNK'; }
    $color = 'black' if $result eq 'n/a';
    my $onclick = 'onclick="return fillDiv('
         .join(',',map('\\\''.$bInfo->{$_}.'\\\'',(qw/benchid stage verifier logurl/)))
         .')"';
    my $popup = '<div class="box"><a class="button" '.$onclick.'>log</a></div>';
    return qq{{v:$time, f: '$time'}, '<b><font color=$color>$result</font></b>','$popup'};
}

sub move_log {
    my ($bInfo) = @_;
    my $tmpname = $bInfo->{htmltmpdir};
    $tmpname =~ s/^.*out-/out-/;
    use File::Copy;
    my $newfn = "".join('-', map($bInfo->{$_},qw/verifier benchid stage/)).".txt";
    warn "move to: $newfn\n";
    copy($bInfo->{logfn},$bInfo->{htmltmpdir}."/".$newfn); # if $bInfo->{verifier} eq 'cpa';
    chmod 0644, "$bInfo->{htmltmpdir}/$newfn";
    $bInfo->{logurl} = "http://dalwhinnie.cs.stevens.edu/cityprover/$tmpname/$newfn";
}
1;