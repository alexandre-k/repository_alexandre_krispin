#!/usr/bin/perl -w

# Get latest version of tei files from sourceforge

use strict;
use LWP::Simple;
use Sort::Versions;

print "Getting listing ...\n";
# my $page = get ("http://prdownloads.sourceforge.net/tei");
my $page = get ("http://www.mirrorservice.org/sites/download.sourceforge.net/pub/sourceforge/t/te/tei/");
print "... Done.\n";

my (@doc, @schema, @xsl, @exemplars, %urls);
while ($page =~ m#<a\s+href\s*=\s*"[^"]*tei/(tei-(?:p5-doc|p5-schema|xsl|p5-exemplars)-[0-9.]+)\.zip">#gi)
{
        my $url = $1;
        next if $urls{$url}++;
#         print "$url\n";
        push @doc, $url if $url =~ m#p5-doc#i;
        push @schema, $url if $url =~ m#p5-schema#i;
        push @xsl, $url if $url =~ m#xsl#i;
        push @exemplars, $url if $url =~ m#exemplars#i;
}

@doc = sort vers @doc;
@schema = sort vers @schema;
@xsl = sort vers @xsl;
@exemplars = sort vers @exemplars;

my $xsl = pop @xsl;
my $doc = pop @doc;
my $schema = pop @schema;
my $exemplar = pop @exemplars;

print "Looking for $xsl, $doc, $schema, $exemplar\n";

for my $file ($xsl, $doc, $schema, $exemplar)
{
    my $full = "http://kent.dl.sourceforge.net/tei/" . $file . ".zip";
    print "Getting $full ...\n";
    getstore ($full, "$file.zip");
    print " ... Done.\n";
}

my $record = "$xsl\n$doc\n$schema\n";
unlink "VERSIONS";
open VER, ">VERSIONS";
print VER $record;
close VER;
    
system("rm -rf p5-schema/");
system("unzip $schema.zip");
system("mv $schema/share/xml/tei/schema/relaxng/ ./p5-schema");
system("rm -rf $schema");
unlink "$schema.zip";

system("rm -rf p5-doc/");
system("unzip $doc.zip");
system("mv $doc/share/doc/tei-p5-doc/en/html/ ./p5-doc");
system("rm -rf $doc");
unlink "$doc.zip";

system("rm -rf p5-xsl/");
system("unzip $xsl.zip");
system("mv $xsl/p5/ ./p5-xsl");
system("rm -rf $xsl");
unlink "$xsl.zip";

system("rm -rf p5-templates/");
system("unzip $exemplar.zip");

system("rm -rf p5-example-schemas/");
system("mkdir p5-example-schemas/");
system("mv $exemplar/share/xml/tei/custom/schema/relaxng/* ./p5-example-schemas/");

system("mkdir p5-example-schemas/doc/");
system("mkdir p5-example-schemas/doc/html");
system("mkdir p5-example-schemas/doc/xml");
system("mv $exemplar/share/doc/tei-p5-exemplars/html/* ./p5-example-schemas/doc/html");
system("mv $exemplar/share/doc/tei-p5-exemplars/xml/* ./p5-example-schemas/doc/xml");

system("mv $exemplar/share/xml/tei/custom/templates/ ./p5-templates");
system("rm -rf $exemplar");
unlink "$exemplar.zip";


###############
sub vers
{
    $a =~ m#([0-9.]+)#;
    my $x = $1;
    $b =~ m#([0-9.]+)#;
    my $y = $1;
    return versioncmp ($x, $y);
}
