use strict;
use 5.26.0;
no autovivification;
use warnings;
binmode STDOUT, ":utf8";
use utf8;
use LWP::UserAgent;
use HTTP::Cookies;
use HTML::Tree;
use Encode qw(decode);
use DBI;
use Time::Piece;
use JSON;
use Data::Printer;
use Math::Round qw(nearest);
use Selenium::Chrome;
use FindBin;
use lib "$FindBin::Bin/../lib";

my $cookie       = HTTP::Cookies->new();
my $driver       = Selenium::Chrome->new();

# Small implicit wait to let elements appear
$driver->set_implicit_wait_timeout(10_000);

my $max_attempts = 5;

my $url          = "https://sdb.socialstyrelsen.se/if_par/val_eng.aspx";
say "Accessing [$url]";
$driver->get($url);

sleep 1;

my $content;
my ($parsing_success, $parsing_attempts) = (0, 0);
while ($parsing_success == 0) {
    $parsing_attempts++;
    eval {
        $content = $driver->get_page_source();
    };
    if ($@) {
        say "failed recovering content from page on [$url] (selenium error)";
        if ($parsing_attempts > $max_attempts) {
            die "Failed to get page.";
        }
        sleep 1;
    } else {
        $parsing_success = 1;
    }
}

### 1. *Type of care:* => value="SVOV"
{
    # Click the option SVOV in the VFORM select
    my $svov_option = $driver->find_element(
        q{//select[@id="VFORM"]/option[@value="SVOV"]},
        'xpath'
    );
    $svov_option->click;
    sleep 1;    # let the page JS react (shows other panels)
}

### 2. *Annual or Monthly Data:* => value="AR"
{
    my $ar_option = $driver->find_element(
        q{//select[@id="ARMANAD_IND"]/option[@value="AR"]},
        'xpath'
    );
    $ar_option->click;
    sleep 1;    # again, let JS update the UI and show diagnoses panel
}

### 3. Expand all diagnoses (SVOV, Annual)
{
    # Make sure the SVOV diagnoses panel has had time to appear
    $driver->set_implicit_wait_timeout(10_000);
    sleep 1;

    # Repeatedly click all expanders (p2.svg) inside the SVOV list
    # until there are no more left.
    my ($cpt, $clicked) = (0, 0);
    while (1) {
        # Click the *link* that wraps the img[src="/images/p2.svg"]
        my @expand_links = $driver->find_elements(
            q{//div[@id="valListArea_DIA_AR_SVOV"]//a[img[@src="/images/p2.svg"]]},
            'xpath'
        );

        last unless @expand_links;   # no more collapsed nodes

        my $clicked = 0;

        for my $link (@expand_links) {
            $cpt++;
            $clicked++;
            if ($cpt == 10) {
                STDOUT->printflush("\r\tClicked [$clicked] expands");
                $cpt = 0;
            }
            eval {
                # Scroll link into view before clicking (important in a long tree)
                $driver->execute_script('arguments[0].scrollIntoView(true);', $link);
                select undef, undef, undef, 0.1;

                $link->click;
                $clicked++;

                # give JS time to inject children
                select undef, undef, undef, 0.2;
            };
            if ($@) {
                warn "Failed to click expander link: $@";
            }
        }

        # Safety: if we didn’t manage to click anything, stop to avoid infinite loop
        last if $clicked == 0;
    }
    STDOUT->printflush("\r\tClicked [$clicked] expands");
    say "";
}

### 4. Check all leaf-diagnosis checkboxes (children only)
{
    # Leaf rows have lChk2 (checkbox) / lTxt3 (text)
    my @leaf_boxes = $driver->find_elements(
        q{//div[@id="valListArea_DIA_AR_SVOV"]
          //div[contains(@class, "lChk2")]/input},
        'xpath'
    );

    my $total = scalar(@leaf_boxes);
    say "Found $total leaf diagnosis checkboxes.";
    my ($cpt, $clicked) = (0, 0);
    for my $cb (@leaf_boxes) {
        $cpt++;
        $clicked++;
        if ($cpt == 10) {
            STDOUT->printflush("\r\tClicked [$clicked / $total] checkboxes");
            $cpt = 0;
        }
        eval {
            # Scroll checkbox into view
            $driver->execute_script('arguments[0].scrollIntoView(true);', $cb);
            select undef, undef, undef, 0.1;

            # Click it (select that diagnosis)
            $cb->click;
            select undef, undef, undef, 0.1;
        };
        if ($@) {
            warn "Failed to click leaf diagnosis checkbox: $@";
        }
    }
    STDOUT->printflush("\r\tClicked [$clicked / $total] checkboxes");
    say "";
}

sleep 120;


my $tree           = HTML::Tree->new();
$tree->parse($content);
open my $out, '>', 'tmp.html';
print $out $tree->as_HTML("<>&", "\t");
close $out;

$driver->shutdown_binary();