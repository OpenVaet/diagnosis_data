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
use File::Path qw(make_path);
use Text::CSV;
use Selenium::Chrome;
use FindBin;
use lib "$FindBin::Bin/../lib";

my $cookie       = HTTP::Cookies->new();
my $driver       = Selenium::Chrome->new();
my $csv          = Text::CSV->new({
    binary       => 1,
    eol          => "\n",
    always_quote => 1,
}) or die "Cannot use Text::CSV: " . Text::CSV->error_diag();


# Small implicit wait to let elements appear
$driver->set_implicit_wait_timeout(10_000);

my $max_attempts = 5;

# Will hold all leaf diagnoses (checkbox id + label text)
my @diagnoses    = ();
# Will hold the data parsed.
my %diag_data    = ();

my $out_dir      = "data/sw/diagnosis_csv";
my $output_file  = "$out_dir/diagnosis_2008_2024.csv";
my $state_file   = "$out_dir/diagnosis_2008_2024.state.json";
unless (-d $out_dir) {
    make_path($out_dir) or die $!;
}

# Age groups targeted.
my %age_groups   = ();
$age_groups{1}   = '0-4 years';
$age_groups{2}   = '5-9 years';
$age_groups{3}   = '10-14 years';
$age_groups{4}   = '15-19 years';
$age_groups{5}   = '20-24 years';
$age_groups{6}   = '25-29 years';
$age_groups{7}   = '30-34 years';
$age_groups{8}   = '35-39 years';
$age_groups{9}   = '40-44 years';
$age_groups{10}  = '45-49 years';
$age_groups{11}  = '50-54 years';
$age_groups{12}  = '55-59 years';
$age_groups{13}  = '60-64 years';
$age_groups{14}  = '65-69 years';
$age_groups{15}  = '70-74 years';
$age_groups{16}  = '75-79 years';
$age_groups{17}  = '80-84 years';
$age_groups{18}  = '85+ years';

my $url          = "https://sdb.socialstyrelsen.se/if_par/val_eng.aspx";
say "Accessing [$url]";
$driver->get($url);

sleep 1;

### 1. *Type of care:* => value="SVOV"
fill_type_of_care();
### 2. *Annual or Monthly Data:* => value="AR"
fill_annual();
### 3. *County of residence* => value="00"
fill_region();
### 4. *Sex* => value="3"
fill_sex();
### 5. *Measure* => "Select all"
click_all_measures();
### 6. *Year* => "Select all"
click_all_years();
### 7. Expand all diagnoses (SVOV, Annual)
expand_all_diags();
### 8. Collect all leaf-diagnosis checkboxes (children only)
collect_diagnosis();
### 9. Process diagnoses in batches of 100 (select 100 codes at a time)
my $last_completed_batch = load_state();
process_diagnoses($last_completed_batch);

$driver->shutdown_binary();

sub expand_all_diags {
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

        for my $link (@expand_links) {
            $cpt++;
            $clicked++;
            if ($cpt == 10) {
                STDOUT->printflush("\rClicked [$clicked] expands");
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
    STDOUT->printflush("\rClicked [$clicked] expands");
    say "";
}


sub collect_diagnosis {
    # Leaf rows have lChk2 (checkbox) / lTxt3 (text)
    my @leaf_inputs = $driver->find_elements(
        q{//div[@id="valListArea_DIA_AR_SVOV"]
          //div[contains(@class, "lChk2")]/input},
        'xpath'
    );

    say "Found " . scalar(@leaf_inputs) . " leaf diagnosis checkboxes.";

    for my $inp (@leaf_inputs) {
        my $id = $inp->get_attribute('id');

        # Use the driver (not $inp) to locate the <a> label corresponding to this checkbox id.
        # XPath logic:
        #   - find the input with this @id inside lChk2
        #   - go up to its lRow ancestor
        #   - then down to lTxt3/a to get the text like "A00 Cholera"
        my $a;
        eval {
            $a = $driver->find_element(
                '//div[@id="valListArea_DIA_AR_SVOV"]'
              . '//div[contains(@class,"lChk2")]/input[@id="' . $id . '"]'
              . '/ancestor::div[contains(@class,"lRow")]'
              . '//div[contains(@class,"lTxt3")]/a',
                'xpath'
            );
        };
        if ($@) {
            warn "Could not find label <a> for checkbox id=$id : $@";
            next;
        }

        my $label = $a->get_text;

        # Optional: extract ICD code prefix from the label (e.g. "A00" from "A00 Cholera")
        my ($code) = $label =~ /^(\S+)/;

        push @diagnoses, {
            id    => $id,
            label => $label,
            code  => $code // '',
        };
    }

    say "Collected " . scalar(@diagnoses) . " leaf diagnoses into memory.";
}

sub process_diagnoses {
    my ($last_completed_batch) = @_;

    my $batch_size = 100;
    my $total      = scalar @diagnoses;
    my $start      = 0;
    my $batch_no   = 0;

    my @prev_batch_ids;    # ids checked in the previous batch, to uncheck them

    while ($start < $total) {
        $batch_no++;
        my $end = $start + $batch_size - 1;
        $end = $total - 1 if $end >= $total;

        # --- NEW: skip batches we already processed ---
        if ($batch_no <= ($last_completed_batch // 0)) {
            say "=== Batch $batch_no: diagnoses $start .. $end (of $total) => already processed, skipping ===";
            $start = $end + 1;
            next;
        }

        say "=== Batch $batch_no: diagnoses $start .. $end (of $total) ===";

        # clear any old data in memory for this batch
        %diag_data = ();

        # 1) Unselect previous diagnoses
        unselect_all_diagnoses();

        # 2) Select the current batch
        my @current_batch_ids;
        my $clicked = 0;
        my $cpt     = 0;
        for my $i ($start .. $end) {
            my $id    = $diagnoses[$i]{id};
            my $label = $diagnoses[$i]{label};

            eval {
                my $cb = $driver->find_element(
                    '//div[@id="valListArea_DIA_AR_SVOV"]//input[@id="' . $id . '"]',
                    'xpath'
                );

                $driver->execute_script('arguments[0].scrollIntoView(true);', $cb);
                select undef, undef, undef, 0.05;

                $cb->click;    # check this diagnosis
                $clicked++;
                push @current_batch_ids, $id;

                $cpt++;
                if ($cpt == 10) {
                    STDOUT->printflush(
                        "\rBatch $batch_no: selected $clicked diagnoses (currently [$diagnoses[$i]{code}] $label)                                                            "
                    );
                    $cpt = 0;
                }
            };
            warn "Failed to click checkbox $id ($label): $@" if $@;
        }
        STDOUT->printflush("\rBatch $batch_no: selected $clicked diagnoses");
        say "";

        # 3) Loop over ages, fetch table, fill %diag_data as you already do
        for my $age_group_code (sort { $a <=> $b } keys %age_groups) {
            my $age_group = $age_groups{$age_group_code} // die;
            select_specific_age($age_group_code, $age_group);
            click_view_all();
            sleep 2;

            # --- your existing parsing code stays the same ---
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
            my $tree = HTML::Tree->new();
            $tree->parse($content);

            my $summary_span = $tree->look_down(id => "ph1_lblTableTitle");
            my $summary      = $summary_span->as_trimmed_text;
            $summary =~ s/,/-/g;
            $diag_data{$age_group_code}{'age_group'} = $age_group;
            $diag_data{$age_group_code}{'summary'}   = $summary;

            my $table = $tree->look_down(id => "ph1_GridView1");
            my @trs   = $table->find('tr');

            my %headers;
            for my $tr (@trs) {
                if ($tr->find('th')) {
                    my @ths = $tr->find('th');
                    my $n = 0;
                    for my $th (@ths) {
                        $n++;
                        $headers{$n} = $th->as_trimmed_text;
                    }
                } else {
                    my %row;
                    my @tds = $tr->find('td');
                    my $n   = 0;
                    for my $td (@tds) {
                        $n++;
                        my $label = $headers{$n} // die;
                        $row{$label} = $td->as_trimmed_text;
                    }
                    my $measure = $row{'Measure'} // die;
                    my $diagnos = $row{'Diagnos'} // die;
                    for my $label (keys %row) {
                        next if $label eq 'Measure' || $label eq 'Diagnos';
                        my $value = $row{$label} // die;
                        $diag_data{$age_group_code}{'measures'}{$measure}{$diagnos}{$label} = $value;
                    }
                }
            }

            return_to_form();
            sleep 2;
            unselect_all_ages();
        }

        # 4) Write what we just collected for this batch to CSV (append)
        print_data_for_batch($batch_no);

        # 5) Save checkpoint so we can resume later
        save_state($batch_no);

        # 6) Prepare for next batch
        @prev_batch_ids = @current_batch_ids;
        $start          = $end + 1;
    }
}

sub fill_type_of_care {
    # Click the option SVOV in the VFORM select
    my $svov_option = $driver->find_element(
        q{//select[@id="VFORM"]/option[@value="SVOV"]},
        'xpath'
    );
    $svov_option->click;
    sleep 1;    # let the page JS react (shows other panels)
}

sub fill_annual {
    my $ar_option = $driver->find_element(
        q{//select[@id="ARMANAD_IND"]/option[@value="AR"]},
        'xpath'
    );
    $ar_option->click;
    sleep 1;    # again, let JS update the UI and show diagnoses panel
}

sub fill_region {
    my $omrhl_option = $driver->find_element(
        q{//select[@id="OMR_HL"]/option[@value="00"]},
        'xpath'
    );
    $omrhl_option->click;
    sleep 1;    # again, let JS update the UI and show diagnoses panel
}

sub fill_sex {
    my $kon_option = $driver->find_element(
        q{//select[@id="KON"]/option[@value="3"]},
        'xpath'
    );
    $kon_option->click;
    sleep 1;    # again, let JS update the UI and show diagnoses panel
}

sub click_all_measures {
    my $all_meas_button = $driver->find_element(
        q{//a[@id="ph1_val_matt_ar_svov_hlAdd"]},
        'xpath'
    );

    $driver->execute_script('arguments[0].scrollIntoView(true);', $all_meas_button);
    select undef, undef, undef, 0.1;

    $all_meas_button->click;
    sleep 1;    # let the page JS react
}

sub click_all_years {
    my $all_years_button = $driver->find_element(
        q{//a[@id="ph1_val_ar_svov_hlAdd"]},
        'xpath'
    );

    $driver->execute_script('arguments[0].scrollIntoView(true);', $all_years_button);
    select undef, undef, undef, 0.1;

    $all_years_button->click;
    sleep 1;    # let the page JS react
}

sub click_view_all {

    my $view_all_button = $driver->find_element(
        q{//a[@id="ph1_Val_data_lnkVisaResultat"]},
        'xpath'
    );

    $driver->execute_script('arguments[0].scrollIntoView(true);', $view_all_button);
    select undef, undef, undef, 0.1;

    $view_all_button->click;
    sleep 1;    # let the page JS react
}

sub unselect_all_diagnoses {
    my $unselect_all_diags_button = $driver->find_element(
        q{//a[@id="ph1_val_dia_ar_svov_hlDel"]},
        'xpath'
    );

    $driver->execute_script('arguments[0].scrollIntoView(true);', $unselect_all_diags_button);
    select undef, undef, undef, 0.1;

    $unselect_all_diags_button->click;
    sleep 1;    # let the page JS react
}

sub select_specific_age {
    my ($age_group_code, $age_group) = @_;

    my $agi_option = $driver->find_element(
        '//select[@id="AGI"]/option[@value="' . $age_group_code . '"]',
        'xpath'
    );

    # scroll into view before clicking
    $driver->execute_script('arguments[0].scrollIntoView(true);', $agi_option);
    select undef, undef, undef, 0.1;

    $agi_option->click;
    sleep 1;    # let the page react
}

sub unselect_all_ages {
    my $unselect_age_button = $driver->find_element(
        q{//a[@id="ph1_val_agi_5ars_hlDel"]},
        'xpath'
    );

    $driver->execute_script('arguments[0].scrollIntoView(true);', $unselect_age_button);
    select undef, undef, undef, 0.1;

    $unselect_age_button->click;
    sleep 1;    # let the page JS react

}

sub return_to_form {

    my $bck_form_button = $driver->find_element(
        q{//a[@id="ph1_lbAndraUrval"]},
        'xpath'
    );

    $driver->execute_script('arguments[0].scrollIntoView(true);', $bck_form_button);
    select undef, undef, undef, 0.1;

    $bck_form_button->click;
    sleep 1;    # let the page JS react

}

sub print_data_for_batch {
    my ($batch_no) = @_;

    my $file_exists = -e $output_file ? 1 : 0;

    open my $out, '>>:encoding(UTF-8)', $output_file
        or die "Can't open $output_file for appending: $!";

    # Write header only once (when file is first created)
    if (!$file_exists) {
        $csv->print($out, [
            'batch_no',
            'age_group_code',
            'age_group',
            'summary',
            'measure',
            'diagnos',
            'year',
            'value',
        ]);
    }

    for my $age_group_code (sort { $a <=> $b } keys %diag_data) {
        my $age_group = sanitize_text($diag_data{$age_group_code}{'age_group'} // die);
        my $summary   = sanitize_text($diag_data{$age_group_code}{'summary'}   // die);

        for my $measure (sort keys %{ $diag_data{$age_group_code}{'measures'} }) {
            my $measure_clean = sanitize_text($measure);

            for my $diagnos (sort keys %{ $diag_data{$age_group_code}{'measures'}{$measure} }) {
                my $diagnos_clean = sanitize_text($diagnos);

                for my $year (
                    sort { $a <=> $b }
                    keys %{ $diag_data{$age_group_code}{'measures'}{$measure}{$diagnos} }
                ) {
                    my $value = $diag_data{$age_group_code}{'measures'}{$measure}{$diagnos}{$year} // die;
                    my $value_clean = sanitize_value($value);

                    $csv->print($out, [
                        $batch_no,
                        $age_group_code,
                        $age_group,
                        $summary,
                        $measure_clean,
                        $diagnos_clean,
                        $year,
                        $value_clean,
                    ]);
                }
            }
        }
    }

    close $out;
    say "Wrote data for batch $batch_no to $output_file";

    # free memory (optional but nice)
    %diag_data = ();
}

sub sanitize_text {
    my ($s) = @_;
    return '' unless defined $s;
    $s =~ s/\s+/ /g;           # collapse all whitespace/newlines to single spaces
    $s =~ s/^\s+//;
    $s =~ s/\s+$//;
    return $s;
}

sub sanitize_value {
    my ($v) = @_;
    return '' unless defined $v;
    $v =~ s/\s+//g;            # remove spaces/thin spaces in numbers
    $v =~ s/[, ]//g;           # if there are thousands separators like "1,234"
    return $v;
}

sub load_state {
    # If no state file, we start from scratch
    return 0 unless -e $state_file;

    open my $fh, '<', $state_file
        or die "Can't open $state_file for reading: $!";
    local $/;
    my $json_text = <$fh>;
    close $fh;

    my $data = eval { decode_json($json_text) };
    if ($@ or ref $data ne 'HASH') {
        warn "State file $state_file is invalid, starting from batch 0.";
        return 0;
    }

    my $last = $data->{last_completed_batch} // 0;
    say "Resuming from batch " . ($last + 1) . " (last completed batch: $last)";
    return $last;
}

sub save_state {
    my ($batch_no) = @_;

    my %state = (
        last_completed_batch => $batch_no,
        saved_at => localtime->strftime('%Y-%m-%d %H:%M:%S')
    );

    open my $fh, '>', $state_file
        or die "Can't open $state_file for writing: $!";
    print $fh encode_json(\%state);
    close $fh;

    say "Saved state: last_completed_batch = $batch_no";
}
