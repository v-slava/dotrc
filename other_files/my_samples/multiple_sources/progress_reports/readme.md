# Biweekly reports

This repository contains hierarchically-structured biweekly reports in HTML
format and a tool to generate final report (in HTML or EML format) from
individual reports.

## Requirements

* python 3.8
* tidy (optional, required only for checking html)<br/>
  Install for Debian / Ubuntu: ``sudo apt-get install tidy``<br/>
  For Windows see: <http://binaries.html-tidy.org/>
* For linux: xclip or xsel (optional, required only for copying html to
  clipboard). To install use ``sudo apt-get install``.

## Usage

<pre>
usage: gen_report.py [-h] [--report-due {this_week,next_week}]
                     [--check-updates] [--check-html] [--view-html]
                     [--html-to-clipboard] [--html FILE] [--eml FILE]
                     [--outlook]

Generate final progress report from individual reports.

optional arguments:
  -h, --help            show this help message and exit
  --report-due {this_week,next_week}
                        update report period dates in
                        data/email/report_time_period.txt. Set report period
                        to be 2 weeks ending on Friday (either this week's
                        Friday or next week's Friday)
  --check-updates       check whether every contributor updated his/her report
                        in this repository during report period
  --check-html          check whether html to be generated is valid ("tidy"
                        tool is required)
  --view-html           view HTML in web browser
  --html-to-clipboard   copy HTML body to clipboard. On linux either "xclip"
                        or "xsel" is required
  --html FILE           file to write report in HTML format to
  --eml FILE            file to write report in EML format to
  --outlook             edit report email in outlook (on windows only)
</pre>

Note: "``members``" folders should contain subfolders according to email
addresses of project members, for example for:
<pre>
viacheslav.volkov@your_company.com
</pre>
the folder should be:
<pre>
members/viacheslav.volkov
</pre>
