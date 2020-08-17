#!/usr/bin/python3

# python version: 3.5
import os, time, requests, subprocess

final_html = '/tmp/final.html'

def update_html():
    # main_url = 'https://www.pravda.com.ua/news'
    main_url = 'https://www.pravda.com.ua/rss'
    windows_1251_charset_line = '<meta http-equiv="content-type" content="text/html; charset=windows-1251">'
    utf8_charset_line = '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
    header = '<img src="/images/logo_ukr.gif" alt="Українська правда"'
    news = '<td bgcolor="#EDEDED" class="sp0"><a href="/news/">НОВИНИ</a></td>'
    last = '<br clear=all><br>'

    # iconv -f windows-1251 -t UTF-8 in.html > out.html
    index_html = requests.get(main_url).content.decode('windows-1251')
    with open(final_html, 'w') as f:
        for line in index_html.splitlines():
            if not line.startswith('<pdalink>http://pda.pravda.com.ua/news/id_'):
                continue
            assert line.endswith('</pdalink>')
            link = line[len('<pdalink>'):-len('</pdalink>')]
            article_html = requests.get(link).content.decode('windows-1251')
            for line in article_html.splitlines():
                if line == windows_1251_charset_line:
                    line = utf8_charset_line
                elif header in line:
                    continue
                elif news in line:
                    continue
                elif line == last:
                    f.write('</body>\n</html>\n')
                    break
                f.write(line + '\n')

def main():
    update_html()
    os.system('firefox file://' + final_html + ' &')
    subprocess.run(['xdotool', 'search', '--sync', '--onlyvisible', '--class',
        'Firefox', 'windowactivate', 'key', 'F11'], check = True)
    while True:
        time.sleep(30 * 60)
        update_html()
        subprocess.run(['xdotool', 'key', 'F5'], check = True)

if __name__ == '__main__':
    main()
