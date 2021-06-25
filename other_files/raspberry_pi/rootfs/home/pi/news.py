#!/usr/bin/python3

# python version: 3.5
import os, sys, time, requests, subprocess, logging, traceback
from logging.handlers import RotatingFileHandler

final_html = '/tmp/final.html'

def init_logging(log_file):
    class StdoutFilter(logging.Filter):
        def filter(self, rec):
            # return rec.levelno in (logging.INFO, logging.DEBUG)
            return rec.levelno == logging.INFO
    # file_handler = logging.FileHandler(log_file)
    file_handler = RotatingFileHandler(log_file, maxBytes = 1024 * 1024,
            backupCount = 5)
    file_handler.setLevel(logging.DEBUG)
    stdout_handler = logging.StreamHandler(sys.stdout)
    stdout_handler.setLevel(logging.DEBUG)
    stdout_handler.addFilter(StdoutFilter())
    stderr_handler = logging.StreamHandler()
    stderr_handler.setLevel(logging.WARNING)
    logging.basicConfig(level = logging.DEBUG, datefmt = '%Y-%m-%d %H:%M:%S',
        format = '%(asctime)s.%(msecs)03d %(levelname)s: %(message)s',
        handlers = [
            file_handler,
            stdout_handler,
            stderr_handler,
        ],
    )

def update_html():
    logging.info('_update_html() has been started...')
    try:
        _update_html()
        logging.info('_update_html() succeeded')
    except Exception as e:
        logging.error(traceback.format_exc())
        logging.info('_update_html() failed')

def _update_html():
    # main_url = 'https://www.pravda.com.ua/news'
    main_url = 'https://www.pravda.com.ua/rss'
    windows_1251_charset_line = '<meta http-equiv="content-type" content="text/html; charset=windows-1251">'
    utf8_charset_line = '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
    header = '<img src="/images/logo_ukr.gif" alt="Українська правда"'
    news = '<td bgcolor="#EDEDED" class="sp0"><a href="/news/">НОВИНИ</a></td>'
    last = '<br clear=all><br>'

    # iconv -f windows-1251 -t UTF-8 in.html > out.html
    index_html = requests.get(main_url, timeout = 100).content.decode('windows-1251')
    with open(final_html, 'w') as f:
        for line in index_html.splitlines():
            if not line.startswith('<pdalink>http://pda.pravda.com.ua/news/id_'):
                continue
            assert line.endswith('</pdalink>')
            link = line[len('<pdalink>'):-len('</pdalink>')]
            article_html = requests.get(link, timeout = 100).content.decode('windows-1251')
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
    init_logging('/tmp/news_py_log.txt')
    update_html()
    os.system('firefox file://' + final_html + ' &')
    logging.info('firefox has been started...')
    subprocess.run(['xdotool', 'search', '--sync', '--onlyvisible', '--class',
        'Firefox', 'windowactivate', 'key', 'F11'], check = True)
    logging.info('firefox full screen has been activated')
    while True:
        time.sleep(30 * 60)
        update_html()
        subprocess.run(['xdotool', 'key', 'F5'], check = True)
        logging.info('web page has been updated in firefox')

if __name__ == '__main__':
    main()
