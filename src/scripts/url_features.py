import argparse
import json
import os
import re
import requests
import time
import tldextract
import urllib
import sys
import threading
import whois

from bs4 import BeautifulSoup
from datetime import datetime
from statistics import mean
from urllib.parse import urlencode, urlparse, urlsplit


def main():
    start = time.time()

    parser = argparse.ArgumentParser(description='Process some URL')
    parser.add_argument('-f', '--file', nargs=1,
                        help='file with a list of URL')
    parser.add_argument('-o', '--output', nargs=1,
                        help='file to write the results in CSV format')
    parser.add_argument('-u', '--url', nargs='+',
                        help='URL to extract features')
    parser.add_argument('-t', '--threads', nargs=1,
                        help='Number of threads to use', default=[5])

    args = parser.parse_args()

    try:
        args.threads[0] = int(args.threads[0])
    except ValueError:
        parser.error('Number of threads must be a positive integer')
        parser.print_help()
        sys.exit(1)

    if args.url is None and args.file is None:
        parser.print_help()
        sys.exit(1)

    if args.file is not None and len(args.file) == 1:
        try:
            f = open(args.file[0], 'r')
            urls = [url.strip() for url in f]
            f.close()
        except FileNotFoundError:
            parser.error(f'File {args.file[0]} not found')
            sys.exit(1)
    elif args.url is not None and len(args.url):
        urls = args.url
    else:
        parser.print_help()
        sys.exit(1)

    result = get_results(urls, args.threads[0])

    if args.output is not None and len(args.output) == 1:
        f = open(args.output[0], 'w+')
        f.write('\n'.join(result) + '\n')
        f.close()
        print(f'Results written in {args.output[0]}')
    else:
        print('\n'.join(result))

    print(f'Time: {time.time() - start} seconds')


def get_results(urls, max_threads):
    results = [
        'url,length_url,length_hostname,nb_dots,nb_hyphens,nb_slash,nb_www,https_token,ratio_digits_url,ratio_digits_host,nb_subdomains,prefix_suffix,shortening_service,nb_redirection,length_words_raw,char_repeat,shortest_words_raw,shortest_word_host,shortest_word_path,longest_words_raw,longest_word_host,longest_word_path,avg_words_raw,avg_word_host,avg_word_path,phish_hints,domain_in_brand,nb_hyperlinks,ratio_intHyperlinks,ratio_extHyperlinks,nb_extCSS,ratio_extRedirection,ratio_extErrors,external_favicon,links_in_tags,ratio_intMedia,ratio_extMedia,safe_anchor,empty_title,domain_in_title,domain_with_copyright,domain_registration_length,domain_age,web_traffic,google_index,page_rank'
    ]

    threads = []

    for i, url in enumerate(urls):
        t = threading.Thread(target=extract_and_print, args=(url, results))
        t.start()
        threads.append(t)

        if len(threads) == max_threads:
            for t in threads:
                t.join()

            threads.clear()

    for t in threads:
        t.join()

    return results


def extract_and_print(url, results):
    res = extract_features(url)

    if res is None:
        print(f'Warning: URL {url} not accessible')
        return

    results.append(','.join(map(str, res.values())))


def is_url_accessible(url):
    page = None

    try:
        page = requests.get(url, timeout=5)
    except Exception:
        parsed = urlparse(url)
        url = f'{parsed.scheme}://{parsed.netloc}'

        if not parsed.netloc.startswith('www'):
            url = f'{parsed.scheme}://www.{parsed.netloc}'

            try:
                page = requests.get(url, timeout=5)
            except Exception:
                page = None

    if page and page.status_code == 200 and page.content not in ["b''", "b' '"]:
        return True, url, page

    return False, None, None


def get_domain(url):
    obj = urlsplit(url)

    return obj.hostname, tldextract.extract(url).domain, obj.path


def beautifulsoup_wrapper(html):
    return BeautifulSoup(html, 'html.parser')


def fill_iframe(soup, obj_iframe):
    def do_iframe(i_frame, special_tag, special_value):
        if i_frame['width'] == '0' and i_frame['height'] == '0' and i_frame[special_tag] == special_value:
            obj_iframe['invisible'].append(i_frame)
        else:
            obj_iframe['visible'].append(i_frame)

    for i_frame in soup.find_all('iframe', width=True, height=True, frameborder=True):
        do_iframe(i_frame, 'frameborder', '0')

    for i_frame in soup.find_all('iframe', width=True, height=True, border=True):
        do_iframe(i_frame, 'border', '0')

    for i_frame in soup.find_all('iframe', width=True, height=True, style=True):
        do_iframe(i_frame, 'style', 'border:none;')


def fill_media(do_media, soup, obj_media):
    for img in soup.find_all('img', src=True):
        do_media(img, 'src', obj_media)

    for audio in soup.find_all('audio', src=True):
        do_media(audio, 'src', obj_media)

    for embed in soup.find_all('embed', src=True):
        do_media(embed, 'src', obj_media)

    for iframe in soup.find_all('iframe', src=True):
        do_media(iframe, 'src', obj_media)


def fill_head(do_media, soup, obj_favicon):
    def do_head_link(head):
        if isinstance(head.link['rel'], list):
            for e_rel in head.link['rel']:
                if e_rel.endswith('icon'):
                    do_media(head.link, 'href', obj_favicon)
                    break
        elif head.link['rel'].endswith('icon'):
            do_media(head.link, 'href', obj_favicon)

    for head in soup.find_all('head'):
        for head.link in soup.find_all('link', href=True):
            do_media(head.link, 'href', obj_favicon)

        for head.link in soup.findAll('link', {'href': True, 'rel': True}):
            do_head_link(head)


def fill_various(hostname, domain, null_format, soup, obj_media, obj_link, obj_css, obj_form, obj_favicon):
    def do_media(tag, attr, obj):
        dots = [x.start(0) for x in re.finditer('\.', tag[attr])]

        if (hostname in tag[attr] or domain in tag[attr] or len(dots) == 1 or not tag[attr].startswith('http')) and not tag[attr].startswith('http'):
            if not tag[attr].startswith('/'):
                obj['internals'].append(f'{hostname}/{tag[attr]}')
            elif tag[attr] in null_format:
                obj['null'].append(tag[attr])
            else:
                obj['internals'].append(f'{hostname}/{tag[attr]}')
        else:
            obj['externals'].append(tag[attr])

    fill_media(do_media, soup, obj_media)

    for link in soup.findAll('link', href=True):
        do_media(link, 'href', obj_link)

    for script in soup.find_all('script', src=True):
        do_media(script, 'src', obj_link)

    for link in soup.find_all('link', rel='stylesheet'):
        do_media(link, 'href', obj_css)

    for form in soup.findAll('form', action=True):
        do_media(form, 'action', obj_form)

    fill_head(do_media, soup, obj_favicon)


def fill_link(hostname, domain, null_format, soup, obj_href, obj_anchor):
    def do_link(link):
        if link['href'].startswith('http'):
            return

        if not link['href'].startswith('/'):
            obj_href['internals'].append(f'{hostname}/{link["href"]}')
        elif link['href'] in null_format:
            obj_href['null'].append(link['href'])
        else:
            obj_href['internals'].append(f'{hostname}{link["href"]}')

    for link in soup.find_all('a', href=True):
        dots = [x.start(0) for x in re.finditer('\.', link['href'])]

        if hostname in link['href'] or domain in link['href'] or len(dots) == 1 or not link['href'].startswith('http'):
            if '#' in link['href'] or 'javascript' in link['href'].lower() or 'mailto' in link['href'].lower():
                obj_anchor['unsafe'].append(link['href'])

            do_link(link)
        else:
            obj_href['externals'].append(link['href'])
            obj_anchor['safe'].append(link['href'])


def fill_style(hostname, domain, null_format, soup, obj_css):
    for style in soup.find_all('style', type='text/css'):
        try:
            start = str(style[0]).index('@import url(')
            end = str(style[0]).index(')')
            css = str(style[0])[start+12:end]
            dots = [x.start(0) for x in re.finditer('\.', css)]

            if (hostname in css or domain in css or len(dots) == 1 or not css.startswith('http')) and not css.startswith('http'):
                if not css.startswith('/'):
                    obj_css['internals'].append(f'{hostname}/{css}')
                elif css in null_format:
                    obj_css['null'].append(css)
                else:
                    obj_css['internals'].append(f'{hostname}{css}')
            else:
                obj_css['externals'].append(css)
        except Exception:
            continue


def extract_data_from_url(hostname, content, domain):
    obj_href = {'internals': [], 'externals': [], 'null': []}
    obj_link = {'internals': [], 'externals': [], 'null': []}
    obj_anchor = {'safe': [], 'unsafe': [], 'null': []}
    obj_media = {'internals': [], 'externals': [], 'null': []}
    obj_form = {'internals': [], 'externals': [], 'null': []}
    obj_css = {'internals': [], 'externals': [], 'null': []}
    obj_favicon = {'internals': [], 'externals': [], 'null': []}
    obj_iframe = {'visible': [], 'invisible': [], 'null': []}
    obj_title = ''
    obj_text = ''

    null_format = ['', '#', '#nothing', '#doesnotexist', '#null', '#void', '#whatever',
                   '#content', 'javascript::void(0)', 'javascript::void(0);', 'javascript::;', 'javascript']

    soup = beautifulsoup_wrapper(content)

    fill_various(hostname, domain, null_format, soup,
                 obj_media, obj_link, obj_css, obj_form, obj_favicon)
    fill_link(hostname, domain, null_format, soup, obj_href, obj_anchor)
    fill_style(hostname, domain, null_format, soup, obj_css)
    fill_iframe(soup, obj_iframe)
    obj_text = soup.get_text()

    try:
        obj_title = soup.title.string
    except Exception:
        pass

    return obj_href, obj_link, obj_anchor, obj_media, obj_form, obj_css, obj_favicon, obj_iframe, obj_title, obj_text


def shortening_service(full_url):
    match = re.search('bit\.ly|goo\.gl|shorte\.st|go2l\.ink|x\.co|ow\.ly|t\.co|tinyurl|tr\.im|is\.gd|cli\.gs|'
                      'yfrog\.com|migre\.me|ff\.im|tiny\.cc|url4\.eu|twit\.ac|su\.pr|twurl\.nl|snipurl\.com|'
                      'short\.to|BudURL\.com|ping\.fm|post\.ly|Just\.as|bkite\.com|snipr\.com|fic\.kr|loopt\.us|'
                      'doiop\.com|short\.ie|kl\.am|wp\.me|rubyurl\.com|om\.ly|to\.ly|bit\.do|t\.co|lnkd\.in|'
                      'db\.tt|qr\.ae|adf\.ly|goo\.gl|bitly\.com|cur\.lv|tinyurl\.com|ow\.ly|bit\.ly|ity\.im|'
                      'q\.gs|is\.gd|po\.st|bc\.vc|twitthis\.com|u\.to|j\.mp|buzurl\.com|cutt\.us|u\.bb|yourls\.org|'
                      'x\.co|prettylinkpro\.com|scrnch\.me|filoops\.info|vzturl\.com|qr\.net|1url\.com|tweez\.me|v\.gd|'
                      'tr\.im|link\.zip\.net',
                      full_url)

    return int(match is not None)


def char_repeat(words_raw):
    repeat = {'2': 0, '3': 0, '4': 0, '5': 0}
    part = [2, 3, 4, 5]

    for word in words_raw:
        for char_repeat_count in part:
            for i in range(len(word) - char_repeat_count + 1):
                sub_word = word[i:i + char_repeat_count]

                if all(x == sub_word[0] for x in sub_word):
                    repeat[str(char_repeat_count)] += 1

    return sum(list(repeat.values()))


def function_word_length(function, words_raw):
    if len(words_raw) == 0:
        return 0

    return function(len(word) for word in words_raw)


def count_subdomain(url):
    n = len(re.findall('\.', url))

    return n if 0 < n < 3 else 3


def nb_hyperlinks(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon):
    return sum(len(obj['internals']) + len(obj['externals']) for obj in [obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon])


def hyperlinks(kind, obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon):
    total = nb_hyperlinks(obj_href, obj_link, obj_media,
                          obj_form, obj_css, obj_favicon)

    if total > 0:
        return sum(len(obj[kind]) for obj in [obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon]) / total

    return 0


def h_e_redirect(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon):
    def get_count(obj):
        count = 0

        for link in obj['externals']:
            try:
                count += int(len(requests.get(link).history) > 0)
            except Exception:
                continue

        return count

    return sum(map(get_count, [obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon]))


def external_redirection(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon):
    externals = sum(len(obj['externals']) for obj in [
                    obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon])

    if (externals > 0):
        return h_e_redirect(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon) / externals

    return 0


def h_e_error(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon):
    def get_count(obj):
        count = 0

        for link in obj['externals']:
            try:
                count += int(requests.get(link).status_code >= 400)
            except Exception:
                continue

        return count

    return sum(map(get_count, [obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon]))


def external_errors(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon):
    externals = sum(len(obj['externals']) for obj in [
                    obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon])

    if externals > 0:
        return h_e_error(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon) / externals

    return 0


def get_avg(obj, kinds):
    total = len(obj[kinds[0]]) + len(obj[kinds[1]])
    partials = len(obj[kinds[0]])

    try:
        return partials / float(total) * 100
    except ZeroDivisionError:
        return 0


def domain_with_copyright(domain, content):
    try:
        m = re.search(
            u'(\N{COPYRIGHT SIGN}|\N{TRADE MARK SIGN}|\N{REGISTERED SIGN})', content)
        cpr = content[m.span()[0] - 50: m.span()[0] + 50]
        return int(domain.lower() not in cpr.lower())
    except Exception:
        return 0


def domain_registration_length(domain):
    try:
        res = whois.whois(domain)
        expiration_date = res.expiration_date
        today = time.strftime('%Y-%m-%d')
        today = datetime.strptime(today, '%Y-%m-%d')

        if expiration_date:
            if type(expiration_date) == list:
                expiration_date = min(expiration_date)

            return abs((expiration_date - today).days)

        return 0

    except Exception:
        return -1


def web_traffic(short_url):
    try:
        return int(BeautifulSoup(urllib.request.urlopen(
            f'http://data.alexa.com/data?cli=10&dat=s&url={short_url}').read(), 'xml').find('REACH')['RANK'])
    except Exception:
        return 0


def domain_age(domain):
    url = domain.split('//')[-1].split('/')[0].split('?')[0]
    show = f'https://input.payapi.io/v1/api/fraud/domain/age/{url}'
    r = requests.get(show)

    if r.status_code == 200:
        data = r.text
        json_to_python = json.loads(data)
        result = json_to_python['result']

        if result == None:
            return -2

        return result

    return -1


def google_index(url):
    user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36'
    headers = {'User-Agent': user_agent}

    data = requests.get(
        f'https://www.google.com/search?q=site:{url}', headers=headers)
    data.encoding = 'utf-8'

    try:
        soup = beautifulsoup_wrapper(data.text)

        if 'Our systems have detected unusual traffic from your computer network.' in str(soup):
            return duckduckgo_index(url, headers)

        check = soup.select("#rso")
        check = soup.find(id='rso').find('div').find('div').find('a')

        return int(not(check and check['href']))
    except urllib.error.HTTPError:
        return duckduckgo_index(url, headers)
    except AttributeError:
        print('error')
        return 1


def duckduckgo_index(url, headers):
    print('INFO: Using duckduckgo.com for google_index')

    data = requests.get(
        f'https://www.duckduckgo.com/html/?q=site:{url}', headers=headers)
    data.encoding = 'utf-8'

    soup = beautifulsoup_wrapper(data.text)
    check = soup.select("div.no-results")

    return int(len(check) > 0)


def page_rank(domain):
    try:
        request = requests.post('https://www.domcop.com/openpagerank/',
                                data={'domains': domain},
                                headers={'X-Requested-With': 'XMLHttpRequest'})
        result = request.json()
        result = result['data']['response'][0]['page_rank_integer']

        return result

    except Exception:
        return -1


def extract_features(url):
    HINTS = ['wp', 'login', 'includes', 'admin', 'content', 'site', 'images', 'js',
             'alibaba', 'css', 'myaccount', 'dropbox', 'themes', 'plugins', 'signin', 'view']

    allbrands_txt = open('data/allbrands.txt', 'r')
    allbrands = [line.strip() for line in allbrands_txt]
    allbrands_txt.close()

    def words_raw_extraction(domain, subdomain, path):
        regex = '\-|\.|\/|\?|\=|\@|\&|\%|\:|\_'
        w_domain = re.split(regex, domain.lower())
        w_subdomain = re.split(regex, subdomain.lower())
        w_path = re.split(regex, path.lower())
        raw_words = w_domain + w_path + w_subdomain
        w_host = w_domain + w_subdomain
        raw_words = list(filter(None, raw_words))

        return raw_words, list(filter(None, w_host)), list(filter(None, w_path))

    state, iurl, page = is_url_accessible(url)

    if not state:
        return None

    content = page.content
    hostname, domain, path = get_domain(url)
    extracted_domain = tldextract.extract(url)
    domain = f'{extracted_domain.domain}.{extracted_domain.suffix}'
    subdomain = extracted_domain.subdomain
    tmp = url[url.find(extracted_domain.suffix):len(url)]
    pth = tmp.partition('/')
    words_raw, words_raw_host, words_raw_path = words_raw_extraction(
        extracted_domain.domain, subdomain, pth[2])
    parsed = urlparse(url)
    scheme = parsed.scheme

    obj_href, obj_link, obj_anchor, obj_media, obj_form, obj_css, obj_favicon, obj_iframe, obj_title, obj_text = extract_data_from_url(
        hostname, content, domain)

    return {
        'url': url,
        'length_url': len(url),
        'length_hostname': len(hostname),
        'nb_dots': url.count('.'),
        'nb_hyphens': url.count('-'),
        'nb_slash': url.count('/'),
        'nb_www': len([word for word in words_raw if word.find('www') != -1]),
        'https_token': int(scheme != 'https'),
        'ratio_digits_url': len(re.sub("[^0-9]", "", url)) / len(url),
        'ratio_digits_host': len(re.sub("[^0-9]", "", hostname)) / len(hostname),
        'nb_subdomains': count_subdomain(url),
        'prefix_suffix': int(len(re.findall(r"https?://[^\-]+-[^\-]+/", url)) > 0),
        'shortening_service': shortening_service(url),
        'nb_redirection': len(page.history),
        'length_words_raw': len(words_raw),
        'char_repeat': char_repeat(words_raw),
        'shortest_words_raw': function_word_length(min, words_raw),
        'shortest_word_host': function_word_length(min, words_raw_host),
        'shortest_word_path': function_word_length(min, words_raw_path),
        'longest_words_raw': function_word_length(max, words_raw),
        'longest_word_host': function_word_length(max, words_raw_host),
        'longest_word_path': function_word_length(max, words_raw_path),
        'avg_words_raw': function_word_length(mean, words_raw),
        'avg_word_host': function_word_length(mean, words_raw_host),
        'avg_word_path': function_word_length(mean, words_raw_path),
        'phish_hints': sum([url.lower().count(hint) for hint in HINTS]),
        'domain_in_brand': int(domain in allbrands),
        'nb_hyperlinks': nb_hyperlinks(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon),
        'ratio_intHyperlinks': hyperlinks('internals', obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon),
        'ratio_extHyperlinks': hyperlinks('externals', obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon),
        'nb_extCSS': len(obj_css['externals']),
        'ratio_extRedirection': external_redirection(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon),
        'ratio_extErrors': external_errors(obj_href, obj_link, obj_media, obj_form, obj_css, obj_favicon),
        'external_favicon': int(len(obj_favicon['externals']) > 0),
        'links_in_tags': get_avg(obj_link, ['internals', 'externals']),
        'ratio_intMedia': get_avg(obj_media, ['internals', 'externals']),
        'ratio_extMedia': get_avg(obj_media, ['internals', 'externals']),
        'safe_anchor': get_avg(obj_anchor, ['unsafe', 'safe']),
        'empty_title': int(not obj_title),
        'domain_in_title': int(extracted_domain.domain.lower() not in obj_title.lower()),
        'domain_with_copyright': domain_with_copyright(extracted_domain.domain, obj_text),
        'domain_registration_length': domain_registration_length(domain),
        'domain_age': domain_age(domain),
        'web_traffic': web_traffic(url),
        'google_index': google_index(url),
        'page_rank': page_rank(domain)
    }


if __name__ == '__main__':
    main()
