#!/usr/bin/env python


from contextlib import closing
import csv
import re

import MySQLdb


OUTPUT = 'intersections-imgs.csv'
re_img = re.compile(ur'<img [^>]*src=["\']([^"\']*)["\'][^>*]')
SQL = '''SELECT id, title, body FROM omeka_neatline_records
    WHERE body LIKE '%jpg%' OR body LIKE '%png%';
    '''


def main():
    with closing(MySQLdb.connect(user='root', db='intersections')) as cxn:
        with closing(cxn.cursor()) as c:
            with open(OUTPUT, 'wb') as fout:
                writer = csv.writer(fout)
                writer.writerow(('Record ID', 'Title', 'Image URL'))

                c.execute(SQL)
                for (record_id, title, body) in c:
                    writer.writerows(
                        (record_id, title, img.group(1))
                        for img in re_img.finditer(body)
                        )


if __name__ == '__main__':
    main()
