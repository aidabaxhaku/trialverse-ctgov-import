import sys
import urllib2
import shutil
import tempfile
import zipfile
import xml.etree.cElementTree as ET

import logging
logger = logging.getLogger()
logger.setLevel(logging.INFO)

ch = logging.StreamHandler(sys.stdout)
formatter = logging.Formatter('[%(levelname)s] %(asctime)s - %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)

blindings = set()

logger.info("Getting NCT records")
url = 'https://clinicaltrials.gov/ct2/results/download?down_stds=all&down_typ=study&down_flds=shown&down_fmt=plain&show_down=Y'
request = urllib2.urlopen(url)
logger.info('Request complete')
tmpfile = tempfile.TemporaryFile()
shutil.copyfileobj(request, tmpfile)
request.close()
logger.info('Copied to temporary file')
z = zipfile.ZipFile(tmpfile, 'r')
logger.info('Opened ZIP contents')
for fname in z.namelist():
    xml = z.open(fname, 'r')
    root = ET.parse(xml)
    xml.close()
    masking = root.find('./study_design_info/masking')
    if (masking != None):
        blindings.add(masking.text)
        print('.')
z.close()
tmpfile.close()
logger.info(blindings)
