#!/usr/bin/env python
# coding=utf8
# py 2.7.5

import hashlib
import hmac
import time
import urllib
import urllib2
import json
import urlparse
import os
import datetime
import ConfigParser
import gzip
import shutil
import platform
import sys
import getopt

def selfHelp():
    print 'Usage: ./growingio.py [OPTION]...'
    print ''
    print 'Mandatory arguments to long options are mandatory for short options too.'
    print
    print 'Startup:'
    print '  -h, --help         print this help'
    print '  -d, --date=DATE    set download date to DATE'

# 生成签名
def sign(secret, project, ai, tm):
    message = ("POST\n/auth/token\nproject=" + project + "&ai=" + ai + "&tm=" + tm).encode('utf-8')
    signature = hmac.new(bytes(secret.encode('utf-8')), bytes(message), digestmod=hashlib.sha256).hexdigest()
    return signature


# 生成token
def authToken(project, ai, tm, auth, clientId):
    authTokenUrl = 'https://www.growingio.com/auth/token'
    values = {
        'project': project,
        'ai': ai,
        'tm': tm,
        'auth': auth
    }
    headers = {
        'X-Client-Id': clientId
    }
    data = urllib.urlencode(values)
    request = urllib2.Request(authTokenUrl, data, headers)
    response = urllib2.urlopen(request)
    page = response.read()
    return page


# 获取下载地址
def getInsights(token, datetime, clientId):
    authTokenUrl = 'https://www.growingio.com/insights/aacd01fff9535e79/' + datetime + '.json'
    headers = {
        'X-Client-Id': clientId,
        'Authorization': token['code']
    }
    request = urllib2.Request(authTokenUrl, None, headers)
    response = urllib2.urlopen(request)
    page = response.read()
    return page


# 下载growingio日志文件
def downloadFiles(days, config):
    tm = str(int(round(time.time() * 1000)))
    auth = sign(config.get('common', 'secret'), config.get('common', 'project'), config.get('common', 'ai'), tm)
    tokenStr = authToken(config.get('common', 'project'), config.get('common', 'ai'), tm, auth,
                         config.get('common', 'clientId'))
    token = json.loads(tokenStr)

    insightsStr = getInsights(token, days.strftime('%Y%m%d'), config.get('common', 'clientId'))
    print insightsStr
    insights = json.loads(insightsStr)

    for downloadLink in insights['downlinks']:
        print 'Download link: ' + downloadLink
        parse = urlparse.urlparse(downloadLink)
        path = os.path.split(parse.path)
        pathList = path[0].split('_')
        destPath = config.get('common', 'dataPath')
        tableName = ''
        for j in range(2, len(pathList)):
            destPath += os.path.sep + pathList[j]
            if not os.path.exists(destPath):
                os.makedirs(destPath)
            if j == 2:
                tableName += pathList[j]
            elif j == len(pathList) - 1:
                break
            else:
                tableName += '_' + pathList[j]

        partFile = destPath + os.path.sep + path[1]
        if not os.path.exists(partFile):
            print 'Downloading part file: ' + partFile
            urllib.urlretrieve(downloadLink, partFile)

        destFile = partFile.split('.')[0]
        with open(destFile, 'wb') as f_out, gzip.open(partFile, 'rb') as f_in:
            shutil.copyfileobj(f_in, f_out)

        if not os.path.exists(destFile):
            continue

        system = ''
        if platform.system() == 'Darwin':
            system = ' ""'
        print 'sed -i' + system + ' "1d" ' + destFile
        os.system('sed -i' + system + ' "1d" ' + destFile)

        hiveScript = 'hive -e "load data local inpath \'' + destFile + '\' into table ods_gio_' + tableName + ' partition(dt=\'' + days.strftime(
            '%Y-%m-%d') + '\')"'
        print 'Hive script: ' + hiveScript
        os.system(hiveScript)
        if os.path.exists(destFile):
            print 'Remove ' + destFile
            os.remove(destFile)


config = ConfigParser.ConfigParser()
config.readfp(open('config.ini', 'rb'))

date = ''
try:
    opts, args = getopt.getopt(sys.argv[1:], 'hd:', ['date='])
except getopt.GetoptError:
    selfHelp()
    sys.exit(2)
for opt, arg in opts:
    if opt in ('-h', '--help'):
        selfHelp()
        sys.exit()
    elif opt in ('-d', '--date'):
        date = arg

if date == '':
    selfHelp()
    exit()

downloadFiles(datetime.datetime.strptime(date, '%Y-%m-%d'), config)

exit()



