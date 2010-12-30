import httplib, urllib, sys, os.path

# Define the parameters for the POST request and encode them in
# a URL-safe format.

try:
    f = open(sys.argv[1], 'r')
    data = f.read()
    f.close()
except:
    print "File not found"
    sys.exit(1)

params = urllib.urlencode([
    ('js_code', data), # <--- This parameter has a new name!
    ('compilation_level', 'ADVANCED_OPTIMIZATIONS'),
    ('output_format', 'text'),
    ('output_info', 'compiled_code'),
  ])

# Always use the following value for the Content-type header.
headers = { "Content-type": "application/x-www-form-urlencoded" }
conn = httplib.HTTPConnection('closure-compiler.appspot.com')
conn.request('POST', '/compile', params, headers)
response = conn.getresponse()

fname = os.path.splitext(sys.argv[1])[0] + ".min.js"
f = open(fname, 'w');
f.write(response.read());
f.close();
conn.close
