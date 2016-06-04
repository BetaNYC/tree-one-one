from flask import Flask
from flask import jsonify, render_template
#from flask_cors import CORS
import math
import pandas as pd
import os
import datetime
import json

app = Flask(__name__)

@app.route("/")
def default():
    return render_template('index.html')

@app.route('/test')    
@app.route('/test/<metric>')  
def test(metric=None):
    global sample
    for s in sample:
        s['color'] = s[metric+'_col']
    return jsonify({'polygons':sample})    
    
def popup_text(s):
    return """percent alive: %s<br>
              average size: %s<br>
              number of species: %s<br>"""%(s['aliveness'],s['average_size'],s['diversity'])

port = os.getenv('VCAP_APP_PORT', '5000')
if __name__ == "__main__":
    # run the app
    print 'loading the data...'
    sample = json.load(open('data/square.json', 'r'))
    for s in sample:
        del s['']
        try:
            s['bounds'] = json.loads(s['bounds'])
            s['size_col'] = s['dbh_col']
            s['diversity_col'] = s['species_col']
            s['size'] = s['average_size']
            s['popup_text'] = popup_text(s)
        except KeyError as e:
            #print e, '||', s
            continue
    print '...done.'
    app.run(debug = True)
    #app.run(host='0.0.0.0', port=int(port))
