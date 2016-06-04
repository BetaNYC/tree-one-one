from flask import Flask
from flask import jsonify, render_template
#from flask_cors import CORS
import math
import pandas as pd
import os
import datetime

app = Flask(__name__)

@app.route("/")
def default():
    return render_template('index.html')
    
@app.route('/test')
def test():
    data = {'polygons':[{'bounds':[[40.736, -73.959], [40.737, -73.959]],
                         'id':20}
                         ]
            }
    return jsonify(data)
    #return jsonify([{'lat'}])
    

port = os.getenv('VCAP_APP_PORT', '5000')
if __name__ == "__main__":
    # run the app
    app.run(debug = True)
    #app.run(host='0.0.0.0', port=int(port))
