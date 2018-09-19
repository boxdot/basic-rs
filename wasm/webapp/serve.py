#!/usr/bin/env python

import SimpleHTTPServer
import SocketServer

PORT = 8000

class Handler(SimpleHTTPServer.SimpleHTTPRequestHandler):
    pass

Handler.extensions_map['.wasm'] = 'application/wasm'

httpd = SocketServer.TCPServer(("", PORT), Handler)
print("Server started at http://localhost:{}".format(PORT))
httpd.serve_forever()
