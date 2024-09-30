import os
from http.server import HTTPServer, BaseHTTPRequestHandler

base_dir = '/Users/nr83/Documents/nasqar_projects/nasqar2-dev/dada2Shiny/src/www/exampleData/MiSeq_SOP/'

class CustomHTTPRequestHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        """Serve a GET request."""
        path = self.translate_path(self.path)
        if os.path.isdir(path):
            self.list_directory(path)
        elif os.path.isfile(path):
            self.serve_file(path)
        else:
            self.send_error(404, "File not found")

    def translate_path(self, path):
        """Convert the request path to the local filesystem path."""
        
        return os.path.join(base_dir, path.strip("/"))

    def list_directory(self, path):
        """Helper function to produce a directory listing."""
        try:
            file_list = os.listdir(path)
        except OSError:
            self.send_error(404, "No permission to list directory")
            return None

        # Sort the files and directories alphabetically
        file_list.sort(key=lambda a: a.lower())

        # Create a response HTML page
        response = []
        response.append('<!DOCTYPE html>')
        response.append(f'<html><head><title>Directory listing for {self.path}</title></head>')
        response.append(f'<body><h2>Directory listing for {self.path}</h2>')
        response.append('<ul>')

        for name in file_list:
            fullpath = os.path.join(path, name)
            if os.path.isdir(fullpath):
                continue
		
            displayname = name + "/" if os.path.isdir(fullpath) else name
            response.append(f'<li><a href="{name}">{displayname}</a></li>')

        response.append('</ul></body></html>')

        # Send headers and the directory listing
        encoded = '\n'.join(response).encode('utf-8', 'surrogateescape')
        self.send_response(200)
        self.send_header('Content-Type', 'text/html; charset=utf-8')
        self.send_header('Content-Length', str(len(encoded)))
        self.end_headers()
        self.wfile.write(encoded)

    def serve_file(self, path):
        """Helper function to serve a file."""
        try:
            with open(path, 'rb') as f:
                self.send_response(200)
                self.send_header('Content-Type', 'application/octet-stream')
                self.end_headers()
                self.wfile.write(f.read())
        except OSError:
            self.send_error(404, "File not found")

# Configure and run the server
def run(server_class=HTTPServer, handler_class=CustomHTTPRequestHandler, port=8008):
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    print(f"Serving on port {port}...")
    httpd.serve_forever()

if __name__ == '__main__':
    run()

