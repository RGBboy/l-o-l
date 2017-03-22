var port = (process.env.PORT || 8080),
    http = require('http'),
    ecstatic = require('ecstatic'),
    server = http.createServer(
      ecstatic({ root: __dirname + '/../public' })
    ),
    WebSocketServer = require('websocket-server'),
    elm = require('../build/server.js'),
    app = elm.Server.worker(),
    wss = new WebSocketServer(
      server,
      app.ports.inputWSS,
      app.ports.outputWSS
    );

server.listen(port);

console.log(`Listening on :${port}`);
