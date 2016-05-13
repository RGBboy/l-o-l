var http = require('http'),
    ecstatic = require('ecstatic'),
    uuid = require('uuid').v4,
    server = http.createServer(
      ecstatic({ root: __dirname + '/../public' })
    ),
    WebSocketServer = require('ws').Server,
    wss = new WebSocketServer({ server: server }),
    elm = require('../build/server.js'),
    app = elm.Server.worker();

function Connection () {
  return {
    type: 'Connection',
    id: uuid()
  }
}

wss.on('connection', function connection (ws) {
  console.log('connection')

  ws.on('message', function (message) {
    console.log('Incoming', message);
    app.ports.input.send(JSON.parse(message));
  });

  function send (message) {
    console.log('Outgoing', message);
    ws.send(JSON.stringify(message));
  };

  app.ports.output.subscribe(send);

  app.ports.input.send(Connection());

  ws.on('close', function (err) {
    app.ports.output.unsubscribe(send);
    console.log('close', err);
  })

  ws.on('error', function (err) {
    app.ports.output.unsubscribe(send);
    console.log('error', err);
  })

});

server.listen(8080);

console.log('Listening on :8080');
