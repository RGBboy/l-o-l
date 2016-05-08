var http = require('http'),
    ecstatic = require('ecstatic'),
    server = http.createServer(
      ecstatic({ root: __dirname + '/../public' })
    ),
    WebSocketServer = require('ws').Server,
    wss = new WebSocketServer({ server: server }),
    elm = require('../build/server.js'),
    fakeDomNode,
    app;

function noop () {};

function FakeDomNode () {
  return {
    appendChild: noop
  };
}

global.document = {
  createTextNode: FakeDomNode
}

app = elm.Server.embed(FakeDomNode());

wss.on('connection', function connection (ws) {
  console.log('connection')

  ws.on('message', function (message) {
    console.log('incoming');
    console.log(message);
    app.ports.input.send(message);
  });

  function send (message) {
    ws.send(message);
  };

  app.ports.output.subscribe(send);

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
