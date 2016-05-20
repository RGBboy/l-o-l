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

function Connection (id) {
  return {
    type: 'Connection',
    id: id
  };
};

function Disconnection (id) {
  return {
    type: 'Disconnection',
    id: id
  };
};

wss.on('connection', function connection (ws) {
  const id = uuid();
  console.log('connection');

  function recieve (message) {
    console.log('Incoming', message);
    app.ports.input.send(JSON.parse(message));
  };

  function send (message) {
    if (message.to === id) {
      console.log('Outgoing', message);
      ws.send(JSON.stringify(message.data));
    }
  };

  function close (err) {
    console.log('close', err);
    app.ports.output.unsubscribe(send);
    app.ports.input.send(Disconnection(id));
    ws.removeListener('message', send);
    ws.removeListener('close', close);
    ws.removeListener('error', close);
  }

  ws.on('message', recieve);
  ws.on('close', close);
  ws.on('error', close);

  app.ports.output.subscribe(send);

  app.ports.input.send(Connection(id));

});

server.listen(8080);

console.log('Listening on :8080');
