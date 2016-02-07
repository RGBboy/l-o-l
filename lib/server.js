var http = require('http'),
    ecstatic = require('ecstatic'),
    server = http.createServer(
      ecstatic({ root: __dirname + '/../public' })
    ),
    WebSocketServer = require('ws').Server,
    wss = new WebSocketServer({ server: server })
    elm = require('../build/server.js'),
    app = elm.worker(elm.Server, { input: null }),
    uuid = require('uuid').v4;

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
  var id = uuid();
  ws.on('message', function (message) {
    console.log('incoming');
    console.log(message);
    app.ports.input.send(JSON.parse(message));
  });

  function send (message) {
    // console.log('sendAll', id);
    // console.log(message);
    ws.send(JSON.stringify(message));
  };

  function sendIfMine (message) {
    if (id === message.id) {
      // console.log('Send only to ', message.id);
      // console.log(message);
      ws.send(JSON.stringify(message));
    };
  };

  app.ports.outputAll.subscribe(send);
  app.ports.outputOne.subscribe(sendIfMine);

  ws.on('close', function () {
    app.ports.outputAll.unsubscribe(send);
    app.ports.outputOne.unsubscribe(sendIfMine);
    // console.log('Disconnection', id);
    app.ports.input.send(Disconnection(id));
  });

  // console.log('Connection', id);
  app.ports.input.send(Connection(id));

});

server.listen(8080);

console.log('Listening on :8080');
