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

function Message (id, message) {
  return {
    type: 'Message',
    id: id,
    message: message
  };
};


wss.on('connection', function connection (ws) {
  const id = uuid();
  console.log('connection');

  function recieve (message) {
    console.log('Incoming', message);
    app.ports.inputWSS.send(Message(id, JSON.parse(message)));
  };

  function handleCommand (command) {
    switch (command.type) {
      case 'Close':
        if (command.id === id) {
          console.log('Closing', id);
          ws.close();
        };
        return;
      case 'Message':
        if (command.id === id) {
          console.log('Sending', command);
          ws.send(JSON.stringify(command.data));
        };
        return;
      default:
        return;
    };
  };

  function close (err) {
    console.log('close', err);
    app.ports.outputWSS.unsubscribe(handleCommand);
    app.ports.inputWSS.send(Disconnection(id));
    ws.removeListener('message', recieve);
    ws.removeListener('close', close);
    ws.removeListener('error', close);
  }

  ws.on('message', recieve);
  ws.on('close', close);
  ws.on('error', close);

  app.ports.outputWSS.subscribe(handleCommand);

  app.ports.inputWSS.send(Connection(id));

});

server.listen(8080);

console.log('Listening on :8080');
