var port = (process.env.PORT || 8080),
    http = require('http'),
    url = require('url'),
    ecstatic = require('ecstatic'),
    uuid = require('uuid').v4,
    server = http.createServer(
      ecstatic({ root: __dirname + '/../public' })
    ),
    WebSocketServer = require('ws').Server,
    wss = new WebSocketServer({ server: server }),
    elm = require('../build/server.js'),
    app = elm.Server.worker();

function Location (nodeUrl) {
  parsedUrl = url.parse(nodeUrl);
  return {
    protocol : parsedUrl.protocol,
    hash : parsedUrl.hash || '',
    search : parsedUrl.search || '',
    pathname : parsedUrl.pathname,
    port_ : parsedUrl.port,
    hostname : parsedUrl.hostname,
    host : parsedUrl.host,
    origin : parsedUrl.protocol + '//' + parsedUrl.host,
    href: parsedUrl.href,
    username : '', // temp
    password : '' // temp
  };
}

function Connection (id, location) {
  return {
    type: 'Connection',
    id: id,
    location: location
  };
};

function Disconnection (id, location) {
  return {
    type: 'Disconnection',
    id: id,
    location: location
  };
};

function Message (id, location, message) {
  return {
    type: 'Message',
    id: id,
    location: location,
    message: message
  };
};


wss.on('connection', function connection (ws) {
  const id = uuid();
  const requestUrl = 'ws://' + ws.upgradeReq.headers.host + ws.upgradeReq.url;
  const location = Location(requestUrl);
  console.log('connection');

  function recieve (message) {
    console.log('Incoming', JSON.stringify(message, null, 2));
    app.ports.inputWSS.send(Message(id, location, JSON.parse(message)));
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
          console.log('Sending', JSON.stringify(command.data, null, 2));
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
    app.ports.inputWSS.send(Disconnection(id, location));
    ws.removeListener('message', recieve);
    ws.removeListener('close', close);
    ws.removeListener('error', close);
  }

  ws.on('message', recieve);
  ws.on('close', close);
  ws.on('error', close);

  app.ports.outputWSS.subscribe(handleCommand);

  app.ports.inputWSS.send(Connection(id, location));

});

server.listen(port);

console.log(`Listening on :${port}`);
