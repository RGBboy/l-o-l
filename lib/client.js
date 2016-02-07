var Elm = global.Elm, // Boooo hoooo, browserify does not like elm and the fact in contains require statements
    document = global.document,
    ws = global.WebSocket,
    element = document.getElementById('mount'),
    app = Elm.embed(Elm.Client, element, { input: null }),
    socket;

function send (message) {
  // console.log('socket send');
  // console.log(message);
  socket.send(JSON.stringify(message));
};

socket = new ws("ws://localhost:8080");

socket.onopen = function (event) {
  // console.log('socket open');
  app.ports.output.subscribe(send);
};

socket.onclose = function (event) {
  // console.log('socket open');
  app.ports.output.unsubscribe(send);
};

socket.onmessage = function (event) {
  var message = JSON.parse(event.data);
  // console.log('socket message');
  // console.log(message);
  app.ports.input.send(message);
};
