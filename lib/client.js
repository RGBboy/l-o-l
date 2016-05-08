var document = global.document,
    element = document.getElementById('mount'),
    elm = require('../build/client.js'),
    app = elm.Main.embed(element);
