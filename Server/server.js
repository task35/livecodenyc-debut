var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);
var edn = require("jsedn");

var socketIds = {};
var state = {};

app.get('/', function(req, res){
  res.sendfile('index.html');
});

app.get('/state', function(req, res){
  res.send(edn.encode(state))
  state = [];
});

io.on('connection', (socket) => {
  socket.on('join', (data) => {
    console.log(data.id + " joined")
    socketIds[socket.id] = data.id;
  });
  socket.on('disconnect', () => {
    var id = socketIds[socket.id];
    console.log(id + " left")
    delete socketIds[socket.id];
    delete state[id];
  });
  socket.on('press', (data) => {
    var id = data.id;
    var button = data.button;
    console.log(id + " pressed " + button)
    state[id] = button;
  });
  socket.on('release', (data) => {
    var id = data.id;
    var button = data.button;
    console.log(id + " released " + button)
    delete state[id];
  });
});

http.listen(3000, function(){
  console.log('listening on *:3000');
});