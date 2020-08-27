
var getUser = function(onSuccess, onError)
{
  $.ajax(
    { url: '/user'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getPawns = function(onSuccess, onError)
{
  $.ajax(
    { url: '/pawns'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getState = function(onSuccess, onError)
{
  $.ajax(
    { url: '/state'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getByTimeBoard = function(time, onSuccess, onError)
{
  $.ajax(
    { url: '/' + encodeURIComponent(time) + '/board'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getTurn = function(onSuccess, onError)
{
  $.ajax(
    { url: '/turn'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
var postThrowRoll = function(options,onSuccess, onError)
{
  var str = "?";
  for(var i = 0; i < options.length; i++)
  {
    str += "options[]=" + options[i] + '&';
  }
  $.ajax(
    { url: '/throw/roll' + str 
    , success: onSuccess
    , error: onError
    , type: 'Post'
    });
}

var getThrowByNumber = function(number, onSuccess, onError)
{
  $.ajax(
    { url: '/throw/' + encodeURIComponent(number)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
var getThrow = function(onSuccess, onError)
{
  $.ajax(
    { url: '/throw'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}


var eraseThrow = function(onSuccess, onError)
{
  $.ajax(
    { url: '/throw/erase'
    , success: onSuccess
    , error: onError
    , type: 'Get'
    });
}

var putMove = function(make, onSuccess, onError)
{
  var str = "?";
  for(var i = 0; i < make.length; i++)
  {
    str += 'make[]=' + make[i] + '&';
  }
  $.ajax(
    { url: '/move' + str
    , success: onSuccess
    , error: onError
    , type: 'Put'
    });
}

var getMoveAllowed = function(amount,onSuccess, onError)
{
  $.ajax(
    { url: '/move/'+ amount + '/allowed'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getWinner = function(onSuccess, onError)
{
  $.ajax(
    { url: '/winner'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}