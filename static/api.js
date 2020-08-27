
var getUser = function(onSuccess, onError)
{
  $.ajax(
    { url: '/user'
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

var postThrowRoll = function(options, onSuccess, onError)
{
  $.ajax(
    { url: '/throw/roll' + '?options[]=' + encodeURIComponent(options)
    , success: onSuccess
    , error: onError
    , type: 'POST'
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

var getThrowByNumber = function(number, onSuccess, onError)
{
  $.ajax(
    { url: '/throw/' + encodeURIComponent(number) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getThrowErase = function(onSuccess, onError)
{
  $.ajax(
    { url: '/throw/erase'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var putMove = function(make, onSuccess, onError)
{
  $.ajax(
    { url: '/move' + '?make[]=' + encodeURIComponent(make)
    , success: onSuccess
    , error: onError
    , type: 'PUT'
    });
}

var putMoveResurrect = function(onSuccess, onError)
{
  $.ajax(
    { url: '/move/resurrect'
    , success: onSuccess
    , error: onError
    , type: 'PUT'
    });
}

var getMoveByAmountAllowed = function(amount, onSuccess, onError)
{
  $.ajax(
    { url: '/move/' + encodeURIComponent(amount) + '/allowed'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
