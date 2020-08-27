
var getUser = function(onSuccess, onError)
{
  $.ajax(
    { url: '/user'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getBoard = function(onSuccess, onError)
{
  $.ajax(
    { url: '/board'
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

var getAlternative = function(onSuccess, onError)
{
  $.ajax(
    { url: '/alternative'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getAllowed = function(onSuccess, onError)
{
  $.ajax(
    { url: '/allowed'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getRoll = function(onSuccess, onError)
{
  $.ajax(
    { url: '/roll'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postMoveByPawn = function(pawn, onSuccess, onError)
{
  $.ajax(
    { url: '/move/' + encodeURIComponent(pawn) + ''
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}

var postMoveSkip = function(onSuccess, onError)
{
  $.ajax(
    { url: '/move/skip'
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}
