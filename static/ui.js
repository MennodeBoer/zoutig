$.when(getUser(welcomeMsg,console.log)).done(function(){
window.setInterval(function(){getTurn(updateMessage,console.log)}, 200);})

function welcomeMsg(data)
{
  $('#body').css("display","block");
  $('#name').text(data.uName);
  $('#team').text(data.uTeam);
  $('#team').css("color",data.uTeam);
  if(data.uTime == "Future")
  {
    $('#period').text("");
    $('#time').text("Future");
    $('#toggle').prop("innerText","Past");
    $('#message').text('It was not your turn.');
  }
  else
  {
    $('#time').text("Past");
    $('#toggle').prop("innerText","Future");
  }

}

function printBoard(data)
{
  for(var i = 1; i <= 45; i++)
  {
    if(data[i]==null)
    {
      defaultColorSquare(i);
      $('#'+i).text("");  
    }
    else 
    {
      $('#'+i).css("background-color",data[i].pTeam);   
      $('#'+i).css("transform",getDeg(i));    
      $('#'+i).text(data[i].pNumber);
    }
  }
}

function checkMatches(data)
{
  for(var i = 1; i <= 45; i++)
  {
    if(data[i]==null)
    {
      $('#'+i).css("border","1px solid black");
    }
    else
    {
      $('#dummy').css("background-color",data[i].pTeam)
      if ($('#dummy').css("background-color") == $('#'+i).css("background-color"))
      {
         $('#'+i).css("box-shadow","0 0 5pt 2pt yellow");
      }
      else
      {
        $('#'+i).css("box-shadow","none");
      }
    }
  }
}

function defaultColorSquare(i)
{
  var str = "";
  switch(i)
  {
    case 1:
      str = "#ffb8b8";
      break;
    case 7:
      str = "#e6b0ee";
      break;
    case 13:
      str = "#9ebdff";
      break;
    case 19:
      str = "#9effdd";
      break;
    case 25:
      str = "#ffe3bd";
      break;
    case 31:
    case 32:
    case 33:
      str ="#ffb8b8";
      break;
    case 34:
    case 35:
    case 36:
      str ="#e6b0ee";
      break;
    case 37:
    case 38:
    case 39:
      str ="#9ebdff";
      break;
    case 40:
    case 41:
    case 42:
      str ="#9effdd";
      break;
    case 43:
    case 44:
    case 45:
      str ="#ffe3bd";
      break;
    default:
      str = "white";
  }
  $('#'+i).css("background-color",str)
}

function getDeg(i)
{
  var str = "";
  switch(i)
  {
    case 27:
    case 28:
    case 29:
    case 30:
    case 1:
    case 2:
    case 31:
    case 32:
    case 33:
      str = "0";
      break;
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 34:
    case 35:
    case 36:
      str = "-72";
      break;  
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 37:
    case 38:
    case 39:
      str = "-144";
      break;  
    case 15:
    case 16:
    case 17:
    case 18:
    case 19:
    case 20:
    case 40:
    case 41:
    case 42:
      str = "-216";
      break;
    case 21:
    case 22:
    case 23:
    case 24:
    case 25:
    case 26:
    case 43:
    case 44:
    case 45:
      str = "-288";
      break; 
  }
  return "rotate(" + str + "deg)";
}

function updateMessage(data)
{
  if($('#toggle').prop("innerText") == $('#time').text())
  {
    $('#period').css("display","none");  
    $('#message').text('You are spectating a different timeline.');
    $('#button1').css("display","none");
    $('#button2').css("display","none");
    $('#button3').css("display","none");
    $('#button4').css("display","none");
    $('#throw').css("display","none");  
    getAlternative(printBoard,console.log);
    getBoard(checkMatches,console.log);
  }
  else
  {
    getBoard(printBoard,console.log)
    getAlternative(checkMatches,console.log);
    if ($('#time').text() == "Past")
    {
      $('#period').css("display","inline");  
      if (data)
      {  
        $.when(getAllowed(updateButtons,console.log)); 
      }
      else
      {
        $('#message').text('It is not your turn');
        $('#button1').css("display","none");
        $('#button2').css("display","none");
        $('#button3').css("display","none");
        $('#button4').css("display","none");
        $('#throw').css("display","none");  
     }
    }
    else
    {
      if (data)
      {
       $('#throw').css("display","none");  
       $('#period').css("display","none");  
       $('#message').text('It was your turn! What did you do?');
       $.when(getAllowed(updateButtonsFuture,console.log));
       $('#button4').css("display","inline-block");
      }
      else
      {       
        $('#throw').css("display","none");  
        $('#message').text('It was not your turn.');
        $('#period').css("display","none");  
        $('#button1').css("display","none");
        $('#button2').css("display","none");
        $('#button3').css("display","none");
        $('#button4').css("display","none");
      }
    }
  }
}

function updateButtonsFuture(data)
{
  for(var i = 0; i < data.length; i++)
  {
    $('#button' + data[i]).css("display","inline-block");
  }
}

function updateButtons(data)
{  
  getRoll(function(data){$('#throw').text(data)},console.log);
  $('#message').text('It is your turn! You rolled a ');
  $('#throw').css("display","inline");
  for(var i = 0; i < data.length; i++)
  {
    $('#button'+data[i]).css("display","inline-block");
  }
  $('#button4').css("display","inline-block");  
}

function makeMove(n)
{  
    $.when(postMoveByPawn(n,null,console.log))
    $('#button1').css("display","none");
    $('#button2').css("display","none");
    $('#button3').css("display","none");
    $('#button4').css("display","none");
    if ($('#time').text() == "Past")
    {
      $('#throw').css("display","none");
    }
    else
    {
      $('#message').text('It was not your turn.');
    };
}

function skip()
{
  $('#button1').css("display","none");
  $('#button2').css("display","none");
  $('#button3').css("display","none");
  $('#button4').css("display","none");  
  if ($('#time').text() == "Past")
  {
    $('#throw').css("display","none");
  }
  else
  {
    $('#message').text('It was not your turne. You skipped.');
  }
  postMoveSkip(null, console.log);
}

function swap()
{
  if($('#toggle').prop("innerText") == "Past")
  {
      $('#toggle').prop("innerText","Future");      
  }
  else
  {
    $('#toggle').prop("innerText","Past");
    if ($('#time').text() == "Future")
      {
        $('#message').text('It was not your turn.');
      }
  }
}

function toBool(str)
{
  if (str == "Past")
  {
    return false;
  }
  else 
  { 
    return true;
  }
}

function checkWinner(data)
{
  if(data != null)
  {
    alert("we have a winner: " + data)
  }
}