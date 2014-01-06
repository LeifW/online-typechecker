/*
$.get("/sources/Foo.idr", function(src) {
  $(document).ready(function () {
    EDITOR.setValue(src);
  });
});
*/
$(document).ready(function () {
  $('#container').layout({
    livePaneResizing: true,
    stateManagement__enabled: true,
    enableCursorHotkey: false,
    east__size: 500,
    north__resizable: false,
    north__closable: false,
    north__paneClass: 'nonexistentClass',
    north__resizerClass: 'nonexistentClass',
    south__resizable: false,
    south__closable: false,
    south__paneClass: 'nonexistentClass',
    south__resizerClass: 'nonexistentClass'
  });

  EDITOR = CodeMirror.fromTextArea(document.getElementById("hs"), {
    mode: "haskell",
    lineNumbers: true,
    matchBrackets: true,
    autofocus: true,
    autoClearEmptyLines: true,
    indentUnit: 2,
    gutters: ["CodeMirror-x-errorLine"],
    // Code folding.
    onGutterClick: CodeMirror.newFoldFunction(CodeMirror.indentRangeFinder),
    // Indentation.
    extraKeys: {
      "Tab": function(cm) {cm.replaceSelection("    ", "end"); },
      "Shift-Tab": "indentLess"
    }
  });
  
    // Highlight active line.
  EDITOR.on("cursorActivity", function() {
    EDITOR.removeLineClass(HIGHLIGHTED_LINE, "background", "CodeMirror-x-activeLine");
    HIGHLIGHTED_LINE = EDITOR.addLineClass(
      EDITOR.getCursor().line, "background", "CodeMirror-x-activeLine");
  });

  EDITOR.on("change", function (edt, changeDescription) { scheduleSubmission(); });

  HIGHLIGHTED_LINE = EDITOR.addLineClass(0, "background", "CodeMirror-x-activeLine");

  $('#clearButton').click(function() {
    EDITOR.setValue('');
  });

  submitCode();
});

var SUBMIT_DELAY = 300;

var EDITOR = null;
var TIMER = null;
var MARKED_LINES = [];
var MARKED_TOKENS = [];
var HIGHLIGHTED_LINE = 0;

var ERROR_REGEXP = /^.*.idr:(\d+):(\d+):/gm;
var MARKER_STRING = '\u25cf';

// The server has reported a failure, stop sending requests.
var SERVICE_FAILURE = false;

// A request to the server is in progress.
var REQUEST_PENDING = false;

// A request is in progress, and another request should be sent after it completes.
var REQUEST_QUEUED = false;


function scheduleSubmission() {
  if (SERVICE_FAILURE) {
    return;
  }
  if (TIMER) {
    window.clearTimeout(TIMER);
  }
  TIMER = window.setTimeout(submitCode, SUBMIT_DELAY);
}

function submitCode() {
  if (REQUEST_PENDING) {
    // Avoid multiple simultaneous active requests.
    REQUEST_QUEUED = true;
    return;
  }
  $.ajax({
    type: 'POST',
    url: '/sources/Foo.idr',
    data: EDITOR.getValue(),
    contentType: "text/x-haskell; charset=utf-8",
    success: validationResultsHandler,
    dataType: 'text'
  });
  REQUEST_PENDING = true;
}

function validationResultsHandler(data, textStatus, jqXHR) {
  console.log("foo");
  console.log(data);
  console.log(textStatus);
  console.log(jqXHR);
  REQUEST_PENDING = false;
  markErrors(data || '');
  //$('#result').addClass('error').html(data);
  $('#result').html(data);
  /*
  if (data.hasOwnProperty('error')) {
    $('#result').addClass('error').text(data.error);
  } else if (data.hasOwnProperty('failure')) {
    $('#result').addClass('error').text(data.failure);
    SERVICE_FAILURE = true;
  } else {
    $('#result').removeClass('error').text(data.result);
  }
  */

  if (REQUEST_QUEUED) {
    REQUEST_QUEUED = false;
    scheduleSubmission();
  }
}

function markErrors(errorTrace) {
  console.log("merkEruers");
  console.log(errorTrace);
  // TODO: do not readd existing markers
  clearMarkers();
  while (match = ERROR_REGEXP.exec(errorTrace)) {
    // CodeMirror uses 0-based line numbering.
    var pos = { line: parseInt(match[1], 10) - 1, ch: parseInt(match[2], 10) };
    addLineMarker(pos.line);
    markToken(pos);
  }
}

function addLineMarker(lineno) {
  MARKED_LINES.push(lineno);
  console.log("setting gutter");
  console.log(lineno);
  console.log(MARKER_STRING);
  EDITOR.setGutterMarker(lineno, "CodeMirror-x-errorLine", $("<span class='CodeMirror-x-errorLine'>" + MARKER_STRING + "</span>")[0]); // document.createTextNode(MARKER_STRING)); //, 'CodeMirror-x-errorLine');
  //EDITOR.setGutterMarker(lineno, "typecheck-error", document.createTextNode(MARKER_STRING)); //, 'CodeMirror-x-errorLine');
}

function clearMarkers() {
  $.each(MARKED_LINES, function (idx, n) {
    EDITOR.clearGutter("CodeMirror-x-errorLine");
  });
  MARKED_LINES = [];

  $.each(MARKED_TOKENS, function (idx, marker) {
    marker.clear();
  });
  MARKED_TOKENS = [];
}

function markToken(pos) {
  var tokenInfo = EDITOR.getTokenAt(pos);
  var marker = EDITOR.markText(
      { line: pos.line, ch: tokenInfo.start },
      { line: pos.line, ch: tokenInfo.end },
      { className: "CodeMirror-x-errorToken"}
  );
  MARKED_TOKENS.push(marker);
}

function elemClassContent(elem, cls, cnt) {
  var e = document.createElement(elem);
  e.className = cls;
  e.textContent = cnt;
  //e.appendChild(document.createTextNode(cnt));
  return e;
}
