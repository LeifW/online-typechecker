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
    indentUnit: 4,
    // Code folding.
    onGutterClick: CodeMirror.newFoldFunction(CodeMirror.indentRangeFinder),
    // Highlight active line.
    onCursorActivity: function() {
      EDITOR.setLineClass(HIGHLIGHTED_LINE, null, null);
      HIGHLIGHTED_LINE = EDITOR.setLineClass(
          EDITOR.getCursor().line, null, "CodeMirror-x-activeLine");
    },
    // Indentation.
    extraKeys: {
      "Tab": function(cm) {cm.replaceSelection("    ", "end"); },
      "Shift-Tab": "indentLess"
    },
    onChange: function (edt, changeDescription) { scheduleSubmission(); }
  });

  HIGHLIGHTED_LINE = EDITOR.setLineClass(0, null, "CodeMirror-x-activeLine");

  $('#clearButton').click(function() {
    EDITOR.setValue('');
  });

  $('#executeButton').click(function() {
    $('#codepadHs').text(EDITOR.getValue());
    return true;
  });

  // Load initial validation result.
  submitCode();
});

var SUBMIT_DELAY = 300;

var EDITOR = null;
var TIMER = null;
var MARKED_LINES = [];
var MARKED_TOKENS = [];
var HIGHLIGHTED_LINE = 0;

var ERROR_REGEXP = /^Module.hs:(\d+):(\d+):/gm;
var MARKER_STRING = '\u25cf%N%';

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
    url: 'check_hs.json',
    data: {
      'hs': EDITOR.getValue(),
      'csrfmiddlewaretoken': HS_CSRF_TOKEN,
    },
    success: validationResultsHandler,
    dataType: 'json'
  });
  REQUEST_PENDING = true;
}

function validationResultsHandler(data, textStatus, jqXHR) {
  REQUEST_PENDING = false;
  markErrors(data.error || '');
  if (data.hasOwnProperty('error')) {
    $('#result').addClass('error').text(data.error);
  } else if (data.hasOwnProperty('failure')) {
    $('#result').addClass('error').text(data.failure);
    SERVICE_FAILURE = true;
  } else {
    $('#result').removeClass('error').text(data.result);
  }

  if (REQUEST_QUEUED) {
    REQUEST_QUEUED = false;
    scheduleSubmission();
  }
}

function markErrors(errorTrace) {
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
  EDITOR.setMarker(lineno, MARKER_STRING, 'CodeMirror-x-errorLine');
}

function clearMarkers() {
  $.each(MARKED_LINES, function (idx, n) {
    EDITOR.clearMarker(n);
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
      "CodeMirror-x-errorToken"
  );
  MARKED_TOKENS.push(marker);
}
