
function downloadAsJson(name, json) {
 data = 'data:application/json;charset=utf-8,' + json;

 link = document.createElement('a');
 link.setAttribute('href', data);
 link.setAttribute('download', name);
 link.click();
 link.remove(); 
}

function createFileUpload(parentId, dataCallback) {
  var parentNode = document.getElementById(parentId);

  var fileSelect = document.createElement('input');
  fileSelect.setAttribute('type', 'file');
  fileSelect.setAttribute('id', 'fileinput');

  fileSelect.onchange = function(e) {
    reader = new FileReader();

    reader.onload = function (event) {
      data = event.target.result;
      dataCallback(data);
    }
    reader.readAsText(fileSelect.files[0]);
  }

  parentNode.append(fileSelect);
}

function removeFileUpload(parentId) {
  var parentNode = document.getElementById(parentId);
  var fileSelect = document.getElementById('fileinput');

  parentNode.removeChild(fileSelect);  
}
