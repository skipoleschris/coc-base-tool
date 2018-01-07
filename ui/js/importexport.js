
function downloadAsJson(obj, filename) {
 json = 'data:application/json;charset=utf-8,' + JSON.stringify(obj);
 data = encodeURI(json);

 link = document.createElement('a');
 link.setAttribute('href', data);
 link.setAttribute('download', filename);
 link.click();
 link.remove(); 
}