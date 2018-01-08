
function downloadAsJson(name, json) {
 data = 'data:application/json;charset=utf-8,' + json;

 link = document.createElement('a');
 link.setAttribute('href', data);
 link.setAttribute('download', name);
 link.click();
 link.remove(); 
}