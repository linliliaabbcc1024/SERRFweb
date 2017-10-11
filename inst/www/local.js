
$(function(){


var db = new PouchDB('http://slfan:Fansly68410298_@localhost:5984/serrfweb');

$.getJSON('//freegeoip.net/json/?callback=?', function(data) {
  var newuser = JSON.stringify(data, null, 2);
  // fetch mittens
db.get('userinfo').then(function (doc) {
  // update their age
  doc.info.push(newuser+"**"+Date())
  // put them back
  return db.put(doc);
})
});





$(".navigate").click(function(){

var tabcontent;
 tabcontent = document.getElementsByClassName("tabcontent");
    for (i = 0; i < tabcontent.length; i++) {
        tabcontent[i].style.display = "none";
    }

  var element = this;
  document.getElementById(element.id.replace('nav','')).style.display = "block";
})



















$('.uploaded').css('display','none');
$(".done").css('display','none');

$('#input').change(function(){

$('#uploadedText').css('display','inline-block');
$("#uploadedText").html('<i  class="fa fa-spinner fa-spin" ></i>')

  var req = ocpu.call("checkDataFormat",{
    input:$("#input")[0].files[0]
  }, function(sess){

  }).fail(function(e){
    alert("The format is not correct. Please check your data with the example data, especially the red-text cells.")
    alert("Error: " +req.responseText )
    $('.uploaded').css('display','none');

  }).then(function(e){
    $('.uploaded').css('display','inline-block');
    $("#uploadedText").html('uploaded')
  })


})

$("#apply").click(function(){


  $.getJSON('//freegeoip.net/json/?callback=?', function(data) {
    var ip = data.ip
    console.log(ip)
  $("#apply").prop("disabled", true);
  $("#applyText").html('<i  class="fa fa-spinner fa-spin" ></i>')
  $(".done").css('display','none');
  var req = ocpu.call("SERRF",{
    input:$("#input")[0].files[0],
    ip:ip
  }, function(sess){

    sss = sess

    $('#rawpca').attr("src", sess.loc + "files/rawpca.png");
    $('#SERRFpca').attr("src", sess.loc + "files/SERRFpca.png");
    $("#download").click(function(){
        console.log("!")
        window.open(sess.loc + "files/SERRF - results.zip");
      })

    sess.getObject(function(obj){

      ooo = obj


    $("#finishtext").html('<h3 style="text-align: center";><i class="fa fa-thumbs-o-up" aria-hidden="true"></i> The normalization is finished!</h3>')
    $("#validateSERRF").text(obj.validateSERRF + "%")
    $("#validateraw").text(obj.validateraw + "%")
    $("#count_less_20_SERRF").text(obj.count_less_20_SERRF)
    $("#count_less_20_raw").text(obj.count_less_20_raw)
    $("#perc_less_20_SERRF").text(obj.perc_less_20_SERRF + "%")
    $("#perc_less_20_raw").text(obj.perc_less_20_raw + "%")








    })



  }).fail(function(e){
    alert("Error: "+req.responseText+". CONTACT ME: slfan@ucdavis.edu")
    $("#applyText").html('Apply SERRF normalization')
    $("#apply").prop("disabled", false);

  }).then(function(e){
    $(".done").css('display','inline-block');
    $("#applyText").html('Apply SERRF normalization')
    $("#apply").prop("disabled", false);

  })
  })





})





})
