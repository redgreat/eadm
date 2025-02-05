$(document).ready(function() {

    $('iframe#page').contents().find('head').append('<base target="_parent">');

});
