/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-02-07 11:29:20
 *
 * Module : login.js
 *
 */

function login() {
    const formData = {
        loginName: $('#loginname').val(),
        password: $('#password').val()
    };
    $.ajaxSetup({async:false});
    $.ajax({
        url: '/login',
        type: 'POST',
        data: formData,
        success: function (resdata) {
            if (resdata && resdata.length > 0) {
                if (resdata[0].logined === 0) {
                    $('#loginalert')
                        .text(resdata[0].Alert)
                        .show();
                } else if (resdata[0].logined === 1) {
                    window.location.href = "/";
                }
            }
        },
        error: function(jqXHR, textStatus, errorThrown) {
          console.error('登录失败:', textStatus, errorThrown);
        }
    });
}

$(document).ready(function() {

    let currentYear = new Date().getFullYear();
    $('.footer p').text("Copyright © wangcw 2020-" + currentYear + " All Rights Reserved");

    $('#loginform').on('submit', function(e) {
        e.preventDefault();
        login();
    });
});
