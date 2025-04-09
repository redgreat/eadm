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
    // 禁用登录按钮，避免重复点击
    $('#user-login-btn').prop('disabled', true);
    
    const formData = {
        loginName: $('#loginname').val(),
        password: $('#password').val()
    };
    // 移除同步设置，使用异步请求
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
                    // 登录失败时启用登录按钮
                    $('#user-login-btn').prop('disabled', false);
                } else if (resdata[0].logined === 1) {
                    // 修改按钮文本提示用户
                    $('#user-login-btn').text('登录成功，跳转中...');
                    // 添加短暂延迟，确保cookie有时间设置
                    setTimeout(function() {
                        window.location.href = "/";
                    }, 500);
                }
            } else {
                // 无效响应时启用登录按钮
                $('#user-login-btn').prop('disabled', false);
                $('#loginalert')
                    .text('登录失败，请重试')
                    .show();
            }
        },
        error: function(jqXHR, textStatus, errorThrown) {
          console.error('登录失败:', textStatus, errorThrown);
          // 发生错误时启用登录按钮
          $('#user-login-btn').prop('disabled', false);
          $('#loginalert')
              .text('网络错误，请稍后重试')
              .show();
        }
    });
}

$(document).ready(function() {
    let currentYear = new Date().getFullYear();
    $('.footer p').text("Copyright © wangcw 2020-" + currentYear + " All Rights Reserved");
});

$('#user-login-btn').click(function(e) {
    e.preventDefault();
    login();
});

// 添加回车键触发登录功能
$('#loginname, #password').keypress(function(e) {
    if (e.which === 13) { // 回车键的键码是13
        e.preventDefault();
        login();
    }
});
