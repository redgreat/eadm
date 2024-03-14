/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-02-07 11:29:20
 *
 * Module : login.js
 *
 */

document.querySelector('.footer').querySelector('p').textContent = "Copyright © wangcw 2020-" + new Date().getFullYear() + " All Rights Reserved";
window.onload = function() {
    var urlParams = new URLSearchParams(window.location.search);
    if (urlParams.has('error')) {
        var error = urlParams.get('error');
        if (error === 'invalid_credentials') {
            // document.getElementById('loginfailed').textContent = "用户名或密码错误，请重新登录！";
            document.getElementById('loginfailed').style.display = 'block';
        }
    }
};
