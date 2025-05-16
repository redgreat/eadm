/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-01-07 11:29:20
 *
 * Module : basic.js
 *
 */

function loadMemu() {
    $.getJSON('/permission', function (resdata) {
        let menuHtml = '';
        if ('data' in resdata[0]) {
            const resobj = resdata[0].data;
            if (resobj.dashboard === true) {
                menuHtml += '<li> <a href="/"><i class="fas fa-home"></i> 信息看板</a></li>';
            }
            if (resobj.health === true) {
                menuHtml += '<li><a href="/menu/health"><i class="fas fa-heartbeat"></i> 我的健康</a></li>';
            }
            if (resobj.locate === true) {
                menuHtml += '<li><a href="/menu/location"><i class="fas fa-map-marked-alt"></i> 轨迹回放</a></li>';
            }
            if (resobj.finance.finlist === true) {
                menuHtml += '<li><a href="/menu/finance"><i class="fas fa-money-bill"></i> 我的财务</a></li>';
            }
            if (resobj.crontab === true) {
                menuHtml += '<li><a href="/menu/crontab"><i class="fas fa-hourglass-half"></i> 定时任务</a></li>';
            }
            if (resobj.usermanage === true) {
                menuHtml += '<li><a href="/menu/user"><i class="fas fa-user"></i> 用户信息</a></li>';
            }
            if (resobj.device && resobj.device.devlist === true) {
                menuHtml += '<li><a href="/menu/device"><i class="fas fa-mobile-alt"></i> 设备管理</a></li>';
            }
            $('#menu-container').prepend(menuHtml);
        }
    });
}

$(document).ready(function() {

    loadMemu();

    $('#sidebarCollapse').on('click', function() {
        $('#sidebar').toggleClass('active');
        $('#body').toggleClass('active');
    });

    let lastScrollTop = 0;
    $(window).scroll(function() {
        const scrollTop = $(this).scrollTop();
        if (scrollTop > lastScrollTop && scrollTop > 10 && !$('#sidebar').hasClass('active')) {
            $('#sidebar').addClass('active');
            $('#body').addClass('active');
        }
        lastScrollTop = scrollTop;
    });

    let currentYear = new Date().getFullYear();
    $('.footer p').text("Copyright © wangcw 2020-" + currentYear + " All Rights Reserved");

    $('body').on('click', '.dropdown-item', function(e) {
        e.preventDefault();
        let clickedId = $(this).attr('id');
        if (clickedId === 'userinfo') {
            $.getJSON('/userinfo', function (resdata) {
                $('#loginname-self').val(resdata[0]);
                $('#username-self').val(resdata[1]);
                $('#email-self').val(resdata[2]);
            });
        } else if (clickedId === 'passwordchg') {
            $('#password-old').val('');
            $('#password-new').val('');
            $('#password-new-confirm').val('');
        } else if (clickedId === 'logout') {
            $.ajax({
                url: '/logout',
                type: 'POST',
                success: function() {
                    window.location.href = '/login';
                },
                error: function(jqXHR, textStatus, errorThrown) {
                    console.error('退出登录失败:', textStatus, errorThrown);
                }
            });
        }
    });

    $('#userinfo-edit-btn').click(function () {
        $('#username-self').prop('readonly', false);
        $('#email-self').prop('readonly', false);
    });

    $('#userinfo-submit-btn').click(function () {
        const editParams = {
            loginName: $('#loginname-self').val(),
            email: $('#email-self').val(),
            userName: $('#username-self').val()
        };
        $.ajaxSetup({async:false});
        $.ajax({
            url: '/useredit',
            type: 'POST',
            data: editParams,
            success: function (resdata) {
                if (resdata && resdata.length > 0 && resdata[0].Alert) {
                    showWarningToast(resdata[0].Alert);
                }
            }
        });
    });

    $('#password-submit-btn').click(function () {
        const passwordNew = $('#password-new').val();
        const passwordConfirm = $('#password-new-confirm').val();
        const paswordParams = {
            passwordOld: $('#password-old').val(),
            passwordNew: passwordNew
        };
        if (passwordNew === passwordConfirm){
            $.ajax({
                url: '/userpwd',
                type: 'POST',
                data: paswordParams,
                success: function (resdata) {
                    if (resdata && resdata.length > 0 && resdata[0].Alert) {
                        showWarningToast(resdata[0].Alert);
                    }
                }
            });
        } else (
            showWarningToast("两次密码输入不一致，请确认！")
        );
    });
});

// 公用函数
window.showWarningToast = window.showWarningToast || {};

function showWarningToast(message) {
    const toastElList = [].slice.call(document.querySelectorAll('.toast'));
    const toastList = toastElList.map(function (toastEl) {
        const toastBodyEl = toastEl.querySelector('.toast-body');
        toastBodyEl.textContent = message;
        return new bootstrap.Toast(toastEl);
    });
    toastList.forEach(toast => toast.show());
}

// 配置常量
defaultLanguage = 'zh';
