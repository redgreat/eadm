/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-01-07 11:29:20
 *
 * Module : basic.js
 *
 */

(function() {
    'use strict';

    // Toggle sidebar on Menu button click
    $('#sidebarCollapse').on('click', function() {
        $('#sidebar').toggleClass('active');
        $('#body').toggleClass('active');
    });

    // Auto-hide sidebar on window resize if window size is small
    // $(window).on('resize', function () {
    //     if ($(window).width() <= 768) {
    //         $('#sidebar, #body').addClass('active');
    //     }
    // });
})();


$(document).ready(function() {
          $('body').on('click', '.dropdown-item', function(e) {
            if ($(this).text().trim() === '退出') {
              e.preventDefault();
              $.ajax({
                url: '/logout',
                type: 'POST',
                data: {},
                success: function(response) {
                  console.log('退出登录成功:', response);
                  window.location.href = '/login';
                },
                error: function(jqXHR, textStatus, errorThrown) {
                  console.error('退出登录失败:', textStatus, errorThrown);
                }
              });
            }
          });
      });

document.getElementById('footer').querySelector('p').textContent = "Copyright © wangcw 2020-" + new Date().getFullYear() + " All Rights Reserved";
