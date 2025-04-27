/*!
 *
 * @author wangcw
 * @copyright (C) 2025, REDGREAT
 * Created : 2025-04-27
 *
 * Module : location-responsive.js
 * 处理地图响应式布局
 *
 */

$(document).ready(function() {
    // 检测侧边栏状态
    function checkSidebarStatus() {
        // 检查侧边栏是否展开
        if ($('.sidebar').hasClass('expanded') || $('.sidebar').css('width') !== '0px') {
            $('body').addClass('sidebar-open');
        } else {
            $('body').removeClass('sidebar-open');
        }
    }

    // 初始检查
    checkSidebarStatus();

    // 监听窗口大小变化
    $(window).resize(function() {
        checkSidebarStatus();
    });

    // 监听侧边栏切换按钮点击
    $('.sidebar-toggle, .sidebar-close').on('click', function() {
        // 延迟执行以确保侧边栏状态已更新
        setTimeout(checkSidebarStatus, 50);
    });

    // 调整地图容器大小以适应页面
    function adjustMapContainer() {
        const headerHeight = $('header').outerHeight() || 56;
        const footerHeight = $('footer').outerHeight() || 0;
        
        $('.map-wrapper').css({
            'top': headerHeight + 'px',
            'bottom': footerHeight + 'px'
        });
        
        // 触发地图重绘
        if (window.AMap && window.mapwong) {
            window.mapwong.resize();
        }
    }

    // 初始调整
    adjustMapContainer();

    // 监听窗口大小变化时调整地图
    $(window).resize(function() {
        adjustMapContainer();
    });
});
