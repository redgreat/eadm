/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-07-10 11:16
 *
 * Module : utils.js
 * 通用工具函数
 */

/**
 * 格式化日期时间
 * @param {string} dateTimeStr - 日期时间字符串
 * @returns {string} - 格式化后的日期时间
 */
function formatDateTime(dateTimeStr) {
    if (!dateTimeStr) return '';
    const date = new Date(dateTimeStr);
    return date.getFullYear() + '-' +
           padZero(date.getMonth() + 1) + '-' +
           padZero(date.getDate()) + ' ' +
           padZero(date.getHours()) + ':' +
           padZero(date.getMinutes()) + ':' +
           padZero(date.getSeconds());
}

/**
 * 数字补零
 * @param {number} num - 数字
 * @returns {string} - 补零后的字符串
 */
function padZero(num) {
    return num < 10 ? '0' + num : num;
}

/**
 * 显示成功提示
 * @param {string} message - 提示信息
 */
function showSuccessToast(message) {
    const toastElList = [].slice.call(document.querySelectorAll('.toast'));
    const toastList = toastElList.map(function (toastEl) {
        const toastHeaderEl = toastEl.querySelector('.toast-header svg rect');
        const toastBodyEl = toastEl.querySelector('.toast-body');

        // 设置成功颜色（绿色）
        if (toastHeaderEl) {
            toastHeaderEl.setAttribute('fill', '#28a745');
        }

        toastBodyEl.textContent = message;
        return new bootstrap.Toast(toastEl);
    });
    toastList.forEach(toast => toast.show());
}

/**
 * 显示警告提示
 * @param {string} message - 提示信息
 */
function showWarningToast(message) {
    const toastElList = [].slice.call(document.querySelectorAll('.toast'));
    const toastList = toastElList.map(function (toastEl) {
        const toastHeaderEl = toastEl.querySelector('.toast-header svg rect');
        const toastBodyEl = toastEl.querySelector('.toast-body');

        // 设置警告颜色（黄色）
        if (toastHeaderEl) {
            toastHeaderEl.setAttribute('fill', '#ffc107');
        }

        toastBodyEl.textContent = message;
        return new bootstrap.Toast(toastEl);
    });
    toastList.forEach(toast => toast.show());
}

/**
 * 显示错误提示
 * @param {string} message - 提示信息
 */
function showErrorToast(message) {
    const toastElList = [].slice.call(document.querySelectorAll('.toast'));
    const toastList = toastElList.map(function (toastEl) {
        const toastHeaderEl = toastEl.querySelector('.toast-header svg rect');
        const toastBodyEl = toastEl.querySelector('.toast-body');

        // 设置错误颜色（红色）
        if (toastHeaderEl) {
            toastHeaderEl.setAttribute('fill', '#dc3545');
        }

        toastBodyEl.textContent = message;
        return new bootstrap.Toast(toastEl);
    });
    toastList.forEach(toast => toast.show());
}

/**
 * 显示信息提示
 * @param {string} message - 提示信息
 */
function showInfoToast(message) {
    const toastElList = [].slice.call(document.querySelectorAll('.toast'));
    const toastList = toastElList.map(function (toastEl) {
        const toastHeaderEl = toastEl.querySelector('.toast-header svg rect');
        const toastBodyEl = toastEl.querySelector('.toast-body');

        // 设置信息颜色（蓝色）
        if (toastHeaderEl) {
            toastHeaderEl.setAttribute('fill', '#007aff');
        }

        toastBodyEl.textContent = message;
        return new bootstrap.Toast(toastEl);
    });
    toastList.forEach(toast => toast.show());
}

// 导出工具函数
window.utils = window.utils || {};
window.utils.formatDateTime = formatDateTime;
window.utils.padZero = padZero;
window.utils.showSuccessToast = showSuccessToast;
window.utils.showWarningToast = showWarningToast;
window.utils.showErrorToast = showErrorToast;
window.utils.showInfoToast = showInfoToast;

// 为了兼容现有代码，将函数暴露到全局作用域
window.formatDateTime = formatDateTime;
window.padZero = padZero;
window.showSuccessToast = showSuccessToast;
window.showWarningToast = showWarningToast;
window.showErrorToast = showErrorToast;
window.showInfoToast = showInfoToast;
