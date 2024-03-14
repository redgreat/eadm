/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-12 11:16
 *
 * Module : utils-datetime.js
 *
 */

// 初始化时间参数
jQuery('#starttime').datetimepicker();
jQuery('#endtime').datetimepicker();

formatDateToNearestTenMinutes = function(date) {
    // 日期格式化
    const minutes = date.getMinutes();
    const roundedMinutes = Math.round(minutes / 10) * 10;
    const roundedDate = new Date(date);
    roundedDate.setMinutes(roundedMinutes);

    const year = roundedDate.getFullYear();
    const month = ('0' + (roundedDate.getMonth() + 1)).slice(-2);
    const day = ('0' + roundedDate.getDate()).slice(-2);
    const hours = ('0' + roundedDate.getHours()).slice(-2);
    const formattedMinutes = ('0' + roundedDate.getMinutes()).slice(-2);

    return year + '-' + month + '-' + day + ' ' + hours + ':' + formattedMinutes + ':00';
}

const now = new Date();
const oneDayBefore = new Date(now - 24 * 60 * 60 * 1000);
const defaultStartTime = formatDateToNearestTenMinutes(oneDayBefore);
const defaultEndTime = formatDateToNearestTenMinutes(now);
$('#starttime').val(defaultStartTime);
$('#endtime').val(defaultEndTime);

window.utilsdatime = window.utilsdatime || {};
window.utilsdatime.formatDateToNearestTenMinutes = formatDateToNearestTenMinutes;
