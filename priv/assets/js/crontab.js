/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-04-02 19:49:43
 *
 * Module : crontab.js
 *
 */

function translateColumnNames(columnName) {
  const translations = i18nCrontab.columnName[defaultLanguage];
  return translations[columnName] || columnName;
}

function loadCrontabData(cronName) {
    let dynamicColumns = [];
    let dynamicDatas = [];

    $.getJSON('/data/crontab/' + cronName, function (response) {

        function buildDynamicData(response) {
            response.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumns.push(dynamicColumn);
            });
            dynamicDatas = response.data;
        }

        if (response && response.length > 0 && response[0].Alert) {
            showWarningToast(response[0].Alert);
        }
        else if (response && response.data.length === 0) {
            showWarningToast("未查询到任务！");
            response.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumns.push(dynamicColumn);
            });
        }
        else {
            buildDynamicData(response)
        }

        $('#table-crontab').DataTable().destroy();
        $('#table-crontab').empty();
        $('#table-crontab').DataTable({
            // lengthChange: true,  //是否允许用户改变表格每页显示的记录数
            // bStateSave: true,  //记录cookie
            destroy: true, // 销毁重新渲染
            columns: dynamicColumns,
            data: dynamicDatas,
            responsive: true,
            info: true, // 是否显示左下角分页信息
            processing: true,  //是否显示处理状态(排序的时候，数据很多耗费时间长的话，也会显示这个)
            orderMulti: true,  //启用多列排序
            ordering: true,  //使用排序
            paging: true,  //是否分页
            pageLength: 10, //每页默认行数
            lengthChange: false, //是否可以改变每页显示的记录数
            pagingType: "full_numbers",  //除首页、上一页、下一页、末页四个按钮还有页数按钮
            searching: false,  //是否开始本地搜索
            stateSave: true,  //刷新时是否保存状态
            // autoWidth: true,  //自动计算宽度
            deferRender: true, // 延迟渲染
            language: {
                // decimal: "",//小数的小数位符号  比如“，”作为数字的小数位符号
                // infoFiltered: "(从 _MAX_ 条记录过滤)",//当表格过滤的时候，将此字符串附加到主要信息
                // infoPostFix: "",//在摘要信息后继续追加的字符串
                // search: "搜索",//用来描述搜索输入框的字符串
                // zeroRecords: "没有找到",//当没有搜索到结果时，显示
                info: "当前 _START_ 条到 _END_ 条 共 _TOTAL_ 条",//左下角的信息，变量可以自定义，到官网详细查看
                infoEmpty: "无记录",//当没有数据时，左下角的信息
                emptyTable: "未查到数据",//当表格为空时，表格中信息
                thousands: ",",//千分位分隔符
                lengthMenu: "每页 _MENU_ 条记录",//用来描述分页长度选项的字符串
                loadingRecords: "加载中...",//用来描述数据在加载中等待的提示字符串 - 当异步读取数据的时候显示
                processing: "处理中...",//用来描述加载进度的字符串
                paginate: {
                  first: "首页",
                  previous: "上一页",
                  next: "下一页",
                  last: "尾页"
                  },
                aria: {
                   sortAscending: "：激活以按升序排序此列",
                   sortDescending: ": 激活以按降序排序此列"
                }
            }
        });
    })
}


$(document).ready(function() {
    // $('#cronname').val('all');
    loadCrontabData("all");

    $('#searchCrontab').click(function() {
        const cronName = $('#cronname').val();
        loadCrontabData(cronName);
    });

    $('#cleanCrontab').click(function() {
        $('input[type="text"]').val('');
    });

});
