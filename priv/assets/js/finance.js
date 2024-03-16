/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-03-15 08:41:01
 *
 * Module : finance.js
 *
 */

function translateColumnNames(column, dictionary) {
    return dictionary[column] || column;
}

function open_modal(id) {
    $.getJSON('/sys/processes/' + id, function (data) {
        $('#info-init-call').html(mfa(data['initial_call']));
        $('#info-current-func').html(mfa(data['current_function']));
        $('#info-reg-name').html(data['registered_name']);
        $('#info-status').html(data['status']);
        $('#info-msg-queue').html(data['message_queue_len']);
        $('#info-grp-leader').html('<a href="javascript:open_modal(\'' + data['group_leader'] + '\');">' + data['group_leader'] + '</a>');
    })
}

function loadFinanceData(sourceType, inorOut, startTime, endTime) {
    const searchParams = {
        sourceType: sourceType,
        inorOut: inorOut,
        startTime: startTime,
        endTime: endTime
    };
    let dynamicColumns = [];
    let dynamicDatas = [];

    $.getJSON('/data/finance', searchParams, function (response) {

        function buildDynamicData(response) {
            response.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column, i18nFinance.columnName);
                dynamicColumn['className'] = "dataTables-column";
                dynamicColumns.push(dynamicColumn);
                if (column === "SourceType") {
                    response.data.forEach(function (rowData) {
                    rowData["SourceType"] = translateColumnNames(rowData["SourceType"], i18nFinance.sourceType);
                    });
                }
            });
            dynamicDatas = response.data;
        }

        if (response && response.length > 0 && response[0].Alert) {
            const toastElList = [].slice.call(document.querySelectorAll('.toast'));
            const toastList = toastElList.map(function (toastEl) {
                const toastBodyEl = toastEl.querySelector('.toast-body');
                toastBodyEl.textContent = response[0].Alert;
                return new bootstrap.Toast(toastEl);
            });
            toastList.forEach(toast => toast.show());
        }
        else if (response && response.data.length === 0) {
            const toastElList = [].slice.call(document.querySelectorAll('.toast'));
            const toastList = toastElList.map(function (toastEl) {
                const toastBodyEl = toastEl.querySelector('.toast-body');
                toastBodyEl.textContent = "此时间段内无财务数据！";
                return new bootstrap.Toast(toastEl);
            });
            toastList.forEach(toast => toast.show());
        }
        else {
            buildDynamicData(response);
        }

        $('#dataTables-finance').DataTable().destroy();
        $('#dataTables-finance').empty();
        $('#dataTables-finance').DataTable({
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
        $('#dataTables-finance').DataTable().column(0).visible(false);
    })
}

$(document).ready(function() {

    loadFinanceData($('#sourceType').val(), $('#inorOut').val(), defaultStartTime, defaultEndTime);

    $('#searchFinance').click(function () {
        const sourceType = $('#sourceType').val();
        const inorOut = $('#inorOut').val();
        const startTime = $('#starttime').val();
        const endTime = $('#endtime').val();
        loadFinanceData(sourceType, inorOut, startTime, endTime);
    });

    $('#cleanFinance').click(function () {
        $('input[type="text"]').val('');
    });

    // $('#bill-info').modal('show');

    // $('#bill tbody tr').click(function () {
    //     return false;
    // })
    //     .dblclick(function () {
    //         open_modal($(this).data('id'));
    //     });

    // $('#modal-menu a').click(function(event) {
    //     event.preventDefault();
    //     if($(this).hasClass('active')) {
    //     return;
    //     }
    //     let old_target = $('#modal-menu .active').attr('href');
    //     let target = $(this).attr('href');
    //     $('#modal-menu a').removeClass('active');
    //     $(this).addClass('active');
    //     $('#'+old_target).hide();
    //     $('#'+target).show();
    // });

});
