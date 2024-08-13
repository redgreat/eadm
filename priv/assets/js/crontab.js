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

function loadCronData(cronName) {
    let dynamicColumns = [];
    let dynamicDatas = [];
    const searchParams = {
        cronName: cronName
    };

    $.getJSON('/crontab', searchParams, function (response) {

        function buildDynamicData(response) {
            response.columns.forEach(function (column) {
                let dynamicColumn = {};
                dynamicColumn['data'] = column;
                dynamicColumn['title'] = translateColumnNames(column);
                dynamicColumns.push(dynamicColumn);
            });
            dynamicColumns.push({"data": "Action", "title": "操作"});
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

        $('#table-cron').DataTable().destroy();
        $('#table-cron').empty();
        $('#table-cron').DataTable({
            columnDefs: [{
                targets: -1,
                render: function (data, type, full, meta) {
                    return `
                        <button class="btn btn-outline-primary btn-rounded cron-detail-btn"
                          data-bs-toggle="modal" data-bs-target="#modal-cron-detail"
                          data-bs-placement="top" title="任务日志">
                          <i class="fas fa-list"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded cron-edit-btn"
                          data-bs-toggle="modal" data-bs-target="#modal-cron-edit"
                          data-bs-placement="top" title="编辑任务信息">
                          <i class="fa-fw select-all fas"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded cron-disable-btn"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="启禁任务">
                          <i class="fas fa-toggle-on"></i>
                        </button>
                        <button class="btn btn-outline-danger btn-rounded cron-delete-btn"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="删除任务">
                          <i class="fas fa-trash"></i>
                        </button>
                    `;
                }
            },{
                targets: 0,
                visible: false,
                orderable: false
            }],
            createdRow: function(row, data) {
                let dataId = data['id'];
                $(row).attr('data-id', dataId);
            },
            destroy: true,
            columns: dynamicColumns,
            data: dynamicDatas,
            responsive: true,
            info: true,
            processing: true,
            orderMulti: true,
            ordering: true,
            paging: true,
            pageLength: 10,
            lengthChange: false,
            pagingType: "full_numbers",
            searching: false,
            stateSave: true,
            deferRender: true,
            language: {
                info: "当前 _START_ 条到 _END_ 条 共 _TOTAL_ 条",
                infoEmpty: "无记录",
                emptyTable: "未查到数据",
                thousands: ",",
                lengthMenu: "每页 _MENU_ 条记录",
                loadingRecords: "加载中...",
                processing: "处理中...",
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

function addCron() {
    const AddParams = {
        cronName: $('#cronname').val(),
        cronType: $('#crontype').val(),
        cronExp: $('#cronexp').val(),
        cronModule: $('#cronmodule').val(),
        startTime: $('#starttime').val() || defaultEndTime,
        endTime: $('#endtime').val()
    };
    $.ajaxSetup({async:false});
    $.ajax({
        url: '/crontab/add',
        type: 'POST',
        data: AddParams,
        success: function (resdata) {
            if (resdata && resdata.length > 0 && resdata[0].Alert) {
                showWarningToast(resdata[0].Alert);
            } else {
                showWarningToast("服务器运行错误，请联系管理员！");
            }
        }
    });
}

function cleanAddTab(){
    $('#cronname').val('');
    $('#crontype').val('');
    $('#cronexp').val('');
    $('#cronmodule').val('');
    $('#starttime').val('');
    $('#endtime').val('');
}

$(document).ready(function() {
    loadCronData("");

    $('#searchCron').click(function() {
        const cronName = $('#cronname').val();
        loadCronData(cronName);
    });

    $('#cleanCron').click(function() {
        $('input[type="text"]').val('');
    });

    $('#addCron').click(function() {
        $('#starttime').val(defaultEndTime);
        $('#endtime').val("");
    });

    $('#cron-add-submit-btn').click(function () {
        addCron();
        loadCronData("");
        cleanAddTab();
    });

    $('#cron-add-cancel-btn').click(function () {
        cleanAddTab();
    });

    let dataTableCron = $('#table-cron').DataTable();
    dataTableCron.on('click', '.cron-edit-btn', function() {
        let editRow = $(this).closest('tr');
        $('#cronname-edit').val(editRow.find('td')[1].innerText);
        $('#crontype-edit').val(editRow.find('td')[2].innerText);
        $('#cronexp-edit').val(editRow.find('td')[3].innerText);
        $('#cronmodule-edit').val(editRow.find('td')[4].innerText);
        $('#starttime-edit').val(editRow.find('td')[5].innerText);
        $('#endtime-edit').val(editRow.find('td')[6].innerText);
    });

    dataTableCron.on('click', '.cron-disable-btn', function() {
        let disableRow = $(this).closest('tr');
        let cronId = disableRow.data('id');
        if (cronId !== "未查到数据" && typeof cronId !== 'undefined' && cronId !== null) {
            disableCron(cronId);
            setTimeout(function () {
                loadCronData("");
            }, 100);
        } else {
            showWarningToast("未查到需禁用任务，请刷新页面重试!");
        }
    });

    dataTableCron.on('click', '.cron-delete-btn', function() {
        let delRow = $(this).closest('tr');
        $('#cron-del-confirm').modal('show');
        $('#cron-del-confirm-btn').click(function () {
            let cronId = delRow.data('id');
            if (cronId !== "未查到数据" && typeof cronId !== 'undefined' && cronId !== null) {
                deleteCron(cronId);
                delRow.remove();
                setTimeout(function () {
                    dataTableCron.draw(false);
                }, 100);
            } else {
                showWarningToast("未查到需删除数据，请刷新页面重试!");
            }
        });
    });
});
