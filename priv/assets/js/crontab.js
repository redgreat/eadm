/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-04-02 19:49:43
 *
 * Module : crontab.js
 *
 */

// 初始化日期时间选择器
jQuery(document).ready(function() {
    // 新增任务模态框
    jQuery('#starttime').datetimepicker({
        format: 'Y-m-d H:i:s',
        step: 10
    });

    jQuery('#endtime').datetimepicker({
        format: 'Y-m-d H:i:s',
        step: 10
    });

    // 编辑任务模态框
    jQuery('#starttime-edit').datetimepicker({
        format: 'Y-m-d H:i:s',
        step: 10
    });

    jQuery('#endtime-edit').datetimepicker({
        format: 'Y-m-d H:i:s',
        step: 10
    });
});

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
                className: 'action-column', // 添加自定义类名，用于CSS样式
                width: '220px', // 设置列宽
                render: function (data, type, full, meta) {
                    return `
                        <button class="btn btn-outline-primary btn-rounded cron-edit-btn"
                          data-bs-toggle="modal" data-bs-target="#modal-cron-edit"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="编辑任务">
                          <i class="fas fa-pen"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded cron-disable-btn"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="启禁任务">
                          <i class="fas fa-toggle-on"></i>
                        </button>
                        <button class="btn btn-outline-primary btn-rounded cron-detail-btn"
                          data-bs-toggle="modal" data-bs-target="#modal-cron-detail"
                          data-bs-toggle="tooltip" data-bs-placement="top" title="任务日志">
                          <i class="fas fa-list"></i>
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
            },{
                targets: 6,
                render: function(data) {
                    return data === "启用" ?
                        '<span class="badge bg-success">启用</span>' :
                        '<span class="badge bg-danger">禁用</span>';
                }
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

function loadCronDetail(cronId) {
    let dynamicColumns = [];
    let dynamicDatas = [];

    $.getJSON('/crontab/detail/' + cronId, function (response) {

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
            showWarningToast("未查询到任务明细！");
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

        $('#table-cron-detail').DataTable().destroy();
        $('#table-cron-detail').empty();
        $('#table-cron-detail').DataTable({
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
        cronExp: $('#cronexp').val(),
        cronModule: $('#cronmodule').val(),
        startTime: $('#starttime').val() || formatDateTime(new Date()),
        endTime: $('#endtime').val()
    };
    $.ajaxSetup({async:false});
    $.ajax({
        url: '/crontab/add',
        type: 'POST',
        data: AddParams,
        success: function (resdata) {
            if (resdata && resdata.status === true) {
                showSuccessToast(resdata.message);
                if (resdata.refresh) {
                    loadCronData("");
                }
            } else {
                showWarningToast(resdata.message || "服务器运行错误，请联系管理员！");
            }
        }
    });
}

/**
 * 编辑定时任务
 * @param {string} currentCronId - 任务ID
 */
function editCron(currentCronId) {
    // 显示加载中提示
    showInfoToast('正在提交数据，请稍候...');

    const editParams = {
        cronId: currentCronId,
        cronName: $('#cronname-edit').val(),
        cronExp: $('#cronexp-edit').val(),
        cronModule: $('#cronmodule-edit').val(),
        startTime: $('#starttime-edit').val(),
        endTime: $('#endtime-edit').val()
    };

    $.ajax({
        url: '/crontab/edit',
        type: 'POST',
        data: editParams,
        timeout: 10000, // 10秒超时
        success: function (resdata) {
            if (resdata && resdata.status === true) {
                showSuccessToast(resdata.message);
                if (resdata.refresh) {
                    loadCronData("");
                }
                // 关闭编辑模态框
                $('#modal-cron-edit').modal('hide');
            } else {
                showWarningToast(resdata.message || "服务器运行错误，请联系管理员！");
            }
        },
        error: function(_, __, error) {
            showErrorToast('编辑任务失败: ' + error);
        }
    });
}

/**
 * 切换任务状态（启用/禁用）
 * @param {string} cronId - 任务ID
 */
function toggleCron(cronId) {
    if (typeof cronId !== 'undefined' && cronId !== null) {
        // 显示加载中提示
        showInfoToast('正在处理，请稍候...');

        $.ajax({
            url: '/crontab/toggle',
            type: 'POST',
            data: {
                cronId: cronId
            },
            timeout: 10000, // 10秒超时
            success: function(response) {
                if (response && response.status === true) {
                    showSuccessToast(response.message);
                    if (response.refresh) {
                        loadCronData("");
                    }
                } else if (response && response.length > 0 && response[0].Alert) {
                    const alertMessage = response[0].Alert;

                    // 检查是否包含"成功"关键字
                    if (alertMessage.includes('成功')) {
                        showSuccessToast(alertMessage);
                    } else {
                        // 显示API返回的具体错误信息
                        showWarningToast(alertMessage);
                    }

                    // 刷新任务列表
                    loadCronData("");
                } else if (response && response.message) {
                    showWarningToast(response.message);
                } else {
                    // 没有明确的消息，显示通用成功信息
                    showSuccessToast('操作已完成');
                    loadCronData("");
                }
            },
            error: function(xhr, textStatus, error) {
                try {
                    // 处理不同类型的错误
                    if (textStatus === 'timeout') {
                        showErrorToast('请求超时，请检查网络连接后重试');
                    } else {
                        // 尝试解析错误响应
                        const errorResponse = xhr.responseJSON || JSON.parse(xhr.responseText);
                        if (errorResponse && errorResponse.length > 0 && errorResponse[0].Alert) {
                            showErrorToast(errorResponse[0].Alert);
                        } else if (errorResponse && errorResponse.message) {
                            showErrorToast(errorResponse.message);
                        } else {
                            showErrorToast('切换任务状态失败: ' + error);
                        }
                    }
                } catch (e) {
                    showErrorToast('切换任务状态失败: ' + error);
                } finally {
                    // 无论如何都刷新任务列表，确保显示最新状态
                    loadCronData("");
                }
            }
        });
    }
}

function deleteCron(cronId) {
    if (typeof cronId !== 'undefined' && cronId !== null) {
        $.ajax({
            url: '/crontab/delete/' + cronId,
            type: 'DELETE',
            success: function (resdata) {
                if (resdata && resdata.status === true) {
                    showSuccessToast(resdata.message);
                    if (resdata.refresh) {
                        loadCronData("");
                    }
                } else if (resdata && resdata.length > 0 && resdata[0].Alert) {
                    showWarningToast(resdata[0].Alert);
                } else if (resdata && resdata.message) {
                    showWarningToast(resdata.message);
                } else {
                    showSuccessToast("数据删除成功！");
                }
            },
            error: function(_, __, error) {
                showWarningToast("任务删除失败：" + error);
            }
        });
    }
}

function cleanAddTab(){
    $('#cronname').val('');
    $('#cronexp').val('');
    $('#cronmodule').val('');
    $('#starttime').val('');
    $('#endtime').val('');
}

function cleanEditTab(){
    $('#cronname-edit').val('');
    $('#cronexp-edit').val('');
    $('#cronmodule-edit').val('');
    $('#starttime-edit').val('');
    $('#endtime-edit').val('');
}

/**
 * 格式化日期时间为字符串
 * @param {Date} date - 日期对象
 * @returns {string} - 格式化后的日期时间字符串 (YYYY-MM-DD HH:MM:SS)
 */
function formatDateTime(date) {
    const year = date.getFullYear();
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');

    return `${year}-${month}-${day} ${hours}:${minutes}:${seconds}`;
}
$(document).ready(function() {
    // 初始化表格
    loadCronData("");

    // 搜索按钮点击事件
    $('#searchCron').click(function() {
        loadCronData($('#cronname-search').val());
    });

    // 清空按钮点击事件
    $('#cleanCron').click(function() {
        $('#cronname-search').val('');
        loadCronData("");
    });

    // 添加任务按钮点击事件
    $('#addCron').click(function() {
        $('#starttime').val(formatDateTime(new Date()));
        $('#endtime').val("");
    });

    // 添加任务提交按钮点击事件
    $('#cron-add-submit-btn').click(function() {
        addCron();
        cleanAddTab();
    });

    // 添加任务取消按钮点击事件
    $('#cron-add-cancel-btn').click(function() {
        cleanAddTab();
    });

    // 编辑任务按钮点击事件
    $('#table-cron').on('click', '.cron-edit-btn', function() {
        const row = $(this).closest('tr');
        const cronId = row.attr('data-id');

        // 显示加载中提示
        showInfoToast('正在加载任务数据，请稍候...');

        // 从API获取最新的任务数据
        $.ajax({
            url: '/crontab',
            type: 'GET',
            data: {
                cronName: '' // 空字符串获取所有任务
            },
            success: function(response) {
                if (response && response.data && Array.isArray(response.data)) {
                    // 查找对应ID的任务
                    const taskData = response.data.find(task => task.id === cronId);

                    if (taskData) {
                        // 正确填充编辑模态框中的字段
                        $('#cronname-edit').val(taskData.cronname || '');
                        $('#cronexp-edit').val(taskData.cronexp || '');
                        $('#cronmodule-edit').val(taskData.cronmfa || '');
                        $('#starttime-edit').val(taskData.starttime || '');
                        $('#endtime-edit').val(taskData.endtime || '');
                        $('#cron-edit-submit-btn').attr('data-id', cronId);

                        // 显示编辑模态框
                        $('#modal-cron-edit').modal('show');
                    } else {
                        showWarningToast('未找到任务数据，请刷新页面后重试');
                    }
                } else {
                    showWarningToast('加载任务数据失败，请刷新页面后重试');
                }
            },
            error: function(_, __, error) {
                showErrorToast('加载任务数据失败: ' + error);
            }
        });
    });

    // 编辑任务提交按钮点击事件
    $('#cron-edit-submit-btn').click(function() {
        const cronId = $(this).attr('data-id');
        editCron(cronId);
        cleanEditTab();
    });

    // 编辑任务取消按钮点击事件
    $('#cron-edit-cancel-btn').click(function() {
        cleanEditTab();
    });

    // 启用/禁用任务按钮点击事件
    $('#table-cron').on('click', '.cron-disable-btn', function() {
        const row = $(this).closest('tr');
        const cronId = row.attr('data-id');
        toggleCron(cronId);
    });

    // 删除任务按钮点击事件
    $('#table-cron').on('click', '.cron-delete-btn', function() {
        const row = $(this).closest('tr');
        const cronId = row.attr('data-id');
        // 设置当前要删除的任务ID
        $('#cron-del-confirm-btn').attr('data-id', cronId);
        // 显示确认删除模态框
        $('#cron-del-confirm').modal('show');
    });

    // 删除确认按钮点击事件
    $('#cron-del-confirm-btn').click(function() {
        const cronId = $(this).attr('data-id');
        deleteCron(cronId);
    });

    // 任务详情按钮点击事件
    $('#table-cron').on('click', '.cron-detail-btn', function() {
        const row = $(this).closest('tr');
        const cronId = row.attr('data-id');
        const cronName = row.find('td:eq(1)').text();
        $('#cron-detail-title').text('任务日志：' + cronName);
        loadCronDetail(cronId);
    });
});
